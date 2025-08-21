# plot-editor/R/server.R
# Main server: package-style, uses the pane files & helpers you've got

app_server <- function(input, output, session) {
  # Require patchwork for grid preview
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("Please install the 'patchwork' package for grid previews.")
  }
  
  # --- Reactive state --------------------------------------------------
  rv <- init_reactive_state()
  rv$active_tab  <- "Grid"
  rv$is_hydrating <- FALSE
  
  # --- Demo loader & .rds loader --------------------------------------
  observeEvent(input$load_demo, {
    rv$plots <- list(
      Demo1 = ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) +
        geom_point(size = 3) + theme_minimal(base_size = BASE$base_size) +
        labs(title = "Fuel efficiency vs weight", subtitle = "Demo 1",
             x = "Weight (1000 lbs)", y = "MPG", color = "Cyl", caption = "mtcars"),
      Demo2 = ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
        geom_point(size = 2.5) + theme_bw(base_size = BASE$base_size) +
        labs(title = "Iris sepal", subtitle = "Demo 2", x = "Sepal L", y = "Sepal W", color = "Species"),
      Demo3 = ggplot(mpg, aes(displ, hwy, color = class)) +
        geom_point(alpha = 0.8) + theme_light(base_size = BASE$base_size) +
        labs(title = "Engine vs Hwy", subtitle = "Demo 3", x = "Displ", y = "Highway", color = "Class")
    )
    lapply(names(rv$plots), function(nm) ensure_edits(rv, nm))
    select_first_plot(rv, session)
  }, ignoreInit = TRUE)
  
  observeEvent(input$plots_rds, {
    files <- input$plots_rds
    if (is.null(files)) return()
    for (i in seq_len(nrow(files))) {
      obj <- try(readRDS(files$datapath[i]), silent = TRUE)
      if (inherits(obj, "ggplot")) {
        nm <- tools::file_path_sans_ext(files$name[i])
        rv$plots[[nm]] <- obj
        ensure_edits(rv, nm)
      }
    }
    select_first_plot(rv, session)
  }, ignoreInit = TRUE)
  
  # --- Tabs & previews -------------------------------------------------
  output$tabs_area <- renderUI({
    # Build a tabset with "Grid" + one tab per plot
    tabs <- list(
      tabPanel("Grid", value = "Grid", plotOutput("grid_preview", height = "70vh"))
    )
    for (nm in names(rv$plots)) {
      tabs[[length(tabs) + 1]] <- tabPanel(nm, value = nm,
                                           plotOutput(paste0("plot_prev_", nm), height = "70vh"))
    }
    do.call(tabsetPanel, c(id = "main_tabs", tabs))
  })
  
  # Track the tabset created in tabs_previews.R (id = "active_tabset")
  observeEvent(input$active_tabset, {
    rv$active_tab <- input$active_tabset
  }, ignoreInit = FALSE)
  
  # Per-plot previews
  observe({
    lapply(names(rv$plots), function(nm) {
      local({
        name <- nm
        output[[paste0("plot_prev_", name)]] <- renderPlot({
          req(!is.null(rv$plots[[name]]))
          ensure_edits(rv, name)
          apply_edits(rv$plots[[name]], rv$edits[[name]])
        })
      })
    })
  })
  
  # Grid preview (uses grid settings; lays out plots in order)
  output$grid_preview <- renderPlot({
    req(length(rv$plots) > 0)
    r <- rv$grid$rows %||% BASE$grid_rows
    c <- rv$grid$cols %||% BASE$grid_cols
    n <- r * c
    picked <- names(rv$plots)[seq_len(min(n, length(rv$plots)))]
    req(length(picked) > 0)
    
    plots <- lapply(picked, function(nm) {
      ensure_edits(rv, nm)
      apply_edits(rv$plots[[nm]], rv$edits[[nm]])
    })
    
    patchwork::wrap_plots(plots, nrow = r, ncol = c,
                          guides = if (isTRUE(rv$grid$collect %||% BASE$grid_collect)) "collect" else "keep") +
      theme(legend.position = legend_pos_value(rv$grid$legend %||% BASE$grid_legend_pos))
  })
  
  # --- Sidebar panes (render) ------------------------------------------
  output$subsidebar <- renderUI({
    cur <- input$mainmenu %||% "text"
    switch(cur,
           text   = text_pane_ui(rv),
           theme  = theme_pane_ui(rv),
           grid   = grid_pane_ui(rv),
           export = export_pane_ui(rv),
           div(helpText("Pick a section from the left."))
    )
  })
  
  # --- Sidebar panes (observers) ---------------------------------------
  output$tabs_area  <- renderUI({ tabs_area_ui(rv) })
  register_preview_outputs(output, rv)
  register_grid_observers(input, rv, session)
  register_text_observers(input, rv, session)
  register_theme_observers(input, rv, session)
  register_export_observers(input, rv, session)
}
