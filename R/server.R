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
  
  # --- Sidebar panes (render) ------------------------------------------
  output$subsidebar <- renderUI({
    cur <- input$mainmenu %||% "text"
    switch(cur,
           text   = text_pane_ui(rv),
           theme  = theme_pane_ui(rv),
           grid   = grid_pane_ui(rv),
           export = export_pane_ui(rv),
           axes   = axes_pane_ui(rv),
           div(helpText("Pick a section from the left."))
    )
  })
  
  # --- Tabs & previews (via tabs_preview.R) ----------------------------
  output$tabs_area  <- renderUI({ tabs_area_ui(rv) })
  output$preview_toolbar <- renderUI({ toolbar_ui(rv) })
  register_preview_outputs(output, rv)
  observe_tab_tracking(input, rv)
  
  # --- Pane observers ---------------------------------------------------
  register_grid_observers(input, rv, session)
  register_text_observers(input, rv, session)
  register_theme_observers(input, rv, session)
  register_axes_observers(input, rv, session)
  register_export_observers(input, rv, session)
  
  # --- Downloads --------------------------------------------------------
  # Active plot download (uses current active plot export settings)
  output$dl_current_plot <- downloadHandler(
    filename = function() {
      ap <- rv$active_tab %||% "plot"
      ex <- rv$export[[ap]] %||% list()
      fmt <- toupper(ex$format %||% BASE$format)
      ext <- switch(fmt, PNG = "png", TIFF = "tiff", PDF = "pdf", SVG = "svg", EPS = "eps", "png")
      paste0(ap, ".", ext)
    },
    content = function(file) {
      ap <- rv$active_tab
      req(!is.null(ap), !identical(ap, "Grid"), !is.null(rv$plots[[ap]]))
      ensure_edits(rv, ap, grid = FALSE)
      p  <- apply_edits(rv$plots[[ap]], rv$edits[[ap]])
      ex <- rv$export[[ap]] %||% list()
      export_plot_file(
        p, file,
        fmt = toupper(ex$format %||% BASE$format),
        width_mm  = ex$width_mm  %||% BASE$width_mm,
        height_mm = ex$height_mm %||% BASE$height_mm,
        dpi       = ex$dpi       %||% BASE$dpi
      )
    }
  )
  
  # Grid download (uses grid_export + grid_edits + assigned cells)
  output$dl_grid <- downloadHandler(
    filename = function() {
      fmt <- toupper(rv$grid_export$format %||% BASE$format)
      ext <- switch(fmt, PNG = "png", TIFF = "tiff", PDF = "pdf", SVG = "svg", EPS = "eps", "png")
      paste0("grid.", ext)
    },
    content = function(file) {
      req(length(rv$plots) > 0)
      pw <- build_grid_patchwork(rv)
      export_plot_file(
        pw, file,
        fmt = toupper(rv$grid_export$format %||% BASE$format),
        width_mm  = rv$grid_export$width_mm  %||% BASE$width_mm,
        height_mm = rv$grid_export$height_mm %||% BASE$height_mm,
        dpi       = rv$grid_export$dpi       %||% BASE$dpi
      )
    }
  )
}
