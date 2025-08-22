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
  rv$last_mainmenu <- "text"  # prefer Text by default on plot tabs
  
  # --- Menu item activation logic --------------------------------------
  menu_activation <- reactive({
    active_tab <- rv$active_tab
    has_plots <- length(rv$plots) > 0
    list(
      grid  = identical(active_tab, "Grid"),
      export= has_plots,
      text  = has_plots && !identical(active_tab, "Grid"),
      theme = has_plots && !identical(active_tab, "Grid")
    )
  })
  
  # Dynamic sidebar menu with disabled look + no click on inactive
  output$sidebar_menu <- shinydashboard::renderMenu({
    ma <- menu_activation()
    add_disabled <- function(item, active) {
      if (isTRUE(active)) return(item)
      htmltools::tagAppendAttributes(item, class = "disabled-item")
    }
    shinydashboard::sidebarMenu(id = "mainmenu",
      add_disabled(shinydashboard::menuItem("Grid",   tabName = "grid",   icon = icon("th")), ma$grid),
      add_disabled(shinydashboard::menuItem("Export", tabName = "export", icon = icon("download")), ma$export),
      add_disabled(shinydashboard::menuItem("Text",   tabName = "text",   icon = icon("font")), ma$text),
      add_disabled(shinydashboard::menuItem("Theme",  tabName = "theme",  icon = icon("paint-brush")), ma$theme),
      hr(),
      fileInput("plots_rds", "Load ggplot (.rds, multiple)", accept = ".rds", multiple = TRUE),
      actionButton("load_demo", "Load 3 demo plots", class = "btn btn-link")
    )
  })
  
  # Persist and guard mainmenu selection
  observeEvent(input$mainmenu, {
    ma <- menu_activation()
    blocked <- c(
      if (!isTRUE(ma$export)) "export" else NULL,
      if (!isTRUE(ma$text))   "text"   else NULL,
      if (!isTRUE(ma$theme))  "theme"  else NULL,
      if (!isTRUE(ma$grid))   "grid"   else NULL
    )
    if (!is.null(input$mainmenu) && input$mainmenu %in% blocked) {
      # choose best allowed target
      allowed <- names(Filter(isTRUE, ma))
      pref <- rv$last_mainmenu
      target <- if (!is.null(pref) && pref %in% allowed) pref else if (length(allowed)) allowed[[1]] else "text"
      updateTabItems(session, "mainmenu", target)
    } else if (!is.null(input$mainmenu)) {
      rv$last_mainmenu <- input$mainmenu
    }
  }, ignoreInit = FALSE)
  
  # Update main menu selection when switching plot tabs
  observeEvent(rv$active_tab, {
    if (is.null(rv$active_tab)) return()
    
    # Update main menu selection
    if (identical(rv$active_tab, "Grid")) {
      updateTabItems(session, "mainmenu", "grid")
    } else if (length(rv$plots) > 0) {
      # For plot tabs, ensure we have fresh edits
      ensure_edits(rv, rv$active_tab, grid = FALSE)
      
      # Only reload color levels if they haven't been set yet for this plot
      if (!is.null(rv$edits[[rv$active_tab]]) && 
          (is.null(rv$edits[[rv$active_tab]]$colour_levels) || 
           is.null(rv$edits[[rv$active_tab]]$fill_levels))) {
        
        # Extract current levels and colors from the plot
        p <- rv$plots[[rv$active_tab]]
        if (!is.null(p)) {
          extract_levels_from_plot <- function(p, aes_name) {
            if (!requireNamespace("rlang", quietly = TRUE)) return(character(0))
            collect <- character(0)
            alt_aes <- if (identical(aes_name, "colour")) "color" else aes_name
            get_expr <- function(mapping) {
              if (is.null(mapping)) return(NULL)
              if (!is.null(mapping[[aes_name]])) return(mapping[[aes_name]])
              if (!is.null(mapping[[alt_aes]])) return(mapping[[alt_aes]])
              NULL
            }
            eval_on <- function(dat, expr) {
              if (is.null(expr) || is.null(dat)) return(NULL)
              if (!is.data.frame(dat) || nrow(dat) == 0) return(NULL)
              vals <- try(rlang::eval_tidy(expr, data = dat), silent = TRUE)
              if (inherits(vals, "try-error") || is.null(vals)) return(NULL)
              vals
            }
            append_vals <- function(vals) {
              if (is.null(vals)) return()
              vals <- vals[!is.na(vals)]
              if (is.factor(vals)) collect <<- c(collect, as.character(levels(vals))) else collect <<- c(collect, unique(as.character(vals)))
            }
            expr_p <- get_expr(p$mapping)
            append_vals(eval_on(p$data, expr_p))
            if (!is.null(p$layers) && length(p$layers)) {
              for (ly in p$layers) {
                expr_l <- get_expr(ly$mapping) %||% expr_p
                append_vals(eval_on(ly$data %||% p$data, expr_l))
              }
            }
            unique(collect[nzchar(collect)])
          }
          
          # Only set levels if they don't exist yet
          if (is.null(rv$edits[[rv$active_tab]]$colour_levels)) {
            col_lvls <- extract_levels_from_plot(p, "colour")
            if (length(col_lvls)) {
              rv$edits[[rv$active_tab]]$colour_levels <- col_lvls
              # Don't set colors yet - let the user choose or preserve original
            }
          }
          
          if (is.null(rv$edits[[rv$active_tab]]$fill_levels)) {
            fill_lvls <- extract_levels_from_plot(p, "fill")
            if (length(fill_lvls)) {
              rv$edits[[rv$active_tab]]$fill_levels <- fill_lvls
              # Don't set colors yet - let the user choose or preserve original
            }
          }
        }
      }
      
      updateTabItems(session, "mainmenu", rv$last_mainmenu %||% "text")
    }
  }, ignoreInit = FALSE)
  
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
      tabPanel("Grid", value = "Grid", 
               div(
                 style = "margin-bottom: 10px;",
                 downloadButton("download_grid", "Download Grid", class = "btn btn-success")
               ),
               plotOutput("grid_preview", height = "65vh"))
    )
    for (nm in names(rv$plots)) {
      tabs[[length(tabs) + 1]] <- tabPanel(nm, value = nm,
                                           div(
                                             style = "margin-bottom: 10px;",
                                             downloadButton(paste0("download_plot_", nm), 
                                                          paste("Download", nm), 
                                                          class = "btn btn-success")
                                           ),
                                           plotOutput(paste0("plot_prev_", nm), height = "65vh"))
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
  
  # --- Download handlers ------------------------------------------------
  # Grid download
  output$download_grid <- downloadHandler(
    filename = function() {
      paste0("grid_plot.", tolower(rv$grid_export$format %||% BASE$format))
    },
    content = function(file) {
      req(length(rv$plots) > 0)
      r <- rv$grid$rows %||% BASE$grid_rows
      c <- rv$grid$cols %||% BASE$grid_cols
      n <- r * c
      picked <- names(rv$plots)[seq_len(min(n, length(rv$plots)))]
      
      plots <- lapply(picked, function(nm) {
        ensure_edits(rv, nm)
        apply_edits(rv$plots[[nm]], rv$edits[[nm]])
      })
      
      grid_plot <- patchwork::wrap_plots(plots, nrow = r, ncol = c,
                                        guides = if (isTRUE(rv$grid$collect %||% BASE$grid_collect)) "collect" else "keep") +
        theme(legend.position = legend_pos_value(rv$grid$legend %||% BASE$grid_legend_pos))
      
      export_plot(grid_plot, file, rv$grid_export)
    }
  )
  
  # Grid download from export pane
  output$download_grid_export <- downloadHandler(
    filename = function() {
      paste0("grid_plot.", tolower(rv$grid_export$format %||% BASE$format))
    },
    content = function(file) {
      req(length(rv$plots) > 0)
      r <- rv$grid$rows %||% BASE$grid_rows
      c <- rv$grid$cols %||% BASE$grid_cols
      n <- r * c
      picked <- names(rv$plots)[seq_len(min(n, length(rv$plots)))]
      
      plots <- lapply(picked, function(nm) {
        ensure_edits(rv, nm)
        apply_edits(rv$plots[[nm]], rv$edits[[nm]])
      })
      
      grid_plot <- patchwork::wrap_plots(plots, nrow = r, ncol = c,
                                        guides = if (isTRUE(rv$grid$collect %||% BASE$grid_collect)) "collect" else "keep") +
        theme(legend.position = legend_pos_value(rv$grid$legend %||% BASE$grid_legend_pos))
      
      export_plot(grid_plot, file, rv$grid_export)
    }
  )
  
  # Individual plot downloads
  observe({
    lapply(names(rv$plots), function(nm) {
      local({
        name <- nm
        output[[paste0("download_plot_", name)]] <- downloadHandler(
          filename = function() {
            paste0(name, ".", tolower(rv$export[[name]]$format %||% BASE$format))
          },
          content = function(file) {
            req(!is.null(rv$plots[[name]]))
            ensure_edits(rv, name)
            plot_with_edits <- apply_edits(rv$plots[[name]], rv$edits[[name]])
            export_plot(plot_with_edits, file, rv$export[[name]])
          }
        )
        
        # Export pane download button
        output[[paste0("download_plot_export_", name)]] <- downloadHandler(
          filename = function() {
            paste0(name, ".", tolower(rv$export[[name]]$format %||% BASE$format))
          },
          content = function(file) {
            req(!is.null(rv$plots[[name]]))
            ensure_edits(rv, name)
            plot_with_edits <- apply_edits(rv$plots[[name]], rv$edits[[name]])
            export_plot(plot_with_edits, file, rv$export[[name]])
          }
        )
      })
    })
  })
  
  # --- Sidebar panes (render) ------------------------------------------
  output$subsidebar <- renderUI({
    cur <- input$mainmenu %||% "grid"
    
    # Check if the current menu item should be active
    activation <- menu_activation()
    if (cur == "text" && !activation$text) {
      return(div(
        h4("Text"),
        helpText("Text editing is only available when viewing individual plots, not the grid layout.")
      ))
    }
    if (cur == "theme" && !activation$theme) {
      return(div(
        h4("Theme"),
        helpText("Theme editing is only available when viewing individual plots, not the grid layout.")
      ))
    }
    if (cur == "export" && !activation$export) {
      return(div(
        h4("Export"),
        helpText("Please load some plots first to access export options.")
      ))
    }
    
    switch(cur,
           grid   = grid_pane_ui(rv),
           export = export_pane_ui(rv),
           text   = text_pane_ui(rv),
           theme  = theme_pane_ui(rv),
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
