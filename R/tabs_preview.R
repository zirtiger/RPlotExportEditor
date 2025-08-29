# plot-editor/R/tabs_previews.R

# ---------- Build tabs ----------
tabs_area_ui <- function(rv) {
  tabs <- list(
    shiny::tabPanel("Grid", value = "Grid", 
                    div(
                      style = "margin-bottom: 10px;",
                      downloadButton("download_grid", "Download Grid", class = "btn btn-success")
                    ),
                    imageOutput("grid_preview", height = "65vh"))
  )
  # Get plot indices and names
  plot_indices <- names(rv$plots)
  for (index in plot_indices) {
    plot_name <- rv$plot_names[[index]] %||% paste("Plot", index)
    tabs[[length(tabs) + 1]] <- shiny::tabPanel(
      title = plot_name, value = index,
      plotOutput(paste0("plot_preview_", index), height = "70vh")
    )
  }
  do.call(shiny::tabsetPanel, c(id = "active_tabset", type = "tabs", tabs))
}

# ---------- Register WYSIWYG previews ----------
register_preview_outputs <- function(output, rv) {
  # Per-plot previews: use each plot's per-plot export settings
  observe({
    lapply(names(rv$plots), function(index) {
      local({
        index_str <- as.character(index)
        output[[paste0("plot_preview_", index)]] <- renderPlot({
          req(rv$plots[[index_str]])
          ensure_edits(rv, index_str, grid = FALSE)
          
          # Make the preview reactive to edits changes by accessing them explicitly
          edits <- rv$edits[[index_str]]
          p <- apply_edits(rv$plots[[index_str]], edits)
          
          # Display the plot directly
          print(p)
        })
      })
    })
  })
  
  # Grid preview: uses grid_export + grid_edits + selected cells
  output$grid_preview <- renderImage({
    req(length(rv$plots) > 0)
    
    r <- rv$grid$rows %||% BASE$grid_rows
    c <- rv$grid$cols %||% BASE$grid_cols
    idxs <- seq_len(r * c)
    
    # Ensure cells exist
    if (is.null(rv$grid$cells) || length(rv$grid$cells) == 0) {
      rv$grid$cells <- lapply(idxs, function(k) "(empty)")
    }
    
    # Which plots are placed?
    cells <- rv$grid$cells
    picked <- Filter(function(x) !is.null(x) && x != "(empty)" && x %in% names(rv$plots), cells)
    req(length(picked) > 0)
    
    # Apply GRID-specific edits (do not touch per-plot tabs)
    plots <- lapply(picked, function(nm) {
      ensure_edits(rv, nm, grid = TRUE)
      apply_edits(rv$plots[[nm]], rv$edits[[nm]])
    })
    
    pw <- patchwork::wrap_plots(plots, nrow = r, ncol = c,
                                guides = if (isTRUE(rv$grid$collect %||% BASE$grid_collect)) "collect" else "keep") +
      ggplot2::theme(legend.position = legend_pos_value(rv$grid$legend %||% BASE$grid_legend_pos))
    
    ex <- rv$grid_export
    render_preview_png(pw, ex$width_mm, ex$height_mm, ex$dpi, ex$format)
  }, deleteFile = TRUE)
}

# ---------- Track active tab ----------
observe_tab_tracking <- function(input, rv) {
  observeEvent(input$active_tabset, {
    rv$active_tab <- input$active_tabset
  }, ignoreInit = TRUE)
}
