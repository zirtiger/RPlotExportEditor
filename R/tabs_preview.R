# plot-editor/R/tabs_previews.R

# ---------- Build tabs ----------
tabs_area_ui <- function(rv) {
  tabs <- list(
    shiny::tabPanel("Grid", value = "Grid", imageOutput("grid_preview", height = "70vh"))
  )
  for (nm in names(rv$plots)) {
    tabs[[length(tabs) + 1]] <- shiny::tabPanel(
      title = nm, value = nm,
      imageOutput(paste0("plot_preview_", nm), height = "70vh")
    )
  }
  do.call(shiny::tabsetPanel, c(id = "active_tabset", type = "tabs", tabs))
}

# ---------- Register WYSIWYG previews ----------
register_preview_outputs <- function(output, rv) {
  # Per-plot previews: use each plot's per-plot export settings
  observe({
    lapply(names(rv$plots), function(nm) {
      local({
        name <- nm
        output[[paste0("plot_preview_", name)]] <- renderImage({
          req(rv$plots[[name]])
          ensure_edits(rv, name, grid = FALSE)
          p <- apply_edits(rv$plots[[name]], rv$edits[[name]])
          
          ex <- rv$export[[name]] %||% list()
          w  <- ex$width_mm  %||% BASE$width_mm
          h  <- ex$height_mm %||% BASE$height_mm
          d  <- ex$dpi       %||% BASE$dpi
          f  <- ex$format    %||% BASE$format
          
          render_preview_png(p, w, h, d, f)
        }, deleteFile = TRUE)
      })
    })
  })
  
  # Grid preview: uses grid_export + grid_edits + selected cells
  output$grid_preview <- renderImage({
    req(length(rv$plots) > 0)
    pw <- build_grid_patchwork(rv)
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
