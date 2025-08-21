# plot-editor/R/panes_export.R

export_pane_ui <- function(rv) {
  grid_ctx <- identical(rv$active_tab, "Grid")
  if (grid_ctx) {
    ex <- rv$grid_export
    return(tagList(
      h4("Export — Grid canvas"),
      numericInput("ui_gridexp_w", "Width (mm)",  value = ex$width_mm  %||% BASE$width_mm,  min = 20, max = 2000, step = 5),
      numericInput("ui_gridexp_h", "Height (mm)", value = ex$height_mm %||% BASE$height_mm, min = 20, max = 2000, step = 5),
      numericInput("ui_gridexp_d", "DPI",         value = ex$dpi       %||% BASE$dpi,       min = 72, max = 1200, step = 10),
      selectInput("ui_gridexp_f", "Format", choices = c("PNG","TIFF","PDF","SVG","EPS"), selected = ex$format %||% BASE$format)
    ))
  }
  
  ap <- rv$active_tab
  if (is.null(ap) || is.null(rv$plots[[ap]])) return(tagList(h4("Export"), helpText("Select a plot tab.")))
  
  ensure_edits(rv, ap, grid = FALSE)
  ex <- rv$export[[ap]]
  
  tagList(
    actionButton("apply_all_export", "Use for all plots", class = "btn btn-sm btn-default"),
    h4(sprintf("Export — %s", ap)),
    numericInput("ui_exp_width",  "Width (mm)",  value = ex$width_mm  %||% BASE$width_mm,  min = 20, max = 2000, step = 5),
    numericInput("ui_exp_height", "Height (mm)", value = ex$height_mm %||% BASE$height_mm, min = 20, max = 2000, step = 5),
    numericInput("ui_exp_dpi",    "DPI",         value = ex$dpi       %||% BASE$dpi,       min = 72, max = 1200, step = 10),
    selectInput("ui_exp_format",  "Format",      choices = c("PNG","TIFF","PDF","SVG","EPS"),
                selected = toupper(ex$format %||% BASE$format))
  )
}

register_export_observers <- function(input, rv, session) {
  # Grid export writers
  observeEvent(input$ui_gridexp_w, { rv$grid_export$width_mm  <- as_num_safe(input$ui_gridexp_w) }, ignoreInit = TRUE, ignoreNULL = TRUE)
  observeEvent(input$ui_gridexp_h, { rv$grid_export$height_mm <- as_num_safe(input$ui_gridexp_h) }, ignoreInit = TRUE, ignoreNULL = TRUE)
  observeEvent(input$ui_gridexp_d, { rv$grid_export$dpi       <- as_num_safe(input$ui_gridexp_d) }, ignoreInit = TRUE, ignoreNULL = TRUE)
  observeEvent(input$ui_gridexp_f, { rv$grid_export$format    <- input$ui_gridexp_f             }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  # Plot export writers
  observeEvent(input$ui_exp_width,  {
    ap <- rv$active_tab; if (is.null(ap) || identical(ap,"Grid") || is.null(rv$plots[[ap]])) return()
    ensure_edits(rv, ap, grid = FALSE); rv$export[[ap]]$width_mm <- as_num_safe(input$ui_exp_width)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  observeEvent(input$ui_exp_height, {
    ap <- rv$active_tab; if (is.null(ap) || identical(ap,"Grid") || is.null(rv$plots[[ap]])) return()
    ensure_edits(rv, ap, grid = FALSE); rv$export[[ap]]$height_mm <- as_num_safe(input$ui_exp_height)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  observeEvent(input$ui_exp_dpi, {
    ap <- rv$active_tab; if (is.null(ap) || identical(ap,"Grid") || is.null(rv$plots[[ap]])) return()
    ensure_edits(rv, ap, grid = FALSE); rv$export[[ap]]$dpi <- as_num_safe(input$ui_exp_dpi)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  observeEvent(input$ui_exp_format, {
    ap <- rv$active_tab; if (is.null(ap) || identical(ap,"Grid") || is.null(rv$plots[[ap]])) return()
    ensure_edits(rv, ap, grid = FALSE); rv$export[[ap]]$format <- input$ui_exp_format
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  observeEvent(input$apply_all_export, {
    ap <- rv$active_tab
    if (is.null(ap) || identical(ap,"Grid") || is.null(rv$plots[[ap]])) return()
    ensure_edits(rv, ap, grid = FALSE)
    src <- rv$export[[ap]]
    for (nm in names(rv$plots)) {
      ensure_edits(rv, nm, grid = FALSE)
      rv$export[[nm]] <- list(
        width_mm  = src$width_mm  %||% BASE$width_mm,
        height_mm = src$height_mm %||% BASE$height_mm,
        dpi       = src$dpi       %||% BASE$dpi,
        format    = src$format    %||% BASE$format
      )
    }
    showNotification("Export settings applied to all plots.", type = "message")
  })
}
