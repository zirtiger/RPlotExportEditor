# plot-editor/R/panes_grid.R

# ---------- UI ----------
grid_pane_ui <- function(rv) {
  if (!identical(rv$active_tab, "Grid")) {
    return(tagList(h4("Grid"), helpText("Switch to the Grid tab to configure grid settings.")))
  }
  
  g <- rv$grid
  choices <- c("(empty)", names(rv$plots))
  
  # Build cell selectors
  r <- g$rows %||% BASE$grid_rows
  c <- g$cols %||% BASE$grid_cols
  idxs <- seq_len(r * c)
  rows_ui <- lapply(seq_len(r), function(i) {
    cols_ui <- lapply(seq_len(c), function(j) {
      k <- (i - 1) * c + j
      selectInput(paste0("cell_", k), paste0("Cell ", k),
                  choices = choices,
                  selected = g$cells[[k]] %||% "(empty)")
    })
    fluidRow(lapply(cols_ui, function(x) column(6, x)))
  })
  
  tagList(
    h4("Grid layout"),
    numericInput("ui_grid_rows",   "Rows",    value = r, min = 1, max = 10, step = 1),
    numericInput("ui_grid_cols",   "Columns", value = c, min = 1, max = 10, step = 1),
    checkboxInput("ui_grid_collect","Collect/common legend", value = g$collect %||% BASE$grid_collect),
    selectInput("ui_grid_legend",  "Legend position", choices = LEGEND_POS,
                selected = g$legend %||% BASE$grid_legend_pos),
    
    tags$hr(),
    h4("Assign plots to grid cells"),
    do.call(tagList, rows_ui),
    
    tags$hr(),
    h4("Export (grid only)"),
    numericInput("ui_grid_width",  "Width (mm)",  value = rv$grid_export$width_mm  %||% BASE$width_mm,  min = 50, max = 2000, step = 10),
    numericInput("ui_grid_height", "Height (mm)", value = rv$grid_export$height_mm %||% BASE$height_mm, min = 50, max = 2000, step = 10),
    numericInput("ui_grid_dpi",    "DPI",         value = rv$grid_export$dpi       %||% BASE$dpi,       min = 72, max = 1200, step = 10),
    selectInput("ui_grid_format",  "Format",      choices = c("PNG","TIFF","PDF","SVG","EPS"),
                selected = rv$grid_export$format %||% BASE$format)
  )
}

# ---------- Observers ----------
register_grid_observers <- function(input, rv, session) {
  # Layout changes
  observeEvent(input$ui_grid_rows, {
    rows <- as_num_safe(input$ui_grid_rows) %||% BASE$grid_rows
    resize_cells(rv, rows, rv$grid$cols %||% BASE$grid_cols)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  observeEvent(input$ui_grid_cols, {
    cols <- as_num_safe(input$ui_grid_cols) %||% BASE$grid_cols
    resize_cells(rv, rv$grid$rows %||% BASE$grid_rows, cols)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  observeEvent(input$ui_grid_collect, {
    rv$grid$collect <- isTRUE(input$ui_grid_collect)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  observeEvent(input$ui_grid_legend, {
    rv$grid$legend <- legend_pos_value(input$ui_grid_legend)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  # Persist cell assignments
  observe({
    r <- rv$grid$rows %||% BASE$grid_rows
    c <- rv$grid$cols %||% BASE$grid_cols
    idxs <- seq_len(r * c)
    rv$grid$cells <- lapply(idxs, function(k) {
      v <- input[[paste0("cell_", k)]]
      if (is.null(v)) rv$grid$cells[[k]] %||% "(empty)" else v
    })
  })
  
  # Grid export writers
  observeEvent(input$ui_grid_width,  { rv$grid_export$width_mm  <- as_num_safe(input$ui_grid_width)  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  observeEvent(input$ui_grid_height, { rv$grid_export$height_mm <- as_num_safe(input$ui_grid_height) }, ignoreInit = TRUE, ignoreNULL = TRUE)
  observeEvent(input$ui_grid_dpi,    { rv$grid_export$dpi       <- as_num_safe(input$ui_grid_dpi)    }, ignoreInit = TRUE, ignoreNULL = TRUE)
  observeEvent(input$ui_grid_format, { rv$grid_export$format    <- input$ui_grid_format              }, ignoreInit = TRUE, ignoreNULL = TRUE)
}
