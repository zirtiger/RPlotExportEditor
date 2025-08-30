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
  
  # Ensure cells exist
  if (length(g$cells) == 0) {
    g$cells <- lapply(idxs, function(k) "(empty)")
  }
  
  rows_ui <- lapply(seq_len(r), function(i) {
    cols_ui <- lapply(seq_len(c), function(j) {
      k <- (i - 1) * c + j
      cell_value <- if (k <= length(g$cells)) g$cells[[k]] else "(empty)"
      selectInput(paste0("cell_", k), paste0("Cell ", k),
                  choices = choices,
                  selected = cell_value)
    })
    fluidRow(lapply(cols_ui, function(x) column(6, x)))
  })
  
  tagList(
    h4("Grid layout"),
    
    # Use tabs for better organization
    tabsetPanel(
      id = "grid_tabs",
      selected = rv$tabs$grid %||% "Layout",
      tabPanel("Layout",
        fluidRow(
          column(6, numericInput("ui_grid_rows", "Rows", value = r, min = 1, max = 10, step = 1)),
          column(6, numericInput("ui_grid_cols", "Columns", value = c, min = 1, max = 10, step = 1))
        ),
        checkboxInput("ui_grid_collect","Collect/common legend", value = g$collect %||% BASE$grid_collect),
        selectInput("ui_grid_legend",  "Legend position", choices = LEGEND_POS,
                    selected = g$legend %||% BASE$grid_legend_pos)
      ),
      tabPanel("Assignments",
        h5("Assign plots to grid cells"),
        do.call(tagList, rows_ui)
      )
    )
  )
}

# ---------- Observers ----------
register_grid_observers <- function(input, rv, session) {
  # Layout changes - numeric inputs use proper debouncing
  # Rows input
  timer_rows <- reactiveTimer(500)
  should_execute_rows <- reactiveVal(FALSE)
  last_value_rows <- reactiveVal(NULL)
  
  observeEvent(input$ui_grid_rows, {
    last_value_rows(input$ui_grid_rows)
    should_execute_rows(TRUE)
    timer_rows()
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  observeEvent(timer_rows(), {
    if (should_execute_rows()) {
      rows <- as_num_safe(last_value_rows()) %||% BASE$grid_rows
      resize_cells(rv, rows, rv$grid$cols %||% BASE$grid_cols)
      should_execute_rows(FALSE)
    }
  })
  
  # Columns input
  timer_cols <- reactiveTimer(500)
  should_execute_cols <- reactiveVal(FALSE)
  last_value_cols <- reactiveVal(NULL)
  
  observeEvent(input$ui_grid_cols, {
    last_value_cols(input$ui_grid_cols)
    should_execute_cols(TRUE)
    timer_cols()
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  observeEvent(timer_cols(), {
    if (should_execute_cols()) {
      cols <- as_num_safe(last_value_cols()) %||% BASE$grid_cols
      resize_cells(rv, rv$grid$rows %||% BASE$grid_rows, cols)
      should_execute_cols(FALSE)
    }
  })
  
  observeEvent(input$ui_grid_collect, {
    rv$grid$collect <- isTRUE(input$ui_grid_collect)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  observeEvent(input$ui_grid_legend, {
    rv$grid$legend <- legend_pos_value(input$ui_grid_legend)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  # Persist selected sub-tab
  observeEvent(input$grid_tabs, {
    rv$tabs$grid <- input$grid_tabs
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  # Persist cell assignments
  observe({
    r <- rv$grid$rows %||% BASE$grid_rows
    c <- rv$grid$cols %||% BASE$grid_cols
    idxs <- seq_len(r * c)
    
    # Ensure cells exist before accessing
    if (length(rv$grid$cells) == 0) {
      rv$grid$cells <- lapply(idxs, function(k) "(empty)")
    }
    
    rv$grid$cells <- lapply(idxs, function(k) {
      v <- input[[paste0("cell_", k)]]
      if (is.null(v)) {
        if (k <= length(rv$grid$cells)) rv$grid$cells[[k]] else "(empty)"
      } else {
        v
      }
    })
  })
}
