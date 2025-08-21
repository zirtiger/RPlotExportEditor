# plot-editor/R/panes_axes.R

axes_pane_ui <- function(rv) {
  grid_ctx <- identical(rv$active_tab, "Grid")
  if (grid_ctx) {
    return(tagList(
      h4("Axes — Grid context"),
      helpText("Axes edits while Grid is active apply ONLY to grid rendering when you place plots. Switch to a plot tab to edit its standalone axes.")
    ))
  }
  ap <- rv$active_tab
  if (is.null(ap) || is.null(rv$plots[[ap]])) return(tagList(h4("Axes"), helpText("Select a plot tab.")))
  ensure_edits(rv, ap, grid = FALSE)
  e <- rv$edits[[ap]]
  
  tagList(
    h4(sprintf("Axes — %s", ap)),
    fluidRow(
      column(6,
             h5("X limits"),
             numericInput("ui_xlim_min", "X min", value = e$xlim_min, step = 0.1),
             numericInput("ui_xlim_max", "X max", value = e$xlim_max, step = 0.1)
      ),
      column(6,
             h5("Y limits"),
             numericInput("ui_ylim_min", "Y min", value = e$ylim_min, step = 0.1),
             numericInput("ui_ylim_max", "Y max", value = e$ylim_max, step = 0.1)
      )
    ),
    tags$hr(),
    fluidRow(
      column(6,
             h5("X breaks"),
             numericInput("ui_x_breaks", "Major step", value = e$x_breaks_step, min = 0, step = 0.1),
             numericInput("ui_x_minor_breaks", "Minor step", value = e$x_minor_breaks_step, min = 0, step = 0.1)
      ),
      column(6,
             h5("Y breaks"),
             numericInput("ui_y_breaks", "Major step", value = e$y_breaks_step, min = 0, step = 0.1),
             numericInput("ui_y_minor_breaks", "Minor step", value = e$y_minor_breaks_step, min = 0, step = 0.1)
      )
    )
  )
}

register_axes_observers <- function(input, rv, session) {
  set_val <- function(field, value) {
    ap <- rv$active_tab; if (is.null(ap) || is.null(rv$plots[[ap]]) || identical(ap, "Grid")) return()
    ensure_edits(rv, ap, grid = FALSE)
    rv$edits[[ap]][[field]] <- value
  }
  observeEvent(input$ui_xlim_min, { set_val("xlim_min", as_num_safe(input$ui_xlim_min)) }, ignoreInit = TRUE, ignoreNULL = TRUE)
  observeEvent(input$ui_xlim_max, { set_val("xlim_max", as_num_safe(input$ui_xlim_max)) }, ignoreInit = TRUE, ignoreNULL = TRUE)
  observeEvent(input$ui_ylim_min, { set_val("ylim_min", as_num_safe(input$ui_ylim_min)) }, ignoreInit = TRUE, ignoreNULL = TRUE)
  observeEvent(input$ui_ylim_max, { set_val("ylim_max", as_num_safe(input$ui_ylim_max)) }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  # Breaks
  observeEvent(input$ui_x_breaks,        { set_val("x_breaks_step",       as_num_safe(input$ui_x_breaks))        }, ignoreInit = TRUE, ignoreNULL = TRUE)
  observeEvent(input$ui_x_minor_breaks,  { set_val("x_minor_breaks_step", as_num_safe(input$ui_x_minor_breaks))  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  observeEvent(input$ui_y_breaks,        { set_val("y_breaks_step",       as_num_safe(input$ui_y_breaks))        }, ignoreInit = TRUE, ignoreNULL = TRUE)
  observeEvent(input$ui_y_minor_breaks,  { set_val("y_minor_breaks_step", as_num_safe(input$ui_y_minor_breaks))  }, ignoreInit = TRUE, ignoreNULL = TRUE)
}