# plot-editor/R/panes_text.R

text_pane_ui <- function(rv) {
  grid_ctx <- identical(rv$active_tab, "Grid")
  if (grid_ctx) {
    return(tagList(
      h4("Text — Grid context"),
      helpText("When Grid is active, text edits are stored as grid-specific overrides per plot."),
      helpText("Select a grid cell in the layout and then edit that plot's text under its own tab, or switch to a plot tab to edit its standalone text.")
    ))
  }
  
  ap <- rv$active_tab
  plot_selected <- !is.null(ap) && !is.null(rv$plots[[ap]])
  if (!plot_selected) return(tagList(h4("Text"), helpText("Select a plot tab to edit its text.")))
  
  ensure_edits(rv, ap, grid = FALSE)
  e <- rv$edits[[ap]]
  p <- rv$plots[[ap]]
  
  get_lab <- function(lbl) {
    val <- tryCatch(p$labels[[lbl]], error = function(...) NULL)
    if (is.null(val)) "" else as.character(val)
  }
  
  # Get current plot values for defaults
  current_x_range <- tryCatch({
    x_data <- p$data[[p$mapping$x %||% names(p$data)[1]]]
    if (is.numeric(x_data)) range(x_data, na.rm = TRUE) else c(0, 10)
  }, error = function(...) c(0, 10))
  
  current_y_range <- tryCatch({
    y_data <- p$data[[p$mapping$y %||% names(p$data)[2]]]
    if (is.numeric(y_data)) range(y_data, na.rm = TRUE) else c(0, 10)
  }, error = function(...) c(0, 10))
  
  tagList(
    actionButton("apply_all_text", "Use for all plots", class = "btn btn-sm btn-default btn-block"),
    tags$hr(),
    h4(sprintf("Text — %s", ap)),
    
    # Use tabset for better organization
    tabsetPanel(
      id = "text_tabs",
      tabPanel("Labels", 
        div(style = "padding: 10px 0;",
          textInput("ui_title",    "Title",    e$title    %||% get_lab("title")),
          textInput("ui_subtitle", "Subtitle", e$subtitle %||% get_lab("subtitle")),
          textInput("ui_caption",  "Caption",  e$caption  %||% get_lab("caption")),
          textInput("ui_xlab",     "X label",  e$xlab     %||% get_lab("x")),
          textInput("ui_ylab",     "Y label",  e$ylab     %||% get_lab("y"))
        )
      ),
      
      tabPanel("Text Sizes",
        div(style = "padding: 10px 0;",
          sliderInput("ui_title_size", "Title size", 
                     value = e$title_size %||% BASE$title_size, 
                     min = 8, max = 24, step = 1, width = "100%"),
          sliderInput("ui_subtitle_size", "Subtitle size", 
                     value = e$subtitle_size %||% BASE$subtitle_size, 
                     min = 6, max = 20, step = 1, width = "100%"),
          sliderInput("ui_caption_size", "Caption size", 
                     value = e$caption_size %||% BASE$caption_size, 
                     min = 6, max = 18, step = 1, width = "100%"),
          sliderInput("ui_axis_title_size", "Axis title size", 
                     value = e$axis_title_size %||% BASE$axis_title_size, 
                     min = 8, max = 20, step = 1, width = "100%"),
          sliderInput("ui_axis_text_size", "Axis text size", 
                     value = e$axis_text_size %||% BASE$axis_text_size, 
                     min = 6, max = 18, step = 1, width = "100%"),
          sliderInput("ui_legend_title_size", "Legend title size", 
                     value = e$legend_title_size %||% BASE$legend_title_size, 
                     min = 8, max = 20, step = 1, width = "100%"),
          sliderInput("ui_legend_text_size", "Legend text size", 
                     value = e$legend_text_size %||% BASE$legend_text_size, 
                     min = 6, max = 18, step = 1, width = "100%")
        )
      ),
      
      tabPanel("Axis Limits",
        div(style = "padding: 10px 0;",
          fluidRow(
            column(6, numericInput("ui_x_min", "X min", 
                                  value = e$x_min %||% current_x_range[1], 
                                  step = 0.1, width = "100%")),
            column(6, numericInput("ui_x_max", "X max", 
                                  value = e$x_max %||% current_x_range[2], 
                                  step = 0.1, width = "100%"))
          ),
          fluidRow(
            column(6, numericInput("ui_y_min", "Y min", 
                                  value = e$y_min %||% current_y_range[1], 
                                  step = 0.1, width = "100%")),
            column(6, numericInput("ui_y_max", "Y max", 
                                  value = e$y_max %||% current_y_range[2], 
                                  step = 0.1, width = "100%"))
          )
        )
      ),
      
      tabPanel("Axis Breaks",
        div(style = "padding: 10px 0;",
          fluidRow(
            column(6, numericInput("ui_x_major", "X major", 
                                  value = e$x_major, min = 1, step = 1, width = "100%")),
            column(6, numericInput("ui_x_minor", "X minor", 
                                  value = e$x_minor, min = 0, step = 1, width = "100%"))
          ),
          fluidRow(
            column(6, numericInput("ui_y_major", "Y major", 
                                  value = e$y_major, min = 1, step = 1, width = "100%")),
            column(6, numericInput("ui_y_minor", "Y minor", 
                                  value = e$y_minor, min = 0, step = 1, width = "100%"))
          )
        )
      )
    )
  )
}

register_text_observers <- function(input, rv, session) {
  bind_edit <- function(input_id, field) {
    observeEvent(input[[input_id]], {
      if (!is.null(rv$is_hydrating) && rv$is_hydrating) return()
      ap <- rv$active_tab
      if (is.null(ap) || is.null(rv$plots[[ap]])) return()
      
      if (identical(ap, "Grid")) {
        # In grid context, no direct target — user should switch to a plot tab to edit that plot's text
        return()
      } else {
        ensure_edits(rv, ap, grid = FALSE)
        rv$edits[[ap]][[field]] <- input[[input_id]]
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
  }
  
  # Basic text labels
  bind_edit("ui_title",    "title")
  bind_edit("ui_subtitle", "subtitle")
  bind_edit("ui_caption",  "caption")
  bind_edit("ui_xlab",     "xlab")
  bind_edit("ui_ylab",     "ylab")
  
  # Text sizes
  bind_edit("ui_title_size", "title_size")
  bind_edit("ui_subtitle_size", "subtitle_size")
  bind_edit("ui_caption_size", "caption_size")
  bind_edit("ui_axis_title_size", "axis_title_size")
  bind_edit("ui_axis_text_size", "axis_text_size")
  bind_edit("ui_legend_title_size", "legend_title_size")
  bind_edit("ui_legend_text_size", "legend_text_size")
  
  # Axis limits
  bind_edit("ui_x_min", "x_min")
  bind_edit("ui_x_max", "x_max")
  bind_edit("ui_y_min", "y_min")
  bind_edit("ui_y_max", "y_max")
  
  # Axis breaks
  bind_edit("ui_x_major", "x_major")
  bind_edit("ui_x_minor", "x_minor")
  bind_edit("ui_y_major", "y_major")
  bind_edit("ui_y_minor", "y_minor")
  
  observeEvent(input$apply_all_text, {
    ap <- rv$active_tab
    if (is.null(ap) || identical(ap, "Grid") || is.null(rv$plots[[ap]])) return()
    ensure_edits(rv, ap, grid = FALSE)
    src <- rv$edits[[ap]]
    text_fields <- c("title","subtitle","caption","xlab","ylab",
                     "title_size","subtitle_size","caption_size",
                     "axis_title_size","axis_text_size","legend_title_size","legend_text_size",
                     "x_min","x_max","y_min","y_max","x_major","x_minor","y_major","y_minor")
    for (nm in names(rv$plots)) {
      ensure_edits(rv, nm, grid = FALSE)
      rv$edits[[nm]][text_fields] <- src[text_fields]
    }
    showNotification("Text settings applied to all plots.", type = "message")
  })
}
