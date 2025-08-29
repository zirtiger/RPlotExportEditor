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
  
  title_now    <- e$title    %||% get_lab("title")
  subtitle_now <- e$subtitle %||% get_lab("subtitle")
  caption_now  <- e$caption  %||% get_lab("caption")
  xlab_now     <- e$xlab     %||% get_lab("x")
  ylab_now     <- e$ylab     %||% get_lab("y")
  
  grid_major_on <- isTRUE(e$grid_major)
  grid_minor_on <- isTRUE(e$grid_minor)
  
  tagList(
    actionButton("apply_all_text", "Use for all plots", class = "btn btn-sm btn-default btn-block"),
    tags$hr(),
    
    # Use tabs for better organization
    tabsetPanel(
      id = "text_tabs",
      selected = rv$tabs$text %||% "Labels",
      tabPanel("Labels", 
        textInput("ui_title",    "Title",    title_now),
        textInput("ui_subtitle", "Subtitle", subtitle_now),
        textInput("ui_caption",  "Caption",  caption_now),
        textInput("ui_xlab",     "X label",  xlab_now),
        textInput("ui_ylab",     "Y label",  ylab_now)
      ),
      tabPanel("Text Sizes",
        # Base size control
        sliderInput("ui_base_size", "Base text size", 
                   value = e$base_size %||% 12, 
                   min = 8, max = 24, step = 1),
        
        # Row 1: Title and Subtitle
        fluidRow(
          column(6,
            sliderInput("ui_title_size", "Title size", 
                       value = e$title_size %||% 14, 
                       min = 8, max = 34, step = 1)
          ),
          column(6,
            sliderInput("ui_subtitle_size", "Subtitle size", 
                       value = e$subtitle_size %||% 12, 
                       min = 6, max = 30, step = 1)
          )
        ),
        
        # Row 2: Caption and Axis Title
        fluidRow(
          column(6,
            sliderInput("ui_caption_size", "Caption size", 
                       value = e$caption_size %||% 10, 
                       min = 6, max = 28, step = 1)
          ),
          column(6,
            sliderInput("ui_axis_title_size", "Axis title size", 
                       value = e$axis_title_size %||% 12, 
                       min = 8, max = 30, step = 1)
          )
        ),
        
        # Row 3: Axis Text and Legend Title
        fluidRow(
          column(6,
            sliderInput("ui_axis_text_size", "Axis text size", 
                       value = e$axis_text_size %||% 10, 
                       min = 6, max = 28, step = 1)
          ),
          column(6,
            sliderInput("ui_legend_title_size", "Legend title size", 
                       value = e$legend_title_size %||% 12, 
                       min = 8, max = 30, step = 1)
          )
        ),
        
        # Row 4: Legend Text (centered)
        fluidRow(
          column(6, offset = 3,
            sliderInput("ui_legend_text_size", "Legend text size", 
                       value = e$legend_text_size %||% 10, 
                       min = 6, max = 28, step = 1)
          )
        )
      ),
      tabPanel("Axis",
        fluidRow(
          column(6, numericInput("ui_x_min", "X min", value = e$x_min, step = 0.1)),
          column(6, numericInput("ui_x_max", "X max", value = e$x_max, step = 0.1))
        ),
        fluidRow(
          column(6, numericInput("ui_y_min", "Y min", value = e$y_min, step = 0.1)),
          column(6, numericInput("ui_y_max", "Y max", value = e$y_max, step = 0.1))
        ),
        tags$hr(),
        h5("Steps"),
        fluidRow(
          column(6, numericInput("ui_x_step_major", "X major", value = e$x_step_major, min = 0, step = 0.1)),
          column(6, numericInput("ui_y_step_major", "Y major", value = e$y_step_major, min = 0, step = 0.1))
        ),
        fluidRow(
          column(6, numericInput("ui_x_step_minor", "X minor", value = e$x_step_minor, min = 0, step = 0.1)),
          column(6, numericInput("ui_y_step_minor", "Y minor", value = e$y_step_minor, min = 0, step = 0.1))
        ),
        if (!grid_major_on) tags$small(style="color:#777;", "Major grid lines are off."),
        if (!grid_minor_on) tags$small(style="color:#777; display:block;", "Minor grid lines are off.")
      )
    )
  )
}

register_text_observers <- function(input, rv, session) {
  bind_edit <- function(input_id, field) {
    # Use onBlur to only update when focus is lost
    observeEvent(input[[input_id]], {
      if (!is.null(rv$is_hydrating) && rv$is_hydrating) return()
      if (rv$force_ui_update > 0) return()  # Skip during UI refresh
      ap <- rv$active_tab
      if (is.null(ap) || identical(ap, "Grid")) return()
      
      # The active tab is now the plot index (1, 2, 3, etc.)
      # Just use it directly if it's a valid plot index
      plot_index <- NULL
      if (ap %in% names(rv$plots)) {
        plot_index <- as.numeric(ap)
      }
      
      if (is.null(plot_index)) return()
      
      # Load settings for this plot if needed
      load_plot_settings(rv, plot_index)
      rv$edits[[as.character(plot_index)]][[field]] <- input[[input_id]]
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
  }
  
  # Persist selected sub-tab
  observeEvent(input$text_tabs, {
    if (rv$force_ui_update > 0) return()  # Skip during UI refresh
    rv$tabs$text <- input$text_tabs
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  # Basic text labels
  bind_edit("ui_title",    "title")
  bind_edit("ui_subtitle", "subtitle")
  bind_edit("ui_caption",  "caption")
  bind_edit("ui_xlab",     "xlab")
  bind_edit("ui_ylab",     "ylab")
  
  # Base size (moved from theme)
  bind_edit("ui_base_size", "base_size")
  
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
  
  # Axis steps
  bind_edit("ui_x_step_minor", "x_step_minor")
  bind_edit("ui_x_step_major", "x_step_major")
  bind_edit("ui_y_step_minor", "y_step_minor")
  bind_edit("ui_y_step_major", "y_step_major")
  
  observeEvent(input$apply_all_text, {
    if (rv$force_ui_update > 0) return()  # Skip during UI refresh
    ap <- rv$active_tab
    if (is.null(ap) || identical(ap, "Grid")) return()
    
    # The active tab is now the plot index (1, 2, 3, etc.)
    # Just use it directly if it's a valid plot index
    plot_index <- NULL
    if (ap %in% names(rv$plots)) {
      plot_index <- as.numeric(ap)
    }
    
    if (is.null(plot_index)) return()
    
    # Load settings for this plot if needed
    load_plot_settings(rv, plot_index)
    src <- rv$edits[[as.character(plot_index)]]
    
    text_fields <- c("title","subtitle","caption","xlab","ylab",
                     "base_size","title_size","subtitle_size","caption_size",
                     "axis_title_size","axis_text_size","legend_title_size","legend_text_size",
                     "x_min","x_max","y_min","y_max","x_step_major","x_step_minor","y_step_major","y_step_minor")
    
    # Apply to all plots
    for (index in get_plot_indices(rv)) {
      index_str <- as.character(index)
      load_plot_settings(rv, index)
      for (field in text_fields) {
        if (!is.null(src[[field]])) {
          rv$edits[[index_str]][[field]] <- src[[field]]
        }
      }
    }
    showNotification("Text settings applied to all plots.", type = "message")
  })
}
