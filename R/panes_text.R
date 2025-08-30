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
  
  cat("DEBUG: text_pane_ui - active_tab:", ap, "type:", typeof(ap), "\n")
  cat("DEBUG: text_pane_ui - plots available:", paste(names(rv$plots), collapse=", "), "\n")
  
  e <- rv$edits[[ap]]
  p <- rv$plots[[ap]]
  
  cat("DEBUG: text_pane_ui - edits for", ap, ":", if(is.null(e)) "NULL" else paste(names(e), collapse=", "), "\n")
  
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
        # Base size control (alone at top)
        sliderInput("ui_base_size", "Base text size", 
                   value = e$base_size %||% 12, 
                   min = 8, max = 24, step = 1),
        
        # Row 1: Title and Subtitle
        fluidRow(
          column(6,
            div(
              sliderInput("ui_title_size", "Title", 
                         value = e$title_size %||% 1.2, 
                         min = 0.5, max = 3.0, step = 0.1),
              tags$small(style="color:#777;", 
                         "Size: ", 
                         round((e$title_size %||% 1.2) * (e$base_size %||% 12), 1))
            )
          ),
          column(6,
            div(
              sliderInput("ui_subtitle_size", "Subtitle", 
                         value = e$subtitle_size %||% 1.0, 
                         min = 0.5, max = 2.5, step = 0.1),
              tags$small(style="color:#777;", 
                         "Size: ", 
                         round((e$subtitle_size %||% 1.0) * (e$base_size %||% 12), 1))
            )
          )
        ),
        
        # Row 2: Axis texts
        fluidRow(
          column(6,
            div(
              sliderInput("ui_axis_title_size", "Axis title", 
                         value = e$axis_title_size %||% 1.0, 
                         min = 0.5, max = 2.5, step = 0.1),
              tags$small(style="color:#777;", 
                         "Size: ", 
                         round((e$axis_title_size %||% 1.0) * (e$base_size %||% 12), 1))
            )
          ),
          column(6,
            div(
              sliderInput("ui_axis_text_size", "Axis text", 
                         value = e$axis_text_size %||% 0.8, 
                         min = 0.5, max = 2.0, step = 0.1),
              tags$small(style="color:#777;", 
                         "Size: ", 
                         round((e$axis_text_size %||% 0.8) * (e$base_size %||% 12), 1))
            )
          )
        ),
        
        # Row 3: Legends
        fluidRow(
          column(6,
            div(
              sliderInput("ui_legend_title_size", "Legend title", 
                         value = e$legend_title_size %||% 1.0, 
                         min = 0.5, max = 2.5, step = 0.1),
              tags$small(style="color:#777;", 
                         "Size: ", 
                         round((e$legend_title_size %||% 1.0) * (e$base_size %||% 12), 1))
            )
          ),
          column(6,
            div(
              sliderInput("ui_legend_text_size", "Legend text", 
                         value = e$legend_text_size %||% 0.8, 
                         min = 0.5, max = 2.0, step = 0.1),
              tags$small(style="color:#777;", 
                         "Size: ", 
                         round((e$legend_text_size %||% 0.8) * (e$base_size %||% 12), 1))
            )
          )
        ),
        
        # Row 4: Caption (alone, centered)
        fluidRow(
          column(6, offset = 3,
            div(
              sliderInput("ui_caption_size", "Caption", 
                         value = e$caption_size %||% 0.8, 
                         min = 0.5, max = 2.0, step = 0.1),
              tags$small(style="color:#777;", 
                         "Size: ", 
                         round((e$caption_size %||% 0.8) * (e$base_size %||% 12), 1))
            )
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
    # Use debouncing for smooth typing experience (500ms delay)
    debounced_observeEvent(input[[input_id]], {
      if (!is.null(rv$is_hydrating) && rv$is_hydrating) return()
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
    }, delay = 500, ignoreInit = TRUE, ignoreNULL = TRUE)
  }
  
  # Persist selected sub-tab
  observeEvent(input$text_tabs, {
    rv$tabs$text <- input$text_tabs
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  # Basic text labels
  bind_edit("ui_title",    "title")
  bind_edit("ui_subtitle", "subtitle")
  bind_edit("ui_caption",  "caption")
  bind_edit("ui_xlab",     "xlab")
  bind_edit("ui_ylab",     "ylab")
  
  # Sliders - use throttling for responsive updates (200ms delay)
  bind_edit_slider <- function(input_id, field) {
    throttled_observeEvent(input[[input_id]], {
      if (!is.null(rv$is_hydrating) && rv$is_hydrating) return()
      ap <- rv$active_tab
      if (is.null(ap) || identical(ap, "Grid")) return()
      
      plot_index <- NULL
      if (ap %in% names(rv$plots)) {
        plot_index <- as.numeric(ap)
      }
      
      if (is.null(plot_index)) return()
      
      load_plot_settings(rv, plot_index)
      rv$edits[[as.character(plot_index)]][[field]] <- input[[input_id]]
    }, delay = 200, ignoreInit = TRUE, ignoreNULL = TRUE)
  }
  
  # Numeric inputs - use debouncing for smooth typing (500ms delay)
  bind_edit_numeric <- function(input_id, field) {
    debounced_observeEvent(input[[input_id]], {
      if (!is.null(rv$is_hydrating) && rv$is_hydrating) return()
      ap <- rv$active_tab
      if (is.null(ap) || identical(ap, "Grid")) return()
      
      plot_index <- NULL
      if (ap %in% names(rv$plots)) {
        plot_index <- as.numeric(ap)
      }
      
      if (is.null(plot_index)) return()
      
      load_plot_settings(rv, plot_index)
      rv$edits[[as.character(plot_index)]][[field]] <- input[[input_id]]
    }, delay = 500, ignoreInit = TRUE, ignoreNULL = TRUE)
  }
  
  # Base size (moved from theme) - slider
  bind_edit_slider("ui_base_size", "base_size")
  
  # Text sizes - sliders
  bind_edit_slider("ui_title_size", "title_size")
  bind_edit_slider("ui_subtitle_size", "subtitle_size")
  bind_edit_slider("ui_caption_size", "caption_size")
  bind_edit_slider("ui_axis_title_size", "axis_title_size")
  bind_edit_slider("ui_axis_text_size", "axis_text_size")
  bind_edit_slider("ui_legend_title_size", "legend_title_size")
  bind_edit_slider("ui_legend_text_size", "legend_text_size")
  
  # Axis limits - numeric inputs
  bind_edit_numeric("ui_x_min", "x_min")
  bind_edit_numeric("ui_x_max", "x_max")
  bind_edit_numeric("ui_y_min", "y_min")
  bind_edit_numeric("ui_y_max", "y_max")
  
  # Axis steps - numeric inputs
  bind_edit_numeric("ui_x_step_minor", "x_step_minor")
  bind_edit_numeric("ui_x_step_major", "x_step_major")
  bind_edit_numeric("ui_y_step_minor", "y_step_minor")
  bind_edit_numeric("ui_y_step_major", "y_step_major")
  
  observeEvent(input$apply_all_text, {
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
