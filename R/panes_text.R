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
  
  tagList(
    actionButton("apply_all_text", "Use for all plots", class = "btn btn-sm btn-default btn-block"),
    tags$hr(),
    h4(sprintf("Text — %s", ap)),
    tags$hr(),
    
    # Use tabs for better organization
    tabsetPanel(
      id = "text_tabs",
      selected = rv$tabs$text %||% "Labels",
      tabPanel("Labels", 
        textInput("ui_title",    "Title",    title_now),
        textInput("ui_subtitle", "Subtitle", subtitle_now),
        textInput("ui_caption",  "Caption",  caption_now),
        textInput("ui_xlab",     "X label",  e$xlab     %||% get_lab("x")),
        textInput("ui_ylab",     "Y label",  e$ylab     %||% get_lab("y"))
      ),
      tabPanel("Text Sizes",
        sliderInput("ui_title_size", "Title size", 
                   value = e$title_size %||% BASE$title_size, 
                   min = 8, max = 34, step = 1),
        div(class = if (nzchar(title_now)) NULL else "muted-control",
            sliderInput("ui_subtitle_size", "Subtitle size", 
                       value = e$subtitle_size %||% BASE$subtitle_size, 
                       min = 6, max = 30, step = 1)),
        div(class = if (nzchar(subtitle_now)) NULL else "muted-control",
            sliderInput("ui_caption_size", "Caption size", 
                       value = e$caption_size %||% BASE$caption_size, 
                       min = 6, max = 28, step = 1)),
        div(class = if (nzchar(caption_now)) NULL else "muted-control",
            sliderInput("ui_axis_title_size", "Axis title size", 
                       value = e$axis_title_size %||% BASE$axis_title_size, 
                       min = 8, max = 30, step = 1)),
        sliderInput("ui_axis_text_size", "Axis text size", 
                   value = e$axis_text_size %||% BASE$axis_text_size, 
                   min = 6, max = 28, step = 1),
        sliderInput("ui_legend_title_size", "Legend title size", 
                   value = e$legend_title_size %||% BASE$legend_title_size, 
                   min = 8, max = 30, step = 1),
        sliderInput("ui_legend_text_size", "Legend text size", 
                   value = e$legend_text_size %||% BASE$legend_text_size, 
                   min = 6, max = 28, step = 1)
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
          column(6, numericInput("ui_x_step_minor", "X minor step", value = e$x_step_minor, min = 0, step = 0.1)),
          column(6, numericInput("ui_x_step_major", "X major step", value = e$x_step_major, min = 0, step = 0.1))
        ),
        fluidRow(
          column(6, numericInput("ui_y_step_minor", "Y minor step", value = e$y_step_minor, min = 0, step = 0.1)),
          column(6, numericInput("ui_y_step_major", "Y major step", value = e$y_step_major, min = 0, step = 0.1))
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
