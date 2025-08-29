# plot-editor/R/panes_originals.R

originals_pane_ui <- function(rv) {
  if (length(rv$plots) == 0) {
    return(tagList(
      h4("Original Settings"),
      helpText("No plots loaded. Load some plots to view their original settings.")
    ))
  }
  
  # Get current active plot for context
  active_plot <- rv$active_tab
  if (is.null(active_plot) || active_plot == "Grid") {
    active_plot <- if (length(rv$plots) > 0) names(rv$plots)[1] else NULL
  }
  
  tagList(
    h4("Original Settings"),
    helpText("View the original settings that were loaded for each plot."),
    tags$hr(),
    
    # Plot selector
    if (length(rv$plots) > 1) {
      selectInput("originals_plot_selector", "Select Plot:", 
                  choices = names(rv$plots), 
                  selected = active_plot)
    },
    
    # Tabs for each main menu option
    tabsetPanel(
      id = "originals_tabs",
      tabPanel("Grid", originals_grid_tab(rv, active_plot)),
      tabPanel("Export", originals_export_tab(rv, active_plot)),
      tabPanel("Text", originals_text_tab(rv, active_plot)),
      tabPanel("Theme", originals_theme_tab(rv, active_plot))
    )
  )
}

# Grid tab showing original grid settings
originals_grid_tab <- function(rv, plot_name) {
  if (is.null(plot_name) || is.null(rv$plots[[plot_name]])) {
    return(helpText("Select a plot to view its original grid settings."))
  }
  
  plot_index <- which(names(rv$plots) == plot_name)
  if (length(plot_index) == 0) return(helpText("Plot not found."))
  
  index_str <- as.character(plot_index)
  originals <- rv$originals[[index_str]]
  
  if (is.null(originals) || length(originals) == 0) {
    return(helpText("No original grid settings found for this plot."))
  }
  
  tagList(
    h5("Grid Settings"),
    fluidRow(
      column(6, strong("Grid Major:"), br(), 
             ifelse(isTRUE(originals$grid_major), "ON", "Not set")),
      column(6, strong("Grid Minor:"), br(), 
             ifelse(isTRUE(originals$grid_minor), "ON", "Not set"))
    ),
    tags$hr(),
    fluidRow(
      column(6, strong("Major Linetype:"), br(), 
             originals$grid_major_linetype %||% "Not set"),
      column(6, strong("Minor Linetype:"), br(), 
             originals$grid_minor_linetype %||% "Not set")
    )
  )
}

# Export tab showing original export settings
originals_export_tab <- function(rv, plot_name) {
  if (is.null(plot_name) || is.null(rv$plots[[plot_name]])) {
    return(helpText("Select a plot to view its original export settings."))
  }
  
  plot_index <- which(names(rv$plots) == plot_name)
  if (length(plot_index) == 0) return(helpText("Plot not found."))
  
  index_str <- as.character(plot_index)
  originals <- rv$originals[[index_str]]
  
  if (is.null(originals) || length(originals) == 0) {
    return(helpText("No original export settings found for this plot."))
  }
  
  tagList(
    h5("Export Settings"),
    fluidRow(
      column(6, strong("Width (mm):"), br(), 
             originals$width_mm %||% "Not set"),
      column(6, strong("Height (mm):"), br(), 
             originals$height_mm %||% "Not set")
    ),
    tags$hr(),
    fluidRow(
      column(6, strong("DPI:"), br(), 
             originals$dpi %||% "Not set"),
      column(6, strong("Format:"), br(), 
             originals$format %||% "Not set")
    )
  )
}

# Text tab showing original text settings
originals_text_tab <- function(rv, plot_name) {
  if (is.null(plot_name) || is.null(rv$plots[[plot_name]])) {
    return(helpText("Select a plot to view its original text settings."))
  }
  
  plot_index <- which(names(rv$plots) == plot_name)
  if (length(plot_index) == 0) return(helpText("Plot not found."))
  
  index_str <- as.character(plot_index)
  originals <- rv$originals[[index_str]]
  
  if (is.null(originals) || length(originals) == 0) {
    return(helpText("No original text settings found for this plot."))
  }
  
  tagList(
    h5("Labels"),
    fluidRow(
      column(12, strong("Title:"), br(), 
             ifelse(!is.null(originals$title) && nzchar(originals$title), originals$title, "Not set"))
    ),
    tags$hr(),
    fluidRow(
      column(12, strong("Subtitle:"), br(), 
             ifelse(!is.null(originals$subtitle) && nzchar(originals$subtitle), originals$subtitle, "Not set"))
    ),
    tags$hr(),
    fluidRow(
      column(12, strong("Caption:"), br(), 
             ifelse(!is.null(originals$caption) && nzchar(originals$caption), originals$caption, "Not set"))
    ),
    tags$hr(),
    fluidRow(
      column(6, strong("X Label:"), br(), 
             ifelse(!is.null(originals$xlab) && nzchar(originals$xlab), originals$xlab, "Not set")),
      column(6, strong("Y Label:"), br(), 
             ifelse(!is.null(originals$ylab) && nzchar(originals$ylab), originals$ylab, "Not set"))
    ),
    tags$hr(),
    h5("Text Sizes"),
    fluidRow(
      column(6, strong("Title Size:"), br(), 
             originals$title_size %||% "Not set"),
      column(6, strong("Subtitle Size:"), br(), 
             originals$subtitle_size %||% "Not set")
    ),
    tags$hr(),
    fluidRow(
      column(6, strong("Caption Size:"), br(), 
             originals$caption_size %||% "Not set"),
      column(6, strong("Axis Title Size:"), br(), 
             originals$axis_title_size %||% "Not set")
    ),
    tags$hr(),
    fluidRow(
      column(6, strong("Axis Text Size:"), br(), 
             originals$axis_text_size %||% "Not set"),
      column(6, strong("Legend Title Size:"), br(), 
             originals$legend_title_size %||% "Not set")
    ),
    tags$hr(),
    fluidRow(
      column(6, strong("Legend Text Size:"), br(), 
             originals$legend_text_size %||% "Not set")
    ),
    tags$hr(),
    h5("Axis Limits"),
    fluidRow(
      column(6, strong("X Min:"), br(), 
             ifelse(!is.null(originals$x_min), originals$x_min, "Not set")),
      column(6, strong("X Max:"), br(), 
             ifelse(!is.null(originals$x_max), originals$x_max, "Not set"))
    ),
    tags$hr(),
    fluidRow(
      column(6, strong("Y Min:"), br(), 
             ifelse(!is.null(originals$y_min), originals$y_min, "Not set")),
      column(6, strong("Y Max:"), br(), 
             ifelse(!is.null(originals$y_max), originals$y_max, "Not set"))
    ),
    tags$hr(),
    h5("Grid Steps"),
    fluidRow(
      column(6, strong("X Major Step:"), br(), 
             originals$x_step_major %||% "Not set"),
      column(6, strong("Y Major Step:"), br(), 
             originals$y_step_major %||% "Not set")
    ),
    tags$hr(),
    fluidRow(
      column(6, strong("X Minor Step:"), br(), 
             originals$x_step_minor %||% "Not set"),
      column(6, strong("Y Minor Step:"), br(), 
             originals$y_step_minor %||% "Not set")
    )
  )
}

# Theme tab showing original theme settings
originals_theme_tab <- function(rv, plot_name) {
  if (is.null(plot_name) || is.null(rv$plots[[plot_name]])) {
    return(helpText("Select a plot to view its original theme settings."))
  }
  
  plot_index <- which(names(rv$plots) == plot_name)
  if (length(plot_index) == 0) return(helpText("Plot not found."))
  
  index_str <- as.character(plot_index)
  originals <- rv$originals[[index_str]]
  
  if (is.null(originals) || length(originals) == 0) {
    return(helpText("No original theme settings found for this plot."))
  }
  
  tagList(
    h5("Theme"),
    fluidRow(
      column(6, strong("Theme:"), br(), 
             originals$theme %||% "Not set"),
      column(6, strong("Base Size:"), br(), 
             originals$base_size %||% "Not set")
    ),
    tags$hr(),
    fluidRow(
      column(6, strong("Legend Position:"), br(), 
             originals$legend_pos %||% "Not set"),
      column(6, strong("Legend Box:"), br(), 
             ifelse(isTRUE(originals$legend_box), "ON", "Not set"))
    ),
    tags$hr(),
    h5("Colors"),
    fluidRow(
      column(6, strong("Continuous Colour Palette:"), br(), 
             originals$continuous_colour_palette %||% "None"),
      column(6, strong("Continuous Fill Palette:"), br(), 
             originals$continuous_fill_palette %||% "None")
    ),
    tags$hr(),
    if (length(originals$colour_levels) > 0) {
      tagList(
        h6("Discrete Colour Levels:"),
        fluidRow(
          column(12, 
                 lapply(seq_along(originals$colour_levels), function(i) {
                   level <- originals$colour_levels[i]
                   color <- if (i <= length(originals$colour_levels_cols)) 
                     originals$colour_levels_cols[i] else "Not set"
                   div(
                     strong(level), ": ", 
                     span(style = paste0("color:", color, ";"), color),
                     br()
                   )
                 })
          )
        ),
        tags$hr()
      )
    },
    if (length(originals$fill_levels) > 0) {
      tagList(
        h6("Discrete Fill Levels:"),
        fluidRow(
          column(12, 
                 lapply(seq_along(originals$fill_levels), function(i) {
                   level <- originals$fill_levels[i]
                   color <- if (i <= length(originals$fill_levels_cols)) 
                     originals$fill_levels_cols[i] else "Not set"
                   div(
                     strong(level), ": ", 
                     span(style = paste0("color:", color, ";"), color),
                     br()
                   )
                 })
          )
        )
      )
    }
  )
}