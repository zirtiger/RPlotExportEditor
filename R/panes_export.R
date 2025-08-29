# plot-editor/R/panes_export.R

export_pane_ui <- function(rv) {
  grid_ctx <- identical(rv$active_tab, "Grid")
  
  if (grid_ctx) {
    ex <- rv$grid_export
    return(tagList(
      h4("Export — Grid canvas"),
      tags$hr(),
      
      # Use tabs for better organization
      tabsetPanel(
        id = "export_tabs",
        selected = rv$tabs$export %||% "Dimensions",
        tabPanel("Dimensions",
          fluidRow(
            column(6, sliderInput("ui_gridexp_w", "Width (mm)", 
                                 value = ex$width_mm %||% BASE$width_mm, 
                                 min = 50, max = 1000, step = 10)),
            column(6, sliderInput("ui_gridexp_h", "Height (mm)", 
                                 value = ex$height_mm %||% BASE$height_mm, 
                                 min = 50, max = 1000, step = 10))
          )
        ),
        tabPanel("Quality",
          selectInput("ui_gridexp_d", "DPI", 
                     choices = c("72" = 72, "150" = 150, "300" = 300, "600" = 600, "Custom" = "custom"),
                     selected = ifelse(ex$dpi %||% BASE$dpi %in% c(72, 150, 300, 600), 
                                     as.character(ex$dpi %||% BASE$dpi), "custom")),
          conditionalPanel(
            condition = "input.ui_gridexp_d == 'custom'",
            sliderInput("ui_gridexp_d_custom", "Custom DPI", 
                       value = ex$dpi %||% BASE$dpi, 
                       min = 72, max = 1200, step = 10)
          ),
          selectInput("ui_gridexp_f", "Format", 
                     choices = c("PNG","TIFF","PDF","SVG","EPS"), 
                     selected = ex$format %||% BASE$format)
        )
      ),
      tags$hr(),
      downloadButton("download_grid_export", "Download Grid", class = "btn btn-success btn-block")
    ))
  }
  
  ap <- rv$active_tab
  if (is.null(ap) || is.null(rv$plots[[ap]])) {
    return(tagList(h4("Export"), helpText("Select a plot tab to configure export settings.")))
  }
  
  ex <- rv$export[[ap]]
  
  tagList(
    actionButton("apply_all_export", "Use for all plots", class = "btn btn-sm btn-default btn-block"),
    		tags$hr(),
    
    # Use tabs for better organization
    tabsetPanel(
      id = "export_tabs",
      selected = rv$tabs$export %||% "Dimensions",
      tabPanel("Dimensions",
        fluidRow(
          column(6, sliderInput("ui_exp_width", "Width (mm)", 
                               value = ex$width_mm %||% BASE$width_mm, 
                               min = 20, max = 1000, step = 5)),
          column(6, sliderInput("ui_exp_height", "Height (mm)", 
                               value = ex$height_mm %||% BASE$height_mm, 
                               min = 20, max = 1000, step = 5))
        )
      ),
      tabPanel("Quality",
        selectInput("ui_exp_dpi", "DPI", 
                   choices = c("72" = 72, "150" = 150, "300" = 300, "600" = 600, "Custom" = "custom"),
                   selected = ifelse(ex$dpi %||% BASE$dpi %in% c(72, 150, 300, 600), 
                                   as.character(ex$dpi %||% BASE$dpi), "custom")),
        conditionalPanel(
          condition = "input.ui_exp_dpi == 'custom'",
          sliderInput("ui_exp_dpi_custom", "Custom DPI", 
                     value = ex$dpi %||% BASE$dpi, 
                     min = 72, max = 1200, step = 10)
        ),
        selectInput("ui_exp_format", "Format", 
                   choices = c("PNG","TIFF","PDF","SVG","EPS"),
                   selected = toupper(ex$format %||% BASE$format))
      )
    ),
    tags$hr(),
    downloadButton(paste0("download_plot_export_", ap), 
                  paste("Download", ap), 
                  class = "btn btn-success btn-block")
  )
}

register_export_observers <- function(input, rv, session) {
  # Grid export writers
  observeEvent(input$ui_gridexp_w, { 
    rv$grid_export$width_mm <- as_num_safe(input$ui_gridexp_w) 
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  observeEvent(input$ui_gridexp_h, { 
    rv$grid_export$height_mm <- as_num_safe(input$ui_gridexp_h) 
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  observeEvent(input$ui_gridexp_d, {
    if (input$ui_gridexp_d == "custom") {
      rv$grid_export$dpi <- as_num_safe(input$ui_gridexp_d_custom)
    } else {
      rv$grid_export$dpi <- as.numeric(input$ui_gridexp_d)
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  observeEvent(input$ui_gridexp_d_custom, {
    if (input$ui_gridexp_d == "custom") {
      rv$grid_export$dpi <- as_num_safe(input$ui_gridexp_d_custom)
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  observeEvent(input$ui_gridexp_f, {
    rv$grid_export$format <- input$ui_gridexp_f
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  # Per-plot export writers
  observeEvent(input$ui_exp_width, {
    ap <- rv$active_tab; if (is.null(ap) || is.null(rv$plots[[ap]])) return()
    ensure_edits(rv, ap, grid = FALSE)
    rv$export[[ap]]$width_mm <- as_num_safe(input$ui_exp_width)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  observeEvent(input$ui_exp_height, {
    ap <- rv$active_tab; if (is.null(ap) || is.null(rv$plots[[ap]])) return()
    ensure_edits(rv, ap, grid = FALSE)
    rv$export[[ap]]$height_mm <- as_num_safe(input$ui_exp_height)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  observeEvent(input$ui_exp_dpi, {
    ap <- rv$active_tab; if (is.null(ap) || is.null(rv$plots[[ap]])) return()
    ensure_edits(rv, ap, grid = FALSE)
    if (input$ui_exp_dpi == "custom") {
      rv$export[[ap]]$dpi <- as_num_safe(input$ui_exp_dpi_custom)
    } else {
      rv$export[[ap]]$dpi <- as.numeric(input$ui_exp_dpi)
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  observeEvent(input$ui_exp_dpi_custom, {
    ap <- rv$active_tab; if (is.null(ap) || is.null(rv$plots[[ap]])) return()
    ensure_edits(rv, ap, grid = FALSE)
    if (input$ui_exp_dpi == "custom") {
      rv$export[[ap]]$dpi <- as_num_safe(input$ui_exp_dpi_custom)
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  observeEvent(input$ui_exp_format, {
    ap <- rv$active_tab; if (is.null(ap) || is.null(rv$plots[[ap]])) return()
    ensure_edits(rv, ap, grid = FALSE)
    rv$export[[ap]]$format <- toupper(input$ui_exp_format)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  observeEvent(input$apply_all_export, {
    ap <- rv$active_tab
    if (is.null(ap) || identical(ap, "Grid") || is.null(rv$plots[[ap]])) return()
    ensure_edits(rv, ap, grid = FALSE)
    src <- rv$export[[ap]]
    export_fields <- c("width_mm","height_mm","dpi","format")
    for (nm in names(rv$plots)) {
      ensure_edits(rv, nm, grid = FALSE)
      rv$export[[nm]][export_fields] <- src[export_fields]
    }
    showNotification("Export settings applied to all plots.", type = "message")
  })
  
  # Persist selected sub-tab
  observeEvent(input$export_tabs, {
    rv$tabs$export <- input$export_tabs
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
}
