# plot-editor/R/state.R
# Reactive state management for the plot editor

init_reactive_state <- function() {
  reactiveValues(
    # Plot management
    plots = list(),
    plot_names = list(),
    next_plot_index = 1,
    active_tab = NULL,
    
    # Grid layout
    grid = list(
      rows = BASE$grid_rows,
      cols = BASE$grid_cols,
      cells = list(),
      collect = BASE$grid_collect,
      legend = BASE$grid_legend_pos
    ),
    
    # Grid export settings
    grid_export = list(
      width_mm = BASE$width_mm,
      height_mm = BASE$height_mm,
      dpi = BASE$dpi,
      format = BASE$format
    ),
    
    # Grid-specific edits
    grid_edits = list(),
    
    # Tab selections for each pane
    tabs = list(
      grid = "Layout",
      export = "Settings",
      text = "Content",
      theme = "Base"
    ),
    
    # Plot edits and originals
    edits = list(),
    originals = list(),
    
    # Export settings
    export = list(),
    
    # UI state
    is_hydrating = FALSE,
    force_ui_update = 0,
    last_mainmenu = "text"
  )
}

# Ensure edits exist for a plot
ensure_edits <- function(rv, plot_name, grid = FALSE) {
  if (is.null(plot_name) || is.null(rv$plots[[plot_name]])) return()
  
  plot_index <- which(names(rv$plots) == plot_name)
  if (length(plot_index) == 0) return()
  
  index_str <- as.character(plot_index)
  
  # Initialize edits if they don't exist
  if (is.null(rv$edits[[index_str]]) || length(rv$edits[[index_str]]) == 0) {
    rv$edits[[index_str]] <- list()
  }
  
  # Initialize originals if they don't exist
  if (is.null(rv$originals[[index_str]]) || length(rv$originals[[index_str]]) == 0) {
    rv$originals[[index_str]] <- list()
  }
  
  # Initialize export settings if they don't exist
  if (is.null(rv$export[[index_str]]) || length(rv$export[[index_str]]) == 0) {
    rv$export[[index_str]] <- list(
      width_mm = BASE$width_mm,
      height_mm = BASE$height_mm,
      dpi = BASE$dpi,
      format = BASE$format
    )
  }
}

# Resize grid cells
resize_cells <- function(rv, rows, cols) {
  if (is.null(rows) || is.null(cols)) return()
  
  rv$grid$rows <- rows
  rv$grid$cols <- cols
  
  # Create or resize cells
  total_cells <- rows * cols
  current_cells <- length(rv$grid$cells)
  
  if (total_cells > current_cells) {
    # Add new cells
    for (i in (current_cells + 1):total_cells) {
      rv$grid$cells[[i]] <- "(empty)"
    }
  } else if (total_cells < current_cells) {
    # Remove excess cells
    rv$grid$cells <- rv$grid$cells[1:total_cells]
  }
}

# Apply edits to a plot
apply_edits <- function(plot_obj, edits) {
  if (is.null(edits) || length(edits) == 0) return(plot_obj)
  
  # Apply theme changes
  if (!is.null(edits$theme)) {
    plot_obj <- plot_obj + get_theme_fun(edits$theme)()
  }
  
  # Apply base size changes
  if (!is.null(edits$base_size)) {
    plot_obj <- plot_obj + theme(text = ggplot2::element_text(size = edits$base_size))
  }
  
  # Apply legend position changes
  if (!is.null(edits$legend_pos)) {
    plot_obj <- plot_obj + theme(legend.position = edits$legend_pos)
  }
  
  # Apply other theme customizations
  if (!is.null(edits$panel_bg)) {
    plot_obj <- plot_obj + theme(panel.background = ggplot2::element_rect(fill = edits$panel_bg))
  }
  
  if (!is.null(edits$plot_bg)) {
    plot_obj <- plot_obj + theme(plot.background = ggplot2::element_rect(fill = edits$plot_bg))
  }
  
  if (!is.null(edits$grid_major)) {
    plot_obj <- plot_obj + theme(panel.grid.major = ggplot2::element_line(color = edits$grid_major))
  }
  
  if (!is.null(edits$grid_minor)) {
    plot_obj <- plot_obj + theme(panel.grid.minor = ggplot2::element_line(color = edits$grid_minor))
  }
  
  return(plot_obj)
}

# Export plot to file
export_plot <- function(plot_obj, file_path, export_settings) {
  if (is.null(export_settings)) return()
  
  width_mm <- export_settings$width_mm %||% BASE$width_mm
  height_mm <- export_settings$height_mm %||% BASE$height_mm
  dpi <- export_settings$dpi %||% BASE$dpi
  format <- export_settings$format %||% BASE$format
  
  # Convert mm to inches for ggsave
  width_in <- width_mm / 25.4
  height_in <- height_mm / 25.4
  
  # Export based on format
  switch(format,
         "PNG" = ggplot2::ggsave(file_path, plot_obj, width = width_in, height = height_in, dpi = dpi),
         "TIFF" = ggplot2::ggsave(file_path, plot_obj, width = width_in, height = height_in, dpi = dpi, device = "tiff"),
         "PDF" = ggplot2::ggsave(file_path, plot_obj, width = width_in, height = height_in, device = "pdf"),
         "SVG" = ggplot2::ggsave(file_path, plot_obj, width = width_in, height = height_in, device = "svg"),
         "EPS" = ggplot2::ggsave(file_path, plot_obj, width = width_in, height = height_in, device = "eps"),
         ggplot2::ggsave(file_path, plot_obj, width = width_in, height = height_in, dpi = dpi)  # Default to PNG
  )
}

# Render preview as PNG
render_preview_png <- function(plot_obj, width_mm, height_mm, dpi, format) {
  # Convert mm to inches
  width_in <- width_mm / 25.4
  height_in <- height_mm / 25.4
  
  # Create temporary file
  temp_file <- base::tempfile(fileext = ".png")
  
  # Save plot as PNG
  ggplot2::ggsave(temp_file, plot_obj, width = width_in, height = height_in, dpi = dpi)
  
  # Return image info for renderImage
  list(
    src = temp_file,
    contentType = "image/png",
    width = width_in * dpi,
    height = height_in * dpi,
    alt = "Plot preview"
  )
}