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
      cells = lapply(seq_len(BASE$grid_rows * BASE$grid_cols), function(i) "(empty)"),
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
    
    # Copy originals to edits so they appear in the UI
    # This preserves NULL values (ggplot handles these automatically)
    orig_settings <- rv$originals[[index_str]]
    if (!is.null(orig_settings)) {
      for (setting_name in names(orig_settings)) {
        rv$edits[[index_str]][[setting_name]] <- orig_settings[[setting_name]]
      }
    }
    
    # Initialize ONLY app-level settings with BASE defaults
    # These are settings that cannot be extracted from plots
    app_settings <- c("width_mm", "height_mm", "dpi", "format")
    for (setting in app_settings) {
      if (is.null(rv$edits[[index_str]][[setting]])) {
        rv$edits[[index_str]][[setting]] <- BASE[[setting]]
      }
    }
    
    # Handle base_size separately - use extracted value if available, otherwise BASE default
    if (is.null(rv$edits[[index_str]]$base_size)) {
      rv$edits[[index_str]]$base_size <- rv$originals[[index_str]]$base_size %||% BASE$base_size
    }
  } else {
    # Edits already exist, but ensure base_size is correct for this plot
    # This prevents accumulation when switching between plots
    if (!is.null(rv$originals[[index_str]]) && !is.null(rv$originals[[index_str]]$base_size)) {
      # Use the original plot's base_size
      rv$edits[[index_str]]$base_size <- rv$originals[[index_str]]$base_size

    } else {
      # Use BASE default if no original base_size
      rv$edits[[index_str]]$base_size <- BASE$base_size

    }
    

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

# Get current value for a setting, with smart fallback logic
get_current_value <- function(rv, plot_name, setting, default = NULL) {
  if (is.null(plot_name) || is.null(rv$plots[[plot_name]])) return(default)
  
  plot_index <- which(names(rv$plots) == plot_name)
  if (length(plot_index) == 0) return(default)
  
  index_str <- as.character(plot_index)
  
  # 1. Check edits first (user modifications)
  if (!is.null(rv$edits[[index_str]]) && !is.null(rv$edits[[index_str]][[setting]])) {
    return(rv$edits[[index_str]][[setting]])
  }
  
  # 2. Check originals (extracted from plot)
  if (!is.null(rv$originals[[index_str]]) && !is.null(rv$originals[[index_str]][[setting]])) {
    return(rv$originals[[index_str]][[setting]])
  }
  
  # 3. Check if ggplot handles this automatically
  if (is_ggplot_auto_handled(setting)) {
    return(NULL)  # Let ggplot handle it
  }
  
  # 4. Use BASE default only for app-level settings
  if (is_app_level_setting(setting)) {
    return(BASE[[setting]])
  }
  
  # 5. For base_size, use BASE default if no value available
  if (setting == "base_size") {
    # Safety check: ensure base_size is never accumulated
    # Always return the BASE default if no specific value is set
    return(BASE$base_size)
  }
  
  # 6. Return NULL (no value available)
  return(NULL)
}

# Get display value for UI (handles NULL values appropriately)
get_display_value <- function(rv, plot_name, setting, default = NULL) {
  value <- get_current_value(rv, plot_name, setting, default)
  
  # For UI display, we want to show the actual value
  # NULL means "not set, ggplot handles it"
  # FALSE means "explicitly disabled"
  # TRUE means "explicitly enabled"
  # Numeric/text means "actual value"
  
  return(value)
}

# Helper function to determine if ggplot handles a setting automatically
is_ggplot_auto_handled <- function(setting) {
  auto_handled <- c(
    # Steps - ggplot calculates these automatically
    "x_step_major", "x_step_minor", "y_step_major", "y_step_minor",
    
    # Grid details - ggplot has sensible defaults
    "grid_major_linetype", "grid_minor_linetype", "grid_color",
    
    # Theme details - ggplot has sensible defaults
    "panel_bg", "plot_bg", "legend_box",
    
    # Colors - ggplot has default palettes
    "palette",
    
    # Theme choice - ggplot has default theme
    "theme", "legend_pos"
  )
  
  setting %in% auto_handled
}

# Helper function to determine if a setting is app-level (should use BASE)
is_app_level_setting <- function(setting) {
  app_level <- c(
    # Export settings
    "width_mm", "height_mm", "dpi", "format",
    
    # Grid layout settings
    "grid_rows", "grid_cols", "grid_collect", "grid_legend_pos"
  )
  
  setting %in% app_level
}

# Resize grid cells
resize_cells <- function(rv, rows, cols) {
  if (is.null(rows) || is.null(cols)) return()
  
  rv$grid$rows <- rows
  rv$grid$cols <- cols
  
  # Create or resize cells
  total_cells <- rows * cols
  
  # Initialize cells if they don't exist
  if (is.null(rv$grid$cells) || length(rv$grid$cells) == 0) {
    rv$grid$cells <- lapply(seq_len(total_cells), function(i) "(empty)")
    return()
  }
  
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