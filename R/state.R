# plot-editor/R/state.R

init_reactive_state <- function() {
  reactiveValues(
    # master source plots
    plots   = list(),
    
    # per-plot edits shown in PLOT tabs
    edits   = list(),     # list(name -> list(title, xlab, theme, ...))
    
    # per-plot export shown in PLOT tabs
    export  = list(),     # list(name -> list(width_mm, height_mm, dpi, format))
    
    # GRID-only stores (isolated)
    grid = list(
      rows    = BASE$grid_rows,
      cols    = BASE$grid_cols,
      collect = BASE$grid_collect,
      legend  = BASE$grid_legend_pos,
      cells   = as.list(rep("(empty)", BASE$grid_rows * BASE$grid_cols))  # assignments by name
    ),
    grid_edits  = list(),   # list(plotName -> edit list) used only when rendering in grid
    grid_export = list(     # export for the combined grid canvas
      width_mm  = BASE$width_mm,
      height_mm = BASE$height_mm,
      dpi       = BASE$dpi,
      format    = BASE$format
    ),
    
    # ui state
    active_tab   = "Grid",
    is_hydrating = FALSE
  )
}

ensure_edits <- function(rv, name, grid = FALSE) {
  bucket <- if (grid) "grid_edits" else "edits"
  
  if (is.null(rv[[bucket]][[name]])) {
    # Seed labels from the plot
    p <- rv$plots[[name]]
    get_lab <- function(lbl) {
      v <- tryCatch(p$labels[[lbl]], error = function(...) NULL)
      if (is.null(v)) NULL else as.character(v)
    }
    
    # Get current plot ranges for axis limits
    get_x_range <- function() {
      tryCatch({
        x_var <- p$mapping$x %||% names(p$data)[1]
        if (!is.null(x_var) && x_var %in% names(p$data)) {
          x_data <- p$data[[x_var]]
          if (is.numeric(x_data)) range(x_data, na.rm = TRUE) else NULL
        } else NULL
      }, error = function(...) NULL)
    }
    
    get_y_range <- function() {
      tryCatch({
        y_var <- p$mapping$y %||% names(p$data)[2]
        if (!is.null(y_var) && y_var %in% names(p$data)) {
          y_data <- p$data[[y_var]]
          if (is.numeric(y_data)) range(y_data, na.rm = TRUE) else NULL
        } else NULL
      }, error = function(...) NULL)
    }
    
    x_range <- get_x_range()
    y_range <- get_y_range()
    
    rv[[bucket]][[name]] <- list(
      # Basic text labels
      title      = get_lab("title"),
      subtitle   = get_lab("subtitle"),
      caption    = get_lab("caption"),
      xlab       = get_lab("x"),
      ylab       = get_lab("y"),
      
      # Theme settings
      theme      = BASE$theme,
      base_size  = BASE$base_size,
      legend_pos = BASE$legend_pos,
      
      # Text sizes
      title_size      = BASE$title_size,
      subtitle_size   = BASE$subtitle_size,
      caption_size    = BASE$caption_size,
      axis_title_size = BASE$axis_title_size,
      axis_text_size  = BASE$axis_text_size,
      legend_title_size = BASE$legend_title_size,
      legend_text_size  = BASE$legend_text_size,
      
      # Axis limits (from plot data if available)
      x_min = if (!is.null(x_range)) x_range[1] else NULL,
      x_max = if (!is.null(x_range)) x_range[2] else NULL,
      y_min = if (!is.null(y_range)) y_range[1] else NULL,
      y_max = if (!is.null(y_range)) y_range[2] else NULL,
      
      # Axis breaks (default to NULL, user can set)
      x_major = NULL,
      x_minor = NULL,
      y_major = NULL,
      y_minor = NULL,
      
      # Theme customizations
      legend_box = TRUE,
      panel_bg = "Default",
      plot_bg = "Default",
      grid_major = TRUE,
      grid_minor = TRUE,
      grid_color = "Default"
    )
  }
  
  if (!grid && is.null(rv$export[[name]])) {
    rv$export[[name]] <- list(
      width_mm  = BASE$width_mm,
      height_mm = BASE$height_mm,
      dpi       = BASE$dpi,
      format    = BASE$format
    )
  }
}

resize_cells <- function(rv, rows, cols) {
  old <- rv$grid$cells %||% list()
  new_len <- max(1, rows * cols)
  new <- vector("list", new_len)
  for (i in seq_len(new_len)) new[[i]] <- old[[i]] %||% "(empty)"
  rv$grid$cells <- new
  rv$grid$rows  <- rows
  rv$grid$cols  <- cols
}
