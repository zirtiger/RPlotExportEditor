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
    rv[[bucket]][[name]] <- list(
      title      = get_lab("title"),
      subtitle   = get_lab("subtitle"),
      caption    = get_lab("caption"),
      xlab       = get_lab("x"),
      ylab       = get_lab("y"),
      theme      = BASE$theme,
      base_size  = BASE$base_size,
      legend_pos = BASE$legend_pos,
      # sizes
      plot_title_size    = NULL,
      plot_subtitle_size = NULL,
      axis_title_size    = NULL,
      axis_text_size     = NULL,
      legend_title_size  = NULL,
      legend_text_size   = NULL,
      # axis controls
      xlim_min = BASE$xlim_min,
      xlim_max = BASE$xlim_max,
      ylim_min = BASE$ylim_min,
      ylim_max = BASE$ylim_max,
      x_breaks_step        = BASE$x_breaks_step,
      x_minor_breaks_step  = BASE$x_minor_breaks_step,
      y_breaks_step        = BASE$y_breaks_step,
      y_minor_breaks_step  = BASE$y_minor_breaks_step
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
