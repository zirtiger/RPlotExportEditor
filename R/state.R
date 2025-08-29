# plot-editor/R/state.R

# Initializes and manages the app's reactive state

init_reactive_state <- function() {
  shiny::reactiveValues(
    # Plots and names
    plots = list(),            # key: plot id (character)
    plot_names = list(),       # key: plot id -> display name
    next_plot_index = 1L,      # next numeric index for new plots

    # Per-plot settings
    originals = list(),        # extracted original settings per plot id
    edits = list(),            # per-plot edits (standalone)
    grid_edits = list(),       # per-plot edits used only for grid rendering
    export = list(),           # per-plot export settings

    # Grid settings
    grid = list(
      rows = BASE$grid_rows,
      cols = BASE$grid_cols,
      collect = BASE$grid_collect,
      legend = BASE$grid_legend_pos,
      cells = as.list(rep("(empty)", BASE$grid_rows * BASE$grid_cols))
    ),
    grid_export = list(
      width_mm = BASE$width_mm,
      height_mm = BASE$height_mm,
      dpi = BASE$dpi,
      format = BASE$format
    ),

    # UI state
    active_tab = "Grid",
    tabs = list(),             # persists last-opened sub-tab per pane
    is_hydrating = FALSE,      # guards against observer feedback while updating UI
    force_ui_update = 0L       # simple counter for temporary UI guard usage
  )
}

# Ensure that edit/export structures exist for a given plot
ensure_edits <- function(rv, plot_key, grid = FALSE) {
  key <- as.character(plot_key)

  if (isTRUE(grid)) {
    if (is.null(rv$grid_edits[[key]])) {
      base <- rv$edits[[key]]
      if (is.null(base)) base <- rv$originals[[key]]
      rv$grid_edits[[key]] <- base %||% list()
    }
    return(invisible(TRUE))
  }

  # Per-plot standalone edits
  if (is.null(rv$edits[[key]])) {
    base <- rv$originals[[key]]
    if (is.null(base)) {
      base <- list(
        # Labels
        title = NULL, subtitle = NULL, caption = NULL, xlab = NULL, ylab = NULL,
        # Theme
        theme = BASE$theme, base_size = BASE$base_size,
        legend_pos = BASE$legend_pos, legend_box = FALSE,
        panel_bg = NULL, plot_bg = NULL,
        # Grid lines
        grid_major = TRUE, grid_minor = TRUE,
        grid_major_linetype = "solid", grid_minor_linetype = "dashed",
        grid_color = NULL,
        # Axis
        x_min = NULL, x_max = NULL, y_min = NULL, y_max = NULL,
        x_step_major = 1, x_step_minor = 0.5, y_step_major = 1, y_step_minor = 0.5,
        # Colors
        palette = "None",
        continuous_colour_palette = "None",
        continuous_fill_palette = "None",
        colour_levels = character(0), colour_levels_cols = character(0),
        fill_levels = character(0),   fill_levels_cols = character(0),
        # Text sizes
        title_size = BASE$title_size,
        subtitle_size = BASE$subtitle_size,
        caption_size = BASE$caption_size,
        axis_title_size = BASE$axis_title_size,
        axis_text_size = BASE$axis_text_size,
        legend_title_size = BASE$legend_title_size,
        legend_text_size = BASE$legend_text_size
      )
    }
    rv$edits[[key]] <- base
  }

  # Per-plot export defaults
  if (is.null(rv$export[[key]])) {
    rv$export[[key]] <- list(
      width_mm = BASE$width_mm,
      height_mm = BASE$height_mm,
      dpi = BASE$dpi,
      format = BASE$format
    )
  }

  invisible(TRUE)
}

# Resize grid (rows x cols) while preserving existing cell assignments when possible
resize_cells <- function(rv, rows, cols) {
  r <- as.integer(rows); c <- as.integer(cols)
  if (!is.finite(r) || r < 1) r <- BASE$grid_rows
  if (!is.finite(c) || c < 1) c <- BASE$grid_cols

  rv$grid$rows <- r
  rv$grid$cols <- c

  n <- r * c
  cells <- rv$grid$cells %||% list()
  old_n <- length(cells)

  # Trim or extend
  if (old_n >= n) {
    cells <- cells[seq_len(n)]
  } else {
    cells <- c(cells, as.list(rep("(empty)", n - old_n)))
  }

  # Normalize empty entries
  for (k in seq_len(n)) {
    if (is.null(cells[[k]]) || !nzchar(cells[[k]])) cells[[k]] <- "(empty)"
  }
  rv$grid$cells <- cells
  invisible(TRUE)
}

# Get the current value for a setting, preferring edits, then originals, then default
get_current_value <- function(rv, plot_key, setting, default = NULL) {
  key <- as.character(plot_key)
  e <- rv$edits[[key]]
  if (!is.null(e) && !is.null(e[[setting]])) return(e[[setting]])
  o <- rv$originals[[key]]
  if (!is.null(o) && !is.null(o[[setting]])) return(o[[setting]])
  default
}

