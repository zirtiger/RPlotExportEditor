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
		is_hydrating = FALSE,
		
		# persisted option sub-tab selections
		tabs = list(
			text   = "Labels",
			theme  = "Base",
			export = "Dimensions",
			grid   = "Layout"
		)
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
		
		# Get axis limits from the plot if available
		get_axis_limits <- function(axis) {
			if (axis == "x") {
				if (inherits(p$coordinates, "CoordCartesian")) {
					x_range <- p$coordinates$limits$x
					if (!is.null(x_range) && length(x_range) == 2) {
						return(list(min = x_range[1], max = x_range[2]))
					}
				}
				# Try to get from scales
				if (length(p$scales$scales) > 0) {
					x_scale_idx <- which(sapply(p$scales$scales, function(s) {
						tryCatch("x" %in% s$aesthetics, error = function(...) FALSE)
					}))
					if (length(x_scale_idx) > 0) {
						x_scale <- p$scales$scales[[x_scale_idx[1]]]
						if (!is.null(x_scale$range$range)) {
							return(list(min = x_scale$range$range[1], max = x_scale$range$range[2]))
						}
					}
				}
			} else if (axis == "y") {
				if (inherits(p$coordinates, "CoordCartesian")) {
					y_range <- p$coordinates$limits$y
					if (!is.null(y_range) && length(y_range) == 2) {
						return(list(min = y_range[1], max = y_range[2]))
					}
				}
				# Try to get from scales
				if (length(p$scales$scales) > 0) {
					y_scale_idx <- which(sapply(p$scales$scales, function(s) {
						tryCatch("y" %in% s$aesthetics, error = function(...) FALSE)
					}))
					if (length(y_scale_idx) > 0) {
						y_scale <- p$scales$scales[[y_scale_idx[1]]]
						if (!is.null(y_scale$range$range)) {
							return(list(min = y_scale$range$range[1], max = y_scale$range$range[2]))
						}
					}
				}
			}
			return(NULL)
		}
		
		x_limits <- get_axis_limits("x")
		y_limits <- get_axis_limits("y")
		
		rv[[bucket]][[name]] <- list(
			title      = get_lab("title"),
			subtitle   = get_lab("subtitle"),
			caption    = get_lab("caption"),
			xlab       = get_lab("x"),
			ylab       = get_lab("y"),
			theme      = BASE$theme,
			base_size  = BASE$base_size,
			legend_pos = BASE$legend_pos,
			x_min      = x_limits$min,
			x_max      = x_limits$max,
			y_min      = y_limits$min,
			y_max      = y_limits$max
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
