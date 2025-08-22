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
		
		# Helpers to extract limits and suggested steps
		get_axis_from_build <- function(axis) {
			res <- list(min = NULL, max = NULL, step_major = NULL, step_minor = NULL)
			gb <- try(ggplot2::ggplot_build(p), silent = TRUE)
			if (inherits(gb, "try-error")) return(res)
			pp <- try(gb$layout$panel_params[[1]], silent = TRUE)
			if (inherits(pp, "try-error") || is.null(pp)) return(res)
			# ranges
			if (axis == "x") {
				range <- pp$x$range %||% pp$x.range %||% NULL
				brks  <- pp$x$breaks %||% pp$breaks_x %||% pp$xb$breaks %||% NULL
				mbrks <- pp$x$minor_breaks %||% pp$minor_breaks_x %||% NULL
			} else {
				range <- pp$y$range %||% pp$y.range %||% NULL
				brks  <- pp$y$breaks %||% pp$breaks_y %||% pp$yb$breaks %||% NULL
				mbrks <- pp$y$minor_breaks %||% pp$minor_breaks_y %||% NULL
			}
			if (!is.null(range) && length(range) >= 2) {
				res$min <- suppressWarnings(as.numeric(range[1]))
				res$max <- suppressWarnings(as.numeric(range[2]))
			}
			# steps
			if (!is.null(brks) && length(brks) >= 2) {
				d <- diff(sort(unique(as.numeric(brks))))
				if (length(d) > 0 && is.finite(median(d))) res$step_major <- unname(median(d))
			}
			if (!is.null(mbrks) && length(mbrks) >= 2) {
				dm <- diff(sort(unique(as.numeric(mbrks))))
				if (length(dm) > 0 && is.finite(median(dm))) res$step_minor <- unname(median(dm))
			}
			# fallback minor step
			if (is.null(res$step_minor) && !is.null(res$step_major) && is.finite(res$step_major)) {
				res$step_minor <- res$step_major / 2
			}
			res
		}
		
		get_theme_elem <- function(elem) {
			tryCatch(p$theme[[elem]], error = function(...) NULL)
		}
		is_blank <- function(x) inherits(x, "element_blank")
		
		x_info <- get_axis_from_build("x")
		y_info <- get_axis_from_build("y")
		
		# Snap limits to clean values using step or pretty()
		snap_limits <- function(minv, maxv, step) {
			if (is.finite(minv) && is.finite(maxv) && maxv > minv) {
				if (is.finite(step) && step > 0) {
					lo <- floor(minv / step) * step
					hi <- ceiling(maxv / step) * step
					c(lo, hi)
				} else {
					pr <- pretty(c(minv, maxv), n = 5)
					range(pr)[1:2]
				}
			} else c(minv, maxv)
		}
		if (!is.null(x_info$min) && !is.null(x_info$max)) {
			sx <- snap_limits(x_info$min, x_info$max, x_info$step_major %||% NA_real_)
			x_info$min <- sx[1]; x_info$max <- sx[2]
		}
		if (!is.null(y_info$min) && !is.null(y_info$max)) {
			sy <- snap_limits(y_info$min, y_info$max, y_info$step_major %||% NA_real_)
			y_info$min <- sy[1]; y_info$max <- sy[2]
		}
		
		# Theme-derived defaults
		maj_el <- get_theme_elem("panel.grid.major")
		min_el <- get_theme_elem("panel.grid.minor")
		lbox   <- get_theme_elem("legend.box.background")
		
		rv[[bucket]][[name]] <- list(
			title      = get_lab("title"),
			subtitle   = get_lab("subtitle"),
			caption    = get_lab("caption"),
			xlab       = get_lab("x"),
			ylab       = get_lab("y"),
			theme      = BASE$theme,
			base_size  = BASE$base_size,
			legend_pos = BASE$legend_pos,
			legend_box = if (!is.null(lbox)) !is_blank(lbox) else NULL,
			panel_bg   = NULL,
			plot_bg    = NULL,
			grid_major = if (!is.null(maj_el)) !is_blank(maj_el) else NULL,
			grid_minor = if (!is.null(min_el)) !is_blank(min_el) else NULL,
			grid_major_linetype = tryCatch(maj_el$linetype, error = function(...) NULL),
			grid_minor_linetype = tryCatch(min_el$linetype, error = function(...) NULL),
			
			# axis limits
			x_min      = x_info$min,
			x_max      = x_info$max,
			y_min      = y_info$min,
			y_max      = y_info$max,
			# step suggestions
			x_step_major = x_info$step_major,
			x_step_minor = x_info$step_minor,
			y_step_major = y_info$step_major,
			y_step_minor = y_info$step_minor
		)
		
		# Seed colour/fill levels with actual colors from the plot (if present)
		eval_levels <- function(p, aes_name) {
			if (!requireNamespace("rlang", quietly = TRUE)) return(list(levels = character(0), colors = character(0)))
			collect <- character(0)
			alt_aes <- if (identical(aes_name, "colour")) "color" else aes_name
			get_expr <- function(mapping) {
				if (is.null(mapping)) return(NULL)
				if (!is.null(mapping[[aes_name]])) return(mapping[[aes_name]])
				if (!is.null(mapping[[alt_aes]])) return(mapping[[alt_aes]])
				NULL
			}
			eval_on <- function(dat, expr) {
				if (is.null(expr) || is.null(dat)) return(NULL)
				if (!is.data.frame(dat) || nrow(dat) == 0) return(NULL)
				vals <- try(rlang::eval_tidy(expr, data = dat), silent = TRUE)
				if (inherits(vals, "try-error") || is.null(vals)) return(NULL)
				vals
			}
			append_vals <- function(vals) {
				if (is.null(vals)) return()
				vals <- vals[!is.na(vals)]
				if (is.factor(vals)) collect <<- c(collect, as.character(levels(vals))) else collect <<- c(collect, unique(as.character(vals)))
			}
			expr_p <- get_expr(p$mapping)
			append_vals(eval_on(p$data, expr_p))
			if (!is.null(p$layers) && length(p$layers)) {
				for (ly in p$layers) {
					expr_l <- get_expr(ly$mapping) %||% expr_p
					append_vals(eval_on(ly$data %||% p$data, expr_l))
				}
			}
			levels <- unique(collect[nzchar(collect)])
			
			# Extract actual colors from the plot
			colors <- character(0)
			if (length(levels) > 0) {
				# Try to get colors from ggplot_build
				tryCatch({
					built <- ggplot2::ggplot_build(p)
					if (!is.null(built$data) && length(built$data) > 0) {
						# Look for the aesthetic in the built data
						for (layer_data in built$data) {
							if (is.data.frame(layer_data) && nrow(layer_data) > 0) {
								# Check if this layer has the aesthetic
								if (aes_name %in% names(layer_data) || alt_aes %in% names(layer_data)) {
									aes_col <- layer_data[[aes_name]] %||% layer_data[[alt_aes]]
									if (!is.null(aes_col)) {
										# Get unique values and their corresponding colors
										unique_vals <- unique(aes_col)
										if (length(unique_vals) == length(levels)) {
											# Try to extract colors from the layer's geom
											colors <- tryCatch({
												# For most geoms, the color is in the data
												as.character(unique_vals)
											}, error = function(e) character(0))
											break
										}
									}
								}
							}
						}
					}
				}, error = function(e) {
					# If ggplot_build fails, try a different approach
				})
				
				# If we couldn't extract colors, try to get them from the plot's scales
				if (length(colors) == 0) {
					tryCatch({
						# Check if there's a manual scale
						if (aes_name == "colour" || aes_name == "color") {
							if (!is.null(p$scales$scales)) {
								for (scale in p$scales$scales) {
									if (scale$aesthetics == "colour" || scale$aesthetics == "color") {
										if (inherits(scale, "ScaleDiscrete")) {
											colors <- as.character(scale$palette(scale$range$range))
											break
										}
									}
								}
							}
						} else if (aes_name == "fill") {
							if (!is.null(p$scales$scales)) {
								for (scale in p$scales$scales) {
									if (scale$aesthetics == "fill") {
										if (inherits(scale, "ScaleDiscrete")) {
											colors <- as.character(scale$palette(scale$range$range))
											break
										}
									}
								}
							}
						}
					}, error = function(e) {
						# If scale extraction fails, fall back to default colors
					})
				}
				
				# If we still don't have colors, use default viridis
				if (length(colors) == 0 || length(colors) != length(levels)) {
					colors <- if (requireNamespace("viridisLite", quietly = TRUE)) viridisLite::viridis(length(levels)) else grDevices::rainbow(length(levels))
				}
			}
			
			list(levels = levels, colors = colors)
		}
		
		col_result <- eval_levels(p, "colour")
		fill_result <- eval_levels(p, "fill")
		
		if (length(col_result$levels)) {
			rv[[bucket]][[name]]$colour_levels <- col_result$levels
			rv[[bucket]][[name]]$colour_levels_cols <- col_result$colors
		}
		if (length(fill_result$levels)) {
			rv[[bucket]][[name]]$fill_levels <- fill_result$levels
			rv[[bucket]][[name]]$fill_levels_cols <- fill_result$colors
		}
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
