# plot-editor/R/state.R

init_reactive_state <- function() {
	reactiveValues(
		plots = list(),
		edits = list(),           # User edits for each plot
		originals = list(),       # Original plot values for each plot
		grid_edits = list(),      # Grid overrides
		export = list(),          # Export settings per plot
		active_tab = NULL,        # Current plot tab
		last_mainmenu = "text",  # Last valid main menu selection
		tabs = list(             # Sub-tab persistence
			text = "Labels",
			theme = "Base", 
			export = "Dimensions",
			grid = "Layout"
		),
		is_hydrating = FALSE,    # Prevent observers during state restoration
		force_ui_update = 0      # Force UI updates when switching plots
	)
}

ensure_edits <- function(rv, name, grid = FALSE) {
	bucket <- if (grid) "grid_edits" else "edits"
	orig_bucket <- if (grid) "grid_originals" else "originals"
	
	# Initialize edits if they don't exist
	if (is.null(rv[[bucket]][[name]])) {
		rv[[bucket]][[name]] <- list()
	}
	
	# Initialize originals if they don't exist (only once per plot)
	if (is.null(rv[[orig_bucket]][[name]])) {
		rv[[orig_bucket]][[name]] <- list()
	}
	
	# If this is a plot (not grid), ensure we have the original values
	if (!grid && !is.null(rv$plots[[name]])) {
		p <- rv$plots[[name]]
		
		# Only extract originals if we haven't done so before
		if (length(rv[[orig_bucket]][[name]]) == 0) {
			# Extract original plot values
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
			
			# Store ALL original values
			rv[[orig_bucket]][[name]] <- list(
				# Labels
				title      = get_lab("title"),
				subtitle   = get_lab("subtitle"),
				caption    = get_lab("caption"),
				xlab       = get_lab("x"),
				ylab       = get_lab("y"),
				
				# Theme
				theme      = BASE$theme,
				base_size  = BASE$base_size,
				legend_pos = BASE$legend_pos,
				legend_box = if (!is.null(lbox)) !is_blank(lbox) else NULL,
				panel_bg   = NULL,
				plot_bg    = NULL,
				
				# Grid
				grid_major = if (!is.null(maj_el)) !is_blank(maj_el) else NULL,
				grid_minor = if (!is.null(min_el)) !is_blank(min_el) else NULL,
				grid_major_linetype = tryCatch(maj_el$linetype, error = function(...) NULL),
				grid_minor_linetype = tryCatch(min_el$linetype, error = function(...) NULL),
				grid_color = NULL,
				
				# Axis limits
				x_min      = x_info$min,
				x_max      = x_info$max,
				y_min      = y_info$min,
				y_max      = y_info$max,
				
				# Step suggestions
				x_step_major = x_info$step_major,
				x_step_minor = x_info$step_minor,
				y_step_major = y_info$step_major,
				y_step_minor = y_info$step_minor,
				
				# Colors
				palette = "None",
				continuous_colour_palette = NULL,
				continuous_fill_palette = NULL,
				
				# Text sizes
				title_size = NULL,
				subtitle_size = NULL,
				caption_size = NULL,
				axis_title_size = NULL,
				axis_text_size = NULL,
				legend_title_size = NULL,
				legend_text_size = NULL
			)
			
			# Extract original color levels and colors
			extract_levels_from_plot <- function(p, aes_name) {
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
				
				col_result <- extract_levels_from_plot(p, "colour")
				fill_result <- extract_levels_from_plot(p, "fill")
				
				if (length(col_result$levels)) {
					rv[[orig_bucket]][[name]]$colour_levels <- col_result$levels
					rv[[orig_bucket]][[name]]$colour_levels_cols <- col_result$colors
				}
				if (length(fill_result$levels)) {
					rv[[orig_bucket]][[name]]$fill_levels <- fill_result$levels
					rv[[orig_bucket]][[name]]$fill_levels_cols <- fill_result$colors
				}
			}
			
			# Initialize edits with original values (if edits are empty)
			if (length(rv[[bucket]][[name]]) == 0) {
				rv[[bucket]][[name]] <- rv[[orig_bucket]][[name]]
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
}

# Helper function to get current value for any setting
get_current_value <- function(rv, plot_name, setting_name, default_value = NULL) {
	# Check edited values first
	if (!is.null(rv$edits[[plot_name]][[setting_name]])) {
		return(rv$edits[[plot_name]][[setting_name]])
	}
	
	# Fall back to original values
	if (!is.null(rv$originals[[plot_name]][[setting_name]])) {
		return(rv$originals[[plot_name]][[setting_name]])
	}
	
	# Finally fall back to default
	return(default_value)
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
