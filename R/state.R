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
		
		# Helper functions - define these first
		get_theme_elem <- function(elem) {
			tryCatch(p$theme[[elem]], error = function(...) NULL)
		}
		is_blank <- function(x) inherits(x, "element_blank")
		
		# Only extract originals if we haven't done so before
		if (length(rv[[orig_bucket]][[name]]) == 0) {
			
			# Debug: Print what we're extracting
			cat("\n=== EXTRACTING ORIGINALS FOR PLOT:", name, "===\n")
			
			# Extract labels
			get_lab <- function(lbl) {
				v <- tryCatch(p$labels[[lbl]], error = function(...) NULL)
				if (is.null(v)) "" else as.character(v)
			}
			
			# Helpers to extract limits and suggested steps
			x_info <- list(min = NULL, max = NULL, step_major = NULL, step_minor = NULL)
			y_info <- list(min = NULL, max = NULL, step_major = NULL, step_minor = NULL)
			
			# Extract axis limits and suggest steps
			if (requireNamespace("scales", quietly = TRUE)) {
				tryCatch({
					built <- ggplot2::ggplot_build(p)
					if (!is.null(built$layout$panel_scales_x) && length(built$layout$panel_scales_x) > 0) {
						sx <- built$layout$panel_scales_x[[1]]$range$range
						if (length(sx) >= 2) {
							x_info$min <- sx[1]; x_info$max <- sx[2]
						}
					}
					if (!is.null(built$layout$panel_scales_y) && length(built$layout$panel_scales_y) > 0) {
						sy <- built$layout$panel_scales_y[[1]]$range$range
						if (length(sy) >= 2) {
							y_info$min <- sy[1]; y_info$max <- sy[2]
						}
					}
				}, error = function(e) NULL)
			}
			
			# Theme-derived defaults
			maj_el <- get_theme_elem("panel.grid.major")
			min_el <- get_theme_elem("panel.grid.minor")
			lbox   <- get_theme_elem("legend.box.background")
			
			# Helper function to extract levels from plot
			extract_levels_from_plot <- function(p, aes_name) {
				if (!requireNamespace("rlang", quietly = TRUE)) return(list(levels = character(0), colors = character(0)))
				
				cat("  Extracting", aes_name, "levels...\n")
				
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
				
				cat("    Found levels:", paste(levels, collapse = ", "), "\n")
				
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
			
			# Check for continuous color scales
			continuous_colour_palette <- NULL
			continuous_fill_palette <- NULL
			
			cat("  Checking for continuous color scales...\n")
			
			for (scale in p$scales$scales) {
				if (inherits(scale, "ScaleContinuous")) {
					if (identical(scale$aesthetics, "colour") || identical(scale$aesthetics, "color")) {
						continuous_colour_palette <- "viridis"  # Default for continuous
						cat("    Found continuous COLOUR scale\n")
					} else if (identical(scale$aesthetics, "fill")) {
						continuous_fill_palette <- "viridis"  # Default for continuous
						cat("    Found continuous FILL scale\n")
					}
				}
			}
			
			# Extract discrete levels only if no continuous scales
			colour_result <- if (is.null(continuous_colour_palette)) {
				extract_levels_from_plot(p, "colour")
			} else {
				list(levels = character(0), colors = character(0))
			}
			
			fill_result <- if (is.null(continuous_fill_palette)) {
				extract_levels_from_plot(p, "fill")
			} else {
				list(levels = character(0), colors = character(0))
			}
			
			cat("  Final color settings:\n")
			cat("    colour_levels:", paste(colour_result$levels, collapse = ", "), "\n")
			cat("    colour_levels_cols:", paste(colour_result$colors, collapse = ", "), "\n")
			cat("    fill_levels:", paste(fill_result$levels, collapse = ", "), "\n")
			cat("    fill_levels_cols:", paste(fill_result$colors, collapse = ", "), "\n")
			cat("    continuous_colour_palette:", continuous_colour_palette, "\n")
			cat("    continuous_fill_palette:", continuous_fill_palette, "\n")
			
			# Store ALL original values (including colors)
			rv[[orig_bucket]][[name]] <- list(
				# Labels
				title      = get_lab("title"),
				subtitle   = get_lab("subtitle"),
				caption    = get_lab("caption"),
				xlab       = get_lab("x"),
				ylab       = get_lab("y"),
				
				# Theme - use BASE defaults for essential settings
				theme      = BASE$theme,
				base_size  = BASE$base_size,
				legend_pos = BASE$legend_pos,
				legend_box = if (!is.null(lbox)) !is_blank(lbox) else NULL,
				panel_bg   = NULL,  # Optional setting
				plot_bg    = NULL,  # Optional setting
				
				# Grid - use BASE defaults for essential settings
				grid_major = if (!is.null(maj_el)) !is_blank(maj_el) else FALSE,
				grid_minor = if (!is.null(min_el)) !is_blank(min_el) else FALSE,
				grid_major_linetype = if (!is.null(maj_el)) tryCatch(maj_el$linetype, error = function(...) "solid") else "solid",
				grid_minor_linetype = if (!is.null(min_el)) tryCatch(min_el$linetype, error = function(...) "dashed") else "dashed",
				grid_color = NULL,  # Optional setting
				
				# Axis limits - only set if actually present in plot
				x_min      = x_info$min,
				x_max      = x_info$max,
				y_min      = y_info$min,
				y_max      = y_info$max,
				
				# Step suggestions - use BASE defaults if not present in plot
				x_step_major = x_info$step_major %||% 1,
				x_step_minor = x_info$step_minor %||% 0.5,
				y_step_major = y_info$step_major %||% 1,
				y_step_minor = y_info$step_minor %||% 0.5,
				
				# Colors - include extracted results
				palette = "None",
				continuous_colour_palette = continuous_colour_palette,
				continuous_fill_palette = continuous_fill_palette,
				colour_levels = colour_result$levels,
				colour_levels_cols = colour_result$colors,
				fill_levels = fill_result$levels,
				fill_levels_cols = fill_result$colors,
				
				# Text sizes - use BASE defaults for essential settings
				title_size = BASE$title_size,
				subtitle_size = BASE$subtitle_size,
				caption_size = BASE$caption_size,
				axis_title_size = BASE$axis_title_size,
				axis_text_size = BASE$axis_text_size,
				legend_title_size = BASE$legend_title_size,
				legend_text_size = BASE$legend_text_size
			)
		}
	}
	
	# Don't automatically initialize edits - let the UI load from originals
	# Edits will only be populated when user actually makes changes
	
	# IMPORTANT: Do NOT automatically copy originals to edits
	# This prevents inheritance issues when switching between plots
	# Edits should start empty and only be populated by user actions
	
	if (!grid && is.null(rv$export[[name]])) {
		rv$export[[name]] <- list(
			width_mm  = BASE$width_mm,
			height_mm = BASE$height_mm,
			dpi       = BASE$dpi,
			format    = BASE$format
		)
	}
}

# Helper function to get current value for any setting
get_current_value <- function(rv, plot_name, setting_name, default_value = NULL) {
	# Check edited values first (only if they actually exist)
	if (!is.null(rv$edits[[plot_name]]) && !is.null(rv$edits[[plot_name]][[setting_name]])) {
		return(rv$edits[[plot_name]][[setting_name]])
	}
	
	# Fall back to original values
	if (!is.null(rv$originals[[plot_name]]) && !is.null(rv$originals[[plot_name]][[setting_name]])) {
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
