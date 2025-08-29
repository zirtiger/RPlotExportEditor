# plot-editor/R/helpers_plots.R

# Apply edits to a plot (works for plot tabs & grid overrides)
apply_edits <- function(p, edits) {
  e <- edits %||% list()
  
  # titles
  if (!is.null(e$title))     p <- p + ggplot2::labs(title = e$title)
  if (!is.null(e$subtitle))  p <- p + ggplot2::labs(subtitle = e$subtitle)
  if (!is.null(e$caption))   p <- p + ggplot2::labs(caption  = e$caption)
  if (!is.null(e$xlab))      p <- p + ggplot2::labs(x = e$xlab)
  if (!is.null(e$ylab))      p <- p + ggplot2::labs(y = e$ylab)
  
  # theme
  tfun <- get_theme_fun(e$theme %||% BASE$theme)
  p <- p + tfun(base_size = e$base_size %||% BASE$base_size)
  
  # Colors: apply per-level mappings using limits-aligned values to avoid name mismatch
  apply_level_colors <- function(p) {
    try_apply <- function(expr, p) { tryCatch({ expr }, error = function(...) p) }
    
    cat("\n=== APPLYING COLORS TO PLOT ===\n")
    cat("  continuous_colour_palette:", e$continuous_colour_palette, "\n")
    cat("  continuous_fill_palette:", e$continuous_fill_palette, "\n")
    cat("  colour_levels:", paste(e$colour_levels, collapse = ", "), "\n")
    cat("  colour_levels_cols:", paste(e$colour_levels_cols, collapse = ", "), "\n")
    
    # Apply continuous palettes first (if present)
    if (!is.null(e$continuous_colour_palette) && e$continuous_colour_palette != "None") {
      cat("  Applying continuous COLOUR palette:", e$continuous_colour_palette, "\n")
      p <- try_apply(p + ggplot2::scale_color_viridis_c(option = e$continuous_colour_palette), p)
    }
    
    if (!is.null(e$continuous_fill_palette) && e$continuous_fill_palette != "None") {
      cat("  Applying continuous FILL palette:", e$continuous_fill_palette, "\n")
      p <- try_apply(p + ggplot2::scale_fill_viridis_c(option = e$continuous_fill_palette), p)
    }
    
    # Apply discrete level colors ONLY if no continuous scales are active
    # colour levels
    if (!is.null(e$colour_levels) && !is.null(e$colour_levels_cols) && 
      (is.null(e$continuous_colour_palette) || e$continuous_colour_palette == "None")) {
      cat("  Applying discrete COLOUR levels\n")
      lv <- as.character(e$colour_levels)
      cols <- as.character(e$colour_levels_cols)
      keep <- nzchar(lv) & nzchar(cols)
      lv <- lv[keep]; cols <- cols[keep]
      if (length(lv) && length(cols) && length(lv) == length(cols)) {
        p <- try_apply(p + ggplot2::scale_color_manual(values = unname(cols), limits = lv, breaks = lv), p)
      }
    }
    # fill levels
    if (!is.null(e$fill_levels) && !is.null(e$fill_levels_cols) &&
      (is.null(e$continuous_fill_palette) || e$continuous_fill_palette == "None")) {
      cat("  Applying discrete FILL levels\n")
      lv <- as.character(e$fill_levels)
      cols <- as.character(e$fill_levels_cols)
      keep <- nzchar(lv) & nzchar(cols)
      lv <- lv[keep]; cols <- cols[keep]
      if (length(lv) && length(cols) && length(lv) == length(cols)) {
        p <- try_apply(p + ggplot2::scale_fill_manual(values = unname(cols), limits = lv, breaks = lv), p)
      }
    }
    
    p
  }
  p <- apply_level_colors(p)
  
  # Custom text sizes (relative to base_size)
  text_theme <- ggplot2::theme()
  
  # Helper function to calculate actual size from relative multiplier
  get_actual_size <- function(relative_size, base_size) {
    if (is.null(relative_size) || is.null(base_size)) return(NULL)
    return(relative_size * base_size)
  }
  
  base_size <- e$base_size %||% BASE$base_size
  
  if (!is.null(e$title_size)) {
    actual_size <- get_actual_size(e$title_size, base_size)
    if (!is.null(actual_size)) {
      text_theme <- text_theme + ggplot2::theme(plot.title = ggplot2::element_text(size = actual_size))
    }
  }
  if (!is.null(e$subtitle_size)) {
    actual_size <- get_actual_size(e$subtitle_size, base_size)
    if (!is.null(actual_size)) {
      text_theme <- text_theme + ggplot2::theme(plot.subtitle = ggplot2::element_text(size = actual_size))
    }
  }
  if (!is.null(e$caption_size)) {
    actual_size <- get_actual_size(e$caption_size, base_size)
    if (!is.null(actual_size)) {
      text_theme <- text_theme + ggplot2::theme(plot.caption = ggplot2::element_text(size = actual_size))
    }
  }
  if (!is.null(e$axis_title_size)) {
    actual_size <- get_actual_size(e$axis_title_size, base_size)
    if (!is.null(actual_size)) {
      text_theme <- text_theme + ggplot2::theme(axis.title = ggplot2::element_text(size = actual_size))
    }
  }
  if (!is.null(e$axis_text_size)) {
    actual_size <- get_actual_size(e$axis_text_size, base_size)
    if (!is.null(actual_size)) {
      text_theme <- text_theme + ggplot2::theme(axis.text = ggplot2::element_text(size = actual_size))
    }
  }
  if (!is.null(e$legend_title_size)) {
    actual_size <- get_actual_size(e$legend_title_size, base_size)
    if (!is.null(actual_size)) {
      text_theme <- text_theme + ggplot2::theme(legend.title = ggplot2::element_text(size = actual_size))
    }
  }
  if (!is.null(e$legend_text_size)) {
    actual_size <- get_actual_size(e$legend_text_size, base_size)
    if (!is.null(actual_size)) {
      text_theme <- text_theme + ggplot2::theme(legend.text = ggplot2::element_text(size = actual_size))
    }
  }
  
  p <- p + text_theme
  
  # Axis limits
  if (!is.null(e$x_min) || !is.null(e$x_max)) {
    x_limits <- c(e$x_min, e$x_max)
    if (all(!is.na(x_limits))) {
      p <- p + ggplot2::xlim(x_limits)
    }
  }
  if (!is.null(e$y_min) || !is.null(e$y_max)) {
    y_limits <- c(e$y_min, e$y_max)
    if (all(!is.na(y_limits))) {
      p <- p + ggplot2::ylim(y_limits)
    }
  }
  
  # Axis breaks — prefer step-based and use scales::breaks_width for clean ticks
  if (!is.null(e$x_step_major) && is.finite(e$x_step_major) && e$x_step_major > 0) {
    x_major_br <- scales::breaks_width(e$x_step_major)
  } else {
    x_major_br <- ggplot2::waiver()
  }
  if (!is.null(e$x_step_minor) && is.finite(e$x_step_minor) && e$x_step_minor > 0) {
    x_minor_br <- scales::breaks_width(e$x_step_minor)
  } else {
    x_minor_br <- ggplot2::waiver()
  }
  if (!identical(x_major_br, ggplot2::waiver()) || !identical(x_minor_br, ggplot2::waiver())) {
    p <- p + ggplot2::scale_x_continuous(breaks = x_major_br, minor_breaks = x_minor_br)
  } else if (!is.null(e$x_major) || !is.null(e$x_minor)) {
    x_min_val <- if (!is.null(e$x_min) && is.finite(e$x_min)) e$x_min else 0
    x_max_val <- if (!is.null(e$x_max) && is.finite(e$x_max)) e$x_max else 10
    x_breaks <- if (!is.null(e$x_major) && is.finite(e$x_major) && e$x_major > 0) seq(x_min_val, x_max_val, length.out = max(1, e$x_major + 1)) else ggplot2::waiver()
    x_minor_breaks <- if (!is.null(e$x_minor) && is.finite(e$x_minor) && e$x_minor > 0) seq(x_min_val, x_max_val, length.out = max(1, e$x_minor + 1)) else ggplot2::waiver()
    p <- p + ggplot2::scale_x_continuous(breaks = x_breaks, minor_breaks = x_minor_breaks)
  }
  
  if (!is.null(e$y_step_major) && is.finite(e$y_step_major) && e$y_step_major > 0) {
    y_major_br <- scales::breaks_width(e$y_step_major)
  } else {
    y_major_br <- ggplot2::waiver()
  }
  if (!is.null(e$y_step_minor) && is.finite(e$y_step_minor) && e$y_step_minor > 0) {
    y_minor_br <- scales::breaks_width(e$y_step_minor)
  } else {
    y_minor_br <- ggplot2::waiver()
  }
  if (!identical(y_major_br, ggplot2::waiver()) || !identical(y_minor_br, ggplot2::waiver())) {
    p <- p + ggplot2::scale_y_continuous(breaks = y_major_br, minor_breaks = y_minor_br)
  } else if (!is.null(e$y_major) || !is.null(e$y_minor)) {
    y_min_val <- if (!is.null(e$y_min) && is.finite(e$y_min)) e$y_min else 0
    y_max_val <- if (!is.null(e$y_max) && is.finite(e$y_max)) e$y_max else 10
    y_breaks <- if (!is.null(e$y_major) && is.finite(e$y_major) && e$y_major > 0) seq(y_min_val, y_max_val, length.out = max(1, e$y_major + 1)) else ggplot2::waiver()
    y_minor_breaks <- if (!is.null(e$y_minor) && is.finite(e$y_minor) && e$y_minor > 0) seq(y_min_val, y_max_val, length.out = max(1, e$y_minor + 1)) else ggplot2::waiver()
    p <- p + ggplot2::scale_y_continuous(breaks = y_breaks, minor_breaks = y_minor_breaks)
  }
  
  # legend + grids
  legend_theme <- ggplot2::theme(
    legend.position = legend_pos_value(e$legend_pos %||% BASE$legend_pos)
  )
  
  # Legend box
  if (!is.null(e$legend_box)) {
    legend_theme <- legend_theme + ggplot2::theme(
      legend.box.background = if (isTRUE(e$legend_box)) ggplot2::element_rect(fill = "white", color = "black") else ggplot2::element_blank()
    )
  }
  
  # Panel and plot backgrounds
  if (!is.null(e$panel_bg) && e$panel_bg != "Default") {
    panel_color <- switch(e$panel_bg,
                         "White" = "white",
                         "Gray" = "gray90",
                         "Light gray" = "gray95",
                         "Dark gray" = "gray80",
                         "Transparent" = "transparent",
                         "white")
    legend_theme <- legend_theme + ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = panel_color)
    )
  }
  
  if (!is.null(e$plot_bg) && e$plot_bg != "Default") {
    plot_color <- switch(e$plot_bg,
                        "White" = "white",
                        "Gray" = "gray90",
                        "Light gray" = "gray95",
                        "Dark gray" = "gray80",
                        "Transparent" = "transparent",
                        "white")
    legend_theme <- legend_theme + ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = plot_color)
    )
  }
  
  # Grid lines
  if (!is.null(e$grid_major) || !is.null(e$grid_minor)) {
    grid_color <- if (!is.null(e$grid_color) && e$grid_color != "Default") {
      switch(e$grid_color,
             "Gray" = "gray50",
             "Light gray" = "gray80",
             "Dark gray" = "gray30",
             "Black" = "black",
             "gray50")
    } else "gray50"
    
    if (!is.null(e$grid_major)) {
      legend_theme <- legend_theme + ggplot2::theme(
        panel.grid.major = if (isTRUE(e$grid_major)) ggplot2::element_line(color = grid_color, linetype = (e$grid_major_linetype %||% "solid")) else ggplot2::element_blank()
      )
    }
    
    if (!is.null(e$grid_minor)) {
      legend_theme <- legend_theme + ggplot2::theme(
        panel.grid.minor = if (isTRUE(e$grid_minor)) ggplot2::element_line(color = grid_color, linetype = (e$grid_minor_linetype %||% "dashed")) else ggplot2::element_blank()
      )
    }
  }
  
  p <- p + legend_theme
  
  p
}

# Export plot for download (wrapper around export_plot_file)
export_plot <- function(p, file, export_settings) {
  width_mm <- export_settings$width_mm %||% BASE$width_mm
  height_mm <- export_settings$height_mm %||% BASE$height_mm
  dpi <- export_settings$dpi %||% BASE$dpi
  format <- export_settings$format %||% BASE$format
  
  export_plot_file(p, file, fmt = format, width_mm = width_mm, height_mm = height_mm, dpi = dpi)
}

# Save/export helper (uses ggsave devices; ragg recommended for rasters)
export_plot_file <- function(p, path, fmt = c("PNG","TIFF","PDF","SVG","EPS"),
                             width_mm = BASE$width_mm, height_mm = BASE$height_mm, dpi = BASE$dpi) {
  fmt <- match.arg(fmt)
  w_in <- width_mm  / 25.4
  h_in <- height_mm / 25.4
  
  switch(fmt,
         PNG = ggplot2::ggsave(path, p, width = w_in, height = h_in, dpi = dpi,
                               device = ragg::agg_png, bg = "white"),
         TIFF= ggplot2::ggsave(path, p, width = w_in, height = h_in, dpi = dpi,
                               device = ragg::agg_tiff, compression = "lzw"),
         PDF = ggplot2::ggsave(path, p, width = w_in, height = h_in, device = grDevices::cairo_pdf),
         SVG = ggplot2::ggsave(path, p, width = w_in, height = h_in, device = svglite::svglite),
         EPS = ggplot2::ggsave(path, p, width = w_in, height = h_in, device = grDevices::cairo_ps)
  )
  invisible(path)
}

# Vector → PNG for preview
vector_to_png <- function(src, fmt, out_png, width_mm, height_mm, dpi) {
  fmt <- toupper(fmt)
  if (fmt == "PDF" && .have_pdftools) {
    pdftools::pdf_convert(src, format = "png", filenames = out_png, pages = 1, dpi = dpi)
  } else if (fmt == "SVG" && .have_rsvg) {
    width_px  <- as.integer((width_mm / 25.4) * dpi)
    height_px <- as.integer((height_mm / 25.4) * dpi)
    rsvg::rsvg_png(src, file = out_png, width = width_px, height = height_px)
  } else if (fmt %in% c("PNG","TIFF")) {
    if (fmt == "PNG") {
      file.copy(src, out_png, overwrite = TRUE)
    } else if (.have_magick) {
      img <- magick::image_read(src)
      magick::image_write(img, path = out_png, format = "png")
    } else {
      file.copy(src, out_png, overwrite = TRUE)
    }
  } else {
    stop("No vector rasterizer available; render PNG directly.")
  }
  out_png
}

# WYSIWYG preview: render to temp file using the given export settings; return list() for renderImage
render_preview_png <- function(p, width_mm, height_mm, dpi, fmt) {
  tmpdir <- tempdir()
  ext    <- switch(toupper(fmt), PNG="png", TIFF="tiff", PDF="pdf", SVG="svg", EPS="eps", "png")
  src    <- file.path(tmpdir, paste0("prev_src.", ext))
  out    <- file.path(tmpdir, "prev.png")
  
  export_plot_file(p, src, fmt = toupper(fmt), width_mm = width_mm, height_mm = height_mm, dpi = dpi)
  tryCatch({
    vector_to_png(src, toupper(fmt), out, width_mm, height_mm, dpi)
  }, error = function(e) {
    export_plot_file(p, out, fmt = "PNG", width_mm = width_mm, height_mm = height_mm, dpi = dpi)
  })
  
  list(src = out, contentType = "image/png")
}

# Select the first available plot tab
select_first_plot <- function(rv, session) {
  first_name <- names(rv$plots)[1]
  if (!is.null(first_name)) {
    updateTabsetPanel(session, "active_tabset", selected = first_name)
    rv$active_tab <- first_name
  }
}

# Select plot by index (for new sequential system)
select_plot_by_index <- function(rv, session, plot_index) {
  index_str <- as.character(plot_index)
  plot_name <- get_plot_display_name(rv, plot_index)
  if (!is.null(plot_name)) {
    updateTabsetPanel(session, "active_tabset", selected = plot_name)
    rv$active_tab <- plot_name
  }
}

# Add a new plot to the system
add_plot <- function(rv, plot_obj, plot_name = NULL) {
	index <- rv$next_plot_index
	
	# Store the plot at this index
	rv$plots[[as.character(index)]] <- plot_obj
	
	# Store the display name
	if (is.null(plot_name)) {
		plot_name <- paste("Plot", index)
	}
	rv$plot_names[[as.character(index)]] <- plot_name
	
	# Extract and store all original settings
	extract_plot_settings(rv, index, plot_obj)
	
	# Initialize empty edits for this plot
	rv$edits[[as.character(index)]] <- list()
	
	# Initialize export settings
	rv$export[[as.character(index)]] <- list(
		width_mm  = BASE$width_mm,
		height_mm = BASE$height_mm,
		dpi       = BASE$dpi,
		format    = BASE$format
	)
	
	# Increment for next plot
	rv$next_plot_index <- rv$next_plot_index + 1
	
	# Return the index for this plot
	return(index)
}

# Extract all possible settings from a plot
extract_plot_settings <- function(rv, index, plot_obj) {
	index_str <- as.character(index)
	
	# Helper functions
	get_theme_elem <- function(elem) {
		tryCatch(plot_obj$theme[[elem]], error = function(...) NULL)
	}
	is_blank <- function(x) inherits(x, "element_blank")
	
	# Extract labels
	get_lab <- function(lbl) {
		v <- tryCatch(plot_obj$labels[[lbl]], error = function(...) NULL)
		if (is.null(v)) "" else as.character(v)
	}
	
	# Extract axis limits and calculated steps
	x_info <- list(min = NULL, max = NULL, step_major = NULL, step_minor = NULL)
	y_info <- list(min = NULL, max = NULL, step_major = NULL, step_minor = NULL)
	
	# Extract axis limits and calculated steps from ggplot_build
	if (requireNamespace("scales", quietly = TRUE)) {
		tryCatch({
			built <- ggplot2::ggplot_build(plot_obj)
			
			# Extract x-axis information
			if (!is.null(built$layout$panel_scales_x) && length(built$layout$panel_scales_x) > 0) {
				scale_x <- built$layout$panel_scales_x[[1]]
				sx <- scale_x$range$range
				if (length(sx) >= 2) {
					x_info$min <- sx[1]
					x_info$max <- sx[2]
					
					# Try to extract calculated steps from ggplot
					if (!is.null(scale_x$get_breaks)) {
						breaks <- scale_x$get_breaks()
						if (length(breaks) > 1) {
							x_info$step_major <- breaks[2] - breaks[1]
						}
					}
					
					# Try to extract minor breaks if available
					if (!is.null(scale_x$get_breaks_minor)) {
						minor_breaks <- scale_x$get_breaks_minor()
						if (length(minor_breaks) > 1) {
							x_info$step_minor <- minor_breaks[2] - minor_breaks[1]
						}
					}
				}
			}
			
			# Extract y-axis information
			if (!is.null(built$layout$panel_scales_y) && length(built$layout$panel_scales_y) > 0) {
				scale_y <- built$layout$panel_scales_y[[1]]
				sy <- scale_y$range$range
				if (length(sy) >= 2) {
					y_info$min <- sy[1]
					y_info$max <- sy[2]
					
					# Try to extract calculated steps from ggplot
					if (!is.null(scale_y$get_breaks)) {
						breaks <- scale_y$get_breaks()
						if (length(breaks) > 1) {
							y_info$step_major <- breaks[2] - breaks[1]
						}
					}
					
					# Try to extract minor breaks if available
					if (!is.null(scale_y$get_breaks_minor)) {
						minor_breaks <- scale_y$get_breaks_minor()
						if (length(minor_breaks) > 1) {
							y_info$step_minor <- minor_breaks[2] - minor_breaks[1]
						}
					}
				}
			}
		}, error = function(e) NULL)
	}
	
	# Extract base size from plot theme
	extract_base_size <- function(plot_obj) {
		# Try to get base_size from the plot's theme
		if (!is.null(plot_obj$theme) && !is.null(plot_obj$theme$text) && !is.null(plot_obj$theme$text$size)) {
			return(plot_obj$theme$text$size)
		}
		# If no base_size in theme, return NULL (will use BASE$base_size as default)
		return(NULL)
	}
	
	# Extract theme settings (only if explicitly set)
	extract_theme_size <- function(elem, plot_obj) {
		theme_elem <- get_theme_elem(elem)
		if (!is.null(theme_elem) && !is_blank(theme_elem) && !is.null(theme_elem$size)) {
			# Extract the base_size from the plot to calculate relative multiplier
			plot_base_size <- extract_base_size(plot_obj) %||% BASE$base_size
			if (!is.null(plot_base_size) && plot_base_size > 0) {
				# Convert absolute size to relative multiplier
				relative_size <- theme_elem$size / plot_base_size
				return(relative_size)
			}
			# Fallback: return the raw size if we can't calculate relative
			return(theme_elem$size)
		}
		return(NULL)
	}
	
	# Extract grid settings (only if explicitly set)
	extract_grid_setting <- function(elem) {
		theme_elem <- get_theme_elem(elem)
		# Only return TRUE if the element is explicitly set to TRUE
		# Return NULL if it's not set (let ggplot handle default)
		if (!is.null(theme_elem) && !is_blank(theme_elem)) {
			# Check if it's explicitly set to TRUE (not just not-blank)
			if (identical(theme_elem, ggplot2::element_line())) {
				return(TRUE)  # Explicitly enabled
			} else if (is.null(theme_elem$colour) && is.null(theme_elem$linetype) && is.null(theme_elem$size)) {
				return(TRUE)  # Explicitly enabled with defaults
			}
		}
		return(NULL)  # Not explicitly set, let ggplot handle
	}
	
	# Extract grid linetype (only if explicitly set)
	extract_grid_linetype <- function(elem) {
		theme_elem <- get_theme_elem(elem)
		# Only return linetype if it's explicitly set
		if (!is.null(theme_elem) && !is_blank(theme_elem) && !is.null(theme_elem$linetype)) {
			return(theme_elem$linetype)
		}
		return(NULL)  # Let ggplot use default
	}
	
	# Helper function to extract levels from plot
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
	
	for (scale in plot_obj$scales$scales) {
		if (inherits(scale, "ScaleContinuous")) {
			if (identical(scale$aesthetics, "colour") || identical(scale$aesthetics, "color")) {
				continuous_colour_palette <- "viridis"  # Default for continuous
			} else if (identical(scale$aesthetics, "fill")) {
				continuous_fill_palette <- "viridis"  # Default for continuous
			}
		}
	}
	
	# Extract discrete levels only if no continuous scales
	colour_result <- if (is.null(continuous_colour_palette)) {
		extract_levels_from_plot(plot_obj, "colour")
	} else {
		list(levels = character(0), colors = character(0))
	}
	
	fill_result <- if (is.null(continuous_fill_palette)) {
		extract_levels_from_plot(plot_obj, "fill")
	} else {
		list(levels = character(0), colors = character(0))
	}
	
	# Store only extracted values (no hardcoded defaults)
	extracted_settings <- list(
		# Labels (always extract if present)
		title      = get_lab("title"),
		subtitle   = get_lab("subtitle"),
		caption    = get_lab("caption"),
		xlab       = get_lab("x"),
		ylab       = get_lab("y"),
		
		# Theme settings (only if explicitly set)
		theme      = NULL,  # Let ggplot handle default
		base_size  = extract_base_size(plot_obj),  # Extract actual base_size from plot
		legend_pos = NULL,  # Let ggplot handle default
		legend_box = if (!is.null(get_theme_elem("legend.box.background"))) !is_blank(get_theme_elem("legend.box.background")) else NULL,
		panel_bg   = NULL,  # Let ggplot handle default
		plot_bg    = NULL,  # Let ggplot handle default
		
		# Grid settings (only if explicitly set)
		grid_major = extract_grid_setting("panel.grid.major"),
		grid_minor = extract_grid_setting("panel.grid.minor"),
		grid_major_linetype = extract_grid_linetype("panel.grid.major"),
		grid_minor_linetype = extract_grid_linetype("panel.grid.minor"),
		grid_color = NULL,  # Let ggplot handle default
		
		# Axis limits (only if present in plot)
		x_min      = x_info$min,
		x_max      = x_info$max,
		y_min      = y_info$min,
		y_max      = y_info$max,
		
		# Steps (only if ggplot calculated them)
		x_step_major = x_info$step_major,
		x_step_minor = x_info$step_minor,
		y_step_major = y_info$step_major,
		y_step_minor = y_info$step_minor,
		
		# Colors (extracted results)
		palette = "None",
		continuous_colour_palette = continuous_colour_palette,
		continuous_fill_palette = continuous_fill_palette,
		colour_levels = colour_result$levels,
		colour_levels_cols = colour_result$colors,
		fill_levels = fill_result$levels,
		fill_levels_cols = fill_result$colors,
		
		# Text sizes (only if explicitly set in theme)
		title_size = extract_theme_size("plot.title", plot_obj),
		subtitle_size = extract_theme_size("plot.subtitle", plot_obj),
		caption_size = extract_theme_size("plot.caption", plot_obj),
		axis_title_size = extract_theme_size("axis.title", plot_obj),
		axis_text_size = extract_theme_size("axis.text", plot_obj),
		legend_title_size = extract_theme_size("legend.title", plot_obj),
		legend_text_size = extract_theme_size("legend.text", plot_obj)
	)
	
	# Remove NULL values (ggplot will handle these automatically)
	extracted_settings <- Filter(Negate(is.null), extracted_settings)
	
	# Store only what we extracted
	rv$originals[[index_str]] <- extracted_settings
}

# Get plot name for display
get_plot_display_name <- function(rv, plot_index) {
	index_str <- as.character(plot_index)
	rv$plot_names[[index_str]] %||% paste("Plot", plot_index)
}

# Get all plot indices in order
get_plot_indices <- function(rv) {
	as.numeric(names(rv$plots))
}

# Get the first of the newly uploaded plots (for selection after upload)
get_first_new_plot <- function(rv, previous_count) {
	current_count <- length(rv$plots)
	if (current_count > previous_count) {
		# Return the first of the newly uploaded plots
		return(previous_count + 1)
	}
	return(NULL)
}

# Load settings for a plot into the UI (creates edits if needed)
load_plot_settings <- function(rv, plot_index) {
	index_str <- as.character(plot_index)
	
	# If we don't have edits for this plot yet, initialize them
	if (is.null(rv$edits[[index_str]]) || length(rv$edits[[index_str]]) == 0) {
		# Copy originals to edits, filling in BASE defaults for missing required settings
		rv$edits[[index_str]] <- list()
		
		# Get all possible settings from originals
		orig_settings <- rv$originals[[index_str]]
		if (!is.null(orig_settings)) {
			for (setting_name in names(orig_settings)) {
				rv$edits[[index_str]][[setting_name]] <- orig_settings[[setting_name]]
			}
		}
	}
}