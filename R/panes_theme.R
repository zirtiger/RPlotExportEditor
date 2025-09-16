# plot-editor/R/panes_theme.R

# Detect the theme used in a plot
detect_plot_theme <- function(plot_obj) {
	if (is.null(plot_obj) || is.null(plot_obj$theme)) {
		return("theme_minimal")  # Default fallback
	}
	
	# Try to detect the theme by comparing with known themes
	# This is a heuristic approach since ggplot doesn't store the original theme name
	
	# Get the plot's theme
	plot_theme <- plot_obj$theme
	
	# Compare with known themes
	known_themes <- list(
		theme_minimal = ggplot2::theme_minimal(),
		theme_classic = ggplot2::theme_classic(),
		theme_bw = ggplot2::theme_bw(),
		theme_light = ggplot2::theme_light(),
		theme_gray = ggplot2::theme_gray()
	)
	
	# Simple heuristic: check for distinctive elements
	# theme_classic: has axis lines, no grid
	if (!is.null(plot_theme$axis.line) && !inherits(plot_theme$axis.line, "element_blank")) {
		if (is.null(plot_theme$panel.grid.major) || inherits(plot_theme$panel.grid.major, "element_blank")) {
			return("theme_classic")
		}
	}
	
	# theme_bw: white background, black borders
	if (!is.null(plot_theme$panel.background) && !inherits(plot_theme$panel.background, "element_blank")) {
		if (!is.null(plot_theme$panel.background$fill) && plot_theme$panel.background$fill == "white") {
			return("theme_bw")
		}
	}
	
	# theme_light: light gray background
	if (!is.null(plot_theme$panel.background) && !inherits(plot_theme$panel.background, "element_blank")) {
		if (!is.null(plot_theme$panel.background$fill) && plot_theme$panel.background$fill == "white") {
			# Check for light gray grid
			if (!is.null(plot_theme$panel.grid.major) && !inherits(plot_theme$panel.grid.major, "element_blank")) {
				if (!is.null(plot_theme$panel.grid.major$colour) && plot_theme$panel.grid.major$colour == "white") {
					return("theme_light")
				}
			}
		}
	}
	
	# theme_gray: gray background
	if (!is.null(plot_theme$panel.background) && !inherits(plot_theme$panel.background, "element_blank")) {
		if (!is.null(plot_theme$panel.background$fill) && plot_theme$panel.background$fill == "gray90") {
			return("theme_gray")
		}
	}
	
	# Default to theme_minimal if we can't detect
	return("theme_minimal")
}

# Generate colors from a palette for a given number of levels
generate_palette_colors <- function(n, palette_name) {
	if (n <= 0) return(character(0))
	
	# Handle viridis-like palettes
	if (palette_name %in% c("viridis", "magma", "plasma", "inferno", "cividis")) {
		if (requireNamespace("viridisLite", quietly = TRUE)) {
			return(viridisLite::viridis(n, option = palette_name))
		} else {
			return(grDevices::rainbow(n))
		}
	}
	
	# Handle RColorBrewer palettes
	if (palette_name %in% c("Set1", "Set2", "Set3", "Paired", "Dark2", "Accent")) {
		if (requireNamespace("RColorBrewer", quietly = TRUE)) {
			# Get the maximum number of colors available for this palette
			max_colors <- RColorBrewer::brewer.pal.info[palette_name, "maxcolors"]
			if (n <= max_colors) {
				# Use the exact number of colors needed
				colors <- RColorBrewer::brewer.pal(min(n, max_colors), palette_name)
				if (n < max_colors) {
					# If we need fewer colors, take the first n
					colors <- colors[1:n]
				}
				return(colors)
			} else {
				# If we need more colors than available, use the max and extend with rainbow
				base_colors <- RColorBrewer::brewer.pal(max_colors, palette_name)
				extra_colors <- grDevices::rainbow(n - max_colors)
				return(c(base_colors, extra_colors))
			}
		} else {
			# Fallback to rainbow if RColorBrewer not available
			return(grDevices::rainbow(n))
		}
	}
	
	# Default fallback
	return(grDevices::rainbow(n))
}

theme_pane_ui <- function(rv) {
	grid_ctx <- identical(rv$active_tab, "Grid")
	if (grid_ctx) {
		return(tagList(
			h4("Theme — Grid context"),
			helpText("Theme edits while Grid is active apply ONLY to grid rendering (grid overrides). Switch to a plot tab to edit its standalone theme.")
		))
	}
	
	ap <- isolate(rv$active_tab)
	if (is.null(ap) || is.null(rv$plots[[ap]])) return(tagList(h4("Theme"), helpText("Select a plot tab.")))
	
	# UI should not modify state - only read values
	
	# Use helper function to get current values
	get_val <- function(setting, default = NULL) {
		get_current_value(rv, ap, setting, default)
	}
	
	# Get current values - detect theme from plot if not set
	theme_val <- get_val("theme", NULL)
	if (is.null(theme_val)) {
		# Try to detect the actual theme from the plot
		theme_val <- detect_plot_theme(rv$plots[[ap]])
		cat("DEBUG: Detected theme from plot:", theme_val, "for plot", ap, "\n")
	} else {
		cat("DEBUG: Using stored theme:", theme_val, "for plot", ap, "\n")
	}
	base_size_val <- get_val("base_size", BASE$base_size)
	legend_pos_val <- get_val("legend_pos", BASE$legend_pos)
	legend_box_val <- get_val("legend_box", FALSE)
	panel_bg_val <- get_val("panel_bg", "Default")
	plot_bg_val <- get_val("plot_bg", "Default")
	grid_major_val <- get_val("grid_major", TRUE)
	grid_minor_val <- get_val("grid_minor", TRUE)
	grid_color_val <- get_val("grid_color", "Default")
	grid_major_linetype_val <- get_val("grid_major_linetype", "solid")
	grid_minor_linetype_val <- get_val("grid_minor_linetype", "dashed")
	palette_val <- get_val("palette", "None")
	continuous_colour_palette_val <- get_val("continuous_colour_palette", "None")
	continuous_fill_palette_val <- get_val("continuous_fill_palette", "None")
	
	# Debug output removed to prevent unnecessary re-renders
	
	# Get color levels from current values
	colour_lvls <- get_val("colour_levels", character(0))
	fill_lvls <- get_val("fill_levels", character(0))
	
	# Get color values, generate defaults if missing
	colour_cols <- get_val("colour_levels_cols", character(0))
	fill_cols <- get_val("fill_levels_cols", character(0))
	
	# Generate default colors if levels exist but colors don't
	if (length(colour_lvls) > 0 && length(colour_cols) == 0) {
		colour_cols <- generate_palette_colors(length(colour_lvls), "viridis")
	}
	
	if (length(fill_lvls) > 0 && length(fill_cols) == 0) {
		fill_cols <- generate_palette_colors(length(fill_lvls), "viridis")
	}
	
	grid_major_on <- isTRUE(grid_major_val)
	grid_minor_on <- isTRUE(grid_minor_val)
	legend_box_on <- isTRUE(legend_box_val)
	
	# Detect discrete levels by inspecting mappings and data (evaluate aes on data)
	extract_levels_from_plot <- function(p, aes_name) {
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
			if (!requireNamespace("rlang", quietly = TRUE)) return(NULL)
			vals <- try(rlang::eval_tidy(expr, data = dat), silent = TRUE)
			if (inherits(vals, "try-error") || is.null(vals)) return(NULL)
			vals
		}
		append_vals <- function(vals) {
			if (is.null(vals)) return()
			vals <- vals[!is.na(vals)]
			if (is.factor(vals)) collect <<- c(collect, as.character(levels(vals))) else collect <<- c(collect, unique(as.character(vals)))
		}
		# plot-level
		expr_p <- get_expr(p$mapping)
		append_vals(eval_on(p$data, expr_p))
		# layers
		if (!is.null(p$layers) && length(p$layers)) {
			for (ly in p$layers) {
				expr_l <- get_expr(ly$mapping) %||% expr_p
				append_vals(eval_on(ly$data %||% p$data, expr_l))
			}
		}
		unique(collect[nzchar(collect)])
	}
	
	make_color_input <- function(id, label, value) {
		if (requireNamespace("colourpicker", quietly = TRUE)) {
			colourpicker::colourInput(id, label, value = value %||% "#1f77b4", allowTransparent = TRUE)
		} else {
			textInput(id, label, value = value %||% "#1f77b4", placeholder = "#RRGGBB or name")
		}
	}
	
	
	tagList(
		div(style="display:flex; gap:8px; margin-bottom:8px;",
			actionButton("apply_all_theme", "Use for all plots", class = "btn btn-sm btn-default"),
			actionButton("ui_revert_all_theme", "Revert All to Original", class = "btn btn-sm btn-warning")
		),
		tags$hr(),
		
		# tabs with persistence
		tabsetPanel(
			id = "theme_tabs",
			selected = rv$tabs$theme %||% "Base",
			tabPanel("Base",
				selectInput("ui_theme", "Theme",
						choices = c("theme_minimal","theme_classic","theme_bw","theme_light","theme_gray"),
						selected = theme_val)
			),
			tabPanel("Legend",
				selectInput("ui_legend_pos", "Legend position", 
						choices = LEGEND_POS, 
						selected = legend_pos_val),
				checkboxInput("ui_legend_box", "Legend box", 
							value = legend_box_val),
				div(class = if (legend_box_on) NULL else "muted-control",
					p(class="help-block", "Legend box must be enabled for these settings to show."))
			),
			tabPanel("Background",
				selectInput("ui_panel_bg", "Panel background", 
						choices = c("Default", "White", "Gray", "Transparent"),
						selected = panel_bg_val),
				selectInput("ui_plot_bg", "Plot background", 
						choices = c("Default", "White", "Gray", "Transparent"),
						selected = plot_bg_val)
			),
			tabPanel("Grid",
				checkboxInput("ui_grid_major", "Major grid lines", 
							value = grid_major_val),
				div(class = if (grid_major_on) NULL else "muted-control",
					selectInput("ui_grid_major_linetype", "Major grid linetype",
							choices = c("Default", "solid","dashed","dotted","dotdash","longdash","twodash"),
							selected = if (is.null(grid_major_linetype_val)) "Default" else grid_major_linetype_val)),
				checkboxInput("ui_grid_minor", "Minor grid lines", 
							value = grid_minor_val),
				div(class = if (grid_minor_on) NULL else "muted-control",
					selectInput("ui_grid_minor_linetype", "Minor grid linetype",
							choices = c("Default", "solid","dashed","dotted","dotdash","longdash","twodash"),
							selected = if (is.null(grid_minor_linetype_val)) "Default" else grid_minor_linetype_val)),
				selectInput("ui_grid_color", "Grid color", 
						choices = c("Default", "Gray", "Light gray", "Dark gray", "Black"),
						selected = grid_color_val)
			),
			tabPanel("Colors",
				div(style="margin-bottom:8px;",
					actionButton("ui_revert_colors", "Revert to Original", class = "btn btn-sm btn-warning")
				),
				tags$hr(),
				
				# Show continuous palette controls if plot has continuous scales
				if (!is.null(continuous_colour_palette_val) && continuous_colour_palette_val != "None") {
					tagList(
						h5("Continuous Color Scale"),
						selectInput("ui_continuous_colour_palette", "Colour Palette",
							choices = c("None", "viridis", "magma", "plasma", "inferno", "cividis"),
							selected = continuous_colour_palette_val),
						tags$hr()
					)
				} else if (!is.null(continuous_fill_palette_val) && continuous_fill_palette_val != "None") {
					tagList(
						h5("Continuous Fill Scale"),
						selectInput("ui_continuous_fill_palette", "Fill Palette",
							choices = c("None", "viridis", "magma", "plasma", "inferno", "cividis"),
							selected = continuous_fill_palette_val),
						tags$hr()
					)
				},
				
				# Show discrete controls only if plot has discrete levels
				if (length(colour_lvls) > 0 || length(fill_lvls) > 0) {
					tagList(
						h5("Discrete Color Palettes"),
						selectInput("ui_palette", "Discrete Palette",
							choices = c("None", "viridis", "magma", "plasma", "inferno", "cividis", "Set1", "Set2", "Set3", "Paired", "Dark2", "Accent"),
							selected = palette_val),
						div(style="margin-top:8px;",
							actionButton("ui_apply_palette_levels", "Apply Palette to Levels", class = "btn btn-sm btn-primary")
						),
						tags$hr(),
						if (length(colour_lvls)) tagList(
							tags$strong("Colour levels"),
							lapply(seq_along(colour_lvls), function(i) {
								tagList(
									div(style="margin-bottom:4px;",
										tags$span(colour_lvls[i], style="font-weight:bold; margin-right:8px;"),
										make_color_input(paste0("ui_colour_", i), "", colour_cols[i] %||% "#1f77b4")
									)
								)
							})
						),
						if (length(fill_lvls)) tagList(
							tags$strong("Fill levels"),
							lapply(seq_along(fill_lvls), function(i) {
								tagList(
									div(style="margin-bottom:4px;",
										tags$span(fill_lvls[i], style="font-weight:bold; margin-right:8px;"),
										make_color_input(paste0("ui_fill_", i), "", fill_cols[i] %||% "#1f77b4")
									)
								)
							})
						)
					)
				} else {
					tags$em("No discrete colour/fill levels detected.")
				}
			)
		)
	)
}

register_theme_observers <- function(input, rv, session) {
	# Helper function to get plot index from active tab
	get_plot_index <- function() {
		ap <- rv$active_tab
		if (is.null(ap) || identical(ap, "Grid")) return(NULL)
		
		# The active tab is now the plot index (1, 2, 3, etc.)
		# Just return it directly if it's a valid plot index
		if (ap %in% names(rv$plots)) {
			return(as.numeric(ap))
		}
		
		return(NULL)
	}
	
	observeEvent(input$ui_theme, {
		if (rv$is_hydrating) return()
		plot_index <- get_plot_index()
		if (is.null(plot_index)) return()
		
		# Only update if the theme is actually different from what's currently stored
		current_theme <- get_current_value(rv, as.character(plot_index), "theme", NULL)
		if (!is.null(current_theme) && input$ui_theme == current_theme) {
			# Theme hasn't changed, don't update
			return()
		}
		
		load_plot_settings(rv, plot_index)
		rv$edits[[as.character(plot_index)]]$theme <- input$ui_theme
	}, ignoreInit = TRUE, ignoreNULL = TRUE)
	

	
	# Dropdowns - keep immediate updates (no debouncing needed)
	observeEvent(input$ui_legend_pos, {
		if (rv$is_hydrating || rv$force_ui_update > 0) return()
		plot_index <- get_plot_index()
		if (is.null(plot_index)) return()
		
		load_plot_settings(rv, plot_index)
		rv$edits[[as.character(plot_index)]]$legend_pos <- legend_pos_value(input$ui_legend_pos)
	}, ignoreInit = TRUE, ignoreNULL = TRUE)
	
	observeEvent(input$ui_legend_box, {
		if (rv$is_hydrating || rv$force_ui_update > 0) return()
		plot_index <- get_plot_index()
		if (is.null(plot_index)) return()
		
		load_plot_settings(rv, plot_index)
		rv$edits[[as.character(plot_index)]]$legend_box <- input$ui_legend_box
	}, ignoreInit = TRUE, ignoreNULL = TRUE)
	
	observeEvent(input$ui_panel_bg, {
		if (rv$is_hydrating || rv$force_ui_update > 0) return()
		plot_index <- get_plot_index()
		if (is.null(plot_index)) return()
		
		load_plot_settings(rv, plot_index)
		rv$edits[[as.character(plot_index)]]$panel_bg <- input$ui_panel_bg
	}, ignoreInit = TRUE, ignoreNULL = TRUE)
	
	observeEvent(input$ui_plot_bg, {
		if (rv$is_hydrating || rv$force_ui_update > 0) return()
		plot_index <- get_plot_index()
		if (is.null(plot_index)) return()
		
		load_plot_settings(rv, plot_index)
		rv$edits[[as.character(plot_index)]]$plot_bg <- input$ui_plot_bg
	}, ignoreInit = TRUE, ignoreNULL = TRUE)
	
	observeEvent(input$ui_grid_major, {
		if (rv$is_hydrating || rv$force_ui_update > 0) return()
		plot_index <- get_plot_index()
		if (is.null(plot_index)) return()
		
		load_plot_settings(rv, plot_index)
		rv$edits[[as.character(plot_index)]]$grid_major <- input$ui_grid_major
	}, ignoreInit = TRUE, ignoreNULL = TRUE)
	
	observeEvent(input$ui_grid_minor, {
		if (rv$is_hydrating || rv$force_ui_update > 0) return()
		plot_index <- get_plot_index()
		if (is.null(plot_index)) return()
		
		load_plot_settings(rv, plot_index)
		rv$edits[[as.character(plot_index)]]$grid_minor <- input$ui_grid_minor
	}, ignoreInit = TRUE, ignoreNULL = TRUE)
	
	observeEvent(input$ui_grid_color, {
		if (rv$is_hydrating || rv$force_ui_update > 0) return()
		plot_index <- get_plot_index()
		if (is.null(plot_index)) return()
		
		load_plot_settings(rv, plot_index)
		rv$edits[[as.character(plot_index)]]$grid_color <- input$ui_grid_color
	}, ignoreInit = TRUE, ignoreNULL = TRUE)
	
	observeEvent(input$ui_grid_major_linetype, {
		if (rv$is_hydrating || rv$force_ui_update > 0) return()
		plot_index <- get_plot_index()
		if (is.null(plot_index)) return()
		
		load_plot_settings(rv, plot_index)
		# Convert "Default" to NULL (let ggplot handle it)
		value <- if (input$ui_grid_major_linetype == "Default") NULL else input$ui_grid_major_linetype
		rv$edits[[as.character(plot_index)]]$grid_major_linetype <- value
	}, ignoreInit = TRUE, ignoreNULL = TRUE)
	
	observeEvent(input$ui_grid_minor_linetype, {
		if (rv$is_hydrating || rv$force_ui_update > 0) return()
		plot_index <- get_plot_index()
		if (is.null(plot_index)) return()
		
		load_plot_settings(rv, plot_index)
		# Convert "Default" to NULL (let ggplot handle it)
		value <- if (input$ui_grid_minor_linetype == "Default") NULL else input$ui_grid_minor_linetype
		rv$edits[[as.character(plot_index)]]$grid_minor_linetype <- value
	}, ignoreInit = TRUE, ignoreNULL = TRUE)
	
	# Colors tab
	observeEvent(input$ui_palette, {
		if (rv$is_hydrating) return()
		plot_index <- get_plot_index()
		if (is.null(plot_index)) return()
		
		cat("\n=== PALETTE INPUT CHANGED ===\n")
		cat("  Plot:", plot_index, "\n")
		cat("  New value:", input$ui_palette, "\n")
		cat("  Current stored value:", get_current_value(rv, plot_index, "palette", "None"), "\n")
		
		load_plot_settings(rv, plot_index)
		index_str <- as.character(plot_index)
		rv$edits[[index_str]]$palette <- input$ui_palette
		
		# Automatically apply palette colors to existing levels
		pal <- input$ui_palette
		if (pal != "None") {
			# Get current levels from the plot
			e <- rv$edits[[index_str]]
			colour_lvls <- e$colour_levels %||% character(0)
			fill_lvls <- e$fill_levels %||% character(0)
			
			# Generate colors for existing colour levels
			if (length(colour_lvls) > 0) {
				cols <- generate_palette_colors(length(colour_lvls), pal)
				rv$edits[[index_str]]$colour_levels_cols <- cols
				cat("  Auto-applied palette to", length(colour_lvls), "colour levels\n")
			}
			
			# Generate colors for existing fill levels
			if (length(fill_lvls) > 0) {
				cols <- generate_palette_colors(length(fill_lvls), pal)
				rv$edits[[index_str]]$fill_levels_cols <- cols
				cat("  Auto-applied palette to", length(fill_lvls), "fill levels\n")
			}
		}
	}, ignoreInit = TRUE, ignoreNULL = TRUE)
	
	# Revert colors to original
	observeEvent(input$ui_revert_colors, {
		plot_index <- get_plot_index()
		if (is.null(plot_index)) return()
		
		cat("\n=== REVERTING COLORS TO ORIGINAL ===\n")
		cat("  Plot:", plot_index, "\n")
		
		# Load settings for this plot if needed
		load_plot_settings(rv, plot_index)
		index_str <- as.character(plot_index)
		
		# Revert discrete colors
		if (!is.null(rv$originals[[index_str]]$colour_levels_cols)) {
			rv$edits[[index_str]]$colour_levels_cols <- rv$originals[[index_str]]$colour_levels_cols
			cat("  Reverted colour_levels_cols\n")
		}
		if (!is.null(rv$originals[[index_str]]$fill_levels_cols)) {
			rv$edits[[index_str]]$fill_levels_cols <- rv$originals[[index_str]]$fill_levels_cols
			cat("  Reverted fill_levels_cols\n")
		}
		
		# Revert continuous palettes
		if (!is.null(rv$originals[[index_str]]$continuous_colour_palette)) {
			rv$edits[[index_str]]$continuous_colour_palette <- rv$originals[[index_str]]$continuous_colour_palette
			cat("  Reverted continuous_colour_palette to:", rv$originals[[index_str]]$continuous_colour_palette, "\n")
		}
		if (!is.null(rv$originals[[index_str]]$continuous_fill_palette)) {
			rv$edits[[index_str]]$continuous_fill_palette <- rv$originals[[index_str]]$continuous_fill_palette
			cat("  Reverted continuous_fill_palette to:", rv$originals[[index_str]]$continuous_fill_palette, "\n")
		}
		
		# Reset discrete palette
		rv$edits[[index_str]]$palette <- "None"
		cat("  Reset discrete palette to None\n")
	}, ignoreInit = TRUE)
	
	# General revert to original for all theme settings
	observeEvent(input$ui_revert_all_theme, {
		plot_index <- get_plot_index()
		if (is.null(plot_index)) return()
		
		# Load settings for this plot if needed
		load_plot_settings(rv, plot_index)
		index_str <- as.character(plot_index)
		
		# Revert all theme settings to original
		if (!is.null(rv$originals[[index_str]])) {
			rv$edits[[index_str]] <- rv$originals[[index_str]]
		}
		
		showNotification("All theme settings reverted to original", type = "message")
	}, ignoreInit = TRUE)
	
	observeEvent(input$ui_apply_palette_levels, {
		plot_index <- get_plot_index()
		if (is.null(plot_index)) return()
		
		# Load settings for this plot if needed
		load_plot_settings(rv, plot_index)
		index_str <- as.character(plot_index)
		
		pal <- rv$edits[[index_str]]$palette %||% "None"
		
		# Get current levels from the plot
		e <- rv$edits[[index_str]]
		colour_lvls <- e$colour_levels %||% character(0)
		fill_lvls <- e$fill_levels %||% character(0)
		
		# Generate colors for existing colour levels
		if (length(colour_lvls) > 0) {
			cols <- generate_palette_colors(length(colour_lvls), if (pal == "None") "viridis" else pal)
			rv$edits[[index_str]]$colour_levels_cols <- cols
		}
		
		# Generate colors for existing fill levels
		if (length(fill_lvls) > 0) {
			cols <- generate_palette_colors(length(fill_lvls), if (pal == "None") "viridis" else pal)
			rv$edits[[index_str]]$fill_levels_cols <- cols
		}
		
		showNotification("Palette applied to existing levels", type = "message")
	}, ignoreInit = TRUE)
	
	# Dynamic level color pickers - create observers for each possible color picker
	# This approach is more robust than creating observers inside an observe() block
	
	# Create a function to handle color picker updates
	create_color_picker_observer <- function(input_id, color_type, level_index) {
		observeEvent(input[[input_id]], {
			if (rv$is_hydrating) return()
			
			plot_index <- get_plot_index()
			if (is.null(plot_index)) return()
			
			load_plot_settings(rv, plot_index)
			index_str <- as.character(plot_index)
			
			new_color <- input[[input_id]]
			if (!is.null(new_color) && nzchar(new_color)) {
				cat("\n=== COLOR PICKER CHANGED ===\n")
				cat("  Input ID:", input_id, "\n")
				cat("  Color type:", color_type, "\n")
				cat("  Level index:", level_index, "\n")
				cat("  New color:", new_color, "\n")
				cat("  Plot index:", plot_index, "\n")
				
				# Ensure the color arrays exist and are the right length
				if (color_type == "colour") {
					colour_levels <- rv$edits[[index_str]]$colour_levels %||% character(0)
					cat("  Colour levels:", paste(colour_levels, collapse = ", "), "\n")
					if (level_index <= length(colour_levels)) {
						# Initialize color array if needed
						if (is.null(rv$edits[[index_str]]$colour_levels_cols)) {
							rv$edits[[index_str]]$colour_levels_cols <- character(length(colour_levels))
							cat("  Initialized colour_levels_cols array\n")
						}
						# Update the specific color
						rv$edits[[index_str]]$colour_levels_cols[level_index] <- new_color
						cat("  Updated colour", level_index, "to", new_color, "for plot", plot_index, "\n")
						cat("  Current colour_levels_cols:", paste(rv$edits[[index_str]]$colour_levels_cols, collapse = ", "), "\n")
					} else {
						cat("  ERROR: Level index", level_index, "exceeds colour levels length", length(colour_levels), "\n")
					}
				} else if (color_type == "fill") {
					fill_levels <- rv$edits[[index_str]]$fill_levels %||% character(0)
					cat("  Fill levels:", paste(fill_levels, collapse = ", "), "\n")
					if (level_index <= length(fill_levels)) {
						# Initialize color array if needed
						if (is.null(rv$edits[[index_str]]$fill_levels_cols)) {
							rv$edits[[index_str]]$fill_levels_cols <- character(length(fill_levels))
							cat("  Initialized fill_levels_cols array\n")
						}
						# Update the specific color
						rv$edits[[index_str]]$fill_levels_cols[level_index] <- new_color
						cat("  Updated fill", level_index, "to", new_color, "for plot", plot_index, "\n")
						cat("  Current fill_levels_cols:", paste(rv$edits[[index_str]]$fill_levels_cols, collapse = ", "), "\n")
					} else {
						cat("  ERROR: Level index", level_index, "exceeds fill levels length", length(fill_levels), "\n")
					}
				}
			}
		}, ignoreInit = TRUE, ignoreNULL = TRUE)
	}
	
	# Create observers for up to 10 color pickers (should be enough for most cases)
	lapply(1:10, function(i) {
		create_color_picker_observer(paste0("ui_colour_", i), "colour", i)
		create_color_picker_observer(paste0("ui_fill_", i), "fill", i)
	})
	

	
	# Continuous color palette observers
	observeEvent(input$ui_continuous_colour_palette, {
		if (rv$is_hydrating) return()
		ap <- rv$active_tab; if (is.null(ap) || is.null(rv$plots[[ap]]) || identical(ap,"Grid")) return()
		
		cat("\n=== CONTINUOUS COLOUR PALETTE INPUT CHANGED ===\n")
		cat("  Plot:", ap, "\n")
		cat("  New value:", input$ui_continuous_colour_palette, "\n")
		
		ensure_edits(rv, ap, grid = FALSE)
		rv$edits[[ap]]$continuous_colour_palette <- input$ui_continuous_colour_palette
	}, ignoreInit = TRUE, ignoreNULL = TRUE)
	
	observeEvent(input$ui_continuous_fill_palette, {
		if (rv$is_hydrating) return()
		ap <- rv$active_tab; if (is.null(ap) || is.null(rv$plots[[ap]]) || identical(ap,"Grid")) return()
		
		cat("\n=== CONTINUOUS FILL PALETTE INPUT CHANGED ===\n")
		cat("  Plot:", ap, "\n")
		cat("  New value:", input$ui_continuous_fill_palette, "\n")
		
		ensure_edits(rv, ap, grid = FALSE)
		rv$edits[[ap]]$continuous_fill_palette <- input$ui_continuous_fill_palette
	}, ignoreInit = TRUE, ignoreNULL = TRUE)
	
	# Persist selected sub-tab
	observeEvent(input$theme_tabs, {
		rv$tabs$theme <- input$theme_tabs
	}, ignoreInit = TRUE, ignoreNULL = TRUE)
}
