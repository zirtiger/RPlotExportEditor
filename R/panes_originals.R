# plot-editor/R/panes_originals.R

originals_pane_ui <- function(rv) {
	# Always available: this is a read-only inspector

	# Helper to display a value with status (loaded vs missing)
	show_val <- function(label, val, origin = NULL) {
		status <- if (is.null(val) || (is.character(val) && length(val) == 0)) "(missing)" else ""
		val_txt <- if (is.null(val)) "" else if (length(val) > 1) paste(val, collapse = ", ") else as.character(val)
		div(
			tags$strong(label), ": ",
			if (nzchar(val_txt)) code(val_txt) else tags$em(""),
			if (nzchar(status)) span(style = "color:#999; margin-left:6px;", status) else NULL,
			if (!is.null(origin)) span(style = "color:#999; margin-left:10px; font-size: 90%;",
				paste0("[", origin, "]")) else NULL
		)
	}

	# Build per-plot panels
	plot_indices <- names(rv$plots)

	plot_panels <- lapply(plot_indices, function(index) {
		index_str <- as.character(index)
		name <- rv$plot_names[[index_str]] %||% paste("Plot", index_str)
		orig <- rv$originals[[index_str]] %||% list()
		src  <- rv$originals_src[[index_str]] %||% list()

		# Tabs corresponding to main menu options
		text_tab <- tagList(
			show_val("Title", orig$title, src$title),
			show_val("Subtitle", orig$subtitle, src$subtitle),
			show_val("Caption", orig$caption, src$caption),
			show_val("X label", orig$xlab, src$xlab),
			show_val("Y label", orig$ylab, src$ylab),
			tags$hr(),
			show_val("Title size", orig$title_size, src$title_size),
			show_val("Subtitle size", orig$subtitle_size, src$subtitle_size),
			show_val("Caption size", orig$caption_size, src$caption_size),
			show_val("Axis title size", orig$axis_title_size, src$axis_title_size),
			show_val("Axis text size", orig$axis_text_size, src$axis_text_size),
			show_val("Legend title size", orig$legend_title_size, src$legend_title_size),
			show_val("Legend text size", orig$legend_text_size, src$legend_text_size),
			tags$hr(),
			show_val("X min", orig$x_min, src$x_min),
			show_val("X max", orig$x_max, src$x_max),
			show_val("Y min", orig$y_min, src$y_min),
			show_val("Y max", orig$y_max, src$y_max),
			tags$hr(),
			show_val("X major step", orig$x_step_major, src$x_step_major),
			show_val("X minor step", orig$x_step_minor, src$x_step_minor),
			show_val("Y major step", orig$y_step_major, src$y_step_major),
			show_val("Y minor step", orig$y_step_minor, src$y_step_minor)
		)

		theme_tab <- tagList(
			show_val("Base theme", if (!is.null(orig$theme)) "(theme)" else NULL, src$theme),
			show_val("Base size", orig$base_size, src$base_size),
			show_val("Legend position", orig$legend_pos, src$legend_pos),
			show_val("Legend box", orig$legend_box, src$legend_box),
			tags$hr(),
			show_val("Panel background", orig$panel_bg, src$panel_bg),
			show_val("Plot background", orig$plot_bg, src$plot_bg),
			tags$hr(),
			show_val("Palette", orig$palette, src$palette),
			show_val("Continuous colour palette", orig$continuous_colour_palette, src$continuous_colour_palette),
			show_val("Continuous fill palette", orig$continuous_fill_palette, src$continuous_fill_palette),
			show_val("Colour levels", orig$colour_levels, src$colour_levels),
			show_val("Colour levels colors", orig$colour_levels_cols, src$colour_levels_cols),
			show_val("Fill levels", orig$fill_levels, src$fill_levels),
			show_val("Fill levels colors", orig$fill_levels_cols, src$fill_levels_cols)
		)

		export_tab <- tagList(
			# Export originals use BASE when added; show those defaults for clarity
			show_val("Width (mm)", BASE$width_mm, "base"),
			show_val("Height (mm)", BASE$height_mm, "base"),
			show_val("DPI", BASE$dpi, "base"),
			show_val("Format", BASE$format, "base")
		)

		grid_tab <- tagList(
			show_val("Grid major on", orig$grid_major, src$grid_major),
			show_val("Grid minor on", orig$grid_minor, src$grid_minor),
			show_val("Grid major linetype", orig$grid_major_linetype, src$grid_major_linetype),
			show_val("Grid minor linetype", orig$grid_minor_linetype, src$grid_minor_linetype),
			show_val("Grid color", orig$grid_color, src$grid_color)
		)

		shiny::tabPanel(
			title = name,
			value = paste0("orig_", index_str),
			# Sub-tabs mirroring main sections (Text/Theme/Axis/Export)
			tabsetPanel(
				id = paste0("originals_tabs_", index_str),
				selected = rv$tabs$originals %||% "Text",
				tabPanel("Grid", grid_tab),
				tabPanel("Export", export_tab),
				tabPanel("Text", text_tab),
				tabPanel("Theme", theme_tab)
			)
		)
	})

	# If no plots, show info
	if (length(plot_panels) == 0) {
		return(tagList(
			h4("Original settings"),
			helpText("Load plots to inspect their original settings.")
		))
	}

	# Render as a tabset per plot so the left panel stays compact
	do.call(tabsetPanel, c(id = "originals_plots", type = "tabs", plot_panels))
}

register_originals_observers <- function(input, rv, session) {
	# Persist selected sub-tab (shared across plots)
	observe({
		lapply(names(rv$plots), function(index) {
			local({
				idx <- as.character(index)
				input_id <- paste0("originals_tabs_", idx)
				observeEvent(input[[input_id]], {
					rv$tabs$originals <- input[[input_id]]
				}, ignoreInit = TRUE, ignoreNULL = TRUE)
			})
		})
	})
}

