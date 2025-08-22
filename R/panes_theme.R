# plot-editor/R/panes_theme.R

theme_pane_ui <- function(rv) {
	grid_ctx <- identical(rv$active_tab, "Grid")
	if (grid_ctx) {
		return(tagList(
			h4("Theme — Grid context"),
			helpText("Theme edits while Grid is active apply ONLY to grid rendering (grid overrides). Switch to a plot tab to edit its standalone theme.")
		))
	}
	
	ap <- rv$active_tab
	if (is.null(ap) || is.null(rv$plots[[ap]])) return(tagList(h4("Theme"), helpText("Select a plot tab.")))
	
	ensure_edits(rv, ap, grid = FALSE)
	e <- rv$edits[[ap]]
	p <- rv$plots[[ap]]
	
	grid_major_on <- isTRUE(e$grid_major)
	grid_minor_on <- isTRUE(e$grid_minor)
	legend_box_on <- isTRUE(e$legend_box)
	
	# Detect discrete levels for colour/fill via ggplot_build
	get_levels <- function(p, aes_name) {
		gb <- try(ggplot2::ggplot_build(p), silent = TRUE)
		if (inherits(gb, "try-error")) return(character(0))
		vals <- unlist(lapply(gb$data, function(d) d[[aes_name]]))
		vals <- unique(vals)
		vals <- vals[!is.na(vals)]
		as.character(vals)
	}
	colour_lvls <- e$colour_levels %||% get_levels(p, "colour")
	fill_lvls   <- e$fill_levels   %||% get_levels(p, "fill")
	
	make_color_input <- function(id, label, value) {
		if (requireNamespace("colourpicker", quietly = TRUE)) {
			colourpicker::colourInput(id, label, value = value %||% "#1f77b4", allowTransparent = TRUE)
		} else {
			textInput(id, label, value = value %||% "#1f77b4", placeholder = "#RRGGBB or name")
		}
	}
	
	tagList(
		actionButton("apply_all_theme", "Use for all plots", class = "btn btn-sm btn-default btn-block"),
		tags$hr(),
		h4(sprintf("Theme — %s", ap)),
		tags$hr(),
		
		# tabs with persistence
		tabsetPanel(
			id = "theme_tabs",
			selected = rv$tabs$theme %||% "Base",
			tabPanel("Base",
				selectInput("ui_theme", "Theme",
						choices = c("theme_minimal","theme_classic","theme_bw","theme_light","theme_gray"),
						selected = e$theme %||% BASE$theme),
				sliderInput("ui_base_size", "Base text size", 
						value = e$base_size %||% BASE$base_size, 
						min = 6, max = 30, step = 1)
			),
			tabPanel("Legend",
				selectInput("ui_legend_pos", "Legend position", 
						choices = LEGEND_POS, 
						selected = e$legend_pos %||% BASE$legend_pos),
				checkboxInput("ui_legend_box", "Legend box", 
							value = isTRUE(e$legend_box)),
				div(class = if (legend_box_on) NULL else "muted-control",
					p(class="help-block", "Legend box must be enabled for these settings to show."))
			),
			tabPanel("Background",
				selectInput("ui_panel_bg", "Panel background", 
						choices = c("Default", "White", "Gray", "Transparent"),
						selected = e$panel_bg %||% "Default"),
				selectInput("ui_plot_bg", "Plot background", 
						choices = c("Default", "White", "Gray", "Transparent"),
						selected = e$plot_bg %||% "Default")
			),
			tabPanel("Grid",
				checkboxInput("ui_grid_major", "Major grid lines", 
							value = isTRUE(e$grid_major)),
				div(class = if (grid_major_on) NULL else "muted-control",
					selectInput("ui_grid_major_linetype", "Major grid linetype",
							choices = c("solid","dashed","dotted","dotdash","longdash","twodash"),
							selected = e$grid_major_linetype %||% "solid")),
				checkboxInput("ui_grid_minor", "Minor grid lines", 
							value = isTRUE(e$grid_minor)),
				div(class = if (grid_minor_on) NULL else "muted-control",
					selectInput("ui_grid_minor_linetype", "Minor grid linetype",
							choices = c("solid","dashed","dotted","dotdash","longdash","twodash"),
							selected = e$grid_minor_linetype %||% "dashed")),
				selectInput("ui_grid_color", "Grid color", 
						choices = c("Default", "Gray", "Light gray", "Dark gray", "Black"),
						selected = e$grid_color %||% "Default")
			),
			tabPanel("Colors",
				selectInput("ui_palette", "Palette", 
						choices = c("None","viridis","magma","plasma","inferno","cividis"),
						selected = e$palette %||% "None"),
				div(style="margin-top:8px;", actionButton("ui_apply_palette_levels", "Generate colors for levels", class = "btn btn-sm btn-default")),
				tags$hr(),
				if (!length(colour_lvls) && !length(fill_lvls)) tags$em("No discrete colour/fill levels detected yet."),
				if (length(colour_lvls)) tagList(
					tags$strong("Colour levels"),
					lapply(seq_along(colour_lvls), function(i) {
						lvl <- as.character(colour_lvls[i]); cur <- (e$colour_levels_cols %||% rep(NA_character_, length(colour_lvls)))[i]
						make_color_input(paste0("ui_col_level_", i), paste0("[", lvl, "]"), cur)
					})
				),
				if (length(fill_lvls)) tagList(
					tags$strong("Fill levels"),
					lapply(seq_along(fill_lvls), function(i) {
						lvl <- as.character(fill_lvls[i]); cur <- (e$fill_levels_cols %||% rep(NA_character_, length(fill_lvls)))[i]
						make_color_input(paste0("ui_fill_level_", i), paste0("[", lvl, "]"), cur)
					})
				)
			)
		)
	)
}

register_theme_observers <- function(input, rv, session) {
	observeEvent(input$ui_theme, {
		if (rv$is_hydrating) return()
		ap <- rv$active_tab; if (is.null(ap) || is.null(rv$plots[[ap]]) || identical(ap,"Grid")) return()
		ensure_edits(rv, ap, grid = FALSE)
		rv$edits[[ap]]$theme <- input$ui_theme
	}, ignoreInit = TRUE, ignoreNULL = TRUE)
	
	observeEvent(input$ui_base_size, {
		if (rv$is_hydrating) return()
		ap <- rv$active_tab; if (is.null(ap) || is.null(rv$plots[[ap]]) || identical(ap,"Grid")) return()
		ensure_edits(rv, ap, grid = FALSE)
		rv$edits[[ap]]$base_size <- as_num_safe(input$ui_base_size)
	}, ignoreInit = TRUE, ignoreNULL = TRUE)
	
	observeEvent(input$ui_legend_pos, {
		if (rv$is_hydrating) return()
		ap <- rv$active_tab; if (is.null(ap) || is.null(rv$plots[[ap]]) || identical(ap,"Grid")) return()
		ensure_edits(rv, ap, grid = FALSE)
		rv$edits[[ap]]$legend_pos <- legend_pos_value(input$ui_legend_pos)
	}, ignoreInit = TRUE, ignoreNULL = TRUE)
	
	observeEvent(input$ui_legend_box, {
		if (rv$is_hydrating) return()
		ap <- rv$active_tab; if (is.null(ap) || is.null(rv$plots[[ap]]) || identical(ap,"Grid")) return()
		ensure_edits(rv, ap, grid = FALSE)
		rv$edits[[ap]]$legend_box <- input$ui_legend_box
	}, ignoreInit = TRUE, ignoreNULL = TRUE)
	
	observeEvent(input$ui_panel_bg, {
		if (rv$is_hydrating) return()
		ap <- rv$active_tab; if (is.null(ap) || is.null(rv$plots[[ap]]) || identical(ap,"Grid")) return()
		ensure_edits(rv, ap, grid = FALSE)
		rv$edits[[ap]]$panel_bg <- input$ui_panel_bg
	}, ignoreInit = TRUE, ignoreNULL = TRUE)
	
	observeEvent(input$ui_plot_bg, {
		if (rv$is_hydrating) return()
		ap <- rv$active_tab; if (is.null(ap) || is.null(rv$plots[[ap]]) || identical(ap,"Grid")) return()
		ensure_edits(rv, ap, grid = FALSE)
		rv$edits[[ap]]$plot_bg <- input$ui_plot_bg
	}, ignoreInit = TRUE, ignoreNULL = TRUE)
	
	observeEvent(input$ui_grid_major, {
		if (rv$is_hydrating) return()
		ap <- rv$active_tab; if (is.null(ap) || is.null(rv$plots[[ap]]) || identical(ap,"Grid")) return()
		ensure_edits(rv, ap, grid = FALSE)
		rv$edits[[ap]]$grid_major <- input$ui_grid_major
	}, ignoreInit = TRUE, ignoreNULL = TRUE)
	
	observeEvent(input$ui_grid_minor, {
		if (rv$is_hydrating) return()
		ap <- rv$active_tab; if (is.null(ap) || is.null(rv$plots[[ap]]) || identical(ap,"Grid")) return()
		ensure_edits(rv, ap, grid = FALSE)
		rv$edits[[ap]]$grid_minor <- input$ui_grid_minor
	}, ignoreInit = TRUE, ignoreNULL = TRUE)
	
	observeEvent(input$ui_grid_color, {
		if (rv$is_hydrating) return()
		ap <- rv$active_tab; if (is.null(ap) || is.null(rv$plots[[ap]]) || identical(ap,"Grid")) return()
		ensure_edits(rv, ap, grid = FALSE)
		rv$edits[[ap]]$grid_color <- input$ui_grid_color
	}, ignoreInit = TRUE, ignoreNULL = TRUE)
	
	observeEvent(input$ui_grid_major_linetype, {
		if (rv$is_hydrating) return()
		ap <- rv$active_tab; if (is.null(ap) || is.null(rv$plots[[ap]]) || identical(ap,"Grid")) return()
		ensure_edits(rv, ap, grid = FALSE)
		rv$edits[[ap]]$grid_major_linetype <- input$ui_grid_major_linetype
	}, ignoreInit = TRUE, ignoreNULL = TRUE)
	
	observeEvent(input$ui_grid_minor_linetype, {
		if (rv$is_hydrating) return()
		ap <- rv$active_tab; if (is.null(ap) || is.null(rv$plots[[ap]]) || identical(ap,"Grid")) return()
		ensure_edits(rv, ap, grid = FALSE)
		rv$edits[[ap]]$grid_minor_linetype <- input$ui_grid_minor_linetype
	}, ignoreInit = TRUE, ignoreNULL = TRUE)
	
	# Colors tab
	observeEvent(input$ui_palette, {
		if (rv$is_hydrating) return()
		ap <- rv$active_tab; if (is.null(ap) || is.null(rv$plots[[ap]]) || identical(ap,"Grid")) return()
		ensure_edits(rv, ap, grid = FALSE)
		rv$edits[[ap]]$palette <- input$ui_palette
	}, ignoreInit = TRUE, ignoreNULL = TRUE)
	
	observeEvent(input$ui_apply_palette_levels, {
		ap <- rv$active_tab; if (is.null(ap) || is.null(rv$plots[[ap]])) return()
		ensure_edits(rv, ap, grid = FALSE)
		pal <- rv$edits[[ap]]$palette %||% "None"
		# compute current levels
		get_levels <- function(p, aes_name) {
			gb <- try(ggplot2::ggplot_build(p), silent = TRUE)
			if (inherits(gb, "try-error")) return(character(0))
			vals <- unlist(lapply(gb$data, function(d) d[[aes_name]]))
			vals <- unique(vals)
			vals <- vals[!is.na(vals)]
			as.character(vals)
		}
		pobj <- rv$plots[[ap]]
		cols <- function(n) {
			if (!requireNamespace("viridisLite", quietly = TRUE)) return(grDevices::rainbow(n))
			viridisLite::viridis(n, option = if (pal == "None") "viridis" else pal)
		}
		colour_lvls <- get_levels(pobj, "colour"); if (length(colour_lvls)) {
			clrs <- cols(length(colour_lvls)); names(clrs) <- colour_lvls
			rv$edits[[ap]]$colour_levels <- colour_lvls
			rv$edits[[ap]]$colour_levels_cols <- unname(clrs)
		}
		fill_lvls <- get_levels(pobj, "fill"); if (length(fill_lvls)) {
			clrs <- cols(length(fill_lvls)); names(clrs) <- fill_lvls
			rv$edits[[ap]]$fill_levels <- fill_lvls
			rv$edits[[ap]]$fill_levels_cols <- unname(clrs)
		}
	}, ignoreInit = TRUE)
	
	# Dynamic level color pickers
	observe({
		ap <- rv$active_tab; if (is.null(ap) || is.null(rv$plots[[ap]])) return()
		e <- rv$edits[[ap]]
		if (!is.null(e$colour_levels) && length(e$colour_levels)) {
			lapply(seq_along(e$colour_levels), function(i) {
				observeEvent(input[[paste0("ui_col_level_", i)]], {
					rv$edits[[ap]]$colour_levels_cols[i] <- input[[paste0("ui_col_level_", i)]]
				}, ignoreInit = TRUE, ignoreNULL = TRUE)
			})
		}
		if (!is.null(e$fill_levels) && length(e$fill_levels)) {
			lapply(seq_along(e$fill_levels), function(i) {
				observeEvent(input[[paste0("ui_fill_level_", i)]], {
					rv$edits[[ap]]$fill_levels_cols[i] <- input[[paste0("ui_fill_level_", i)]]
				}, ignoreInit = TRUE, ignoreNULL = TRUE)
			})
		}
	})
	
	# Persist selected sub-tab
	observeEvent(input$theme_tabs, {
		rv$tabs$theme <- input$theme_tabs
	}, ignoreInit = TRUE, ignoreNULL = TRUE)
}
