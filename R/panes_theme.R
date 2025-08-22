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
							value = isTRUE(e$legend_box))
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
				selectInput("ui_grid_major_linetype", "Major grid linetype",
						choices = c("solid","dashed","dotted","dotdash","longdash","twodash"),
						selected = e$grid_major_linetype %||% "solid"),
				checkboxInput("ui_grid_minor", "Minor grid lines", 
							value = isTRUE(e$grid_minor)),
				selectInput("ui_grid_minor_linetype", "Minor grid linetype",
						choices = c("solid","dashed","dotted","dotdash","longdash","twodash"),
						selected = e$grid_minor_linetype %||% "dashed"),
				selectInput("ui_grid_color", "Grid color", 
						choices = c("Default", "Gray", "Light gray", "Dark gray", "Black"),
						selected = e$grid_color %||% "Default")
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
	
	# Persist selected sub-tab
	observeEvent(input$theme_tabs, {
		rv$tabs$theme <- input$theme_tabs
	}, ignoreInit = TRUE, ignoreNULL = TRUE)
}
