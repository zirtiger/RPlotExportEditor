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
    actionButton("apply_all_theme", "Use for all plots", class = "btn btn-sm btn-default"),
    h4(sprintf("Theme — %s", ap)),
    selectInput("ui_theme", "Theme",
                choices = c("theme_minimal","theme_classic","theme_bw","theme_light","theme_gray"),
                selected = e$theme %||% BASE$theme),
    numericInput("ui_base_size", "Base text size", value = e$base_size %||% BASE$base_size, min = 6, max = 30, step = 1),
    selectInput("ui_legend_pos", "Legend position", choices = LEGEND_POS, selected = e$legend_pos %||% BASE$legend_pos)
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
  
  observeEvent(input$apply_all_theme, {
    ap <- rv$active_tab
    if (is.null(ap) || identical(ap,"Grid") || is.null(rv$plots[[ap]])) return()
    ensure_edits(rv, ap, grid = FALSE)
    src <- rv$edits[[ap]]
    for (nm in names(rv$plots)) {
      ensure_edits(rv, nm, grid = FALSE)
      rv$edits[[nm]][c("theme","base_size","legend_pos")] <- src[c("theme","base_size","legend_pos")]
    }
    showNotification("Theme settings applied to all plots.", type = "message")
  })
}
