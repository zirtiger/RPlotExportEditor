# plot-editor/R/panes_text.R

text_pane_ui <- function(rv) {
  grid_ctx <- identical(rv$active_tab, "Grid")
  if (grid_ctx) {
    return(tagList(
      h4("Text — Grid context"),
      helpText("When Grid is active, text edits are stored as grid-specific overrides per plot."),
      helpText("Select a grid cell in the layout and then edit that plot's text under its own tab, or switch to a plot tab to edit its standalone text.")
    ))
  }
  
  ap <- rv$active_tab
  plot_selected <- !is.null(ap) && !is.null(rv$plots[[ap]])
  if (!plot_selected) return(tagList(h4("Text"), helpText("Select a plot tab to edit its text.")))
  
  ensure_edits(rv, ap, grid = FALSE)
  e <- rv$edits[[ap]]
  p <- rv$plots[[ap]]
  
  get_lab <- function(lbl) {
    val <- tryCatch(p$labels[[lbl]], error = function(...) NULL)
    if (is.null(val)) "" else as.character(val)
  }
  
  tagList(
    actionButton("apply_all_text", "Use for all plots", class = "btn btn-sm btn-default"),
    h4(sprintf("Text — %s", ap)),
    textInput("ui_title",    "Title",    e$title    %||% get_lab("title")),
    textInput("ui_subtitle", "Subtitle", e$subtitle %||% get_lab("subtitle")),
    textInput("ui_caption",  "Caption",  e$caption  %||% get_lab("caption")),
    textInput("ui_xlab",     "X label",  e$xlab     %||% get_lab("x")),
    textInput("ui_ylab",     "Y label",  e$ylab     %||% get_lab("y"))
  )
}

register_text_observers <- function(input, rv, session) {
  bind_edit <- function(input_id, field) {
    observeEvent(input[[input_id]], {
      if (!is.null(rv$is_hydrating) && rv$is_hydrating) return()
      ap <- rv$active_tab
      if (is.null(ap) || is.null(rv$plots[[ap]])) return()
      
      if (identical(ap, "Grid")) {
        # In grid context, no direct target — user should switch to a plot tab to edit that plot's text
        return()
      } else {
        ensure_edits(rv, ap, grid = FALSE)
        rv$edits[[ap]][[field]] <- input[[input_id]]
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
  }
  
  bind_edit("ui_title",    "title")
  bind_edit("ui_subtitle", "subtitle")
  bind_edit("ui_caption",  "caption")
  bind_edit("ui_xlab",     "xlab")
  bind_edit("ui_ylab",     "ylab")
  
  observeEvent(input$apply_all_text, {
    ap <- rv$active_tab
    if (is.null(ap) || identical(ap, "Grid") || is.null(rv$plots[[ap]])) return()
    ensure_edits(rv, ap, grid = FALSE)
    src <- rv$edits[[ap]]
    for (nm in names(rv$plots)) {
      ensure_edits(rv, nm, grid = FALSE)
      rv$edits[[nm]][c("title","subtitle","caption","xlab","ylab")] <- src[c("title","subtitle","caption","xlab","ylab")]
    }
    showNotification("Text settings applied to all plots.", type = "message")
  })
}
