# plot-editor/R/ui.R
# Top-level UI layout: header, sidebar, body with placeholders

app_ui <- function(request = NULL) {
  header <- shinydashboard::dashboardHeader(title = "Plot Editor (WYSIWYG)")
  
  sidebar <- shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      id = "mainmenu",
      shinydashboard::menuItem("Grid",   tabName = "grid",   icon = icon("th")),
      shinydashboard::menuItem("Export", tabName = "export", icon = icon("download")),
      shinydashboard::menuItem("Text",   tabName = "text",   icon = icon("font")),
      shinydashboard::menuItem("Theme",  tabName = "theme",  icon = icon("paint-brush")),
      shinydashboard::menuItem("Axes",   tabName = "axes",   icon = icon("ruler-combined")),
      hr(),
      fileInput("plots_rds", "Load ggplot (.rds, multiple)", accept = ".rds", multiple = TRUE),
      actionButton("load_demo", "Load 3 demo plots", class = "btn btn-link")
    )
  )
  
  body <- shinydashboard::dashboardBody(
    # Optional static assets
    if (file.exists("www/style.css")) includeCSS("www/style.css"),
    if (file.exists("www/script.js")) includeScript("www/script.js"),
    
    fluidRow(
      class = "row-fullheight",
      column(
        width = 4,
        class = "col-fill side-controls",
        div(
          class = "pane-container",
          uiOutput("subsidebar")   # pane content (Text/Theme/Grid/Export/Axes)
        )
      ),
      column(
        width = 8,
        class = "col-fill preview-area",
        div(
          class = "preview-toolbar",
          uiOutput("preview_toolbar")
        ),
        div(
          class = "preview-container",
          uiOutput("tabs_area")    # Grid + one tab per plot (previews)
        )
      )
    )
  )
  
  shinydashboard::dashboardPage(header, sidebar, body)
}
