# plot-editor/R/ui.R
# Top-level UI layout: header, sidebar, body with placeholders

app_ui <- function(request = NULL) {
  header <- shinydashboard::dashboardHeader(disable = TRUE)
  
  sidebar <- shinydashboard::dashboardSidebar(
    width = 300,
    shinydashboard::sidebarMenu(
      id = "mainmenu",
      shinydashboard::menuItem("Grid",   tabName = "grid",   icon = icon("th")),
      shinydashboard::menuItem("Export", tabName = "export", icon = icon("download")),
      shinydashboard::menuItem("Text",   tabName = "text",   icon = icon("font")),
      shinydashboard::menuItem("Theme",  tabName = "theme",  icon = icon("paint-brush")),
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
      column(
        width = 3,
        shinydashboard::box(
          title = NULL,
          width = 12, status = "primary", solidHeader = FALSE,
          height = "90vh",
          uiOutput("subsidebar")   # pane content (Grid/Export/Text/Theme)
        )
      ),
      column(
        width = 9,
        shinydashboard::box(
          title = NULL,
          width = 12, status = "primary", solidHeader = FALSE,
          height = "90vh",
          uiOutput("tabs_area")    # Grid + one tab per plot (previews)
        )
      )
    )
  )
  
  shinydashboard::dashboardPage(header, sidebar, body)
}
