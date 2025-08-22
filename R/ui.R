# plot-editor/R/ui.R
# Top-level UI layout: header, sidebar, body with placeholders

app_ui <- function(request = NULL) {
  header <- shinydashboard::dashboardHeader(title = "")
  
  sidebar <- shinydashboard::dashboardSidebar(
    width = 300,
    shinydashboard::sidebarMenuOutput("sidebar_menu")
  )
  
  body <- shinydashboard::dashboardBody(
    # Optional static assets
    if (file.exists("www/style.css")) includeCSS("www/style.css"),
    if (file.exists("www/script.js")) includeScript("www/script.js"),
    
    fluidRow(
      column(
        width = 3,
        shinydashboard::box(
          title = "Settings",
          width = 12, status = "primary", solidHeader = TRUE,
          height = "90vh",
          uiOutput("subsidebar")   # pane content (Grid/Export/Text/Theme)
        )
      ),
      column(
        width = 9,
        shinydashboard::box(
          title = "Plots",
          width = 12, status = "primary", solidHeader = TRUE,
          height = "90vh",
          uiOutput("tabs_area")    # Grid + one tab per plot (previews)
        )
      )
    )
  )
  
  shinydashboard::dashboardPage(header, sidebar, body)
}
