# plot-editor/R/ui.R
# Top-level UI layout: header, sidebar, body with placeholders

app_ui <- function(request = NULL) {
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
    
    # Remove padding and make layout tight
    tags$style("
      .content-wrapper { padding: 0 !important; }
      .content { padding: 0 !important; }
      .row { margin: 0 !important; }
      .col-sm-3, .col-sm-9 { padding: 0 !important; }
    "),
    
    fluidRow(
      column(
        width = 3,
        div(
          style = "height: 100vh; background: white; border-right: 1px solid #ddd;",
          uiOutput("subsidebar")
        )
      ),
      column(
        width = 9,
        div(
          style = "height: 100vh; background: white;",
          uiOutput("tabs_area")
        )
      )
    )
  )
  
  shinydashboard::dashboardPage(
    header = NULL,  # Remove header completely
    sidebar = sidebar, 
    body = body
  )
}
