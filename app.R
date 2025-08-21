# plot-editor/app.R
# Entry point for the Shiny app (package-style: app_ui/app_server)

suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(ggplot2)
})

# Source all R/ files (ensure core ones load first)
r_dir   <- file.path(getwd(), "R")
r_files <- list.files(r_dir, pattern = "\\.R$", full.names = TRUE)

priority <- c("globals.R", "helpers_plots.R", "state.R", "ui.R", "server.R")
ordered  <- c(file.path(r_dir, priority), setdiff(r_files, file.path(r_dir, priority)))
invisible(lapply(ordered, source, local = TRUE))

# Launch
shinyApp(ui = app_ui(), server = app_server)
