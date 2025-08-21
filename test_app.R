# Test script to verify the app loads correctly
cat("Testing Plot Editor app...\n")

# Check if required packages are available
required_packages <- c("shiny", "shinydashboard", "ggplot2")
missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

if (length(missing_packages) > 0) {
  cat("Missing packages:", paste(missing_packages, collapse = ", "), "\n")
  cat("Please install them with: install.packages(c(", paste0('"', missing_packages, '"', collapse = ", "), "))\n")
} else {
  cat("✓ All required packages are available.\n")
}

# Try to source the app files
tryCatch({
  source("R/globals.R")
  cat("✓ globals.R loaded successfully\n")
}, error = function(e) {
  cat("✗ Error loading globals.R:", e$message, "\n")
})

tryCatch({
  source("R/helpers_plots.R")
  cat("✓ helpers_plots.R loaded successfully\n")
}, error = function(e) {
  cat("✗ Error loading helpers_plots.R:", e$message, "\n")
})

tryCatch({
  source("R/state.R")
  cat("✓ state.R loaded successfully\n")
}, error = function(e) {
  cat("✗ Error loading state.R:", e$message, "\n")
})

tryCatch({
  source("R/ui.R")
  cat("✓ ui.R loaded successfully\n")
}, error = function(e) {
  cat("✗ Error loading ui.R:", e$message, "\n")
})

tryCatch({
  source("R/server.R")
  cat("✓ server.R loaded successfully\n")
}, error = function(e) {
  cat("✗ Error loading server.R:", e$message, "\n")
})

cat("\n✓ App files loaded successfully. You can now run the app with:\n")
cat("shiny::runApp()\n")