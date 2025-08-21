# plot-editor/R/globals.R

# ---- BASE (edit these to change app-wide defaults) -------------------
BASE <- list(
  base_size       = 12,
  width_mm        = 350,
  height_mm       = 240,
  dpi             = 300,
  theme           = "theme_minimal",
  format          = "PNG",
  legend_pos      = "right",
  grid_rows       = 1,
  grid_cols       = 2,
  grid_collect    = TRUE,
  grid_legend_pos = "bottom",
  # Text sizes
  title_size      = 14,
  subtitle_size   = 12,
  caption_size    = 10,
  axis_title_size = 12,
  axis_text_size  = 10,
  legend_title_size = 12,
  legend_text_size  = 10
)

# ---- Optional converters for vector → PNG preview --------------------
.have_pdftools <- requireNamespace("pdftools", quietly = TRUE)
.have_rsvg     <- requireNamespace("rsvg",     quietly = TRUE)
.have_magick   <- requireNamespace("magick",   quietly = TRUE)

# ---- Utils ------------------------------------------------------------
`%||%` <- function(x, y) if (is.null(x)) y else x

LEGEND_POS <- c("right","left","top","bottom","none")

legend_pos_value <- function(pos) {
  if (is.null(pos)) return("right")
  if (!pos %in% LEGEND_POS) return("right")
  pos
}

get_theme_fun <- function(name) {
  switch(name,
         theme_minimal = ggplot2::theme_minimal,
         theme_classic = ggplot2::theme_classic,
         theme_bw      = ggplot2::theme_bw,
         theme_light   = ggplot2::theme_light,
         theme_gray    = ggplot2::theme_gray,
         ggplot2::theme_minimal
  )
}

as_num_safe <- function(x) {
  if (is.null(x)) return(NULL)
  out <- suppressWarnings(as.numeric(x))
  ifelse(is.na(out), NULL, out)
}
