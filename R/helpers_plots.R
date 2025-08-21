# plot-editor/R/helpers_plots.R

# Apply edits to a plot (works for plot tabs & grid overrides)
apply_edits <- function(p, edits) {
  e <- edits %||% list()
  
  # titles
  if (!is.null(e$title))     p <- p + ggplot2::labs(title = e$title)
  if (!is.null(e$subtitle))  p <- p + ggplot2::labs(subtitle = e$subtitle)
  if (!is.null(e$caption))   p <- p + ggplot2::labs(caption  = e$caption)
  if (!is.null(e$xlab))      p <- p + ggplot2::labs(x = e$xlab)
  if (!is.null(e$ylab))      p <- p + ggplot2::labs(y = e$ylab)
  
  # theme
  tfun <- get_theme_fun(e$theme %||% BASE$theme)
  p <- p + tfun(base_size = e$base_size %||% BASE$base_size)
  
  # legend + grids
  p <- p + ggplot2::theme(
    legend.position = legend_pos_value(e$legend_pos %||% BASE$legend_pos)
  )
  
  p
}

# Save/export helper (uses ggsave devices; ragg recommended for rasters)
export_plot_file <- function(p, path, fmt = c("PNG","TIFF","PDF","SVG","EPS"),
                             width_mm = BASE$width_mm, height_mm = BASE$height_mm, dpi = BASE$dpi) {
  fmt <- match.arg(fmt)
  w_in <- width_mm  / 25.4
  h_in <- height_mm / 25.4
  
  switch(fmt,
         PNG = ggplot2::ggsave(path, p, width = w_in, height = h_in, dpi = dpi,
                               device = ragg::agg_png, bg = "white"),
         TIFF= ggplot2::ggsave(path, p, width = w_in, height = h_in, dpi = dpi,
                               device = ragg::agg_tiff, compression = "lzw"),
         PDF = ggplot2::ggsave(path, p, width = w_in, height = h_in, device = grDevices::cairo_pdf),
         SVG = ggplot2::ggsave(path, p, width = w_in, height = h_in, device = svglite::svglite),
         EPS = ggplot2::ggsave(path, p, width = w_in, height = h_in, device = grDevices::cairo_ps)
  )
  invisible(path)
}

# Vector → PNG for preview
vector_to_png <- function(src, fmt, out_png, width_mm, height_mm, dpi) {
  fmt <- toupper(fmt)
  if (fmt == "PDF" && .have_pdftools) {
    pdftools::pdf_convert(src, format = "png", filenames = out_png, pages = 1, dpi = dpi)
  } else if (fmt == "SVG" && .have_rsvg) {
    width_px  <- as.integer((width_mm / 25.4) * dpi)
    height_px <- as.integer((height_mm / 25.4) * dpi)
    rsvg::rsvg_png(src, file = out_png, width = width_px, height = height_px)
  } else if (fmt %in% c("PNG","TIFF")) {
    if (fmt == "PNG") {
      file.copy(src, out_png, overwrite = TRUE)
    } else if (.have_magick) {
      img <- magick::image_read(src)
      magick::image_write(img, path = out_png, format = "png")
    } else {
      file.copy(src, out_png, overwrite = TRUE)
    }
  } else {
    stop("No vector rasterizer available; render PNG directly.")
  }
  out_png
}

# WYSIWYG preview: render to temp file using the given export settings; return list() for renderImage
render_preview_png <- function(p, width_mm, height_mm, dpi, fmt) {
  tmpdir <- tempdir()
  ext    <- switch(toupper(fmt), PNG="png", TIFF="tiff", PDF="pdf", SVG="svg", EPS="eps", "png")
  src    <- file.path(tmpdir, paste0("prev_src.", ext))
  out    <- file.path(tmpdir, "prev.png")
  
  export_plot_file(p, src, fmt = toupper(fmt), width_mm = width_mm, height_mm = height_mm, dpi = dpi)
  tryCatch({
    vector_to_png(src, toupper(fmt), out, width_mm, height_mm, dpi)
  }, error = function(e) {
    export_plot_file(p, out, fmt = "PNG", width_mm = width_mm, height_mm = height_mm, dpi = dpi)
  })
  
  list(src = out, contentType = "image/png")
}

# Select the first available plot tab
select_first_plot <- function(rv, session) {
  first_name <- names(rv$plots)[1]
  if (!is.null(first_name)) {
    updateTabsetPanel(session, "active_tabset", selected = first_name)
    rv$active_tab <- first_name
  }
}