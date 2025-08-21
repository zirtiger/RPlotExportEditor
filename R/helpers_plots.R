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
  
  # element-level sizes if provided
  theme_args <- list()
  if (!is.null(e$plot_title_size))     theme_args$`plot.title`    <- ggplot2::element_text(size = e$plot_title_size)
  if (!is.null(e$plot_subtitle_size))  theme_args$`plot.subtitle` <- ggplot2::element_text(size = e$plot_subtitle_size)
  if (!is.null(e$axis_title_size))     theme_args$`axis.title`    <- ggplot2::element_text(size = e$axis_title_size)
  if (!is.null(e$axis_text_size))      theme_args$`axis.text`     <- ggplot2::element_text(size = e$axis_text_size)
  if (!is.null(e$legend_title_size))   theme_args$`legend.title`  <- ggplot2::element_text(size = e$legend_title_size)
  if (!is.null(e$legend_text_size))    theme_args$`legend.text`   <- ggplot2::element_text(size = e$legend_text_size)
  if (length(theme_args) > 0) {
    p <- p + do.call(ggplot2::theme, theme_args)
  }
  
  # legend + grids
  p <- p + ggplot2::theme(
    legend.position = legend_pos_value(e$legend_pos %||% BASE$legend_pos)
  )
  
  # scales (limits and breaks)
  make_breaks <- function(step) {
    if (is.null(step)) ggplot2::waiver() else scales::breaks_width(step)
  }
  make_minor_breaks <- function(step) {
    if (is.null(step)) ggplot2::waiver() else scales::minor_breaks_width(step)
  }
  x_limits <- NULL
  y_limits <- NULL
  if (!is.null(e$xlim_min) || !is.null(e$xlim_max)) x_limits <- c(e$xlim_min %||% NA_real_, e$xlim_max %||% NA_real_)
  if (!is.null(e$ylim_min) || !is.null(e$ylim_max)) y_limits <- c(e$ylim_min %||% NA_real_, e$ylim_max %||% NA_real_)
  
  # Apply scales if user provided settings; ignore if discrete or errors
  if (!is.null(x_limits) || !is.null(e$x_breaks_step) || !is.null(e$x_minor_breaks_step)) {
    p <- tryCatch({
      p + ggplot2::scale_x_continuous(
        limits = x_limits,
        breaks = make_breaks(e$x_breaks_step),
        minor_breaks = make_minor_breaks(e$x_minor_breaks_step)
      )
    }, error = function(err) p)
  }
  if (!is.null(y_limits) || !is.null(e$y_breaks_step) || !is.null(e$y_minor_breaks_step)) {
    p <- tryCatch({
      p + ggplot2::scale_y_continuous(
        limits = y_limits,
        breaks = make_breaks(e$y_breaks_step),
        minor_breaks = make_minor_breaks(e$y_minor_breaks_step)
      )
    }, error = function(err) p)
  }
  
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

# Build grid patchwork object for grid preview/export
build_grid_patchwork <- function(rv) {
  r <- rv$grid$rows %||% BASE$grid_rows
  c <- rv$grid$cols %||% BASE$grid_cols
  cells <- rv$grid$cells %||% list()
  picked <- Filter(function(x) !is.null(x) && x != "(empty)" && x %in% names(rv$plots), cells)
  req(length(picked) > 0)
  plots <- lapply(picked, function(nm) {
    ensure_edits(rv, nm, grid = TRUE)
    apply_edits(rv$plots[[nm]], rv$grid_edits[[nm]])
  })
  patchwork::wrap_plots(plots, nrow = r, ncol = c,
                        guides = if (isTRUE(rv$grid$collect %||% BASE$grid_collect)) "collect" else "keep") +
    ggplot2::theme(legend.position = legend_pos_value(rv$grid$legend %||% BASE$grid_legend_pos))
}