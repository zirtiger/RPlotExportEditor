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
  
  # Colors: apply per-level mappings using limits-aligned values to avoid name mismatch
  apply_level_colors <- function(p) {
    try_apply <- function(expr, p) { tryCatch({ expr }, error = function(...) p) }
    
    cat("\n=== APPLYING COLORS TO PLOT ===\n")
    cat("  continuous_colour_palette:", e$continuous_colour_palette, "\n")
    cat("  continuous_fill_palette:", e$continuous_fill_palette, "\n")
    cat("  colour_levels:", paste(e$colour_levels, collapse = ", "), "\n")
    cat("  colour_levels_cols:", paste(e$colour_levels_cols, collapse = ", "), "\n")
    
    # Apply continuous palettes first (if present)
    if (!is.null(e$continuous_colour_palette) && e$continuous_colour_palette != "None") {
      cat("  Applying continuous COLOUR palette:", e$continuous_colour_palette, "\n")
      palette_func <- switch(e$continuous_colour_palette,
        "viridis" = viridisLite::viridis,
        "magma" = viridisLite::magma,
        "plasma" = viridisLite::plasma,
        "inferno" = viridisLite::inferno,
        "cividis" = viridisLite::cividis,
        viridisLite::viridis
      )
      p <- try_apply(p + ggplot2::scale_color_viridis_c(option = e$continuous_colour_palette), p)
    }
    
    if (!is.null(e$continuous_fill_palette) && e$continuous_fill_palette != "None") {
      palette_func <- switch(e$continuous_fill_palette,
        "viridis" = viridisLite::viridis,
        "magma" = viridisLite::magma,
        "plasma" = viridisLite::plasma,
        "inferno" = viridisLite::inferno,
        "cividis" = viridisLite::cividis,
        viridisLite::viridis
      )
      p <- try_apply(p + ggplot2::scale_fill_viridis_c(option = e$continuous_fill_palette), p)
    }
    
    # Apply discrete level colors
    # colour levels
    if (!is.null(e$colour_levels) && !is.null(e$colour_levels_cols)) {
      lv <- as.character(e$colour_levels)
      cols <- as.character(e$colour_levels_cols)
      keep <- nzchar(lv) & nzchar(cols)
      lv <- lv[keep]; cols <- cols[keep]
      if (length(lv) && length(cols) && length(lv) == length(cols)) {
        p <- try_apply(p + ggplot2::scale_color_manual(values = unname(cols), limits = lv, breaks = lv), p)
      }
    }
    # fill levels
    if (!is.null(e$fill_levels) && !is.null(e$fill_levels_cols)) {
      lv <- as.character(e$fill_levels)
      cols <- as.character(e$fill_levels_cols)
      keep <- nzchar(lv) & nzchar(cols)
      lv <- lv[keep]; cols <- cols[keep]
      if (length(lv) && length(cols) && length(lv) == length(cols)) {
        p <- try_apply(p + ggplot2::scale_fill_manual(values = unname(cols), limits = lv, breaks = lv), p)
      }
    }
    p
  }
  p <- apply_level_colors(p)
  
  # Custom text sizes
  text_theme <- ggplot2::theme()
  
  if (!is.null(e$title_size)) {
    text_theme <- text_theme + ggplot2::theme(plot.title = ggplot2::element_text(size = e$title_size))
  }
  if (!is.null(e$subtitle_size)) {
    text_theme <- text_theme + ggplot2::theme(plot.subtitle = ggplot2::element_text(size = e$subtitle_size))
  }
  if (!is.null(e$caption_size)) {
    text_theme <- text_theme + ggplot2::theme(plot.caption = ggplot2::element_text(size = e$caption_size))
  }
  if (!is.null(e$axis_title_size)) {
    text_theme <- text_theme + ggplot2::theme(axis.title = ggplot2::element_text(size = e$axis_title_size))
  }
  if (!is.null(e$axis_text_size)) {
    text_theme <- text_theme + ggplot2::theme(axis.text = ggplot2::element_text(size = e$axis_text_size))
  }
  if (!is.null(e$legend_title_size)) {
    text_theme <- text_theme + ggplot2::theme(legend.title = ggplot2::element_text(size = e$legend_title_size))
  }
  if (!is.null(e$legend_text_size)) {
    text_theme <- text_theme + ggplot2::theme(legend.text = ggplot2::element_text(size = e$legend_text_size))
  }
  
  p <- p + text_theme
  
  # Axis limits
  if (!is.null(e$x_min) || !is.null(e$x_max)) {
    x_limits <- c(e$x_min, e$x_max)
    if (all(!is.na(x_limits))) {
      p <- p + ggplot2::xlim(x_limits)
    }
  }
  if (!is.null(e$y_min) || !is.null(e$y_max)) {
    y_limits <- c(e$y_min, e$y_max)
    if (all(!is.na(y_limits))) {
      p <- p + ggplot2::ylim(y_limits)
    }
  }
  
  # Axis breaks — prefer step-based and use scales::breaks_width for clean ticks
  if (!is.null(e$x_step_major) && is.finite(e$x_step_major) && e$x_step_major > 0) {
    x_major_br <- scales::breaks_width(e$x_step_major)
  } else {
    x_major_br <- ggplot2::waiver()
  }
  if (!is.null(e$x_step_minor) && is.finite(e$x_step_minor) && e$x_step_minor > 0) {
    x_minor_br <- scales::breaks_width(e$x_step_minor)
  } else {
    x_minor_br <- ggplot2::waiver()
  }
  if (!identical(x_major_br, ggplot2::waiver()) || !identical(x_minor_br, ggplot2::waiver())) {
    p <- p + ggplot2::scale_x_continuous(breaks = x_major_br, minor_breaks = x_minor_br)
  } else if (!is.null(e$x_major) || !is.null(e$x_minor)) {
    x_min_val <- if (!is.null(e$x_min) && is.finite(e$x_min)) e$x_min else 0
    x_max_val <- if (!is.null(e$x_max) && is.finite(e$x_max)) e$x_max else 10
    x_breaks <- if (!is.null(e$x_major) && is.finite(e$x_major) && e$x_major > 0) seq(x_min_val, x_max_val, length.out = max(1, e$x_major + 1)) else ggplot2::waiver()
    x_minor_breaks <- if (!is.null(e$x_minor) && is.finite(e$x_minor) && e$x_minor > 0) seq(x_min_val, x_max_val, length.out = max(1, e$x_minor + 1)) else ggplot2::waiver()
    p <- p + ggplot2::scale_x_continuous(breaks = x_breaks, minor_breaks = x_minor_breaks)
  }
  
  if (!is.null(e$y_step_major) && is.finite(e$y_step_major) && e$y_step_major > 0) {
    y_major_br <- scales::breaks_width(e$y_step_major)
  } else {
    y_major_br <- ggplot2::waiver()
  }
  if (!is.null(e$y_step_minor) && is.finite(e$y_step_minor) && e$y_step_minor > 0) {
    y_minor_br <- scales::breaks_width(e$y_step_minor)
  } else {
    y_minor_br <- ggplot2::waiver()
  }
  if (!identical(y_major_br, ggplot2::waiver()) || !identical(y_minor_br, ggplot2::waiver())) {
    p <- p + ggplot2::scale_y_continuous(breaks = y_major_br, minor_breaks = y_minor_br)
  } else if (!is.null(e$y_major) || !is.null(e$y_minor)) {
    y_min_val <- if (!is.null(e$y_min) && is.finite(e$y_min)) e$y_min else 0
    y_max_val <- if (!is.null(e$y_max) && is.finite(e$y_max)) e$y_max else 10
    y_breaks <- if (!is.null(e$y_major) && is.finite(e$y_major) && e$y_major > 0) seq(y_min_val, y_max_val, length.out = max(1, e$y_major + 1)) else ggplot2::waiver()
    y_minor_breaks <- if (!is.null(e$y_minor) && is.finite(e$y_minor) && e$y_minor > 0) seq(y_min_val, y_max_val, length.out = max(1, e$y_minor + 1)) else ggplot2::waiver()
    p <- p + ggplot2::scale_y_continuous(breaks = y_breaks, minor_breaks = y_minor_breaks)
  }
  
  # legend + grids
  legend_theme <- ggplot2::theme(
    legend.position = legend_pos_value(e$legend_pos %||% BASE$legend_pos)
  )
  
  # Legend box
  if (!is.null(e$legend_box)) {
    legend_theme <- legend_theme + ggplot2::theme(
      legend.box.background = if (isTRUE(e$legend_box)) ggplot2::element_rect(fill = "white", color = "black") else ggplot2::element_blank()
    )
  }
  
  # Panel and plot backgrounds
  if (!is.null(e$panel_bg) && e$panel_bg != "Default") {
    panel_color <- switch(e$panel_bg,
                         "White" = "white",
                         "Gray" = "gray90",
                         "Light gray" = "gray95",
                         "Dark gray" = "gray80",
                         "Transparent" = "transparent",
                         "white")
    legend_theme <- legend_theme + ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = panel_color)
    )
  }
  
  if (!is.null(e$plot_bg) && e$plot_bg != "Default") {
    plot_color <- switch(e$plot_bg,
                        "White" = "white",
                        "Gray" = "gray90",
                        "Light gray" = "gray95",
                        "Dark gray" = "gray80",
                        "Transparent" = "transparent",
                        "white")
    legend_theme <- legend_theme + ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = plot_color)
    )
  }
  
  # Grid lines
  if (!is.null(e$grid_major) || !is.null(e$grid_minor)) {
    grid_color <- if (!is.null(e$grid_color) && e$grid_color != "Default") {
      switch(e$grid_color,
             "Gray" = "gray50",
             "Light gray" = "gray80",
             "Dark gray" = "gray30",
             "Black" = "black",
             "gray50")
    } else "gray50"
    
    if (!is.null(e$grid_major)) {
      legend_theme <- legend_theme + ggplot2::theme(
        panel.grid.major = if (isTRUE(e$grid_major)) ggplot2::element_line(color = grid_color, linetype = (e$grid_major_linetype %||% "solid")) else ggplot2::element_blank()
      )
    }
    
    if (!is.null(e$grid_minor)) {
      legend_theme <- legend_theme + ggplot2::theme(
        panel.grid.minor = if (isTRUE(e$grid_minor)) ggplot2::element_line(color = grid_color, linetype = (e$grid_minor_linetype %||% "dashed")) else ggplot2::element_blank()
      )
    }
  }
  
  p <- p + legend_theme
  
  p
}

# Export plot for download (wrapper around export_plot_file)
export_plot <- function(p, file, export_settings) {
  width_mm <- export_settings$width_mm %||% BASE$width_mm
  height_mm <- export_settings$height_mm %||% BASE$height_mm
  dpi <- export_settings$dpi %||% BASE$dpi
  format <- export_settings$format %||% BASE$format
  
  export_plot_file(p, file, fmt = format, width_mm = width_mm, height_mm = height_mm, dpi = dpi)
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