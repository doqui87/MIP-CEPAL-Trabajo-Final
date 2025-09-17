# R/io_export.R
# ============================================================
# Export utilitario: CSV y figuras
# ============================================================

suppressPackageStartupMessages({ library(readr); library(ggplot2) })

export_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(df, path)
  invisible(path)
}

export_plot <- function(plot, path, width = 9, height = 6, dpi = 300) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  ggsave(path, plot = plot, width = width, height = height, dpi = dpi)
  invisible(path)
}
