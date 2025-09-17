# R/io_plots.R
# ============================================================
# Gráficos reciben data.frames y devuelven ggplots
# - plot_compare_linkages: BL_n vs FL_n con flechas y0→y1 y size = mult_emp
# - plot_compare_years:    genérico para comparar y0 vs y1
# - theme_project / set_project_theme: tema global homogéneo
# ============================================================

suppressPackageStartupMessages({
  library(ggplot2); library(ggrepel); library(dplyr); library(tidyr); library(scales)
})



# -------------------------------
# 1) Tema global del proyecto
# -------------------------------
# Paleta: acento + neutros; pensada para papers/slides
project_colors <- list(
  accent   = "#2E7DAF",   # azul petróleo
  accent2  = "#E4572E",   # naranja cálido (segundo año)
  good     = "#4CAF50",
  warn     = "#FF9800",
  grid_m   = "grey85",
  grid_s   = "grey93",
  text     = "#1B1B1B"
)

theme_project <- function(base_size = 13, base_family = NULL) {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
      panel.grid.major = element_line(linewidth = 0.35, colour = project_colors$grid_m),
      panel.grid.minor = element_line(linewidth = 0.25, colour = project_colors$grid_s),
      axis.line        = element_line(linewidth = 0.4, colour = "grey35"),
      axis.title       = element_text(colour = project_colors$text),
      axis.text        = element_text(colour = project_colors$text),
      plot.title       = element_text(face = "bold", size = base_size + 3, colour = project_colors$text,
                                      margin = margin(b = 4)),
      plot.subtitle    = element_text(colour = "grey30", margin = margin(b = 10)),
      plot.caption     = element_text(colour = "grey35", size = base_size - 2),
      legend.position  = "bottom",
      legend.box       = "horizontal",
      legend.title     = element_text(face = "bold")
    )
}

# Setea tema global (opcional llamarlo una vez al iniciar el proyecto)
set_project_theme <- function(base_size = 13, base_family = NULL) {
  theme_set(theme_project(base_size, base_family))
  invisible(TRUE)
}

# Colores para años (hasta 5)
scale_year_colors <- function() {
  scale_color_manual(values = c(
    project_colors$accent,
    project_colors$accent2,
    "#4CAF50", "#9C27B0", "#FFB300"
  ))
}

# Helpers de robustez ------------------------------------------
.ensure_sector <- function(df) {
  if ("sector" %in% names(df)) return(df)
  cand <- intersect(c("Sector","desc_short","industry","industria","SECTOR","sect"), names(df))
  if (length(cand) >= 1) return(dplyr::rename(df, sector = !!rlang::sym(cand[1])))
  stop("Falta columna 'sector'.", call. = FALSE)
}
.ensure_year_numeric <- function(df) { 
  stopifnot("year" %in% names(df)); 
  df$year <- suppressWarnings(as.integer(df$year)); 
  df 
}

# Opcional: aplicar escalas log según flags
.apply_log_scales <- function(p, xlog = FALSE, ylog = FALSE) {
  if (isTRUE(xlog)) p <- p + scale_x_log10(labels = label_number(accuracy = 0.01))
  if (isTRUE(ylog)) p <- p + scale_y_log10(labels = label_number(accuracy = 0.01))
  p
}

# --------------------------------------------------------------
# 2) BL–FL por sector con flechas (y0→y1), size = mult_emp
#    Parámetros extra:
#      - xaxis_log, yaxis_log: booleans para ejes log
#      - label_topk: si no es NULL, etiqueta top-k sectores (evita saturación)
plot_compare_linkages <- function(linkages_stack, multipliers_stack,
                                  title = NULL, subtitle = NULL,
                                  xaxis_log = FALSE, yaxis_log = FALSE,
                                  label_topk = NULL) {
  # Preparar data
  linkages_stack    <- linkages_stack    |> .ensure_sector() |> .ensure_year_numeric()
  multipliers_stack <- multipliers_stack |> .ensure_sector() |> .ensure_year_numeric()
  
  yrs <- linkages_stack |> distinct(year) |> arrange(year) |> pull(year)
  stopifnot(length(yrs) >= 2)
  y0 <- min(yrs); y1 <- max(yrs)
  
  df <- linkages_stack |>
    select(year, sector, BL_n, FL_n) |>
    left_join(
      multipliers_stack |> select(year, sector, mult_emp),
      by = c("year","sector")
    ) |>
    filter(year %in% c(y0, y1)) |>
    mutate(
      # usamos mult_emp para size; si hay NA, imputamos la mediana para NO perder puntos
      mult_emp_size = ifelse(is.finite(mult_emp), mult_emp,
                             ifelse(is.finite(stats::median(mult_emp, na.rm = TRUE)),
                                    stats::median(mult_emp, na.rm = TRUE), 1))
    )
  
  start <- df |> filter(year == y0) |> select(sector, BL0 = BL_n, FL0 = FL_n)
  end   <- df |> filter(year == y1) |> select(sector, BL1 = BL_n, FL1 = FL_n, mult_emp)
  seg   <- inner_join(start, end, by = "sector")
  
  # selección opcional de etiquetas (top-k más “interesantes”)
  end_lab <- end
  if (!is.null(label_topk)) {
    rank_tbl <- seg |>
      mutate(dist = sqrt((BL1 - BL0)^2 + (FL1 - FL0)^2),
             imp  = replace(mult_emp, !is.finite(mult_emp), NA_real_),
             score = rank(-dist, ties.method = "first") + rank(-imp, ties.method = "first")) |>
      arrange(score) |> slice(1:label_topk) |> select(sector)
    end_lab <- end |> semi_join(rank_tbl, by = "sector")
  }
  
  p <- ggplot(df, aes(BL_n, FL_n, color = factor(year))) +
    geom_hline(yintercept = 1, linetype = 3, colour = "grey50") +
    geom_vline(xintercept = 1, linetype = 3, colour = "grey50") +
    geom_point(aes(size = mult_emp_size), alpha = 0.9) +
    geom_segment(
      data = seg,
      aes(x = BL0, y = FL0, xend = BL1, yend = FL1),
      inherit.aes = FALSE,
      arrow = arrow(length = unit(0.18, "cm"), type = "closed"),
      linewidth = 0.5, alpha = 0.7, color = "grey40"
    ) +
    ggrepel::geom_text_repel(
      data = end_lab,
      aes(x = BL1, y = FL1, label = sector),
      size = 3.2, max.overlaps = 300, seed = 123, color = "grey20",
      inherit.aes = FALSE
    ) +
    scale_year_colors() +
    scale_size_continuous(range = c(2.2, 7), guide = guide_legend(title = "Mult. empleo")) +
    labs(
      x = "Backward Linkages (BL normalizado)",
      y = "Forward Linkages (FL normalizado)",
      title = title %||% "Encadenamientos BL–FL y multiplicador de empleo",
      subtitle = subtitle %||% sprintf("%s → %s", y0, y1),
      color = "Año"
    ) +
    theme_project()
  
  p <- .apply_log_scales(p, xlog = xaxis_log, ylog = yaxis_log)
  p
}

# --------------------------------------------------------------
# 3) Comparador genérico (min→max) para UNA métrica
#    Parámetros extra:
#      - xaxis_log, yaxis_log: booleans para poner ejes log
#      - show_labels: TRUE/FALSE
#      - label_topk: si se pasa, etiqueta sólo top-k (extremos/distancia)
plot_compare_years <- function(df_stack, metric_col,
                               title = NULL, subtitle = NULL,
                               xaxis_log = TRUE, yaxis_log = TRUE,
                               show_labels = TRUE, label_topk = NULL) {
  df_stack <- df_stack |> .ensure_sector() |> .ensure_year_numeric()
  stopifnot(metric_col %in% names(df_stack))
  
  yrs <- df_stack |> distinct(year) |> arrange(year) |> pull(year)
  stopifnot(length(yrs) >= 2)
  y0 <- min(yrs); y1 <- max(yrs)
  
  wide <- df_stack |>
    filter(year %in% c(y0, y1)) |>
    select(sector, year, value = !!rlang::sym(metric_col)) |>
    tidyr::pivot_wider(names_from = year, values_from = value) |>
    rename(x = !!as.name(as.character(y0)),
           y = !!as.name(as.character(y1)))
  
  p <- ggplot(wide, aes(x, y)) +
    geom_abline(slope = 1, intercept = 0, linetype = 3, colour = "grey50") +
    geom_point(alpha = 0.9, size = 2.8, colour = project_colors$accent) +
    labs(
      x = sprintf("%s (%s)", metric_col, y0),
      y = sprintf("%s (%s)", metric_col, y1),
      title = title %||% sprintf("Comparación de %s entre años", metric_col),
      subtitle = subtitle %||% sprintf("%s → %s", y0, y1)
    ) +
    theme_project()
  
  # Etiquetas
  if (isTRUE(show_labels)) {
    lab_df <- wide
    if (!is.null(label_topk)) {
      lab_df <- wide |>
        mutate(score = abs(log10((x + 1e-9)) - log10((y + 1e-9))) +
                 abs(scale(x)) + abs(scale(y))) |>
        arrange(desc(score)) |> slice(1:label_topk)
    }
    p <- p + ggrepel::geom_text_repel(aes(label = sector), size = 3.2, max.overlaps = 300)
  }
  
  p <- .apply_log_scales(p, xlog = xaxis_log, ylog = yaxis_log)
  p
}

