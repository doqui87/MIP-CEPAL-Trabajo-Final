# R/io_tables.R  — versión "stacked" (apilada por año)
# ============================================================
# Objetivo:
#   Este módulo provee funciones para transformar la salida de
#   `io_orchestrator::build_year_objects()` (resultados por país–año) 
#   en data.frames apilados ("stacked") que facilitan el post-procesamiento,
#   exportación y graficado.
#
# Lógica principal:
#   - A partir de una lista de resultados (uno por año), se generan
#     tablas apilada donde cada fila corresponde a un par
#     sector–año.
#
# Funciones expuestas:
#
#   make_stacked_tables(results_list)
#     * Input:
#         - results_list: lista de objetos construidos por
#           `build_year_objects()`, típicamente un elemento por año.
#     * Output:
#         - list con tres data.frames:
#             - linkages_stack:
#                 columnas: year, sector, BL, FL, BL_n, FL_n, type
#                 (encadenamientos hacia atrás/delante y su tipología).
#             - multipliers_stack:
#                 columnas: year, sector, mult_prod, mult_imp, mult_emp
#                 (multiplicadores de producción, importaciones y empleo).
#             - vi_stack:
#                 columnas: year, sector, imp_vi, emp_vi
#                 (coeficientes verticalmente integrados de importaciones y empleo).
#
# Uso típico:
#   results <- list(
#     "1995" = build_year_objects("ARG", 1995, "data/raw"),
#     "2018" = build_year_objects("ARG", 2018, "data/raw")
#   )
#   stacked <- make_stacked_tables(results)
#   head(stacked$multipliers_stack)
#
# De este modo, las tablas quedan listas para ser exportadas
# (via io_export.R) o utilizadas directamente en gráficos
# (via io_plots.R).
# ============================================================


suppressPackageStartupMessages({
  library(dplyr); library(purrr)
})




# Tablas apiladas (stacked) por año: una fila por sector–año
make_stacked_tables <- function(results_list) {
  
  # Encadenamientos apilados
  linkages_stack <- results_list|>                      # Consumir lista de resultados
    map_dfr(function(r) {                               # Mapear a una custom_function
      r$links |>                                        # Extraer los linkages
        mutate(year = r$year) |>                        # Agregar columna year (metadata)
        select(year, sector, BL, FL, BL_n, FL_n, type)  # Select final
  })
  
  # Multiplicadores apilados (columnas)
  multipliers_stack <- results_list |>
    map_dfr(function(r) {
      tibble::tibble(
        year      = r$year,
        sector    = r$ind_labs,
        mult_prod = as.numeric(r$mult$mult_prod),
        mult_imp  = as.numeric(r$mult$mult_imp),
        mult_emp  = if (!is.null(r$mult$mult_emp)) as.numeric(r$mult$mult_emp) else NA_real_
    )
  })
  
  # Integración vertical apilada (columnas)
  vi_stack <- results_list |>
    map_dfr(function(r) {
      tibble::tibble(
        year   = r$year,
        sector = r$ind_labs,
        imp_vi = as.numeric(r$vert$imp_vi),
        emp_vi = if (!is.null(r$vert$emp_vi)) as.numeric(r$vert$emp_vi) else NA_real_
      )
  })
  
  list(
    linkages_stack    = linkages_stack,
    multipliers_stack = multipliers_stack,
    vi_stack          = vi_stack
  )
}
