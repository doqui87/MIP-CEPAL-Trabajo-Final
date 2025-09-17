# R/io_orchestrator.R
# ------------------------------------------------------------
# Orquesta la ingesta y el procesamiento por país–año con las
# funciones ya definidas en el proyecto. El objetivo es dejar,
# para cada (country, year), un paquete consistente de:
#   - mats : objetos de sistema (Z, F, M, M_f, Y, f, m, m_f, y, x)
#            + coeficientes intensivos mínimos para modelo abierto
#            (A, B, a_m, a_l, l)
#   - mult : multiplicadores abiertos (prod, impo, empleo)
#   - vert : integración vertical (emp_vi, imp_vi, bc_vi)
#   - links: encadenamientos BL/FL (absolutos y normalizados; y su taxonomía)
#
# Usa la API existente en este proyecto:
#   load_matrices, partition_MIP, get_labels, fix_labels,
#   load_tim_labour, get_multipliers_open, get_vertical_open,
#   compute_linkages, taxonomy
#
# Firma principal:
#   build_country_year_objects(country, year, paths_raw, q_ind = 45, q_f = 9)
#     - Devuelve: list(year, labs, mats, mult, vert, links)
#
# Helpers opcionales al final:
#   build_many(country, years, paths_raw)  → lista con un elemento por año
#
# Test de integración (smoke test):
#   Se ejecuta sólo si este archivo se corre directamente:
#     Rscript R/io_orchestrator.R
#   Requiere que los módulos hayan pasado sus tests unitarios.
# ------------------------------------------------------------



# ------------------------------------------------------------
# 1. Orquestador por (country, year)
# ------------------------------------------------------------
build_country_year_objects <- function(country, year, paths_raw, q_ind=45L, q_f=9L) {
  
  # 1) Carga MIP y trabajo/ingresos (TIM)
  # MIP
  io    <- load_matrices(country, year, paths_raw) 
  # w, rta, l
  lab   <- load_tim_labour(country, year, paths_raw, codes = io$tbl_ind$cod, unit = "USD") 
  
  # 2) Etiquetas de industrias
  ind_labs  <- get_labels(io$tbl_ind, col = "desc_short", q_ind = q_ind)
  
  # 3) Particionar MIP y embellecer etiquetas de industrias
  # Z, F, M, M_F, Y, f, m, m_f, y, x
  mats  <- io$tbl_IP|>
    partition_MIP(q_ind, q_f) |>
    fix_labels(ind_labs)
  
  # 3.5) Agregar mats$l de tim_labour
  mats$l <- lab$l
  
  # 4) Agregar coeficientes intensivos (abierto)
  #     A, B, D, G, a_m, a_y, a_l
  mats <- add_intensive_coefficients(mats)   
  
  # 5) multiplicadores modelo  abierto (abierto)
  #     mult_prod, mult_imp, mult_emp
  mult  <- get_multipliers_open(mats) 
  
  # 4) integración vertical modelo abierto (abierto)
  #     imp_vi, emp_vi, bc_vi
  vert  <- get_vertical_open(mats, expo_col = "EXPO")  
  
  # 5) Encadenamientos y taxonomia
  #   BL, FL, BL_n, FL_n y type
  links <- compute_linkages(
      B = mats$B, G = mats$G, 
      sectors = ind_labs
    ) |> 
    add_taxonomy(thr = 1)
  
  # 6) Empaquetar
  list(
    year = year,
    ind_labs = ind_labs,
    mats = mats, 
    mult = mult, 
    vert = vert, 
    links = links
    )
}

# ------------------------------------------------------------
# 2) Conveniencia: construir para múltiples años
# ------------------------------------------------------------
build_many <- function(country, years, paths_raw, q_ind, q_f) {
  out <- lapply(years, function(yr) build_country_year_objects(country, yr, paths_raw, q_ind=45, q_f=9))
  names(out) <- as.character(years)
  out
}


# ------------------------------------------------------------
# 3) Orquestación completa: tablas apiladas + plots + export
# ------------------------------------------------------------
run_orchestration <- function(country, years,
                              paths = list(
                                root   = getwd(),
                                raw    = file.path(getwd(), "data", "raw"),
                                tables = file.path(getwd(), "results", "tables"),
                                figs   = file.path(getwd(), "results", "figs")
                                ),
                              q_ind = NULL, q_f = NULL,
                              print_plots = FALSE,
                              save_plots = TRUE
                              ) {
  # 3.1 Construcción por año
  results <- build_many(country, years, paths$raw, q_ind, q_f)
  
  # 3.2 Tablas apiladas (stacked)
  stacked <- make_stacked_tables(results)
  linkages_stack    <- stacked$linkages_stack
  multipliers_stack <- stacked$multipliers_stack
  vi_stack          <- stacked$vi_stack
  
  # 3.3 Export CSV
  dir.create(paths$tables, recursive = TRUE, showWarnings = FALSE)
  export_csv(linkages_stack,    file.path(paths$tables, sprintf("linkages_stacked_%s_%s_%s.csv", country, min(years), max(years))))
  export_csv(multipliers_stack, file.path(paths$tables, sprintf("multipliers_open_stacked_%s_%s_%s.csv", country, min(years), max(years))))
  export_csv(vi_stack,          file.path(paths$tables, sprintf("vi_stacked_%s_%s_%s.csv", country, min(years), max(years))))
  

  # 3.4 Gráficos (infieren años min/max desde las tablas)
  dir.create(paths$figs, recursive = TRUE, showWarnings = FALSE)
  set_project_theme(base_size = 13)  # o 14 si es para slides
  
  # Linkages
  g_link <- plot_compare_linkages(
    linkages_stack, multipliers_stack,
    title = "Encadenamientos BL–FL y multiplicador de empleo"
  )
  if (isTRUE(print_plots)) print(g_link)
  if (isTRUE(save_plots))
    export_plot(g_link, file.path(paths$figs, sprintf("BL_FL_scatter_%s_%s_%s.png", country, min(years), max(years))), width = 10, height = 7)
  
  # Mult importaciones
  g_imp <- plot_compare_years(
    multipliers_stack, "mult_imp",
    title = "Multiplicador de importaciones — comparación entre años"
  )
  if (isTRUE(print_plots)) print(g_imp)
  export_plot(g_imp, file.path(paths$figs, sprintf("mult_imp_compare_%s_%s_%s.png", country, min(years), max(years))))
  
  # Multiplicadores empleo
  g_emp <- plot_compare_years(
    multipliers_stack, "mult_emp",
    xaxis_log = TRUE, yaxis_log = TRUE,
    title = "Multiplicador de empleo — comparación entre años"
  )
  if (isTRUE(print_plots)) print(g_emp)
  if (isTRUE(save_plots)) export_plot(g_emp, file.path(paths$figs, sprintf("mult_emp_compare_%s_%s_%s.png", country, min(years), max(years))))
  
  # VI importaciones comparado
  g_vi_imp <- plot_compare_years(
    vi_stack, "imp_vi",
    xaxis_log = TRUE, yaxis_log = TRUE,
    title = "Importaciones verticalmente integradas — comparación entre años"
  )
  if (isTRUE(print_plots)) print(g_vi_imp)
  if (isTRUE(save_plots)) export_plot(g_vi_imp, file.path(paths$figs, sprintf("imp_vi_compare_%s_%s_%s.png", country, min(years), max(years))))
  
  invisible(list(
    results = results,
    linkages_stack = linkages_stack,
    multipliers_stack = multipliers_stack,
    vi_stack = vi_stack,
    plots = list(                 # <- NUEVO: devolvé los ggplot
      blfl   = g_link,
      imp    = g_imp,
      emp    = g_emp,
      vi_imp = g_vi_imp
    )
  )) 
}



# ------------------------------------------------------------
# Test de integración (smoke test)
# ------------------------------------------------------------
if (sys.nframe() == 0) {
  
  SRC_DIR  <- here::here('R')                          # carpeta donde está este .R
  ROOT_DIR <- dirname(SRC_DIR)                      # raíz del proyecto (asumiendo ./R)
  PATH_RAW <- file.path(ROOT_DIR, "data", "raw")
  mod_files <- c("io_utils.R","io_load.R",
                 "io_build.R","io_multipliers.R","io_linkages.R","io_vertical.R",
                 "io_tables.R","io_plots.R","io_export.R"
                 )
  for (f in mod_files) sys.source(file.path(SRC_DIR, f), envir = environment())
  
  
  suppressPackageStartupMessages({
    library(here); library(purrr)
  })
  
  # ------------------------------------------------------------
  message(">> Smoke test io_orchestrator")
  
  country   <- "ARG"
  years     <- c(1995L, 2018L)
  paths     <- list(
    root   = ROOT_DIR,
    raw    = file.path(ROOT_DIR, "data", "raw"),
    tables = file.path(ROOT_DIR, "results", "tables"),
    figs   = file.path(ROOT_DIR, "results", "figs")
  )
  
  out <- run_orchestration(country, years, paths)
  
  # Resumen mínimo
  s_link <- out$linkages_stack |> dplyr::count(year)
  s_mult <- out$multipliers_stack |> dplyr::summarise(
    years = paste0(min(year), "–", max(year)),
    nrows = dplyr::n(),
    any_emp = any(!is.na(mult_emp))
  )
  print(s_link); print(s_mult)
  
  # Chequeos adicionales
  
  res <- out$results
  stopifnot(
    length(res) == length(years),
    all(vapply(res, function(r) is.matrix(r$mats$B),     logical(1))),
    all(vapply(res, function(r) is.matrix(r$mult$mult_prod), logical(1))),
    all(vapply(res, function(r) is.data.frame(r$links),  logical(1)))
  )
  
  for (yr in names(res)) {
    r <- res[[yr]]
    cat("\n--", yr, "--\n",
        "Sectores:", nrow(r$mats$Z), "\n",
        "BL mean:", mean(r$links$BL, na.rm = TRUE),
        "FL mean:", mean(r$links$FL, na.rm = TRUE), "\n",
        "mult_prod range:", paste(range(as.numeric(r$mult$mult_prod), na.rm = TRUE), collapse = " - "), "\n",
        sep = "")
    if (!is.null(r$mult$mult_emp)) {
      cat("mult_emp range:", paste(range(as.numeric(r$mult$mult_emp), na.rm = TRUE), collapse = " - "), "\n")
    }
  }
  message("OK io_orchestrator.")
}



