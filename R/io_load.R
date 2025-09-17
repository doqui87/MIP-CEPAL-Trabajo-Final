# R/io_load.R
# ---------------------------------------------
# load_matrices(country, year, path)
# - Lee DOM (paste0(country, year, "dom.csv")) y tbl_ind.csv desde `path`
# - Devuelve:
#     * tbl_IP  : matriz NUMÉRICA con rownames = 1ª columna del CSV y colnames = header de datos
#     * tbl_ind : data.frame con metadatos sectoriales

load_matrices <- function(country, year, path) {
  suppressPackageStartupMessages({
    library(readr); library(dplyr)
  })
  
  ip_path  <- file.path(path, paste0(country, year, "dom.csv"))  # p.ej., ARG2018dom.csv
  ind_path <- file.path(path, "tbl_ind.csv")
  
  # --- leer DOM: 1ª columna = etiquetas de fila
  dom_df <- read_csv(ip_path, show_col_types = FALSE)
  
  row_labs <- dom_df[[1]] |> as.character()       # etiquetas de fila
  col_labs <- names(dom_df)[-1]                   # nombres de columnas de datos (sin la 1ª)
  
  # helper: garantizar unicidad (evita error si hay etiquetas repetidas)
  ensure_unique <- function(x) if (anyDuplicated(x)) make.unique(x) else x
  row_labs <- ensure_unique(row_labs)
  col_labs <- ensure_unique(col_labs)
  
  # convertir a matriz numérica y ASIGNAR nombres de filas/columnas
  tbl_IP <- dom_df |>
    select(-1) |>
    mutate(across(everything(), as.double)) |>
    as.matrix()
  
  storage.mode(tbl_IP) <- "double"
  
  if (length(row_labs) != nrow(tbl_IP)) {
    stop(sprintf("Las etiquetas de fila (%d) no coinciden con las filas de la matriz (%d).",
                 length(row_labs), nrow(tbl_IP)))
  }
  if (length(col_labs) != ncol(tbl_IP)) {
    stop(sprintf("Los nombres de columnas (%d) no coinciden con las columnas de la matriz (%d).",
                 length(col_labs), ncol(tbl_IP)))
  }
  
  rownames(tbl_IP) <- row_labs
  colnames(tbl_IP) <- col_labs
  
  # --- leer tabla de industrias
  tbl_ind <- read_csv(ind_path, show_col_types = FALSE) |> as.data.frame()
  
  list(
    tbl_IP  = tbl_IP,
    tbl_ind = tbl_ind
  )
}

# Carga de empleo (EMPN) y remuneración laboral (LABR) desde los .RData de TiM/OECD
# Asume que en cada .RData existen data.frames llamados:
#   - indic.EMPN
#   - indic.LABR
# Usa el vector `codes` (p.ej. tbl_ind$cod) para ordenar las filas.

load_tim_labour <- function(country, year, path, codes, unit = "USD") {
  suppressPackageStartupMessages({ library(dplyr) })
  
  # --- EMPN ---
  e1 <- new.env(parent = emptyenv())
  load(file.path(path, "EMPN.RData"), envir = e1)
  stopifnot("No se encontró 'indic.EMPN' en EMPN.RData" = exists("indic.EMPN", envir = e1, inherits = FALSE))
  df_empn <- e1$indic.EMPN
  
  l <- df_empn |>
    filter(MEASURE == "EMPN",
           REF_AREA == country,
           TIME_PERIOD == year,
           ACTIVITY %in% codes) |>
    select(ACTIVITY, OBS_VALUE) |>
    mutate(ACTIVITY = factor(ACTIVITY, levels = codes)) |>
    arrange(ACTIVITY) |>
    pull(OBS_VALUE) |>
    as.numeric() |>
    matrix(ncol = 1)
  
  
  # --- LABR ---
  e2 <- new.env(parent = emptyenv())
  load(file.path(path, "LABR.RData"), envir = e2)
  stopifnot("No se encontró 'indic.LABR' en LABR.RData" = exists("indic.LABR", envir = e2, inherits = FALSE))
  df_labr <- e2$indic.LABR
  
  rta <- df_labr |>
    filter(MEASURE == "LABR",
           REF_AREA == country,
           TIME_PERIOD == year,
           UNIT_MEASURE == unit,
           ACTIVITY %in% codes) |>
    select(ACTIVITY, OBS_VALUE) |>
    mutate(ACTIVITY = factor(ACTIVITY, levels = codes)) |>
    arrange(ACTIVITY) |>
    pull(OBS_VALUE) |>
    as.numeric() |>
    matrix(ncol=1)

  
  # salarios por trabajador
  w <- rta / l
  w[!is.finite(w)] <- 0
  
  return(list(l = l, rta = rta, w = w))
}





# --- test interno (solo si ejecuto este archivo directamente) -------------
if (sys.nframe() == 0) {
  suppressPackageStartupMessages({ library(here) })
  
  #----
  message(">> Test de load_matrices()")
  
  path <- here::here("data", "raw")
  mats <- load_matrices("ARG", 2018, path)
  
  # Exponer SOLO en este test para inspección rápida
  tbl_IP  <- mats$tbl_IP
  tbl_ind <- mats$tbl_ind
  
  cat("\nDim(tbl_IP): ", paste(dim(tbl_IP), collapse = " x "), "\n", sep = "")
  cat("Primeras filas nombradas de tbl_IP:\n"); print(head(rownames(tbl_IP)))
  cat("Primeras columnas de tbl_IP:\n");       print(head(colnames(tbl_IP)))
  cat("\nHead(tbl_ind):\n");                   print(utils::head(tbl_ind))
  
  #----
  message(">> Test load_tim_labour()")
  
  codes    <- mats$tbl_ind$cod
  
  lab <- load_tim_labour("ARG", 2018, path, codes, unit = "USD")
  
  cat("Dim(l):   ", paste(dim(lab$l),   collapse = " x "), "\n", sep = "")
  cat("Dim(rta): ", paste(dim(lab$rta), collapse = " x "), "\n", sep = "")
  cat("Head(w):\n"); print(head(lab$w))
  
  
}
