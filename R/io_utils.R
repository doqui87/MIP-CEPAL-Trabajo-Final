# R/io_utils.R
# ------------------------------------------------------------
# Funciones para embellecer las salidas
#
# API:
#   get_labels(tbl_IP, col_desc, q_ind) -> industry_labels (vector)
#   
#   fix_labels(mats, industry_labels, q_ind = NULL) -> mats (fixed labels)
# 
# Notas: fix_labels() espera que `mats` sea una lista nombrada con al menos:
#         Z, F, M, M_F, Y, f, m, y, x
# ------------------------------------------------------------


# Extraer etiquetas de industrias (desde tbl_ind) 
# get_labels(tbl_ind, col = "desc_short", q_ind = NULL)
# - Extrae un vector character de etiquetas desde tbl_ind[[col]].
# - Limpia espacios, resuelve NAs/vacíos con un fallback simple.
# - Garantiza unicidad (make.unique).
# - Si se pasa q_ind, verifica que length(labs) == q_ind.

get_labels <- function(tbl_ind, col = "desc_short", q_ind = NULL) {
  # checks mínimos y claros
  if (!is.data.frame(tbl_ind)) stop("tbl_ind debe ser un data.frame.", call. = FALSE)
  if (!col %in% names(tbl_ind)) {
    stop(sprintf("La columna '%s' no existe en tbl_ind.", col), call. = FALSE)
  }
  
  labs <- as.character(tbl_ind[[col]])
  labs <- trimws(labs)
  
  # reemplazo de NAs o vacíos por un fallback neutro
  if (any(is.na(labs) | labs == "")) {
    idx <- which(is.na(labs) | labs == "")
    labs[idx] <- paste0("Sector_", seq_along(idx))
  }
  
  # asegurar unicidad (evita choques al setear dimnames)
  if (anyDuplicated(labs)) labs <- make.unique(labs)
  
  # si se especifica q_ind, validar longitud
  if (!is.null(q_ind)) {
    q_ind <- as.integer(q_ind)
    if (!(length(labs) == q_ind)) {
      stop(sprintf("length(labs)=%d debe igualar q_ind=%d.", length(labs), q_ind), call. = FALSE)
    }
  }
  
  return(labs)
}




# Ajuste de labels sobre todas las matrices ------------------
# fix_labels(mats, industry_labels, q_ind = NULL)
# - Asigna etiquetas de industrias a las piezas de `mats`.
# - `q_ind` es opcional; si no se provee, se infiere de nrow(Z).
#
# Espera que `mats` sea una lista nombrada con al menos:
#   Z, F, M, M_F, Y, f, m, y, x
# donde:
#   Z: q_ind x q_ind
#   F: q_ind x q_f
#   M: q_ind x q_ind
#   M_F: q_ind x q_f
#   Y: 3 x q_ind
#   f, m, y, x: q_ind x 1
# Las dimensiones q_ind reciben las etiquetas

fix_labels <- function(mats, industry_labels, q_ind = NULL) {
  stopifnot(is.list(mats), length(industry_labels) > 0)
  
  # Inferir q_ind si no se pasa
  if (is.null(q_ind)) {
    stopifnot("Z no está en `mats` o no es matriz" = is.matrix(mats$Z))
    q_ind <- nrow(mats$Z)
  }
  
  # Z: filas/columnas = industrias
  mats$Z <- .dimn(mats$Z, r = industry_labels, c = industry_labels)
  
  # F: filas = industrias; columnas = demanda final (conservar colnames existentes)
  mats$F <- .dimn(mats$F, r = industry_labels, c = NULL)
  
  # M: filas/columnas = industrias
  mats$M <- .dimn(mats$M, r = industry_labels, c = industry_labels)
  
  # M_F: filas = industrias, columnas = demanda final (conservar colnames existentes)
  mats$M_F <- .dimn(mats$M_F, r = industry_labels, c = NULL)
  
  # Y: columnas = industrias (filas: 3 renglones de insumos primarios)
  mats$Y <- .dimn(mats$Y, r = NULL, c = industry_labels)
  
  # Vectores por industria
  rownames(mats$f)   <- industry_labels
  rownames(mats$m)   <- industry_labels
  rownames(mats$y)   <- industry_labels
  rownames(mats$x)   <- industry_labels
  
  return(mats)
}


`%||%` <- function(a, b) {
  if (is.null(a)) return(b)
  if (length(a) == 1L && is.na(a)) return(b)
  a
}