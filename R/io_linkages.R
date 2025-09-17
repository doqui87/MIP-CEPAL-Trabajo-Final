# R/io_linkages.R
# ------------------------------------------------------------
#
# compute_linkages -> Encadenamientos hacia atrás (BL) y hacia adelante (FL)
#   - BL  (Rasmussen):   BL = 1' B        → suma por columnas de B (q x 1)
#   - FL  (Ghosh):       FL = G 1         → suma por filas de G     (q x 1)
#   Normalización clásica (Hirschman-Rasmussen):
#     BL_n = BL / mean(BL),   FL_n = FL / mean(FL)
#
# Nota: si sólo pasás B y NO G, FL no se calcula; idem a la inversa.
#
# -----
#
# add_taxonomy -> Taxonomía BL/FL (Hirschman-Rasmussen)
#   Clasifica sectores según BL_n y FL_n con umbral (default 1):
#   - "Clave"         : BL_n > thr & FL_n > thr
#   - "Impulsor"      : BL_n > thr & FL_n <= thr
#   - "Impulsado"     : BL_n <= thr & FL_n > thr
#   - "Independiente" : BL_n <= thr & FL_n <= thr
#   - "Otros"         : casos con NA
#
# -----
#
# API principal:
#   compute_linkages(B = NULL, G = NULL, sectors = NULL)
#     - B: Inversa de Leontief.
#     - G: Matriz de Gosh.
#     - sectors: vector de nombres (desc_short), opcional.
#     Devuelve tibble con: sector, BL, FL, BL_n, FL_n
#
#   add_taxonomy(link_tbl, bl_col="BL_n", fl_col="FL_n", thr=1, as_factor=TRUE)
#     link_tbl: tibble de compute_linkages()
#     retorna el mismo tibble con columna adicional `type`
# ------------------------------------------------------------


suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
})

# --- compute_linkages --------------------------------------------------------
compute_linkages <- function(B = NULL, G = NULL, sectors = NULL) {
  # Validaciones suaves: al menos una de (B, G) debe venir
  if (is.null(B) && is.null(G)) {
    stop("Proveé B, o G para calcular los encadenamientos.")
  }
  
  q <- NULL
  
  # BL a partir de B
  BL <- NULL
  if (!is.null(B)) {
    stopifnot(is.matrix(B), nrow(B) == ncol(B))
    q <- nrow(B)
    u <- matrix(1, q, 1)
    BL <- t(u) %*% B |> t()         # q x 1 (suma por columnas de B)
  }
  
  # FL a partir de G
  FL <- NULL
  if (!is.null(G)) {
    stopifnot(is.matrix(G), nrow(G) == ncol(G))
    if (is.null(q)) q <- nrow(G) else stopifnot(nrow(G) == q)
    u <- matrix(1, q, 1)
    FL <- G %*% u                   # q x 1 (suma por filas de G)
  } 
  
  # Asegurar q
  if (is.null(q)) {
    # Si sólo vino G o sólo vino B lo establecimos arriba; si no, marcá error
    stop("No se pudo inferir la dimensión q.")
  }
  
  # Construcción del tibble
  df <- tibble(
    sector = if (!is.null(sectors) && length(sectors) == q) sectors else as.character(seq_len(q)),
    BL = if (!is.null(BL)) as.numeric(BL) else NA_real_,
    FL = if (!is.null(FL)) as.numeric(FL) else NA_real_
  )
  
  # Normalizaciones (evitar división por 0)
  df <- df |>
    mutate(
      BL_n = if (all(is.na(BL))) NA_real_ else BL / mean(BL, na.rm = TRUE),
      FL_n = if (all(is.na(FL))) NA_real_ else FL / mean(FL, na.rm = TRUE)
    )
  
  df
}

# --- taxonomy ------------------------------------------------------------
add_taxonomy <- function(link_tbl,
                     bl_col = "BL_n",
                     fl_col = "FL_n",
                     thr = 1,
                     as_factor = TRUE) {
  stopifnot(is.data.frame(link_tbl),
            bl_col %in% names(link_tbl),
            fl_col %in% names(link_tbl),
            is.numeric(thr), length(thr) == 1)
  
  BLn <- link_tbl[[bl_col]]
  FLn <- link_tbl[[fl_col]]
  
  type <- dplyr::case_when(
    is.na(BLn) | is.na(FLn)              ~ "Otros",
    BLn >  thr & FLn >  thr              ~ "Clave",
    BLn >  thr & FLn <= thr              ~ "Impulsor",
    BLn <= thr & FLn >  thr              ~ "Impulsado",
    BLn <= thr & FLn <= thr              ~ "Independiente",
    TRUE                                 ~ "Otros"
  )
  
  if (as_factor) {
    type <- factor(type,
                   levels = c("Clave", "Impulsor", "Impulsado", "Independiente", "Otros"),
                   ordered = TRUE)
  }
  
  dplyr::mutate(link_tbl, type = type)
}

# --- Wrapper conjunto ------------------------------------------------------------
# Conveniencia: computar encadenamientos y taxonomía en una sola llamada
compute_linkages_with_taxonomy <- function(B = NULL, G = NULL,
                                           sectors = NULL,
                                           thr = 1) {
  tbl <- compute_linkages(B = B, G = G, sectors = sectors)
  add_taxonomy(tbl, thr = thr)
}

# --- Test mínimo (ejecuta sólo si corrés este archivo directamente) -------
if (sys.nframe() == 0) {
  
  #---- compute_linkages
  message(">> Smoke test compute_linkages()")
  set.seed(42); q <- 4
  # Armar A tal que (I-A) invertible
  A <- matrix(runif(q*q, 0, 0.2), q, q); diag(A) <- 0
  B <- solve(diag(q) - A)
  G <- B
  
  sectors <- paste("Sector", 1:q)
  lk <- compute_linkages(B = B,G = G, sectors = sectors)
  
  stopifnot(all(c("sector","BL","FL","BL_n","FL_n") %in% names(lk)))
  print(lk)
  message("OK.")
  
  
  #---- taxonomy
  message(">> Smoke test taxonomy()")
  
  lk_tax <- add_taxonomy(lk)
  
  stopifnot("type" %in% names(lk_tax),
            all(!is.na(lk_tax$type)))
  print(lk_tax)
  message("OK taxonomy.")
}
