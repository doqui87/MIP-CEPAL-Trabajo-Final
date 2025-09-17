# R/io_build.R
# ------------------------------------------------------------
# Funciones para particionar el DOM y construir los sistemas
# de gasto e ingreso (con ajustes contables mínimos).
#
# API:
#   partition_MIP(tbl_IP, q_ind, q_f) -> mats (lista nombrada)
#   get_intensive_coefficients(mats)    -> intensive (lista nombrada)
#
# Notas:
# - Se asume que tbl_IP ya viene con rownames/colnames asignados (io_load.R).
# - q_ind: número de industrias; q_f: número de componentes de demanda final.
# - Convención de nombres en la lista `mats`:
#     Z, F, M, M_F, Y, f, m, m_f, y, x
# - intensive contiene:
#      A, B, D, G, inv_x, a_m, a_y, a_l
# ------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
})

# Utilitarios internos -----------------------------------------

.inv_safe <- function(x) {
  z <- 1 / x
  z[is.nan(z) | is.infinite(z)] <- 0
  z
}

.dimn <- function(mat, r = NULL, c = NULL) {
  if (!is.null(r)) rownames(mat) <- r
  if (!is.null(c)) colnames(mat) <- c
  mat
}

`%||%` <- function(a,b) if (is.null(a)) b else a

.must <- function(cond, msg) if (!isTRUE(cond)) stop(msg, call. = FALSE)


  
  

# 1) Partición del DOM en bloques 
# --------------------------------------------------------------
partition_MIP <- function(tbl_IP, q_ind, q_f) {
  
  # --- validaciones mínimas, sin empantanar ---
  .must(is.matrix(tbl_IP), "tbl_IP debe ser una matriz.")
  q_ind <- as.integer(q_ind); q_f <- as.integer(q_f)
  .must(q_ind > 0 && q_f > 0, "q_ind y q_f deben ser enteros positivos.")

  # --- Índices de filas/columnas (layout TiM/OECD) ---
  ind_rows <- seq.int(1L, q_ind)                  # industrias (Z)
  imp_rows <- seq.int(q_ind + 1L, 2L * q_ind)     # importaciones (M, M_F)
  
  txs_imp_row <- 2L * q_ind + 1L                  # TXS_IMP_FNL
  txs_dom_row <- 2L * q_ind + 2L                  # TXS_INT_FNL
  # ttl_tax_row <- 2L * q_ind + 3L                # TTL_INT_FNL (NO usar en Y)
  valu_row    <- 2L * q_ind + 4L                  # VALU
  vbp_row     <- 2L * q_ind + 5L                  # OUTPUT
  
  va_rows <- c(txs_imp_row, txs_dom_row, valu_row)
  
  ind_cols <- ind_rows
  fd_cols <- seq.int(q_ind + 1L, q_ind + q_f)
  
  # --- Extracción de bloques ---
  Z   <- tbl_IP[ind_rows, ind_cols, drop = FALSE]
  F   <- tbl_IP[ind_rows, fd_cols,  drop = FALSE]
  M   <- tbl_IP[imp_rows, ind_cols, drop = FALSE]
  M_F <- tbl_IP[imp_rows, fd_cols,  drop = FALSE]
  Y   <- tbl_IP[va_rows,  ind_cols, drop = FALSE]
  
  # --- Vectores agregados (columna) ---
  m   <- matrix(colSums(M),   ncol = 1)  # q_ind x 1
  m_f <- matrix(colSums(M_F), ncol = 1)  # q_f   x 1  (suma por componentes de DF)
  y   <- matrix(colSums(Y),   ncol = 1)  # q_ind x 1
  x   <- matrix(tbl_IP[vbp_row, ind_cols], ncol = 1)   # q_ind x 1
  f   <- matrix(rowSums(F),   ncol = 1)  # q_ind x 1  (DF total por industria)
  
  # --- Chequeos de dimensiones coherentes ---
  .must(nrow(Z) == q_ind && ncol(Z) == q_ind,  "Z debe ser q_ind x q_ind.")
  .must(nrow(F) == q_ind && ncol(F) == q_f,    "F debe ser q_ind x q_f.")
  .must(nrow(M) == q_ind && ncol(M) == q_ind,  "M debe ser q_ind x q_ind.")
  .must(nrow(M_F) == q_ind && ncol(M_F) == q_f,"M_F debe ser q_ind x q_f.")
  .must(nrow(Y) == 3 && ncol(Y) == q_ind,      "Y debe ser 3 x q_ind.")
  .must(length(f)   == q_ind,                  "f debe tener longitud q_ind.")
  .must(length(m)   == q_ind,                  "m debe tener longitud q_ind.")
  .must(length(y)   == q_ind,                  "y debe tener longitud q_ind.")
  .must(length(x)   == q_ind,                  "x debe tener longitud q_ind.")
  .must(length(m_f) == q_f,                    "m_f debe tener longitud q_f.")
  
 
  # --- Cerrar sistemas ---
  income_gap <- x-colSums(Z)-m-y
  y_adj <- y + income_gap
  
  expenditure_gap <- x - rowSums(Z) - f
  f_adj <- f + expenditure_gap
  
  # --- Preparar lista de matrices para el return ---
  mats <- list(
    Z = Z, F = F, M = M, M_F = M_F, Y = Y,
    f = f_adj, m = m, m_f = m_f, y = y_adj, x = x
  )
  return(mats)
}





# 2) Sistema de gasto en términos intensivos (modelo abierto) 
# ------------------------------------------------------------
# Cálculo de coeficientes “intensivos” y matrices básicas
# a partir de `mats` (bloques del DOM ya particionados).
#
# Requisitos mínimos en `mats`:
#   Z, x, m, y           (si faltan, la función falla)
#   l (opcional), w (opcional)
#
# Agrega a `mats` las siguientes matrices (si ya estaban, sobreescribe):
#   A, B                 # tecnología Leontief
#   D, G                 # Ghosh (lado oferta)
#   inv_x                # 1 ./ x   (con 0 donde x == 0)
#   a_m, a_y             # coef. importado y VA por unidad de producto
#   a_l (opcional)       # coef. empleo por unidad (si viene l)

add_intensive_coefficients <- function(mats) {
  # --- Unpack y validaciones mínimas ---
  Z <- mats$Z; x <- mats$x; m <- mats$m; y <- mats$y
  stopifnot(is.matrix(Z), is.matrix(x), is.matrix(m), is.matrix(y))
  q_ind <- nrow(Z)
  stopifnot(ncol(Z) == q_ind, all(dim(x) == c(q_ind,1)),
            all(dim(m) == c(q_ind,1)), all(dim(y) == c(q_ind,1)))
  
  l <- mats$l %||% NULL
  
  # --- Básicos: inv_x, A, B, D, G ---
  I  <- diag(q_ind)
  inv_x <- .inv_safe(x)
  
  # A = Z diag(x)^{-1};  B = (I - A)^{-1}
  A <- Z %*% diag(as.vector(inv_x))
  B <- solve(I - A)
  
  # D = diag(x)^{-1} Z;  G = (I - D)^{-1}
  D <- diag(as.vector(inv_x)) %*% Z
  G <- solve(I - D)
  
  # --- Coeficientes unitarios siempre disponibles ---
  a_m <- m * inv_x                  # importado por unidad
  a_y <- y * inv_x                  # VA por unidad
  
  # --- a_l (opcional) ---
  a_l <- NULL
  if (!is.null(l)) {
    stopifnot(all(dim(l) == c(q_ind,1)))
    a_l <- l * inv_x               # empleo por unidad
    mats$a_l <- a_l
  }
  
  mats$A <- A; mats$B <- B
  mats$D <- D; mats$G <- G
  mats$inv_x <- inv_x
  mats$a_m <- a_m; mats$a_y <- a_y
  
  return(mats)
}

