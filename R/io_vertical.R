# R/io_vertical.R
# ------------------------------------------------------------
# Coeficientes y agregados de INTEGRACIÓN VERTICAL (VI)
# Versión OPEN: usa B y f
# Versión CLOSED: usa B_cl y f_z
#
# Entradas esperadas:
#   Para OPEN  : mats$B (q x q), mats$f (q x 1), mats$a_l (qx1 opc.), mats$a_m (qx1)
#                 opcional: mats$F (q x q_f), mats$m (q x 1) para BC
#   Para CLOSED: mats$B_cl (q x q), mats$f_z (q x 1), mats$a_l (qx1 opc.), mats$a_m (qx1)
#                 opcional: mats$F (q x q_f), mats$m (q x 1) para BC VI
#
# Salidas:
#   emp_vi  : empleo VI por industria (q x 1)  → (a_l' B diag(f))'
#   imp_vi  : import VI por industria (q x 1)  → (a_m' B diag(f))'
#   bc_vi   : tabla con Balance Comercial estándar y VI (si F y m están disponibles)
#   Nota: En CLOSED se emplean B_cl y f_z en lugar de B y f.
#
# Uso típico:
#   open_vi   <- get_vertical_open(mats, expo_col = "EXPO")
#   closed_vi <- get_vertical_closed(mats, expo_col = "EXPO")
# ------------------------------------------------------------

# --- helpers internos (no exportar) ---
.vi_vector <- function(a_vec, B, f) {
  stopifnot(is.matrix(a_vec), ncol(a_vec) == 1, is.matrix(B), is.matrix(f), ncol(f) == 1)
  (t(a_vec) %*% B %*% diag(as.vector(f))) |> t()
}

.bc_table <- function(F, m, imp_vi, expo_col, rn = NULL) {
  stopifnot(is.matrix(F), is.matrix(m), is.matrix(imp_vi))
  stopifnot(expo_col %in% colnames(F))
  BC_est <- as.matrix(F[, expo_col, drop = TRUE]) - m
  BC_vi  <- as.matrix(F[, expo_col, drop = TRUE]) - imp_vi
  out <- cbind(`Balance Comercial` = BC_est,
               `Balance Comercial VI` = BC_vi,
               `Diferencia` = BC_est - BC_vi)
  if (!is.null(rn)) rownames(out) <- rn
  out
}

# 1) Integración vertical — MODELO ABIERTO ---------------------
get_vertical_open <- function(mats, expo_col = "EXPO") {
  # mínimos obligatorios
  B   <- mats$B
  f   <- mats$f
  a_m <- mats$a_m
  a_l <- mats$a_l %||% NULL
  
  stopifnot(
    "B debe ser cuadrada" = is.matrix(B) && nrow(B) == ncol(B),
    "f debe ser qx1"      = is.matrix(f) && ncol(f) == 1 && nrow(f) == nrow(B),
    "a_m debe ser qx1"    = is.matrix(a_m) && all(dim(a_m) == c(nrow(B), 1))
  )
  if (!is.null(a_l)) stopifnot("a_l debe ser qx1" = all(dim(a_l) == c(nrow(B), 1)))
  
  rn <- rownames(B)
  
  # coeficientes VI
  imp_vi <- .vi_vector(a_m, B, f); rownames(imp_vi) <- rn
  emp_vi <- if (!is.null(a_l)) { x <- .vi_vector(a_l, B, f); rownames(x) <- rn; x } else NULL
  
  # BC vertical (si hay F y m)
  bc_vi <- NULL
  if (all(c("F","m") %in% names(mats)) && is.matrix(mats$F) && is.matrix(mats$m)) {
    bc_vi <- .bc_table(mats$F, mats$m, imp_vi, expo_col, rn = rn)
  }
  
  list(
    emp_vi = emp_vi,
    imp_vi = imp_vi,
    bc_vi  = bc_vi
  )
}

# 2) Integración vertical — MODELO CERRADO ---------------------
get_vertical_closed <- function(mats, expo_col = "EXPO") {
  # mínimos obligatorios
  B_cl <- mats$B_cl
  f_z  <- mats$f_z
  a_m  <- mats$a_m
  a_l  <- mats$a_l %||% NULL
  
  stopifnot(
    "B_cl debe ser cuadrada" = is.matrix(B_cl) && nrow(B_cl) == ncol(B_cl),
    "f_z debe ser qx1"       = is.matrix(f_z)  && ncol(f_z)  == 1 && nrow(f_z)  == nrow(B_cl),
    "a_m debe ser qx1"       = is.matrix(a_m)  && all(dim(a_m) == c(nrow(B_cl), 1))
  )
  if (!is.null(a_l)) stopifnot("a_l debe ser qx1" = all(dim(a_l) == c(nrow(B_cl), 1)))
  
  rn <- rownames(B_cl)
  
  # coeficientes VI (cerrado)
  imp_vi <- .vi_vector(a_m, B_cl, f_z); rownames(imp_vi) <- rn
  emp_vi <- if (!is.null(a_l)) { x <- .vi_vector(a_l, B_cl, f_z); rownames(x) <- rn; x } else NULL
  
  # BC vertical (si hay F y m — mismo F y m que en abierto; el cambio es en el multiplicador)
  bc_vi <- NULL
  if (all(c("F","m") %in% names(mats)) && is.matrix(mats$F) && is.matrix(mats$m)) {
    bc_vi <- .bc_table(mats$F, mats$m, imp_vi, expo_col, rn = rn)
  }
  
  list(
    emp_vi = emp_vi,
    imp_vi = imp_vi,
    bc_vi  = bc_vi
  )
}

# --- Smoke test mínimo (solo si ejecutás este archivo directo) ----------
if (sys.nframe() == 0) {
  message(">> Smoke test io_vertical")
  q <- 4; set.seed(1)
  B    <- matrix(runif(q*q, 0, .5), q, q); diag(B) <- diag(B) + 1  # invertible-ish
  B_cl <- matrix(runif(q*q, 0, .5), q, q); diag(B_cl) <- diag(B_cl) + 1
  f    <- matrix(runif(q, 10, 20), q, 1)
  f_z  <- matrix(runif(q,  8, 15), q, 1)
  a_m  <- matrix(runif(q,  0, .2), q, 1)
  a_l  <- matrix(runif(q,  0, .1), q, 1)
  F    <- cbind(HFCE = runif(q, 5, 10), EXPO = runif(q, 3, 8))
  m    <- matrix(runif(q, 0, 5), q, 1)
  
  mats <- list(B=B, f=f, a_m=a_m, a_l=a_l, F=F, m=m)
  open <- get_vertical_open(mats, expo_col = "EXPO")
  
  mats2 <- list(B_cl=B_cl, f_z=f_z, a_m=a_m, a_l=a_l, F=F, m=m)
  closed <- get_vertical_closed(mats2, expo_col = "EXPO")
  
  stopifnot(is.matrix(open$imp_vi), is.matrix(closed$imp_vi))
  message("OK.")
}
