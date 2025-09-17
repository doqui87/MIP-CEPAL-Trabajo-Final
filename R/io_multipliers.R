
# R/io_multipliers.R
# ------------------------------------------------------------
# API “pura”: calcula multiplicadores suponiendo que YA tenés:
# mats = list(Z, F, f, m, y, x)  con dimensiones consistentes
# l, w (si vas a usar empleo/consumo)
#
# Funciones:
#  - get_multipliers_open(mats, l = NULL)         -> list(mult_prod, mult_emp, mult_imp)
#  - get_multipliers_closed(mats, w, cons_col)    -> list(B_cl, mult_prod_cl, mult_emp_cl)

suppressPackageStartupMessages({ library(dplyr) })

# --------------------------------------------------------------
# 1) Multiplicadores — Modelo ABIERTO --------------------------
# Asume que `mats` YA trae:
#   - B   : inversa de Gosh (q x q)     [OBLIGATORIA]
#   - a_m : coef. de importaciones (q x 1)  [OBLIGATORIA]
#   - a_l : coef. de empleo (q x 1)         [OPCIONAL → mult_emp si existe]
# (Opcional, solo para chequeos contables y control):
#   - Z, f, m, y, x  (todas q x 1 salvo Z q x q)
#
# Devuelve:
#   list(mult_prod, mult_imp, mult_emp (o NULL), checks (o NULL))

get_multipliers_open <- function(mats) {
  # --- Unpack mínimos obligatorios ---
  B   <- mats$B
  a_m <- mats$a_m
  a_l <- mats$a_l %||% NULL   # opcional
  
  # --- Checks de forma mínimos ---
  stopifnot("B debe ser matriz cuadrada" = is.matrix(B) && nrow(B) == ncol(B))
  q <- nrow(B)
  
  stopifnot("a_m debe ser qx1" = is.matrix(a_m) && all(dim(a_m) == c(q, 1)))
  if (!is.null(a_l)) {
    stopifnot("a_l debe ser qx1" = is.matrix(a_l) && all(dim(a_l) == c(q, 1)))
  }
  
  # --- Multiplicadores ---
  u <- matrix(1, q, 1)
  mult_prod <- t(u)  %*% B |> t()          # qx1
  mult_imp  <- t(a_m) %*% B |> t()         # qx1
  mult_emp  <- if (!is.null(a_l)) t(a_l) %*% B |> t() else NULL
  
  # --- Chequeos contables (solo si hay insumos suficientes) ---
  checks <- list()
  
  have_Zfmx <- all(c("Z","f","m","y","x") %in% names(mats))
  if (have_Zfmx &&
      is.matrix(mats$Z) && is.matrix(mats$f) && is.matrix(mats$m) &&
      is.matrix(mats$y) && is.matrix(mats$x)) {
    
    Z <- mats$Z; f <- mats$f; m <- mats$m; y <- mats$y; x <- mats$x
    
    if (nrow(Z) == q && ncol(Z) == q &&
        all(dim(f) == c(q,1)) && all(dim(m) == c(q,1)) &&
        all(dim(y) == c(q,1)) && all(dim(x) == c(q,1))) {
      
      checks$open_prod_check  <- B %*% f - x                         # ≈ 0
      checks$expenditure_gap  <- x - matrix(rowSums(Z), ncol = 1) - f
      checks$income_gap       <- x - matrix(colSums(Z), ncol = 1) - m - y
    }
  }
  if (length(checks) == 0) checks <- NULL
  
  # --- Salida ---
  list(
    mult_prod = mult_prod,
    mult_imp  = mult_imp,
    mult_emp  = mult_emp,   # NULL si no se pasó a_l
    checks    = checks
  )
}

# --------------------------------------------------------------------
# 2) Multiplicadores Modelo CERRADO (respecto al consumo de hogares) 
# Asume que `mats` ya trae (mínimo):
#   A   : matriz de coeficientes técnicos (q x q)
#   a_l : empleo por unidad de producto (q x 1)
#   a_c : matriz de consumo inducido por unidad (q x q), típica: a_c = w %*% s_c
#   f   : demanda final total (q x 1)
#   x   : VBP (q x 1)
# Opcional:
#   l   : empleo (q x 1) para descontar el consumo inducido de f (f_z = f - a_c' l)
#
# Devuelve:
#   B_cl, f_z, mult_prod_cl, mult_emp_cl y chequeo de cierre (≈0)
# Nota:
#   - Como no se pide en el TP final, algunos insumos del modelo
#     cerrado no estan incoporporados al pipeline (específicamente, a_c)

get_multipliers_closed <- function(mats) {
  # --- Unpack mínimos ---
  A   <- mats$A
  a_l <- mats$a_l
  a_c <- mats$a_c
  f   <- mats$f
  x   <- mats$x
  l   <- mats$l %||% NULL
  
  # --- Checks mínimos de dimensiones ---
  stopifnot(
    "A debe ser matriz cuadrada"          = is.matrix(A) && nrow(A) == ncol(A),
    "a_l debe ser qx1"                    = is.matrix(a_l) && ncol(a_l) == 1 && nrow(a_l) == nrow(A),
    "a_c debe ser qxq"                    = is.matrix(a_c) && all(dim(a_c) == dim(A)),
    "f debe ser qx1"                      = is.matrix(f)   && ncol(f)   == 1 && nrow(f)   == nrow(A),
    "x debe ser qx1"                      = is.matrix(x)   && ncol(x)   == 1 && nrow(x)   == nrow(A)
  )
  if (!is.null(l)) {
    stopifnot("l debe ser qx1"            = is.matrix(l)   && all(dim(l) == c(nrow(A), 1)))
  }
  
  q <- nrow(A); I <- diag(q); u <- matrix(1, q, 1)
  
  # --- Demanda final exógena (descontar consumo inducido si hay l) ---
  c_induced <- if (!is.null(l)) t(a_c) %*% l else matrix(0, q, 1)
  f_z <- f - c_induced
  
  # --- Inversa de Leontief cerrada ---
  B_cl <- solve(I - A - t(a_c) %*% diag(as.vector(a_l)))
  
  # --- Multiplicadores cerrados ---
  mult_prod_cl <- t(u)  %*% B_cl |> t()   # qx1
  mult_emp_cl  <- t(a_l) %*% B_cl |> t()  # qx1
  
  # --- Chequeo de cierre (≈ 0 salvo redondeos) ---
  closed_prod_check <- B_cl %*% f_z - x
  
  out <- list(
    B_cl = B_cl,
    f_z  = f_z,
    mult_prod_cl = mult_prod_cl,
    mult_emp_cl  = mult_emp_cl,
    induced_consumption = c_induced,
    checks = list(closed_prod_check = closed_prod_check)
  )
  
  return(out)
}




