(archivo generado con inteligencia artificial)
# Módulos del proyecto (carpeta `R/`)

Este directorio contiene la **API modular** para cargar, construir y analizar matrices insumo–producto, además de generar tablas y gráficos reproducibles. Los módulos están pensados para usarse en conjunto a través del orquestador, pero cada función es **pura** (sin efectos colaterales) salvo las de exportación.

---

## Principios de diseño

- **Pureza**: funciones devuelven resultados sin modificar estado global (excepto exportación).
- **Chequeos defensivos**: validan dimensiones, `NA` y singularidad; ante error: `stop()` con mensaje claro.
- **Nombres consistentes**: `Z, Y, x, v, A, B, L=(I-A)^{-1}, G=(I-B)^{-1}`.
- **Separación**: carga (`io_load`), construcción (`io_build`), métricas (`io_multipliers`, `io_linkages`, `io_vertical`), tablas (`io_tables`), gráficos (`io_plots`), exportación (`io_export`), orquestación (`io_orchestrator`).

---

## Carga rápida de módulos

```r
.env <- if (isTRUE(getOption("knitr.in.progress"))) knitr::knit_global() else .GlobalEnv
mods <- c("io_utils.R","io_load.R","io_build.R","io_linkages.R",
          "io_multipliers.R","io_vertical.R","io_tables.R",
          "io_plots.R","io_export.R","io_orchestrator.R")
for (f in mods) source(file.path("R", f), local = .env, chdir = TRUE)
```

---

## Índice de módulos y funciones principales

### `io_utils.R`
- `%||%(x, y)` — operador “o nulo”.
- `check_square(mat, name)` — valida que `mat` sea cuadrada.
- `check_compat(Z, x)` — valida compatibilidad de dimensiones.
- `get_labels(tbl_ind, col, q_ind)` — construye catálogo de etiquetas.
- `fix_labels(obj, ind_labs)` — embellece etiquetas.

### `io_load.R`
- `load_matrices(country, year, path)` — lee CSV de OECD (MIP, industrias).
- `load_tim_labour(country, year, path, codes, unit)` — coeficientes laborales.

### `io_build.R`
- `partition_MIP(tbl_IP, q_ind, q_f)` — particiona en `Z, Y, M, v, x, f, ...`.
- `add_intensive_coefficients(mats)` — calcula `A, B, L, G, a_m, a_l`.

### `io_multipliers.R`
- `get_multipliers_open(mats)` — produce `mult_prod`, `mult_imp`, `mult_emp`.

### `io_linkages.R`
- `compute_linkages(B, G, sectors)` — calcula BL, FL, normalizaciones.
- `add_taxonomy(linkages, thr=1)` — clasifica sectores (Clave, Impulsor, etc.).

### `io_vertical.R`
- `get_vertical_open(mats, expo_col="EXPO")` — importaciones y trabajo embebido.

### `io_tables.R`
- `make_stacked_tables(results)` — apila resultados multianuales.

### `io_plots.R`
- `set_project_theme(base_size=12)` — tema unificado.
- `plot_compare_linkages(linkages_stack, multipliers_stack, ...)` — BL–FL comparativo.
- `plot_compare_years(stack, value_col, ...)` — comparación 1995–2018 de un indicador.

### `io_export.R`
- `export_csv(x, path)` — guarda tabla en CSV.
- `export_plot(p, path, width, height, dpi)` — guarda `ggplot` como imagen.

### `io_orchestrator.R`
- `build_country_year_objects(country, year, paths_raw, q_ind, q_f)` — pipeline anual.
- `build_many(country, years, paths_raw, q_ind, q_f)` — aplica a varios años.
- `run_orchestration(country, years, paths, q_ind, q_f, print_plots, save_plots)` — ejecuta flujo completo y devuelve resultados + plots.

---

## Grafo de dependencias

```
io_load  ─┐
io_build ─┼─> io_orchestrator ──┐
io_utils ─┘                     │
io_multipliers ────────────────┐│
io_vertical    ───────────────┤│
io_linkages    ───────────────┤│
io_tables      ───────────────┤└─> io_export (archivos)
io_plots       ───────────────┘    + retorna ggplots
```

---

## Ejemplo mínimo

```r
# Orquestación completa
res <- run_orchestration(
  country = "ARG",
  years   = c(1995, 2018),
  paths   = list(
    root   = getwd(),
    raw    = file.path(getwd(),"data","raw"),
    tables = file.path(getwd(),"results","tables"),
    figs   = file.path(getwd(),"results","figs")
  ),
  print_plots = FALSE,
  save_plots  = TRUE
)

# Acceso a resultados
res$linkages_stack
res$multipliers_stack
res$plots$blfl
```
