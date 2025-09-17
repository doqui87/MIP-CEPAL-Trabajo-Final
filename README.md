## Reproducibilidad

1. Clonar el repo y abrir `MIP-Trabajo-Final.Rproj`.
2. En consola R:
   ```r
   if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
   renv::restore()
```
3. Colocar ARG1995dom.csv, ARG2018dom.csv, tbl_ind.csv en data/raw/.
4. Renderizar el informe:
```r
quarto::quarto_render("informe_final.qmd")
```

Outputs: figuras en results/figs/, tablas en results/tables/.


## Versión freezada

Alternativamente, debiera haber una versión HTML congelada en /docs/
