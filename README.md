## Reproducibilidad

1. Clonar el repo y abrir `MIP-Trabajo-Final.Rproj`.
2. En consola R:
   ```r
   if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
   renv::restore()
   ```
3. Colocar ARG1995dom.csv, ARG2018dom.csv, tbl_ind.csv EMPN.Rdata y LABR.Rdata en `data/raw/`.
4. Renderizar el informe:
```r
quarto::quarto_render("informe_final.qmd")
```
Outputs: 
- figuras en /figs/,
- tablas en /tables/.


## Versión freezada
Alternativamente, debiera haber una versión HTML congelada en /docs/index.html (local), \
también es accesible desde https://doqui87.github.io/MIP-CEPAL-Trabajo-Final/ (cloud)
