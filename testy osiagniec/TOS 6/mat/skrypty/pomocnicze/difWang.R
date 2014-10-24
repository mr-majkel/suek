difWang <- function(resp, dif_facet = NULL, formulaY = NULL, group = NULL, ...){
  # Nie podano dif_facet
  if (is.null(dif_facet)) {
    cat("\nNie wskazano aspektu dla testu DIF. Analiza zatrzymana.\n")
    break
  }
  # stworzenie odpowiednich interakcji dla testu DIF
  dif_formula = paste0(" + ", dif_facet, " + ", paste0(dif_facet, ":item"))
  
  # formuła dla pełnej wariantności
  formulaA = as.formula(paste0("~ item + item:step", dif_formula))
  # print(formulaA)
  
}

difWang(mat_all_r, "kobieta")

