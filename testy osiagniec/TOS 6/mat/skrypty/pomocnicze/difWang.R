difWang <- function(resp, dif_facet = NULL, round = 5, facets = NULL,
                    formulaY = NULL, group = NULL, formulaA = NULL,
                    control = NULL, ...) {
  
  # Nie podano dif_facet
  # print(missing(mgroup))
  if (is.null(dif_facet)) {
    stop("\nNie wskazano aspektu dla testu DIF. Analiza zatrzymana.\n")
    
  # lub podano wektor
  } else if (length(dif_facet) > 1) {
    stop("\nAnaliza DIF może zostać przeprowadzona tylko",
        "ze względu na jeden aspekt. Podano", length(dif_facet), ".\n")
  }
  # sprawdzenie czy podano data.frame z aspektami
  if(is.null(facets)) {
    stop("\nNiepodano zbioru danych z określeniem aspektów.",
        "Analiza zatrzymana.\n")
  }
  # nazwy itemów w bazie
  items = names(resp)
  
  # stworzenie odpowiednich interakcji dla testu DIF
  dif_formula = paste0(" + ", dif_facet, " + ", paste0(dif_facet, ":item"))
  
  # formuła dla pełnej wariantności
  formulaA = as.formula(paste0("~ item + item:step", dif_formula))

  # zmienna kontrolna dla modelu z regresją latentną
  dif_latent = FALSE
  
  # zmienna kontrolna dla modelu multigroup
  dif_group = FALSE
  
  # sprawdzenie, czy dif_facet występuje w formulaY jako efekt główny
  if(!is.null(formulaY)) {
    termsY = attr(terms(formulaY), "term.labels")

    # jeśli tak...
    if (dif_facet %in% termsY) {
      # średnia policzona latentnie
      dif_latent = TRUE
      # usuń dif_facet z dif_formula
      dif_formula = gsub(paste0("\\+ ", dif_facet, " "), "", dif_formula)
      # i zmodyfikuj formulaA
      formulaA = as.formula(paste0("~ item + item:step", dif_formula))
      # 
    }
  }
  
  # sprawdzenie czy zażądana analiza wielogrupowa
  if(!is.null(group)) {
    # sprawdzenie, czy group jest tożsamy z factor
    if(sum(factor[, dif_facet] != group) == 0) {
      # średnia policzona latentnie
      dif_group = TRUE
      # usuń dif_facet z dif_formula
      dif_formula = gsub(paste0("\\+ ", dif_facet, " "), "", dif_formula)
      # i zmodyfikuj formulaA
      formulaA = as.formula(paste0("~ item + item:step", dif_formula))
    }
  }
  
  # ustawienie domyślnych opcji kontrolnych estymacji modelu
  if (missing(control)) {
    control = list(progress = FALSE,
                   QMC = FALSE)
  } else {
    control$progress = FALSE
  }

  # stworzenie macierzy potrzebnych do estymacji modelu z pełną wariantnością na
  # płeć.
  des2 = designMatrices.mfr2(resp = resp,
                             facets = facets,
                             formulaA = formulaA)

  # wyciągnięcie przetworzonej bazy danych
  resp2 = des2$gresp$gresp.noStep
  
  # macierz A
  A = des2$A$A.3d
  
  # macierz B
  B = des2$B$B.3d
  
  # parametry nieestymowalne (np. step 2, dla zadań kodowanych 0-1)
  xsi.elim = des2$xsi.elim
  
  # parametry zawiązane z DIF
  # dif term
  dif_term = paste0("^", dif_facet)
  # dif items
  dif_items = paste0(":", dif_facet)
  # indeksy uogólnionych zadań
  dif_term_ind = grep(dif_term, dimnames(A)[[3]])
  dif_items_ind = grep(dif_items, dimnames(A)[[3]])
  
  # usunięcie odpowiednich itemów z modelu H0
  if (dif_latent || dif_group) {
    A0 = A[, , -c(xsi.elim[, 2], dif_term_ind, dif_items_ind)]
  } else {
    A0 = A[, , -c(xsi.elim[, 2], dif_items_ind)]
  }
  # policzenie modelu dla H0
  cat("\nLiczę model z pełną inwariantnością...", fill = TRUE)
  modH0 = tam.mml(resp = resp2, group = group, formulaY = formulaY,
                  A = A0, B = B,
                  control = control,
                  ...)
  cat("Policzono.", fill = TRUE)
  # wybranie potrzebnych statytstyk z modelu H0.
  n_pars0 = modH0$ic$Npars
  H0_dev = modH0$ic$deviance
  
  # tabela do porównań modelu
  compare_df = data.frame(item_dif = "null_model", estim_pars = n_pars0,
                          deviance = H0_dev, LR = NA,
                          LR_df = NA)
  for (it in items){
    # zadania kotwiczące - wszystkie poza danym zadaniem
    anchor_it = grep(paste0(it, "$"), items, value = TRUE, invert = TRUE)
    # stwórz odpowiednią macierz A
    # nazwy zgeneralizowanych itemów do nieestymowania
    if (dif_latent || dif_group) {
      gen_it = c(paste0(anchor_it, dif_items), dif_term)
    } else {
      gen_it = c(paste0(anchor_it, dif_items))
    }
    # ich indeksy w macierzy A
    gen_it_ind = grep(paste0(gen_it, collapse = "|"),
                      dimnames(A)[[3]])
    # usunięcie ich z macierzy A
    A1 = A[, , -c(xsi.elim[, 2], gen_it_ind)]

    # policz model z uwolnionym DIF dla itemu
    cat("\nLiczę model z efektem DIF dla zadania", it, "...", fill = TRUE)
    modH1 = tam.mml(resp = resp2, group = group, formulaY = formulaY,
                    A = A1, B = B,
                    control = control,
                    ...)
    cat("Policzono.", fill = TRUE)
    # wytnij informacje o modelu
    n_pars = modH1$ic$Npars
    H1_dev = modH1$ic$deviance
    
    # wylicz statystyki
    LR = round(H0_dev - H1_dev, round)
    LR_df = round(LR/(n_pars - n_pars0), round)
    
    # wypełnij tabelę
    compare_df = rbind(compare_df, data.frame(item_dif = it, estim_pars = n_pars,
                                              deviance = H1_dev, LR = LR,
                                              LR_df = LR_df))
  }
  # zwróć tabelę
  return(compare_df)

}
# test
# mm = difWang(mat_all_r[, items1], dif_facet = "kobieta", round = 5,
#         facets = mat_all_r[, "kobieta", drop = FALSE],
#         formulaY = ~ kobieta, dataY = mat_all_r[, "kobieta", drop = FALSE],
#         control = list(QMC = FALSE, 
#                        increment.factor=1.01,
#                        fac.oldxsi=.2))
