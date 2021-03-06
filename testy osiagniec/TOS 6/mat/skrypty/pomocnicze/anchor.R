# funkcja do kopiowania zmiennych kotwicz�cych
# df - data.frame
# anchor_mat - macierz dwukolumnowa z informacj� o kotwicz�cych zadaniach
anchor = function(df, anchor_mat) {
  # p�tla po wierszach, czyli koljenych parach zada�
  for(i in 1:nrow(anchor_mat)) {
    # nazwa zadania z wersji a
    it_a = anchor_mat[i, 1]
    # nazw zadania z wersji b
    it_b = anchor_mat[i, 2]
    # nazwa zmiennych kotwicz�cych
    kit = paste0("K", it_a)
    # indeksy uczni�w z wersji A
    ind_ucz_a = which(df$wersja == "A")
    # indeksy uczni�w z wersji B
    ind_ucz_b = which(df$wersja == "B")
    # wyci�gni�cie odpowiednich warto�ci
    df[ind_ucz_a, kit] = df[ind_ucz_a, it_a]
    df[ind_ucz_b, kit] = df[ind_ucz_b, it_b]
  }
  return(df)
}
