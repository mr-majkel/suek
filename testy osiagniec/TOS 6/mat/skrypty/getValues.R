# Wyciąga wartości zmiennych z definicji w codebooku
# Argumenty
# valnames    string z definicją etykiet wartości w codebooku
# Uwagi
# Zakłada, że etykiety dla kolejnych wartości zmiennej opisane są w nowej
# linii, a także, że wartości od swoich etykiet rozdzielone są dwukropkiem
# (':').
# Wartość
# Wektor tekstowy z wartościami, dla których zdefiniowane zostały etykiety.
getValues = function (valnames) {
    # łamie etykiety wartości po znaku nowej linii
    valnames = unlist(strsplit(valnames, "\n"))
    
    # łamie etykiety weartości po dwukropku
    values = unlist(lapply(strsplit(valnames, ":"), "[", 1))
    
    # zwraca wektor tekstowy z kodami
    return(values)
}
