# wyci¹ga wartoœci zmiennych
getValues = function (valnames) {
    # ³amie etykiety wartoœci po znaku nowej linii
    valnames = unlist(strsplit(valnames, "\n"))
    
    # ³amie etykiety weartoœci po dwukropku
    values = unlist(lapply(strsplit(valnames, ":"), "[", 1))
    
    # zwraca wektor tekstowy z kodami
    return(values)
}
