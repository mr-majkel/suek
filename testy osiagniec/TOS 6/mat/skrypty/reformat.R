reformat = function(cdbook, var_names = "Nazwa", val_names = "Etykiety") {
    # zapytanie do grepa
    grep_querry = paste(c(var_names, val_names), collapse = "|")
    # wybranie zmiennych z codebooka
    cdbook = cdbook[, grep(grep_querry, names(cdbook))]

    # zmiana nazw zmiennych na bardziej przyjazne
    names(cdbook) = c("var_id", "values")
    
    # zwraca data.frame z wybranymi kolumnami
    return(cdbook)
}