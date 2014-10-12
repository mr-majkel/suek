# Zmienia format codebooka
# Funkcja pomocnicza do getKey
# Argumenty
# cdbook      data.frame z surowym codebookiem.
# var_names   string pozwalający na zidentyfikowanie kolumny z nazwami
#             zmiennych.
# val_names   string pozwalający na zidentyfikowanie kolumny z etykietami
#             wartości.
# Wartość
# Data.frame z codebookiem z kolumną 'var_id', w której znajdują się nazwy
# zmiennych oraz kolumną 'values', w której znajdują się etykiety wartości.
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