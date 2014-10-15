# Funkcja pomocnicza do getKey().
# Wyciąga itemy z codebooka. Zwraca wektor tekstowy z nazwami zmiennych.
# Argumenty
# cdbook  data.frame z codebookiem, w którym nazwy zmiennych znajdują się w
#         kolumnie 'var_id'.
# vars    string z zapytaniem do grep() lub wektor tekstowy z nazwami
#         zmiennych.
getItems = function(cdbook, vars) {    
    # stworzenie listy itemów
    grep_querry = paste(vars, collapse = "|")
    items = grep(grep_querry, cdbook$var_id, value = TRUE)
    # zwraca wektor z nazwami itemów    
    return(items)
}