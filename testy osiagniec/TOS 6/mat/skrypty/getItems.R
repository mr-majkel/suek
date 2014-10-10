# wyci¹ga itemy z codebooka
getItems = function(cdbook, vars) {    
    # stworzenie listy itemów
    grep_querry = paste(vars, collapse = "|")
    items = grep(grep_querry, cdbook$var_id, value = TRUE)
    # zwraca wektor z nazwami itemów    
    return(items)
}