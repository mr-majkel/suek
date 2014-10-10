getKey = function(cdbook, vars, open_key = c("1", "2"), mcq_key,
                  na_codes) {
    # wczytanie codebooka
    cdbook = reformat(cdbook)
    # nazwy itemów
    items = getItems(cdbook, vars)
    # lista wartości
    values = lapply(cdbook[cdbook$var_id %in% items, "values"], getValues)
    # zadania zamknięte (posiadają kod 4)
    closed = unlist(lapply(values, function(x) {y = 4 %in% x; return(y)}))
    items_c = items[closed]
    # zadania otwarte
    items_o = items[!closed]
    
    # tworzenie recodes dla zadań otwartych
    key_list = list()
    for (it in items) {
        item_index = which(items == it)
        it_vals = values[[item_index]]
        n_vals = length(it_vals)
        key_list[[it]] = rep(0, n_vals)
        if (it %in% items_o) {
            open_index = match(open_key[open_key %in% it_vals], it_vals)
            key_list[[it]][open_index] = open_key[open_key %in% it_vals]
        } else if (!missing(mcq_key)){
            if (length(mcq_key) == length(items_c)) {
                mcq_index = which(items_c == it)
                key_list[[it]][match(mcq_key[mcq_index], it_vals)] = 1
            }
        }
        if (!missing(na_codes)) {
            na_index = match(na_codes, it_vals)
            key_list[[it]][na_index] = NA
        }
    }
    
    # tworzenie listy obiektów
    return_list = list()
    # nazwy zmiennych
    return_list[["items"]] = items
    # wartości zmiennych
    names(values) = items
    return_list[["values"]] = values
    # rekodowane wartości
    return_list[["recodes"]] = key_list
    # zadania otwarte
    return_list[["open"]] = items_o
    # zadania zamknięte
    return_list[["mcq"]] = items_c
    # kody otwarte
    return_list[["open_key"]] = open_key
    # kody zamknięte
    if (!missing(mcq_key)){
        if (length(mcq_key) != length(items_c)) {
            cat("Liczba kodow do zadan zamknietych (", length(mcq_key),
                ") ", "nie zgadza sie z wykryta ich liczba (",
                length(items_c),")!", sep = "", fill = TRUE)
            cat("Zadaniom zamknietym nie przypisano kodow.", fill = TRUE)          
        } else {
            names(mcq_key) = items_c
        }
        return_list[["mcq_key"]] = mcq_key
    }
    # wartości braków danych
    if (!missing(na_codes)) {
        return_list[["na_codes"]] = na_codes
    }
    return(return_list)
}
