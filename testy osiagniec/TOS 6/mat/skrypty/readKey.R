readKey = function(file) {
    separate = function(x, ind) {
        y = strsplit(x, ";")
        y = unlist(lapply(y, "[", ind))
        return(y)
    }
    raw_file = readLines(con = file)
    n_lines = length(raw_file)
    
    empty_lines = which(raw_file == "")
    n_empty_lines = length(empty_lines)
    item_lines = c(1, empty_lines[1:(n_empty_lines - 1)] + 1)
    
    items = raw_file[item_lines]
    mcq_bool = grepl(";", items)
    items_c = NULL
    
    if (sum(mcq_bool) > 0) {
        items = separate(items, 1)
        items_c = items[mcq_bool]
    }
    key_list = list()
    for (it in items) {
        item_index = which(items == it)
        begin_key = item_lines[item_index] + 2
        end_key = empty_lines[item_index] - 1
        key_list[[it]] = raw_file[begin_key:end_key]
    }

    values = lapply(key_list, separate, 1)
    recodes = lapply(key_list, separate, 2)
    items_o = items[!mcq_bool]
    
    # tworzenie listy obiektów
    return_list = list()
    # nazwy zmiennych
    return_list[["items"]] = items
    # wartoœci zmiennych
    return_list[["values"]] = values
    # rekodowane wartoœci
    return_list[["recodes"]] = recodes
    # zadania otwarte
    return_list[["open"]] = items_o
    # zadania zamkniête
    return_list[["mcq"]] = items_c
    # # kody otwarte
    # return_list[["open_key"]] = open_key
    # # kody zamkniête
    # if (!missing(mcq_key)){
        # names(mcq_key) = items_c
        # return_list[["mcq_key"]] = mcq_key
    # }
    # # wartoœci braków danych
    # if (!missing(na_codes)) {
        # return_list[["na_codes"]] = na_codes
    # }
    return(return_list)
}