# Wczytuje plik ze zdefiniowanym kluczem
# Argumenty
# file    string wskazujący plik .csv z kluczem.
# 
# Wartość
# Lista o następujących elementach
# items     wektor tekstowy z nazwami zmiennych pasujących do 'vars'.
# values    lista wartości zdefiniowanych dla 'items'.
# recodes   lista wartości, na które mają być zrekodowane 'values'.
# open      wektor tekstowy z nazwami zmiennych określonych jako otwarte
#           (tj. nie-zamknięte).
# mcq       wektor tekstowy z nazwami zmiennych określonych jako zamknięte.
# 
# Uwagi
# Plik .csv powinien opisywać zmienne do zrekodowania w następujący sposób:
# nazwa_zmiennej; ["mcq"]
# values; recodes
# [w kolejnych liniach wartości zmiennej i odpowiadające im wartości do
# zrekodowania oddzielone średnikiem (';')]
# pusta_linia
readKey = function(file) {
    # funkcja pomocnicza do wyciągania elementów rozdzielonych średnikiem
    separate = function(x, ind) {
        y = strsplit(x, ";")
        y = unlist(lapply(y, "[", ind))
        return(y)
    }
    # wczytanie pliku
    raw_file = readLines(con = file)
    # liczba linii w pliku
    n_lines = length(raw_file)
    # liczba pustych linii w pliku
    empty_lines = which(raw_file == "")
    n_empty_lines = length(empty_lines)
    # określenie indeksów z nazwami 'itemów'
    item_lines = c(1, empty_lines[1:(n_empty_lines - 1)] + 1)
    # wyłowienie nazw itemów
    items = raw_file[item_lines]
    # określenie itemów zamkniętych
    mcq_bool = grepl(";", items)
    items_c = NULL
    
    if (sum(mcq_bool) > 0) {
        items = separate(items, 1)
        items_c = items[mcq_bool]
    }
    # lista z wartościami
    key_list = list()
    for (it in items) {
        item_index = which(items == it)
        begin_key = item_lines[item_index] + 2
        end_key = empty_lines[item_index] - 1
        key_list[[it]] = raw_file[begin_key:end_key]
    }
    # wyłonienie 'values' i 'recodes'
    values = lapply(key_list, separate, 1)
    recodes = lapply(key_list, separate, 2)
    # wyłonienie zadań otwartych
    items_o = items[!mcq_bool]
    
    # tworzenie listy obiektów
    return_list = list()
    # nazwy zmiennych
    return_list[["items"]] = items
    # wartości zmiennych
    return_list[["values"]] = values
    # rekodowane wartości
    return_list[["recodes"]] = recodes
    # zadania otwarte
    return_list[["open"]] = items_o
    # zadania zamknięte
    return_list[["mcq"]] = items_c
    # # kody otwarte
    # return_list[["open_key"]] = open_key
    # # kody zamknięte
    # if (!missing(mcq_key)){
        # names(mcq_key) = items_c
        # return_list[["mcq_key"]] = mcq_key
    # }
    # # wartości braków danych
    # if (!missing(na_codes)) {
        # return_list[["na_codes"]] = na_codes
    # }
    return(return_list)
}