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
# Update
readKey = function(file) {
    # funkcja pomocnicza do wyciągania elementów rozdzielonych średnikiem
    separate = function(x, ind) {
        y = strsplit(x, ";")
        y = unlist(lapply(y, "[", ind))
        return(y)
    }
    # wczytanie pliku
    code_df = read.csv2(file, stringsAsFactors = FALSE)
    # wyłowienie nazw itemów
    items = unique(code_df$item)
    # określenie itemów zamkniętych
    # mcq_bool = grepl(";", items)
    # items_c = NULL
    
#     if (sum(mcq_bool) > 0) {
#         items = separate(items, 1)
#         items_c = items[mcq_bool]
#     }
    # wyłonienie 'values' i 'recodes'
    values = list()
    recodes = list()
    for (it in items) {
      values[[it]] = code_df[code_df$item == it, "value"]
      recodes[[it]] = code_df[code_df$item == it, "recode"]
    }
    # wyłonienie zadań otwartych
    # items_o = items[!mcq_bool]
    
    # tworzenie listy obiektów
    return_list = list()
    # nazwy zmiennych
    return_list[["items"]] = items
    # wartości zmiennych
    return_list[["values"]] = values
    # rekodowane wartości
    return_list[["recodes"]] = recodes
    # zadania otwarte
    # return_list[["open"]] = items_o
    # zadania zamknięte
    # return_list[["mcq"]] = items_c
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