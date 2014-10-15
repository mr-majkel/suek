# Generuje z codebooka klucz wartości potrzebnych do rekodowania.
# Używa następujących funkcji
# pomocniczych: reformat(), getItems(), getValues().
# 
# Użycie
# getKey(cdbook, vars, open_key = c("1", "2"), mcq_key, na_codes)
#
# Argumenty
# cdbook    data.frame z codebookiem z weryfikatora. 
# vars      string z zapytaniem do grep() lub wektor tekstowy z nazwami
#           zmiennych.
# open_key  wektor tekstowy określający wartości w bazie do zrekodowania, które
#           mają nie być rekodowane na 0. Dotyczy tylko zadań nie określonych
#           jako zamknięte (patrz Uwagi niżej). Domyślnie c("1", "2").
# mcq_key   wektor określający wartości dla zadań zamkniętych, którym po 
#           zrekodowaniu przypisać wartość 1, a nie 0. Liczba kodów musi
#           zgadzać się z liczbą wykrytych zadań zamkniętych (patrz Uwagi
#           niżej).
# na_codes  wektor określający wartości w bazie, które mają być zrekodowane
#           jako NA, a nie 0. Dotyczy wszystkich zmiennych określonych w vars.
# 
# Wartość
# Lista o następujących elementach
# items     wektor tekstowy z nazwami zmiennych pasujących do 'vars'.
# values    lista wartości zdefiniowanych dla 'items'.
# recodes   lista wartości, na które mają być zrekodowane 'values'.
# open      wektor tekstowy z nazwami zmiennych określonych jako otwarte
#           (tj. nie-zamknięte).
# mcq       wektor tekstowy z nazwami zmiennych określonych jako zamknięte.
# open_key  wykorzystany 'open_key'.
# mcq_key   wykorzystany 'mcq_key' (o ile został podany).
# 
# Uwagi
# 
# W trakcie generowania klucza zakłada, że zmienne, które posiadają etykietę
# dla wartości 4, są zadaniami zamkniętymi. Ważne, żeby nazwy zmiennych w
# 'cdbook' były zdefiniowane w kolumnie 'Nazwa( zmiennej)', a etykiety wartości
# w kolumnie 'Etykiety( wartości)'. Część nie w nawiasie stanowi zapytanie
# odnajdujące odpowiednie kolumny. Wyrażenia 'Nazwa' oraz 'Etykiety' muszą więc
# identyfikować dokładnie po jednej zmiennej (patrz reformat()).
# Zakłada, że wartości są oddzielone od swoich etykiet za pomocą dwukropka ':',
# patrz getValues().
getKey = function(cdbook, vars, open_key = c("1", "2"), mcq_key = NULL,
                  na_codes = NULL) {
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
        # i dla zamkniętych
        } else if (!is.null(mcq_key)){
            if (length(mcq_key) == length(items_c)) {
                mcq_index = which(items_c == it)
                key_list[[it]][match(mcq_key[mcq_index], it_vals)] = 1
            }
        }
        # recodes dla braków danych
        if (!is.null(na_codes)) {
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
    if (!is.null(mcq_key)){
        if (length(mcq_key) != length(items_c)) {
            cat("Liczba kodow do zadan zamknietych (", length(mcq_key),
                ") ", "nie zgadza sie z wykryta ich liczba (",
                length(items_c),")!", sep = "", fill = TRUE)
            cat("Zadaniom zamknietym nie przypisano kodow.", fill = TRUE)          
        #} else {
        #    names(mcq_key) = items_c
        }
        return_list[["mcq_key"]] = mcq_key
    }
    # wartości braków danych
    if (!is.null(na_codes)) {
        return_list[["na_codes"]] = na_codes
    }
    return(return_list)
}
