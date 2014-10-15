# Zapisuje listę z kluczem do pliku
# Argumenty
# key_list    lista z kluczem.
# file        string z nazwą pliku docelowego.
# append      boolean określający, czy klucz dopisać na końcu pliku. Domyślnie
#             FALSE.
# Uwagi
# Wykorzystuje funkcję writeKeySingle() do zapisu poszczególnych zmiennych.
writeKey = function(key_list, file = "", append = FALSE) {
    if (!append) {
        cat("", sep = "", file = file, append = FALSE)
    }
    items = names(key_list[["values"]])
    mcq_bool = items %in% key_list[["mcq"]]
    i = 1
    for (it in items) {
        writeKeySingle(it, key_list$values[[it]], key_list$recodes[[it]],
                       mcq = mcq_bool[i], file = file)
        i = i + 1
    }
}