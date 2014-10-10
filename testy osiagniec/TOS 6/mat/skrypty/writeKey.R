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