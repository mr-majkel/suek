# Funkcja do rekodowania
# Argumenty
# raw_df    data.frame z bazą do zrekodowania.
# key_list  lista zawierająca nazwy zmiennych do rekodowania w wektorze
#           tekstowym 'items', wartości oryginalne w liście 'values' oraz
#           wartości docelowe w liście 'recodes'.
# Wartość
# Data.frame, w którym dla zmiennych wyspecyfikowanych w 'items' odpowiednie
# wartości w 'values' zostały podmienione na wartości z 'recodes'.
recode = function(raw_df, key_list) {
    recoded_df = raw_df
    items = key_list[["items"]]
    
    for(it in items) {
        it_vals = key_list[["values"]][[it]]
        it_recodes = key_list[["recodes"]][[it]]
        for (v in it_vals) {
            recode_ind = which(raw_df[, it] == v)
            recoded_df[recode_ind, it] = it_recodes[which(it_vals == v)]
        }
    }
    recoded_df[, items] = data.frame(lapply(recoded_df[, items], as.numeric))
    return(recoded_df)
}
