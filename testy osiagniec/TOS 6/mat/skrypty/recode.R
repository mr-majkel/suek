recode = function(raw_df, key_list) {
    recoded_df = raw_df
    items = names(key_list[["values"]])
    
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