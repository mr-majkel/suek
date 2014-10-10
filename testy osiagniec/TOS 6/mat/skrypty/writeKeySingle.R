# zapisuje do pliku
writeKeySingle = function(name, values, recodes, mcq = FALSE, file = "") {
    if (mcq){
        name = paste(c(name, "mcq"), collapse = ";")
    }
    output = c(name, paste("value", "recode", sep = ";"))
    i = 1
    for (v in values) {
        if (!missing(recodes)) {
            output = c(output, paste(v, recodes[[i]], sep = ";"))
            i = i + 1
        } else {
            output = c(output, v)
        }       
    }
    cat(output, sep = "\n", file = file, append = TRUE)
    cat("\n", file = file, append = TRUE)
}