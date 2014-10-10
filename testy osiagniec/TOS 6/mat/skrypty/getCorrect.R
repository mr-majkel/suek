# Funkcja pomocnicza do wskazywania poprawnej odpowiedzi przy wczytywaniu
# codebooka. Nieskończona. Obecnie nie używana.
getCorrect = function(cdbook, mcq, char = "*") {
    # wybranie zmiennych z codebooka
    cdbook2 = cdbook[, grep("Nazwa|Etykiety", names(cdbook))]
    # print(names(cdbook))
    # zmiana nazw zmiennych na bardziej przyjazne
    names(cdbook2) = c("var_id", "values")
    
    # etykiety dla zamkniętych
    mcq_val = cdbook[cdbook2$var_id %in% mcq, "values"]
    names(mcq_val) = mcq
    
    # data.frame z poprawnymi odpowiedziami
    correct_df = data.frame()
    
    # poszukiwanie specjalnego znaku dla każdego zadania
    for (m in mcq) {
        # rozbicie etykiet
        mcq_val_list = lapply(mcq_val, strplit, "\n")
        correct_df[, m] = grep(char, mcq_val_list[[m]], val) 
    }
}