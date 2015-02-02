# ścieżka, opcje, biblioteki

setwd(paste0("C:\\Users\\Uzytkownik\\Documents\\SUEK\\",
             "Projects\\suek\\testy osiagniec\\TOS 6\\mat"))
wd = getwd()

options("stringsAsFactors" = FALSE)

# wczytanie funkcji pomocniczych
source("skrypty\\pomocnicze\\getKey.R")
source("skrypty\\pomocnicze\\reformat.R")
source("skrypty\\pomocnicze\\getItems.R")
source("skrypty\\pomocnicze\\getValues.R")
source("skrypty\\pomocnicze\\writeKeySingle.R")
source("skrypty\\pomocnicze\\writeKey.R")
source("skrypty\\pomocnicze\\readKey.R")
source("skrypty\\pomocnicze\\recode.R")
source("skrypty\\pomocnicze\\showNotReached.R")

# wczytanie baz
mat_a = read.csv2("bazy oryg\\SUEK7_TOS6 Matematyka A zasadniczeDane.csv")
mat_b = read.csv2("bazy oryg\\SUEK7_TOS6 Matematyka B zasadniczeDane.csv")

# wczytanie codebooków
code_mat_a = read.csv2(paste0("bazy oryg\\",
                              "SUEK7_TOS6 Matematyka A zasadniczeCodebook.csv"))
code_mat_b = read.csv2(paste0("bazy oryg\\",
                              "SUEK7_TOS6 Matematyka B zasadniczeCodebook.csv"))

# dodanie zmiennej na określenie wersji
mat_a$wersja = "A"
mat_b$wersja = "B"

# wersja z not reached bazy
mat_a = showNotReached(mat_a, "MA")
mat_b = showNotReached(mat_b, "MB")

# poprawne kody do zadań zamkniętych
# wersja A
key_a = c(2,4,3,1,3,1,3,3,3,2,3,1,3,2,3,3,2)
# wersja B
key_b = c(3,4,3,4,3,3,2,2,3,2,2,3,1,3,4,3,2)

# stworzenie klucza do rekodowania
# wersja A
key_mat_a = getKey(code_mat_a, "MA", c("1","2"), key_a, na_codes = "N")

# wersja B
key_mat_b = getKey(code_mat_b, "MB", c("1","2"), key_b, na_codes = "N")

# zapisanie klucza
# writeKey(key_mat_a, "bazy zmien\\klucz_A.csv")
# writeKey(key_mat_b, "bazy zmien\\klucz_B.csv")

# w kluczu w wersji A zad. 25, jako poprawną odpowiedź traktuje się odpowiedzi
# B i D.

# wczytanie klucza
key_mat_a = readKey("bazy zmien\\klucz_A.csv")
key_mat_b = readKey("bazy zmien\\klucz_B.csv")

# zrekodowanie bazy surowej
mat_a_r = recode(mat_a, key_mat_a)
mat_b_r = recode(mat_b, key_mat_b)

# zapisanie baz rekodowanych
# write.csv2(mat_a_r, "bazy zmien\\tos6_mat_a_rek.csv", row.names = FALSE)
# write.csv2(mat_b_r, "bazy zmien\\tos6_mat_b_rek.csv", row.names = FALSE)

# połączenie baz dla dwóch wersji 
mat_all = merge(mat_a, mat_b, all = TRUE)
mat_all_r = merge(mat_a_r, mat_b_r, all = TRUE)

# sprawdzenie występowania dubli na ID_ucz
any(duplicated(mat_all$ID_ucz))

# zapisanie połączonych baz
write.csv2(mat_all, "bazy zmien\\tos6_mat.csv", row.names = FALSE)
write.csv2(mat_all_r, "bazy zmien\\tos6_mat_rek.csv", row.names = FALSE)
