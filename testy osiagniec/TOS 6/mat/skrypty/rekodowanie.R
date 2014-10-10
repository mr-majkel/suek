# œcie¿ka, opcje, biblioteki
setwd(paste0("C:\\Users\\Uzytkownik\\Documents\\SUEK\\",
             "Raport koñcowy\\miary osi¹gniêæ\\TOS 6\\mat"))
wd = getwd()
options("stringsAsFactors" = FALSE)

source("skrypty\\getKey.R")
source("skrypty\\reformat.R")
source("skrypty\\getItems.R")
source("skrypty\\getValues.R")
source("skrypty\\writeKeySingle.R")
source("skrypty\\writeKey.R")
source("skrypty\\readKey.R")
source("skrypty\\recode.R")

# wczytanie baz
mat_a = read.csv2("bazy oryg\\SUEK7_TOS6 Matematyka A.csv")
mat_b = read.csv2("bazy oryg\\SUEK7_TOS6 Matematyka B.csv")

# wczytanie codebooków
code_mat_a = read.csv2("bazy oryg\\SUEK7_TOS6 Matematyka A popr20140808.csv")
code_mat_b = read.csv2("bazy oryg\\SUEK7_TOS6 Matematyka B popr20140808.csv")

# poprawne kody do zadañ zamkniêtych
# A
key_a = c(2,4,3,1,3,1,3,3,3,2,3,1,3,2,3,3,2)

# stworzenie klucza
key_mat_a = getKey(code_mat_a, "MA", c("1","2"), key_a)
# zapisanie klucza
writeKey(key_mat_a, "bazy zmien\\klucz.csv")
# wczytanie klucza
key_mat_a = readKey("bazy zmien\\klucz.csv")
# zrekodowanie bazy surowej
mat_a_r = recode(mat_a, key_mat_a)
# zapisanie bazy rekodowanej
write.csv2(mat_a_r, "bazy zmien\\tos6_mat_a_rek.csv", row.names = FALSE)
