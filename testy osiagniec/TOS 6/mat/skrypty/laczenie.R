# ścieżka, opcje, biblioteki
setwd(paste0("C:\\Users\\Uzytkownik\\Documents\\SUEK\\",
             "Projects\\suek\\testy osiagniec\\TOS 6\\mat"))
wd = getwd()

source("skrypty\\pomocnicze\\difWang.R")
source("skrypty\\pomocnicze\\anchor.R")

# wczytanie baz
mat_all = read.csv2("bazy zmien\\tos6_mat.csv", stringsAsFactors = FALSE)
mat_all_r = read.csv2("bazy zmien\\tos6_mat_rek.csv", stringsAsFactors = FALSE)

ucz_df = read.csv2("bazy oryg\\uczen_dane.csv", stringsAsFactors = FALSE)

# dodanie płci, id szkoły, oznaczenia oddziału w klasie VI i nru z dziennika
mat_all = merge(mat_all, ucz_df, all.x = TRUE)
mat_all_r = merge(mat_all_r, ucz_df, all.x = TRUE)

# określenie kotwiczących
kotw = matrix(c("MA_4", "MB_4",
                "MA_6", "MB_6",
                "MA_8", "MB_7",
                "MA_9", "MB_8",
                "MA_10", "MB_9",
                "MA_12", "MB_12",
                "MA_13", "MB_13",
                "MA_14", "MB_14",
                "MA_15", "MB_16",
                "MA_16", "MB_17",
                "MA_18", "MB_18",
                "MA_22", "MB_22",
                "MA_23", "MB_23",
                "MA_24", "MB_24"), ncol = 2, byrow = TRUE)

colnames(kotw) = c("wersja A", "wersja B")
# stworzenie zmiennych kotwiczących w bazach
mat_all[, paste0("K", kotw[, 1])] = NA
mat_all_r[, paste0("K", kotw[, 1])] = NA

# skopiowanie wartości dla zmiennych kotwiczących
mat_all = anchor(mat_all, kotw)
mat_all_r = anchor(mat_all_r, kotw)

# Stworzenie zmiennej 0-1 dla płci
mat_all_r$kobieta = NA
mat_all_r[mat_all_r$plec == "K", "kobieta"] = 1
mat_all_r[mat_all_r$plec == "M", "kobieta"] = 0

mat_all$kobieta = NA
mat_all[mat_all$plec == "K", "kobieta"] = 1
mat_all[mat_all$plec == "M", "kobieta"] = 0

# sortowanie zmiennych
zmienne = c(grep("ID_ucz|ID_szk|oddz|nr_dz", names(mat_all), value = TRUE),
  "plec", "kobieta", grep("M", names(mat_all), value = TRUE))
mat_all = mat_all[, zmienne]
mat_all_r = mat_all_r[, zmienne]

save("mat_all", "mat_all_r", "kotw", file = "bazy zmien/bazy_mat.RData")
