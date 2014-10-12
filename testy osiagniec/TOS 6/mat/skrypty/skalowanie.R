# ścieżka, opcje, biblioteki
setwd(paste0("C:\\Users\\Uzytkownik\\Documents\\SUEK\\",
             "Projects\\suek\\testy osiagniec\\TOS 6\\mat"))
wd = getwd()

options("stringsAsFactors" = FALSE)

library(TAM)
library(sirt)
library(mirt)
# funkcja do kopiowania zmiennych kotwiczących
anchor = function(df, anchor_mat) {
  for(i in 1:nrow(kotw)) {
    it_a = anchor_mat[i, 1]
    it_b = anchor_mat[i, 2]
    kit = paste0("K", it_a)
    ind_ucz_a = which(df$wersja == "A")
    ind_ucz_b = which(df$wersja == "B")
    
    df[ind_ucz_a, kit] = df[ind_ucz_a, it_a]
    df[ind_ucz_b, kit] = df[ind_ucz_b, it_b]
  }
  return(df)
}

# wczytanie baz (surowej i rekodowanej)
mat_all = read.csv2("bazy zmien\\tos6_mat.csv")
mat_all_r = read.csv2("bazy zmien\\tos6_mat_rek.csv")

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
# stworzenie zmiennych kotwiczących w bazach
mat_all[, paste0("K", kotw[, 1])] = NA
mat_all_r[, paste0("K", kotw[, 1])] = NA

# skopiowanie wartości dla zmiennych kotwiczących
mat_all = anchor(mat_all, kotw)
mat_all_r = anchor(mat_all_r, kotw)
head(mat_all_r)
head(mat_all)

# itemy do skalowania nr 1
items_all = grep("MA|MB", names(mat_all_r), value = TRUE)
items1 = items_all[!(items_all %in% kotw)]
# itemy kotwiczące
items_kotw = paste0("K", kotw[, 1])

# pierwsze skalowanie
rasch1 = tam(mat_all_r[, items1], "PCM")

# wyrysowanie wykresów dla zadań
# poniższa funkcja otwiera okno do rysowania wykresów
# umożliwia pominięcie okienka do rysowania w RStudio.
windows()

# rysuje wykresy expected score curve dla zadań
plot(rasch1)

# sprawdzenie dopasowania
tam.fit(rasch1)

# wyliczenie pv dla modelu
rasch1_pv = tam.pv(rasch1)$pv
# wyliczenie statystyk KTT
ctt1 = tam.ctt(mat_all[, items_all], pvscores = rasch1_pv[, -1])

for (it in items_all) {
  out = ctt1[ctt1$item ==  it, grep("item|group|PV|N|Categ|Freq", names(ctt1))]
  print(out, digits = 3)
}  

# sprawdzenie międzygrupowej inwariantności (DIF na wersję)

# stworzenie data.frame dla aspektóW
facets = data.frame(wersja = mat_all_r$wersja)
# formuła modelu multi-faceted rasch
dif_wer_form = ~ item + item:step + wersja + wersja*item
# policzenie modelu multi-faceted rasch
rasch_dif_wer = tam.mml.mfr(mat_all_r[, items_kotw], irtmodel = "PCM",
                            facets = facets, formulaA = dif_wer_form)

# analiza wyników
summary(rasch_dif_wer)
names(rasch_dif_wer)
# pv dla mfr
dif_wer_pv = tam.pv(rasch_dif_wer)$pv
head(dif_wer_pv)
# KTT dla mfr
ctt2 = tam.ctt(mat_all[, items_kotw], pvscores = dif_wer_pv[, -1],
        group = mat_all$wersja)
for (it in items_kotw) {
  out = ctt2[ctt2$item ==  it, grep("item|group|PV|N|Categ|Freq", names(ctt2))]
  print(out, digits = 3)
}  
