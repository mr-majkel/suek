# ścieżka, opcje, biblioteki
setwd("C:\\Users\\Uzytkownik\\Dropbox\\szkolenie R\\analizy TAM\\mat")
wd = getwd()

options("stringsAsFactors" = FALSE)

install.packages("WrightMap")

library(TAM)
library(sirt)
library(mirt)
library(WrightMap)

# funkcja do kopiowania zmiennych kotwiczących
anchor = function(df, anchor_mat) {
  for(i in 1:nrow(anchor_mat)) {
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

plec_df = read.csv2("bazy oryg\\plec.csv")
head(plec_df)
head(mat_all)

# dodanie płci
mat_all = merge(mat_all, plec_df, all.x = TRUE)
mat_all_r = merge(mat_all_r, plec_df, all.x = TRUE)

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
rasch1 = tam(mat_all_r[, items1], "PCM2", pid = mat_all_r$ID_ucz)

# podsumowanie modelu
summary(rasch1)

# wykres ze zmianami deviance w trakcie iteracji
windows()
plotDevianceTAM(rasch1)

# podejrzenie parametrów osób
head(rasch1$person)

# podejrzenie parametrów zadań
rasch1$xsi

# wyrysowanie wykresów dla zadań
# poniższa funkcja otwiera okno do rysowania wykresów
# umożliwia pominięcie okienka do rysowania w RStudio.
windows()

# rysuje wykresy expected score curve dla zadań
plot(rasch1, export = TRUE, ask = FALSE)

# sprawdzenie dopasowania
fit1 = tam.fit(rasch1)

head(cbind(rasch1$xsi, fit1))
# zapisanie parametrów zadań i miar dopasowania
write.csv2(cbind(rasch1$xsi, fit1), "bazy zmien\\skal_mat_1.csv",
           row.names = TRUE)

# wyliczenie pv dla modelu
rasch1_pv = tam.pv(rasch1)$pv
# wyliczenie statystyk KTT
tam.ctt(mat_all[, items_all])
ctt1 = tam.ctt(mat_all[, items_all], pvscores = rasch1_pv[, -1])
head(ctt1,20)
for (it in items_all) {
  out = ctt1[ctt1$item ==  it, grep("item|group|PV|N|Categ|Freq", names(ctt1))]
  print(out, digits = 3)
}  

# sprawdzenie międzygrupowej inwariantności (DIF na wersję)

# stworzenie data.frame dla aspektóW
facets = data.frame(wersja = mat_all_r$wersja)
# formuła modelu multi-faceted rasch
dif_wer_form = ~ item + item:step + wersja + wersja:item

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
head(ctt2)
for (it in items_kotw) {
  out = ctt2[ctt2$item ==  it, grep("item|group|PV|N|Categ|Freq", names(ctt2))]
  print(out, digits = 3)
}  

# baza z aspektem plec
facets_p = data.frame(plec = plec_df[, "plec"])

# formuła na DIF z płcią
dif_plec_form = ~ item + item:step + plec + item:plec

# DIF na plec
rasch_dif_plec = tam.mml.mfr(mat_all_r[, items1], irtmodel = "PCM",
                             facets = facets_p, formulaA = dif_plec_form)

# wykres ze zmianami deviance w trakcie iteracji
windows()
plotDevianceTAM(rasch_dif_plec)

summary(rasch_dif_plec)

# pv dla mfr
dif_plec_pv = tam.pv(rasch_dif_plec)$pv
head(dif_plec_pv)
# KTT dla mfr
ctt3 = tam.ctt(mat_all[, items1], pvscores = dif_plec_pv[, -1],
               group = mat_all$plec)
head(ctt3)
for (it in items1) {
  out = ctt3[ctt3$item ==  it, grep("item|group|PV|N|Categ|Freq", names(ctt3))]
  print(out, digits = 3)
}  


