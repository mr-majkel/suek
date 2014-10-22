# ścieżka, opcje, biblioteki
setwd(paste0("C:\\Users\\Uzytkownik\\Documents\\SUEK\\",
             "Projects\\suek\\testy osiagniec\\TOS 6\\mat"))
wd = getwd()

options("stringsAsFactors" = FALSE)

library(TAM)
library(sirt)
library(mirt)
library(WrightMap)

# funkcja do kopiowania zmiennych kotwiczących
# df - data.frame
# anchor_mat - macierz dwukolumnowa z informacją o kotwiczących zadaniach
anchor = function(df, anchor_mat) {
  # pętla po wierszach, czyli koljenych parach zadań
  for(i in 1:nrow(anchor_mat)) {
    # nazwa zadania z wersji a
    it_a = anchor_mat[i, 1]
    # nazw zadania z wersji b
    it_b = anchor_mat[i, 2]
    # nazwa zmiennych kotwiczących
    kit = paste0("K", it_a)
    # indeksy uczniów z wersji A
    ind_ucz_a = which(df$wersja == "A")
    # indeksy uczniów z wersji B
    ind_ucz_b = which(df$wersja == "B")
    # wyciągnięcie odpowiednich wartości
    df[ind_ucz_a, kit] = df[ind_ucz_a, it_a]
    df[ind_ucz_b, kit] = df[ind_ucz_b, it_b]
  }
  return(df)
}

# wczytanie baz
mat_all = read.csv2("bazy zmien\\tos6_mat.csv")
mat_all_r = read.csv2("bazy zmien\\tos6_mat_rek.csv")

ucz_df = read.csv2("bazy oryg\\uczen_dane.csv")

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

# stworzenie zmiennych kotwiczących w bazach
mat_all[, paste0("K", kotw[, 1])] = NA
mat_all_r[, paste0("K", kotw[, 1])] = NA

# skopiowanie wartości dla zmiennych kotwiczących
mat_all = anchor(mat_all, kotw)
mat_all_r = anchor(mat_all_r, kotw)

# nazwy wszystkich itemów w bazie
items_all = grep("MA|MB", names(mat_all_r), value = TRUE)

##############################################
# Skalowanie rozpoznawcze ####################
##############################################

# nazwy wszystkich itemów do skalowania
items1 = items_all[!(items_all %in% kotw)]

# model partial credit, bez warunkowania, bez dif
mod0 = tam(mat_all_r[, items1], "PCM", pid = mat_all_r$ID_ucz)

# model 2pl, bez warunkowania, bez dif
mod0a = tam.mml.2pl(mat_all_r[, items1], pid = mat_all_r$ID_ucz)

# sprawdzenie polepszenia dopasowania dla modelu 2pl
anova(mod0, mod0a)
# model dwuparametryczny jest lepiej dopasowany do danych,
# ale czy to może dziwić na takiej próbie...

# podsumowanie modelu, zapisanie summary do plików tekstowych
summary(mod0, "modele\\mod0")
summary(mod0a, "modele\\mod0a")

# wykres ze zmianami deviance w trakcie iteracji
windows()
plotDevianceTAM(mod0)
plotDevianceTAM(mod0a)
# Deviance spadało spójnie z każdą iteracją o mniejszą wartość

# podejrzenie parametrów osób
head(mod0$person)

# podejrzenie parametrów zadań
rasch1$xsi

# wyrysowanie wykresów dla zadań
# poniższa funkcja otwiera okno do rysowania wykresów
# umożliwia pominięcie okienka do rysowania w RStudio.
windows()

# rysuje wykresy expected score curve dla zadań
# export = czy zapisać do pliku w folderze Plots
# ask = czy pytać, żeby wyświetlić następne zadanie
# ngroups = liczba grup dla wyników obserwowanych
# items = wektor liczbowy z numerami zmiennych do wyświetlenia
# overlay = czy na jednym wykresie rysować
plot(mod0, items = 1:2, export = FALSE, ask = FALSE, overlay = TRUE)
legend(1, 0.2, rownames(mod0$xsi)[1:2], lty = 1, lwd = 2,
       col = c("blue", "red"))

# sprawdzenie dopasowania
fit0 = tam.fit(mod0)

# połączenie parametrów z modelu i miar dopasowania dla zadań
mod0_fit = cbind(mod0$xsi, fit0)
# zapisanie parametrów zadań i miar dopasowania
write.csv2(mod0_fit, "bazy zmien\\mod0_fit.csv", row.names = TRUE)

# wyliczenie pv dla modelu
mod0_pv = tam.pv(mod0)$pv

# wyliczenie statystyk KTT
ctt0 = tam.ctt(mat_all[, items1], pvscores = mod0_pv[, -1])

# podejrzenie ktt
head(ctt0,20)

# oczywiście ktt można zapisać do pliku i sobie popatrzeć w excelu
write.csv2(ctt0, "bazy zmien\\ktt0.csv", row.names = FALSE)

# ale można też za pomoca inteligentnej pętli podzielić sobie output na zadania
# i wyświetlić tylko te zmienne, które nas interesują
for (it in items1) {
  out = ctt0[ctt0$item ==  it, grep("item|group|PV|N|Categ|Freq", names(ctt0))]
  print(out, digits = 3)
}

##############################################
### Sprawdzenie zadań kotwiczących ###########
###### DIF na wersję #########################
##############################################

# sprawdzenie międzygrupowej inwariantności (DIF na wersję)
# nazwy itemów kotwiczących
items_kotw = paste0("K", kotw[, 1])

# policzenie modelu multi-faceted rasch, pełna wariantność parametrów
mod1 = tam.mml.mfr(mat_all_r[, items_kotw],
                   facets = mat_all_r[, "wersja", drop = FALSE],
                   formulaA = ~ item + item:step + wersja + wersja:item,
                   control = list(QMC = FALSE,
                                  increment.factor=1.03,
                                  fac.oldxsi=.2))
# sprawdzenie deviance
windows()
plotDevianceTAM(mod1)
# widać, że estymacja modelu przebiegała z problemami, ale po wprowadzeniu
# modyfikacji działania algorytmu w żadnym momencie deviance nie rosła

# błędy standardowe oszacowań dla DIFów
tam.se(mod1)
# nie wszystkie DIFy są istotne statystycznie, większość jest tez nie wielka.
# zadania potencjalnie podejrzane
# na korzyść A: KMA_14, KMA_15, KMA_24
# na korzyść B: KMA_8, KMA_12, KMA_18, KMA_23

# model z pełną inwariantnością
mod1a = tam.mml.mfr(mat_all_r[, items_kotw],
                    facets = mat_all_r[, "wersja", drop = FALSE],
                    formulaA = ~ item + item:step,
                    control = list(QMC = FALSE))

# model z pełna inwariantnościa parametrów, ale różną średnią
mod1b = tam.mml.mfr(mat_all_r[, items_kotw],
                    facets = mat_all_r[, "wersja", drop = FALSE],
                    formulaA = ~ item + item:step + wersja,
                    control = list(QMC = FALSE))
# sprawdzenie deviance
windows()
plotDevianceTAM(mod1a)
plotDevianceTAM(mod1b)

# porównanie dopasowania dla modeli
# te same średnie i inw. vs wariantność
anova(mod1a, mod1)
# różne średnie i inw. vs. wariantność
anova(mod1b, mod1)
# model z DIFami jest tylko troszkę lepszy, nie jest to efekt różnych średnich
# poziomów umiejetności w tych dwóch grupach, ale trzeba pamiętać, że test jest
# wrażliwy na wielkość próby (która jest zawyżona w stosunku do efektywnej
# wielkości próby. Wydaje się, że póki co można traktować zadania kotwiczące
# jako inwariantne.

# przyjrzyjmy się jeszcze popularności różnych kategorii w dwóch wersjach
# pv dla modelu z pełną inwariantnością
mod1a_pv = tam.pv(mod1a)$pv


# KTT dla mfr, dif na wersję
ctt1 = tam.ctt(mat_all[, items_kotw], pvscores = mod1a_pv[, -1],
               group = mat_all$wersja)

# zapisanie ktt do pliku
write.csv2(ctt1, "bazy zmien\\ktt1.csv", row.names = FALSE)

# pętla do oglądania w konsoli
for (it in items_kotw) {
  out = ctt1[ctt1$item ==  it, grep("item|group|PV|N|Categ|Freq", names(ctt1))]
  print(out, digits = 3)
}
# różnica w odsetakch poprawnych odpowiedzi dla zadań na korzyść grupy B wynosi
# około 4-5 pp, natomiast w zadaniach na korzyść grupy A ok. 1-2 pp. To nie
# jest jakoś b.dużo.

##############################################
### Sprawdzenie inwariantności na płeć #######
###### DIF na płeć ###########################
##############################################
# Stworzenie zmiennej 0-1 dla płci
mat_all_r$kobieta = NA
mat_all_r[mat_all_r$plec == "K", "kobieta"] = 1
mat_all_r[mat_all_r$plec == "M", "kobieta"] = 0

### wielo aspektowy Rasch ####################
# formuła na DIF z płcią
dif_plec_form = ~ item + item:step + kobieta + item:kobieta

# DIF na plec
mod1 = tam.mml.mfr(mat_all_r[, items1], irtmodel = "PCM",
                             facets = mat_all_r[, "kobieta", drop = FALSE],
                             formulaA = dif_plec_form,
                             control = list(QMC = FALSE,
                                            increment.factor=1.03,
                                            fac.oldxsi=.2))
mod1a = tam.mml.mfr(mat_all_r[, items1],
                             facets = mat_all_r[, "kobieta", drop = FALSE],
                             formulaA = dif_plec_form,
                             constraint = "items",
                             control = list(QMC = FALSE,
                                            increment.factor=1.03,
                                            fac.oldxsi=.2))
mod1b = tam.mml.mfr(mat_all_r[, items1],
                             facets = mat_all_r[, "kobieta", drop = FALSE],
                             formulaA = ~ item:step + kobieta + item:kobieta,
                             control = list(QMC = FALSE,
                                            increment.factor=1.03,
                                            fac.oldxsi=.2))

# wykres ze zmianami deviance w trakcie iteracji
windows()
plotDevianceTAM(mod1)

summary(mod1, "bazy zmien\\mod1")
summary(mod1a, "bazy zmien\\mod1a")

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

# analia na podstawie przykładu w helpie
des2 = designMatrices.mfr2(resp = mat_all_r[, items1],
                            facets = mat_all_r[, "kobieta", drop = FALSE],
                            formula = ~item + item:step + kobieta +
                                       item:kobieta)
resp2 = des2$gresp$gresp.noStep
A = des2$A$A.3d
xsi.elim = des2$xsi.elim
dif_items = c(paste0("MA_", c(1,2,21,17,7)), paste0("KMA_", c(8,10,14,6,9,15,16)),
              paste0("MB_", c(10,3,5,2,21,11,15,25,20)))
xsi.elim.hand = paste0(items1[!(items1 %in% dif_items)],":kobieta")
xsi.elim.hand.ind = grep(paste(xsi.elim.hand, collapse = "|"), dimnames(A)[[3]])
A1 = A[, , -c(xsi.elim[, 2],xsi.elim.hand.ind)]
B = des2$B$B.3d
mod2 = tam.mml(resp2, A= A1, B = B, control = list(QMC = FALSE,
                                                increment.factor=1.03,
                                                fac.oldxsi=.2))
windows()
plotDevianceTAM(mod2)
summary(mod2, "bazy zmien\\mod2")
xsi1 = mod2$xsi
difxsi = xsi1[intersect(grep("kobieta", rownames(xsi1)),
                        grep("MA_10", rownames(xsi1))), ]

A1a = A1[, ,-grep("^kobieta", dimnames(A1)[[3]])]
mod2a = tam.mml(resp2, formulaY = ~ kobieta,
                dataY = mat_all_r[, "kobieta", drop = FALSE],
                A= A1a, B = B,
                control = list(QMC = FALSE, 
                               increment.factor=1.03,
                               fac.oldxsi=.2))
summary(mod2a, "bazy zmien\\mod2a")

mod2b = tam.mml(resp2, group = mat_all_r[, "kobieta"],
                A= A1a, B = B,
                control = list(QMC = FALSE, 
                               increment.factor=1.03,
                               fac.oldxsi=.2))
tam.se(mod2b)
summary(mod2b, "bazy zmien\\mod2b")

mod2c = tam.mml(mat_all_r[,items1[which(!items1%in%dif_items)]],
                group = mat_all_r[, "kobieta"],
                control = list(QMC = FALSE, 
                               increment.factor=1.03,
                               fac.oldxsi=.2))

mod2d = tam.mml(mat_all_r[,items1[which(items1%in%dif_items)]],
                group = mat_all_r[, "kobieta"],
                control = list(QMC = FALSE, 
                               increment.factor=1.03,
                               fac.oldxsi=.2))
### regresja latentna ########################


rasch_plec_lat = tam(mat_all_r[, items1], irtmodel = "PCM",
                     formulaY = ~ kobieta,
                     dataY = mat_all_r[, "kobieta", drop = FALSE],
                     pid = mat_all_r$ID_ucz,
                     #variance.fixed = matrix(c(1, 1, 1), ncol = 3),
                     control = list(QMC = FALSE))
summary(rasch_plec_lat3)
tam.se(rasch_plec_lat)$beta
tam.se(rasch_plec_lat3)$beta

### multi-group #####
rasch_plec_grup = tam(mat_all_r[, items1], group = mat_all_r[, "kobieta"])
rasch_plec_grup2 = tam(mat_all_r[, items1], group = mat_all_r[, "plec"])
summary(rasch_plec_grup, "bazy zmien\\mgroup_num")
summary(rasch_plec_grup2, "bazy zmien\\mgroup_lab")

# multi-group z mfr
mod3 = tam.mml.mfr(mat_all_r[, items1], group = mat_all_r[, "kobieta"],
           facets = mat_all_r[, "kobieta", drop = FALSE],
           formulaA = dif_plec_form,
           control = list(QMC = FALSE,
                          increment.factor=1.03,
                          fac.oldxsi=.2))
summary(mod3, "bazy zmien\\mod3")


mod4 = tam.mml.mfr(mat_all_r[, items1], group = mat_all_r[, "kobieta"],
                   facets = mat_all_r[, "kobieta", drop = FALSE],
                   formulaA = ~ item + item:step + item:kobieta,
                   control = list(QMC = FALSE,
                                  increment.factor=1.03,
                                  fac.oldxsi=.2))
summary(mod4, "bazy zmien\\mod4")
anova(mod3, rasch_plec_grup)

mean(mod4$person[mat_all_r$plec == "K", "EAP"])
mean(mod4$person[mat_all_r$plec == "M", "EAP"])


# latentna z mfr
mod5 = tam.mml.mfr(mat_all_r[, items1], formulaY = ~ kobieta,
                   dataY = mat_all_r[, "kobieta", drop = FALSE],
                   facets = mat_all_r[, "kobieta", drop = FALSE],
                   formulaA = ~ item + item:step + item:kobieta,
                   control = list(QMC = FALSE,
                                  increment.factor=1.03,
                                  fac.oldxsi=.2))
mod5a = tam.mml.mfr(mat_all_r[, items1], formulaY = ~ kobieta,
                   dataY = mat_all_r[, "kobieta", drop = FALSE],
                   facets = mat_all_r[, "kobieta", drop = FALSE],
                   formulaA = ~ item + item:step + item:kobieta,
                   constrain = "items",
                   control = list(QMC = FALSE,
                                  increment.factor=1.03,
                                  fac.oldxsi=.2))

summary(mod5, "bazy zmien\\mod5")
summary(mod5a, "bazy zmien\\mod5a")
mod5$person
library(ggplot2)
library(psych)
windows()
ggplot(data.frame(mod5$person, plec = mat_all_r$plec), aes(x=score)) + 
  geom_histogram(fill="white", colour="black") +
  facet_grid(. ~ plec)
  
  
lapply(split(data.frame(mod4$person), mat_all_r$plec), describe)
  
  describe