# zakładam, że mat_all to baza nierekodowana, a mat_all_r to rekodowana,
# a także, ze mod_ost to ostateczny model wyliczony dla znalezienia oszacowań
# parametrów zadań

### Krok pierwszy: przygotowanie baz bez NA pod koniec zeszytów

# sprawdzenie, czy w case'y są tak samo poukladane w bazie (powinny być,
# ale...;) )
any(mat_all$ID_ucz != mat_all_r$ID_ucz)
# powinno być FALSE

# głównym problemem jest istnienie NA pod koniec zeszytów w bazie mat_all_r
# podmieńmy je na zera
# prosta funkcja do rekodowania "zwrotnego"
# funkcja niepodmienia eNek w zadaniach, które nie występują w bazie
# nierekodowanej (zostawia je jak są), dlatego warto rozpłciowić zadania w bazie
# nierekodowanej w taki sam sposób, jak w rekodowanej.

recode_back = function(df_r, df, item = "^[KC]", code = "N") {
  zad_df_r = df_r[, grep(item, names(df_r))]
  zad_df = df[, grep(item, names(df))]
  
  zad_df_r2 = as.data.frame(lapply(names(zad_df_r), function(nm) {
    if(nm %in% names(zad_df)) {
      zad_df_r[which(zad_df[, nm] == code), nm] = 0      
    }
    zad_df_r[, nm]
    
  }), stringsAsFactors = FALSE)
  
  df_r[, grep(item, names(df_r))] = zad_df_r2
  df_r
}

mat_all_r2 = recode_back(mat_all_r, mat_all, item = "^[KC]")

### Krok drugi: wyekstrahowanie macierzy z oszacowaniami z mod_ost
xsi_fixed = cbind(1:length(mod_ost$xsi$xsi),
                  mod_ost$xsi$xsi)

### Krok trzeci: policzenie średnich klasowych
mod_mean = tam(mat_all_r2[, zadania_do_skal], xsi.fixed = xsi_fixed,
               formulaY = ~ kobieta,
               dataY = mat_all_r2[, "kobieta", drop = FALSE])

# dołączenie oszacowań do uczniów z informacją o klasie i szkole
osz_ucz = data.frame(mat_all_r2[, grep("ID_ucz|ID_szk|oddz_6|kobieta",
                                        names(mat_all_r2))],
                     wyn = mod_mean$person$EAP) 
# policzenie średniej dla klas
osz_kl_mn = plyr::ddply(osz_ucz, c("ID_szk", "oddz_6"),
                         summarize, srednia_kl = mean(wyn))
# dodanie średniej do zbioru danych
osz_ucz2 = merge(osz_ucz, osz_kl_mn, all.x = TRUE)

### Krok czwarty: policzenie wyników z uwzględnieniem średniej klasowej
mod_ost_mn = tam(mat_all_r2[, zadania_do_skal], xsi.fixed = xsi_fixed,
                 formulaY = ~ kobieta + srednia_kl,
                 dataY = osz_ucz2,
                 pid = mat_all_r2$ID_ucz)

# policzenie 5 PV
pv_ucz = tam.pv(mod_ost_mn, 5)$pv

# dołączenie PV do wyników EAP i bł.st.
wyn_ucz = merge(mod_ost_mn$person, pv_ucz, all.x = TRUE)

# zapisanie zbioru
write.csv2("bazy zmien/TOS6_wyniki_czyt.csv", row.names = FALSE)
