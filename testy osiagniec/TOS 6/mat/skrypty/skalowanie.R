install.packages("TAM")
install.packages("sirt")

library(TAM)
library(sirt)
library(mirt)

rasch = tam(mat_a_r[, key_mat_a[["items"]]], "PCM")

plot(rasch)
tam.fit(rasch)