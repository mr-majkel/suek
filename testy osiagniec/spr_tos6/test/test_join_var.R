# testy do join_var
setwd("~/SUEK/Projects/suek/testy osiagniec/spr_tos6")
source.with.encoding("R/join_var.R", encoding = "UTF8")


dfx = data.frame(a = letters[1:20], b = 1:20,
                 c = c(rep(letters[1:10], 2)), stringsAsFactors = FALSE)
dfy = data.frame(a = c(letters[1:19], NA), b = c(5,6,3:20),
                 c = c(rep(letters[1:10], 2)), stringsAsFactors = FALSE)

# test 1
j1 = join_var(dfy, dfy, c("a"), joinVar = "b", na_rm.y = FALSE)[, "b_new"]
w1 = rep(NA, 20)

identical(j1, w1)

# test 2
j2 = join_var(dfy, dfy, c("a"), joinVar = "b", na_rm.y = TRUE)[, "b_new"]
w2 = c(5, 6, 3:19, NA)

identical(j2, w2)

# test 3
j3 = join_var(dfy, dfx, c("a", "b"), joinVar = "b", na_rm.y = FALSE)[, "b_new"]
w3 = as.character(c(NA, NA, 3:19, NA))

identical(j3, w3)

# test 4
j4 = join_var(dfx, dfy, c("b"), joinVar = "b", na_rm.y = FALSE)[, "b_new"]
w4 = as.character(c(NA, NA, 3:4, NA, NA, 7:20))

identical(j4, w4)

# test 5
j5 = join_var(dfx, dfy, c("a", "b"), joinVar = "b", na_rm.y = FALSE)[, "b_new"]
w5 = as.character(c(NA, NA, 3:19, NA))

identical(j5, w5)
