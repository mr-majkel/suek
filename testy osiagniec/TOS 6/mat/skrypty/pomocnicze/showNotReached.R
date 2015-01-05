# funkcja do wstawiania not-reached
showNotReached = function(dataframe, tasks = NULL, missing_code = 9,
                          replace_with = "N") {
  # tworzy kopię zbioru danych
  dataframe2 = dataframe
  # wybranie zadań
  if(is.null(tasks)) {
    tasks = names(dataframe)
  } else if(length(tasks) == 1) {
    tasks = grep(tasks, names(dataframe), value = TRUE)
  }
  df_tasks = dataframe[, tasks]
  missed_tasks = integer()
  # przegląda wiersze
  for (r in 1:nrow(dataframe)) {
    row = df_tasks[r, ]
    n_miss = sum(row == missing_code)
    # nie ma w ogóle pominiętych zadań
    if (n_miss == 0) {
      dataframe2[r, tasks] = row
      missed_tasks = c(0, missed_tasks)
    } else {
      # indeksy pominiętych zadań
      miss_ind = integer()
      for(i in length(row):1) {
        if (row[i] == missing_code) {
          miss_ind = c(i, miss_ind)
        } else {
          break
        }
      }
      # missed_tasks = c(length(miss_ind), missed_tasks)
      # podmienia kody na zastępnik
      miss_ind = miss_ind[-1]
      row[miss_ind] = replace_with
      dataframe2[r, tasks] = row
    }
  }
  #table(missed_tasks)
  dataframe2
}

# # test
# mat_b2 = showNotReached(dataframe = mat_b, tasks = "MB")
# head(mat_b == mat_b2, 100)
# mat_b2[59, ]
