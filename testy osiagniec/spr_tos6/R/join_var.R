#' Interaktywnie dołącza zmienną do data.frame'a.
#' 
#' \code{join_var} pozwala na interaktywne przyłączenie zmiennej z data.frame
#' \code{y} do \code{x}. Nazwy zmiennych \code{askBy.x} i \code{askBy.y} pozwalają
#' na znalezienie wierszy w \code{y}, dla których \code{askBy.y} jest identyczne z
#' \code{askBy.x} w \code{x}. Całe wiersze (tj. wszystkie zmienne) z \code{y},
#' które pasują są następnie wyświetlane w konsoli z prośbą o dokonanie wskazania
#' wiersza, z którego wartość zmiennej \code{joinVar} zostanie dodana do \code{x}
#' w kolumnie o nazwie będącej wynikiem \code{paste0(joinVar, "_new")}.
#'
#' @param x data.frame, do którego chcemy dodać nową zmienną.
#' @param y data.frame, z którego wyszukujemy wartość.
#' @param askBy.x character z nazwą zmiennej w bazie x, po której wyszukujemy w y.
#' @param askBy.y character z nazwą zmiennej w bazie y, która odpowiada askBy.x.
#' @param joinVar character z nazwą zmiennej w bazie y, której wartość ma być wstawiona z y do x.
#' @param auto  logical, czy automatycznie nadpisywać jednoznaczne trafienia i
#' przechodzić dalej dla niejednoznacznych? Domyślnie TRUE.
#'
#'
join_var = function(x, y, askBy.x, askBy.y=askBy.x, joinVar, auto = TRUE) {
  # Nazwa nowej zmiennej
  new_var = paste0(joinVar, "_new")
  # Sprawdza, czy nowa zmienna jest już w x
  if (new_var %in% names(x)) { 
    repeat {
      cat("\n", new_var, "znajduje się już w bazie.",
          "Czy chcesz nadpisać wartości zmiennej (t/n)?\n")
      choice0 = readline(">>> ")
      # Nadpisuje wiersze z wartością na nowej zmiennej
      if (choice0 == "t"){
        x[, new_var] = NA
        filled_cases = 0
        break
        # Pomija wiersze z wartością na nowej zmiennej
      } else if (choice0 == "n") {
        filled_cases = which(complete.cases(x[, new_var]))
        break
        # Błędny input
      } else {
        cat("\n", choice0, "nie jest rozpoznawanym wyborem.\n")
      }
    }
    # Jeśli nie ma, to tworzy nową zmienną
  } else {
    x[, new_var] = NA
    filled_cases = 0
  }
  
  # Interaktywna pętla po wierszach w x
  for (i in 1:nrow(x)) {    
    # Wyświetla postęp
    cat("\nWiersz", i, "/", nrow(x),"(",
        round(i/nrow(x)*100),"%)","\n") 
    # Sprawdza, czy wiersz należy ominąć
    if (i %in% filled_cases) {
      cat("\nWiersz posiada już wartość na zmiennej", new_var,
          ". Przechodzę do następnego wiersza.\n")
      next
    }
    search_term = x[i, askBy.x]   # poszukiwana wartość
    query_result = y[y[, askBy.y] == search_term, ] # pasujące wiersze z y
    nqr = nrow(query_result)    # liczba wierszy w wyniku
    
    # Brak pasujących wierszy w bazie y
    if(nqr < 1) {
      repeat {
        cat("\nNie ma wiersza, dla którego", askBy.y, "wynosi",
            search_term, "!!!\n")
        if(!auto) {
          cat("\nWpisz q, żeby wyjść",
              "lub wpisz n, żeby przejść do następnego wiersza.\n")
          choice1 = readline(">>> ")
          if (choice1 %in% c("q", "n")){ # wyjście z pętli
            break
          } else {
            cat("\n", choice1, "nie jest rozpoznawanym wyborem.\n")
          }
        } else {
          break
        }
      }
      if (auto) {
        next
      }
      if (choice1 %in% c("n")){
        next
      } else if (choice1 == "q"){
        break
      }
      # jeden wiersz w bazie y pasuje        
    } else if (nqr == 1 && auto == TRUE) {
      row.names(query_result) = 1:nqr # nazwy wierszy do pokazania
      # wartość wybrana
      result_value = query_result[1, joinVar]   
      # Dodaje nową wartość do x
      x[i, new_var] = result_value    
      cat("\nDla wiersza ", i,
          " znaleziono tylko jedną pasującą wartość w bazie",
          as.character(quote(y)), ". Nadpisano automatycznie.\n")
      next
      # więcej niż jeden pasuje
    } else if (!auto) {
      row.names(query_result) = 1:nqr # nazwy wierszy do pokazania
      repeat {
        cat(paste0(rep("\n", 2)))
        # Wyświetla wiersz z x
        print(x[i, ]) 
        cat("\nKtóry wiersz pasuje?",
            "Podaj numer wiersza (od 1 do", nqr, ").",
            "\nWpisz q, żeby wyjść",
            "lub wpisz n, żeby przejść do następnego wiersza.\n")
        cat(paste0(rep("\n", 2)))
        flush.console() # wyrzuca printy do konsoli w nieinteraktywnej pętli
        
        # Wyświetla pasujące wiersze
        print(query_result)
        
        # Prosi użytkownika o wskazanie wiersza
        choice2 = readline(">>> ")
        # Poprawne wskazanie wiersza
        if (choice2 %in% as.character(1:nqr)) { 
          # wartość wybrana
          result_value = query_result[as.numeric(choice2), joinVar]   
          # Dodaje nową wartość do x
          x[i, new_var] = result_value    
          break
          # Wyjście z pętli pytającej
        } else if (choice2 %in% c("q", "n")) { 
          break
          # Błędny input
        } else {
          cat("\n", choice2, "nie jest rozpoznawanym wyborem.\n")
        }
      }
      # Przechodzi do następnego wiersza w x
      if (choice2 == "n") { 
        next
        # Kończy działanie funkcji i zwraca dotychczasowy wynik
      } else if (choice2 == "q") {
        break
      }
    } else {
      next
    }
  }
  return(x)
}
