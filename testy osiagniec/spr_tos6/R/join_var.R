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
#' @param na_rm.y logical, czy usunąć niekompletne wiersze z y? Domyślnie FALSE.
#' @param auto  logical, czy automatycznie nadpisywać jednoznaczne trafienia i
#' przechodzić dalej dla niejednoznacznych? Nie wyświetla komunikatów w konsoli.
#' Domyślnie TRUE.
#' @param log_op character, specyfikuje czy warunek wyszukiwania ma bazować na
#' koniunkcji ("AND", domyślnie), czy na alternatywie ("OR") wyszukiwanych
#' wektorów wartości.
#'
join_var = function(x, y, askBy.x, askBy.y=askBy.x, joinVar, na_rm.y = FALSE,
                    auto = TRUE, log_op = "AND") {
  # sprawdż poprawność argumentów
  miss_args = c(missing(x), missing(y), missing(askBy.x), missing(joinVar))
  if (any(miss_args)) {
    err_msg = c("bazy x", "bazy y", "klucza do łączenia (askBy.x)",
                paste0("zmiennej, której wartości ",
                       "mają być dołączone do zbioru x (joinVar)!"))
    stop("Nie podano:\n", paste(err_msg[miss_args], collapse = "\n"))
  }
  
  if (length(joinVar) != 1) {
    stop("Można przyłączyć tylko jedną zmienną (joinVar)!")
  }
  
  if (length(askBy.x) != length(askBy.y)) {
    stop("Zadeklarowano różną liczbę zmiennych w kluczach (askBy.x, askBy.y)")
  }
  
  if(any(!(askBy.x %in% names(x))) || any(!(askBy.y %in% names(y)))) {
    stop("Zadeklarowany klucz do łączenia",
         "nie występuje w przynajmniej jednej z baz")
  }
  
  # liczba zmiennych tworzących klucz
  ask_ln = length(askBy.x)
  
  # Nazwa nowej zmiennej
  new_var = paste0(joinVar, "_new")
  # Sprawdza, czy nowa zmienna jest już w x
  if (new_var %in% names(x)) { 
    if(!auto) {
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
    } else {
      filled_cases = which(complete.cases(x[, new_var]))
    }
    # Jeśli nie ma, to tworzy nową zmienną
  } else {
    x[, new_var] = NA
    filled_cases = 0
  }
  
  # zmiana kolumn w y na charactery
  y[, askBy.y] = as.data.frame(lapply(y[, askBy.y, drop = FALSE],
                                                    as.character),
                               stringsAsFactors = FALSE)
  # usunięcie braków danych z y
  if(na_rm.y) {
    na_ind = complete.cases(y[, askBy.y])
    y = y[na_ind, ]
  }
  
  # Interaktywna pętla po wierszach w x
  for (i in 1:nrow(x)) {    
    # Wyświetla postęp
    if(!auto) {
      cat("\nWiersz", i, "/", nrow(x),"(",
          round(i/nrow(x)*100),"%)","\n") 
    }
    # Sprawdza, czy wiersz należy ominąć
    if (i %in% filled_cases) {
      if(!auto) {
        cat("\nWiersz posiada już wartość na zmiennej", new_var,
            ". Przechodzę do następnego wiersza.\n")
      }
      next
    }
    # określenie wierszy w y pasujących do wiersza z x
    
    search_term = as.list(x[i, askBy.x, drop = FALSE])   # poszukiwana wartość
    searched_list = as.list(y[, askBy.y, drop = FALSE])  # lista wektorów w y
    result_list = Map(`==`, search_term, searched_list)  # lista z trafieniami
    if (log_op == "AND") {
      query_ind = unlist(Reduce(`&`, result_list))
    } else if(log_op == "OR") {
      query_ind = unlist(Reduce(`|`, result_list))
    }
    query_result = y[query_ind, ]

    nqr = nrow(query_result)    # liczba wierszy w wyniku
    # Brak pasujących wierszy w bazie y
    if(nqr < 1) {
      repeat {
        if (!auto) {
          cat("\nNie ma wiersza, dla którego", askBy.y, "wynosi (odpowiednio)",
              unlist(search_term), "!!!\n")
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
#       cat("\nDla wiersza ", i,
#           " znaleziono tylko jedną pasującą wartość w bazie",
#           as.character(quote(y)), ". Nadpisano automatycznie.\n")
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
