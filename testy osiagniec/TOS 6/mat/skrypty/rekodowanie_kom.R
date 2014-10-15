# W tym skrypcie opisane są poszczególne kroki rekodowania bazy surowej
# z wynikami testów matematycznych. Do analizy potrzebujemy bowiem odpowiedzi
# w formacie kategorii punktowych - 0 (błędna odp.), 1 (,2,3... kolejne progi
# poprawnych odpowiedzi) oraz NA (brak danych).

# ścieżka, opcje, biblioteki

# Funkcja 'setwd()' służy do określenia ścieżki katalogu roboczego. Ścieżkę
# zawsze podajemy w cudzysłowie. Foldery oddzielamy '\\' lub '/'. Pomimo tego,
# że w RStudio możemy wybrać katalog roboczy klikając, warto umieścić go
# w skrypcie, żeby można było skrypt z dowolnej lokalizacji wczytać. Jako
# katalog roboczy warto podawać folder nadrzędny w stosunku do podfolderów
# naszego projektu.
setwd(paste0("C:\\Users\\Uzytkownik\\Documents\\SUEK\\",
             "Projects\\suek\\testy osiagniec\\TOS 6\\mat"))
# Funkcja 'getwd()' zwraca ścieżkę katalogu roboczego. Zapisanie jej do obiektu
# pozwala na szybkie przeniesienie się do docelowego katalogu roboczego, jeżeli
# gdzieś pobłądzimy:). Żeby wczytać tak zapisaną ścieżkę używamy znów funkcji
# 'setwd(wd)'. Prawda, że krócej:)?
wd = getwd()

# To, jak R się zachowuje domyślnie, kontrolowane jest przez niezliczone opcje.
# Opcje te są określone w pliku startowym. Można je na stałe zmienić, jednakże
# nie jest to polecane, gdyż wiele funkcji bazuje na domyślnych wartościach
# opcji. I choć większość opcji jest tak ustawiona, że nie sprawia problemów,
# to jest jedna, która wielu doprowadza do szewskiej pasji.
# Domyślnie wszelkie wektory tekstowe są wczytywane jako factory. Czym są
# faktory? Odpowiedź na zajęciach lub w pomocy - '?factor'. Dość powiedzieć,
# że jest to specyficzne połączenie wektora tekstowego z liczbowym, które nie
# rzuca się w oczy, ale potrafi napsuć trochę krwi, jeżeli funkcja oczekuje
# np. wektora tekstowego.
options("stringsAsFactors" = FALSE)

# Funkcja source() pozwala na wczytanie całych skryptów. W tych poniżej
# zapisane są funkcje pomocne w rekodowaniu. W związku z tym, że są to funkcje
# zdefiniowane przez użytkownika (a nie w pakiecie) nie działa na nich funkcja
# 'help()'. Jak używać poszczególnych funkcji opsiane jest w samych plikach.
source("skrypty\\pomocnicze\\getKey.R")
source("skrypty\\pomocnicze\\reformat.R")
source("skrypty\\pomocnicze\\getItems.R")
source("skrypty\\pomocnicze\\getValues.R")
source("skrypty\\pomocnicze\\writeKeySingle.R")
source("skrypty\\pomocnicze\\writeKey.R")
source("skrypty\\pomocnicze\\readKey.R")
source("skrypty\\pomocnicze\\recode.R")

# wczytanie baz
# R potrafi wczytać pliki w większości formatów z różnych pakietów
# statystycznych (.sav, .dat) czy Excela (.xls, .xlsx). Jednakże do tego
# potrzebne jest wczytanie odpowiednich pakietów. Czym są pakiety i jak
# się je instaluje i wczytuje? O tym w następnej części warsztatów.
mat_a = read.csv2("bazy oryg\\SUEK7_TOS6 Matematyka A.csv")
mat_b = read.csv2("bazy oryg\\SUEK7_TOS6 Matematyka B.csv")

# wczytanie codebooków
# Są potrzebne do funkcji tworzenia klucza do rekodowania
code_mat_a = read.csv2("bazy oryg\\SUEK7_TOS6 Matematyka A popr20140808.csv")
code_mat_b = read.csv2("bazy oryg\\SUEK7_TOS6 Matematyka B popr20140808.csv")

# dodanie zmiennej na określenie wersji
mat_a$wersja = "A"
mat_b$wersja = "B"

# poprawne kody do zadań zamkniętych
# Są potrzebne do funkcji tworzenia klucza do rekodowania
# wersja A
key_a = c(2,4,3,1,3,1,3,3,3,2,3,1,3,2,3,3,2)
# wersja B
key_b = c(3,4,3,4,3,3,2,2,3,2,2,3,1,3,4,3,2)

# stworzenie klucza do rekodowania
# Używamy funkcji 'getKey()', która na podstawie codebooka i określonych przez
# nas kluczy tworzy listę wykorzystywaną do rekodowania.
# wersja A
key_mat_a = getKey(code_mat_a, "MA", c("1","2"), key_a)
# wersja B - todo
key_mat_b = getKey(code_mat_b, "MB", c("1","2"), key_b)

# zapisanie klucza
# Funkcja 'writeKey()' zapisuje klucz w wersji edytowalnej (.csv oddzielany
# średnikami). Możemy go potem otworzyć w Excelu i zmodyfikować ręcznie,
# np. po to, żeby zdefiniować dwie poprawne odpowiedzi w zadaniu zamkniętym,
# lub żeby określić, które kategorie w zadaniu otwartym mają być kodowane na
# 1, a które na 2.
writeKey(key_mat_a, "bazy zmien\\klucz_A.csv")
writeKey(key_mat_b, "bazy zmien\\klucz_B.csv")

# wczytanie klucza
# Funkcja 'readKey()' wczytuje klucz do R.
key_mat_a = readKey("bazy zmien\\klucz_A.csv")
key_mat_b = readKey("bazy zmien\\klucz_B.csv")

# zrekodowanie bazy surowej
# To możemy już rekodować. Funkcja 'recode' wykorzystując informację z listy
# do rekodowania przetwarza wybrane zmienne (itemy) w bazie i zwraca taką samą
# bazę, ale ze zrokodowanymi zmiennymi.
mat_a_r = recode(mat_a, key_mat_a)
mat_b_r = recode(mat_b, key_mat_b)

# zapisanie bazy rekodowanej
# Przetworzyliśmy już bazy - są gotowe do analizy. Skoro tak, to trzeba je
# zapisać w jakimś pliku (np. po to, żebyśmy mogli go przekazać innym wraz
# ze skryptem do analiz).
write.csv2(mat_a_r, "bazy zmien\\tos6_mat_a_rek.csv", row.names = FALSE)
write.csv2(mat_b_r, "bazy zmien\\tos6_mat_b_rek.csv", row.names = FALSE)

# połączenie baz dla dwóch wersji
# Do połączenia dwóch data.frame'ów używamy funkcji merge().
mat_all = merge(mat_a, mat_b, all = TRUE)
mat_all_r = merge(mat_a_r, mat_b_r, all = TRUE)

# zapisanie połączonych baz
write.csv2(mat_all, "bazy zmien\\tos6_mat.csv", row.names = FALSE)
write.csv2(mat_all_r, "bazy zmien\\tos6_mat_rek.csv", row.names = FALSE)

# w następnej części analizy!!!
