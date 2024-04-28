[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-24ddc0f5d75046c5622901739e7c5dd533143b0c8e959d652212380cedb1ea36.svg)](https://classroom.github.com/a/bOqsVR9s)
# Wprowadzenie do tidverse - część druga

Celem ćwiczenia jest zapoznanie się z podstawami przekształceń dataframe w tidyverse i R. 

Będziemy pracować na pakietach
```tidyverse``` oraz ```nycflights13```. 

Dostępny jest również dodatkowy plik z danymi - notowania z listy bilboardu z 2000 roku.  

Dodatkowo utworzymy dwa datasety:

```R
# Załaduj niezbędne biblioteki
library(tidyr)

# Utwórz przykładowe dane
set.seed(...) # zamiast ... wpisz sumaryczną liczbę liter z twojego imienia i nazwiska pomnżone przez 101
produkty <- c("Produkt_A", "Produkt_B", "Produkt_C")
kwartały <- c("Q1", "Q2", "Q3", "Q4")

sales_data <- data.frame(
  Produkt = rep(produkty, each = length(kwartały)),
  Q1 = sample(100:500, 12),
  Q2 = sample(100:500, 12),
  Q3 = sample(100:500, 12),
  Q4 = sample(100:500, 12)
)
```

```R
set.seed(...) # zamiast ... wpisz sumaryczną liczbę liter z twojego imienia i nazwiska pomnżone przez 101
produkty <- c("Produkt_A", "Produkt_B", "Produkt_C")
sklepy <- c("Sklep_X", "Sklep_Y", "Sklep_Z")
miesiace <- c("Styczeń", "Luty", "Marzec")

sales_data_advanced <- data.frame(
  Produkt = rep(produkty, each = length(sklepy) * length(miesiace)),
  Sklep = rep(sklepy, times = length(produkty) * length(miesiace)),
  Miesiac = rep(miesiace, each = length(produkty) * length(sklepy)),
  Sprzedaz = sample(100:500, 27),
  Koszty = sample(30:200, 27),
  Zysk = NA
)
sales_data_advanced$Zysk <- sales_data_advanced$Sprzedaz - sales_data_advanced$Koszty
```


Będziemy używać następujących funkcji 

```r
read_csv()
filter()
arrange()
mutate()
group_by()
pivot_longer()
pivot_wider()
slice_max()
```
oraz operatora pipeline ```|>``` 

Zadanie będzie polegało na utworzeniu dokumentu w quarto (```*.qmd```) i zrealizowaniu następujących zadań.
Dla każdego zadania najpierw opisz słownie ciąg operacji, które chcesz wykonać a następnie skonstruuj odpowiedni pipeline. 



### Analiza zbioru danych  `sales_data`. 
Zawiera on informacje o kwartalnych sprzedażach różnych produktów w formie szerokiej.

1. Użyj funkcji `pivot_longer()` do przekształcenia zbioru danych `sales_data` z formatu szerokiego na format długi. Wynikowy zbiór danych powinien mieć kolumny: `Produkt`, `Kwartał`, i `Sprzedaż`.

2. Stwórz tabelę podsumowującą, która pokaże całkowitą sprzedaż dla każdego produktu we wszystkich kwartałach. Możesz użyć funkcji `group_by()` i `summarize()` z pakietu `dplyr`.

3. Teraz użyj funkcji `pivot_wider()` do przywrócenia danych w formacie szerokim.

### Analiza zbioru danych `sales_data_advanced`

**Opis Sytuacji:**

Zbiór danych dotyczących wyników sprzedaży produktów w różnych sklepach przez kilka miesięcy. Zawiera on informacje o sprzedaży, kosztach, i zyskach dla różnych produktów w różnych sklepach.


1. **Filtrowanie danych:** Wybierz tylko te rekordy, gdzie zysk jest dodatni. 

2. **Sortowanie danych:** Posortuj dane według malejącego zysku, a następnie według nazw produktów rosnąco. 

3. **Tworzenie nowych kolumn:** Dodaj kolumnę `Marza_Procentowa`, która będzie zawierała procentową marżę zysku dla każdego rekordu.

4. **Agregacja danych:** Stwórz tabelę, która pokaże średnią sprzedaż, koszty i zysk dla każdego sklepu. 

5. **Pivotowanie danych:** Przekształć dane w taki sposób, aby w jednym wierszu były informacje o sprzedaży, kosztach i zysku dla danego produktu w danym sklepie w danym miesiącu. 

6. **Dodatkowe przekształcenie danych:** Dodaj nową kolumnę `Koszty_na_Sprzedaz` reprezentującą procentowy udział kosztów w sprzedaży. 

7. **Powrót do szerokiego formatu:** Przekształć dane tak, aby wrócić do pierwotnego formatu, gdzie każdy wiersz reprezentuje produkt w danym sklepie w danym miesiącu. 

8. **Zaawansowane wybieranie danych:** Wybierz rekordy z największymi kosztami na sprzedaż dla każdego produktu.

### Analiza zbioru danych `flights`
Zbiór analizowaliśmy na poprzednich ćwiczeniach.

1. Określ, który przewoźnik ma największe opóźnienia przylotu? Przeanalizuj czy opóźnienia mają związek tylko z przewoźnikiem czy też również z lotniskiem docelowym. 

2. Znajdź loty, które są najbardziej opóźnione przy odlocie z każdego lotniska.

### Analiza zbioru danych z listy Billboardu.

Zbiór danych zawiera informacje o piosenkach, które w 2000 roku znalazły się na tej liście. 

1. Wczytaj zbiór danych za pomocą funkcji `read_csv()`
2. Sporządź tabelę zawierającą wszystkie piosenki, które **spadły** z listy we wrześniu 2000. (Wskazówka: poszukaj w Google jak dodawać określony czas do daty w R. Jeżeli piszesz własną funkcję do użīcia w `mutate()` pamiętaj o wektoryzacji) 
3. Określ, która z piosenek danego gatunku była na liście najdłużej. 







