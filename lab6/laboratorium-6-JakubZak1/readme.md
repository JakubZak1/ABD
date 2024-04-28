[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-24ddc0f5d75046c5622901739e7c5dd533143b0c8e959d652212380cedb1ea36.svg)](https://classroom.github.com/a/go9zWT0g)
# Wprowadzenie do tidverse

Celem ćwiczenia jest zapoznanie się z podstawami składni używanej w tidyverse. 

Będziemy pracować na pakietach
```tidyverse``` oraz ```nycflights13```. 
Pierwszy z nich zawiera zestaw potrzebnych nam narzędzi drugi zaś zbiory danych, które potrzebujemy do ćwiczenia. 

Będziemy używać następujących funkcji z base R oraz pakietu ```dyplr``` (ładowanego razem z tidyverse)

```r
glimpse()
filter()
arrange()
distinct()
count()
```
oraz operatora pipeline ```|>``` 

Będziemy pracować na tabeli ```flights```. 
Zadanie będzie polegało na utworzeniu dokumentu w quarto (```*.qmd```) i zrealizowaniu następujących zadań.

1. Korzystając z dokumentacji pakietu i podglądając zawartość tabeli opisać co znaczą wszystkie zmienne oraz jakich są typów. Wyjaśnić też co oznaczają poszczególne kody.
2. Za pomocą pojedynczego pipelina na każdy warunek, znajdź wszystkie loty, które:
   - Miały opóźnienie o co najmniej 2 godziny.
   - Leciały do Huston (IAH lub HOU)
   - Były z lini lotniczych United, American, lub Delta
   - Wylatywały w lecie (Lipiec, Sierpień, Wrzesień)
   - Miały opóźnienie o co najmniej dwie godziny ale nie miały opóźnienia odlotu. 
   - Miały co najmniej godzinę opóźnienia ale odrobiły ponad 30 minut w trakcie lotu.
   - Posortuj loty by znaleźć loty z najdłuższym opóźnieniem odlotu. Znajdź loty, który odlatywały najwcześniej rano.
3. Posortuj loty by znaleźć najszybsze połączenia. Wskazówka: Wykorzystać obliczenia matematyczne wewnątrz funkcji.
4. Czy każdego dnia 2013 odbył się co najmniej jeden lot?
5. Które loty pokonały największy dystans a które najkrótszy (top i bottom 10)
6. Czy kolejność używania ```filter()``` i ```arrange()``` ma znaczenie jeśli używasz obydwu? Dlaczego tak/nie? Weź pod uwagę zarówno rezultaty jak też i zużycie zasobów. 




