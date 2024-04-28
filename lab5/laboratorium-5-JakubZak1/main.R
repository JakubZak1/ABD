# NIE EDYTOWAĆ *****************************************************************
dsn_database = "wbauer_adb_2023"   # Specify the name of  Database
dsn_hostname = "pgsql-196447.vipserv.org"  # Specify host name 
dsn_port = "5432"                # Specify your port number. 
dsn_uid = "wbauer_adb"         # Specify your username. 
dsn_pwd = "adb2020"        # Specify your password.

library(DBI)
library(RPostgres)
library(testthat)

con <- dbConnect(Postgres(), dbname = dsn_database, host=dsn_hostname, port=dsn_port, user=dsn_uid, password=dsn_pwd) 
# ******************************************************************************

film_in_category <- function(category)
{
  # Funkcja zwracająca wynik zapytania do bazy o tytuł filmu, język, oraz kategorię dla zadanego:
  #     - id: jeżeli categry jest integer
  #     - name: jeżeli category jest character, dokładnie taki jak podana wartość
  # Przykład wynikowej tabeli:
  # |   |title          |languge    |category|
  # |0	|Amadeus Holy	|English	|Action|
  # 
  # Tabela wynikowa ma być posortowana po tylule filmu i języku.
  # 
  # Jeżeli warunki wejściowe nie są spełnione to funkcja powinna zwracać wartość NULL.
  # 
  # Parameters:
  # category (integer,character): wartość kategorii po id (jeżeli typ integer) lub nazwie (jeżeli typ character)  dla którego wykonujemy zapytanie
  # 
  # Returns:
  # DataFrame: DataFrame zawierający wyniki zapytania
  if(typeof(category) == "integer"){
    query <- paste("
            SELECT
                film.title, language.name as languge, category.name as category
              FROM
                film
              INNER JOIN language
                ON film.language_id = language.language_id
              INNER JOIN film_category
                ON film.film_id = film_category.film_id
              INNER JOIN category
                ON film_category.category_id = category.category_id
              WHERE category.category_id = ", category, 
              "ORDER BY film.title, language.name", sep=""
    )
  } else if(typeof(category) == "character"){
    query <- paste("
            SELECT
              film.title, language.name as languge, category.name as category
            FROM
              film
            INNER JOIN language
              ON film.language_id = language.language_id
            INNER JOIN film_category
              ON film.film_id = film_category.film_id
            INNER JOIN category
              ON film_category.category_id = category.category_id
            WHERE category.name = '", category, 
                   "' ORDER BY film.title, language.name", sep=""
    )
  } else{
    return(NULL)
  }
  df <- dbGetQuery(con, query)
  return(df)
}


film_in_category_case_insensitive <- function(category)
{
  #  Funkcja zwracająca wynik zapytania do bazy o tytuł filmu, język, oraz kategorię dla zadanego:
  #     - id: jeżeli categry jest integer
  #     - name: jeżeli category jest character
  #  Przykład wynikowej tabeli:
  #     |   |title          |languge    |category|
  #     |0	|Amadeus Holy	|English	|Action|
  #     
  #   Tabela wynikowa ma być posortowana po tylule filmu i języku.
  #     
  #     Jeżeli warunki wejściowe nie są spełnione to funkcja powinna zwracać wartość NULL.
  
  #   Parameters:
  #   category (integer,str): wartość kategorii po id (jeżeli typ integer) lub nazwie (jeżeli typ character)  dla którego wykonujemy zapytanie
  #
  #   Returns:
  #   DataFrame: DataFrame zawierający wyniki zapytania
  if(typeof(category) == "integer"){
    query <- paste("
              SELECT
                film.title, language.name as languge, category.name as category
              FROM
                film
              INNER JOIN language
                ON film.language_id = language.language_id
              INNER JOIN film_category
                ON film.film_id = film_category.film_id
              INNER JOIN category
                ON film_category.category_id = category.category_id
              WHERE category.category_id = ", category, 
              "ORDER BY film.title, language.name", sep=""
    )
  } else if(typeof(category) == "character"){
    query <- paste("
            SELECT
              film.title, language.name as languge, category.name as category
            FROM
              film
            INNER JOIN language
              ON film.language_id = language.language_id
            INNER JOIN film_category
              ON film.film_id = film_category.film_id
            INNER JOIN category
              ON film_category.category_id = category.category_id
            WHERE category.name = INITCAP('", category, 
            "') ORDER BY film.title, language.name", sep=""
    )
  } else{
    return(NULL)
  }
  df <- dbGetQuery(con, query)
  return(df)
}


film_cast <- function(title)
{
  # Funkcja zwracająca wynik zapytania do bazy o obsadę filmu o dokładnie zadanym tytule.
  # Przykład wynikowej tabeli:
  #     |   |first_name |last_name  |
  #     |0	|Greg       |Chaplin    | 
  #     
  # Tabela wynikowa ma być posortowana po nazwisku i imieniu klienta.
  # Jeżeli warunki wejściowe nie są spełnione to funkcja powinna zwracać wartość NULL.
  #         
  # Parameters:
  # title (character): wartość id kategorii dla którego wykonujemy zapytanie
  #     
  # Returns:
  # DataFrame: DataFrame zawierający wyniki zapytania
  if(typeof(title) != "character"){
    return(NULL)
  }
  query <- paste("
                SELECT
                  actor.first_name, actor.last_name
                FROM
                  actor
                INNER JOIN film_actor
                 ON actor.actor_id = film_actor.actor_id
                INNER JOIN film
                 ON film_actor.film_id = film.film_id
                WHERE film.title = '", title,
                "' ORDER BY actor.last_name ASC, actor.first_name ASC", sep=""
                )
  df <- dbGetQuery(con, query)
  return(df)
}


film_title_case_insensitive <- function(words)
{
  # Funkcja zwracająca wynik zapytania do bazy o tytuły filmów zawierających conajmniej jedno z podanych słów z listy words.
  # Przykład wynikowej tabeli:
  #     |   |title              |
  #     |0	|Crystal Breaking 	| 
  #     
  # Tabela wynikowa ma być posortowana po nazwisku i imieniu klienta.
  # 
  # Jeżeli warunki wejściowe nie są spełnione to funkcja powinna zwracać wartość NULL.
  #         
  # Parameters:
  # words(list[character]): wartość minimalnej długości filmu
  #     
  # Returns:
  # DataFrame: DataFrame zawierający wyniki zapytania
  # 
  if(typeof(words) != "character"){
    return(NULL)
  }
  string <- ""
  for(word in words){
    string <- paste(string, "LOWER(title) LIKE LOWER('", word, " %') OR LOWER(title) LIKE LOWER('% ", word, "') OR ", sep="")
  }
  string <- substring(string, 1, nchar(string)-4)
  query <- paste("
                 SELECT
                  film.title
                 FROM
                  film
                 WHERE ", string, sep=""
                 )
  df <- dbGetQuery(con, query)
  return(df)
}


# NIE EDYTOWAĆ *****************************************************************
test_dir('tests/testthat')
# ******************************************************************************


# zadania

# zad 1
query1 <- "SELECT
            country.country
          FROM
            country
          WHERE country.country ~ '^P.+$'"
df1 <- dbGetQuery(con, query1)
df1

# zad 2
query2 <- "SELECT
            country.country
          FROM
            country
          WHERE country.country ~ '^P.+s$'"
df2 <- dbGetQuery(con, query2)
df2

# zad 3
query3 <- "SELECT
            film.title
          FROM
            film
          WHERE film.title ~ '^.*[0-9].*$'"
df3 <- dbGetQuery(con, query3)
df3

# zad 4
query4 <- "SELECT
            staff.first_name, staff.last_name
          FROM 
            staff
          WHERE staff.first_name ~ '^.* .*$' OR staff.first_name ~ '^.*-.*$'
            OR staff.last_name ~ '^.* .*$' OR staff.last_name ~ '^.*-.*$'"
df4 <- dbGetQuery(con, query4)
df4

# zad 5
query5 <- "SELECT
            actor.last_name
          FROM
            actor
          WHERE actor.last_name ~ '^(P|C).{4}$'"
df5 <- dbGetQuery(con, query5)
df5

# zad 6
df6 <- film_title_case_insensitive(c("Trip", "Alone"))
df6

# zad 7

# Zapytanie "select first_name from actor where first_name ~ '^Al[a:z,1:9]*'"
# znajduje aktorów, których imię zaczyna się na "Al", po czym występuje dowolnie
# długi ciąg złożony ze znaków "a", "z", ",", ":", "1" oraz "9". Gdyby 
# wyrażenie regularne wyglądało tak: '^Al[a-z1-9]*', to zapytanie znajdowałoby
# aktorów, których imię zaczyna się na "Al", po czym występowałby dowolny ciąg 
# znaków a-z, "," i 1-9.


# Zapytanie "select first_name from actor where first_name ~* '^al[a:z,1:9]*'"
# robi to samo, poza tym, że ignoruje wielkość początkowych znaków "al".