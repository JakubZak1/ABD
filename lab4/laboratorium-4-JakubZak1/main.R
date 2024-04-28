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

film_in_category<- function(category_id)
{
  # Funkcja zwracająca wynik zapytania do bazy o tytuł filmu, język, oraz kategorię dla zadanego id kategorii.
  # Przykład wynikowej tabeli:
  # |   |title          |language    |category|
  # |0	|Amadeus Holy	|English	|Action|
  # 
  # Tabela wynikowa ma być posortowana po tylule filmu i języku.
  # 
  # Jeżeli warunki wejściowe nie są spełnione to funkcja powinna zwracać wartość NULL
  # 
  # Parameters:
  # category_id (integer): wartość id kategorii dla którego wykonujemy zapytanie
  # 
  # Returns:
  # DataFrame: DataFrame zawierający wyniki zapytania
  # 
  if(typeof(category_id) != "integer") {
    return(NULL)
  }

  query <- paste(
            "SELECT
              title, language.name AS language, category.name AS category
            FROM
              category
            INNER JOIN film_category
              ON category.category_id = film_category.category_id
            INNER JOIN film
              ON film_category.film_id = film.film_id
            INNER JOIN language
              ON film.language_id = language.language_id
            WHERE category.category_id = ", category_id,
            "ORDER BY
              title ASC,
              language ASC", sep="")
  df <- dbGetQuery(con, query)
  return(df)
}


number_films_in_category <- function(category_id){
  #   Funkcja zwracająca wynik zapytania do bazy o ilość filmów w zadanej kategori przez id kategorii.
  #     Przykład wynikowej tabeli:
  #     |   |category   |count|
  #     |0	|Action 	|64	  | 
  #     
  #     Jeżeli warunki wejściowe nie są spełnione to funkcja powinna zwracać wartość NULL.
  #         
  #     Parameters:
  #     category_id (integer): wartość id kategorii dla którego wykonujemy zapytanie
  #     
  #     Returns:
  #     DataFrame: DataFrame zawierający wyniki zapytania
  if(typeof(category_id) != "integer") {
    return(NULL)
  }
  
  query <- paste(
            "SELECT 
              category.name AS category, COUNT(film.title) AS count
            FROM
              category
            INNER JOIN film_category
              ON category.category_id = film_category.category_id
            INNER JOIN film
              ON film_category.film_id = film.film_id
            WHERE category.category_id = ", category_id,
            "GROUP BY 
              category.name", sep=''
  )
  df <- dbGetQuery(con, query)
  return(df) 
}


number_film_by_length <- function(min_length, max_length){
  #   Funkcja zwracająca wynik zapytania do bazy o ilość filmów dla poszczegulnych długości pomiędzy wartościami min_length a max_length.
  #     Przykład wynikowej tabeli:
  #     |   |length     |count|
  #     |0	|46 	    |64	  | 
  #     
  #     Jeżeli warunki wejściowe nie są spełnione to funkcja powinna zwracać wartość NULL.
  #         
  #     Parameters:
  #     min_length (int,double): wartość minimalnej długości filmu
  #     max_length (int,double): wartość maksymalnej długości filmu
  #     
  #     Returns:
  #     pd.DataFrame: DataFrame zawierający wyniki zapytania
  if(typeof(min_length) != "integer" && typeof(min_length) != "double") {
    return(NULL)
  }
  if(typeof(max_length) != "integer" && typeof(max_length) != "double") {
    return(NULL)
  }
  if(min_length > max_length) {
    return(NULL)
  }
  
  query <- paste(
            "SELECT
              film.length AS length, COUNT(film.title) AS count
            FROM
              film
            WHERE length BETWEEN ", min_length, "AND ", max_length,
            "GROUP BY film.length", sep=""
  )
  df <- dbGetQuery(con, query)
  return(df)
}


client_from_city<- function(city){
  #   Funkcja zwracająca wynik zapytania do bazy o listę klientów z zadanego miasta przez wartość city.
  #     Przykład wynikowej tabeli:
  #     |   |city	    |first_name	|last_name
  #     |0	|Athenai	|Linda	    |Williams
  #     
  #     Tabela wynikowa ma być posortowana po nazwisku i imieniu klienta.
  #     
  #     Jeżeli warunki wejściowe nie są spełnione to funkcja powinna zwracać wartość NULL.
  #         
  #     Parameters:
  #     city (character): nazwa miaste dla którego mamy sporządzić listę klientów
  #     
  #     Returns:
  #     DataFrame: DataFrame zawierający wyniki zapytania
  if(typeof(city) != "character") {
    return(NULL)
  }
  
  query <- paste(
            "SELECT 
              city.city, customer.first_name, customer.last_name
            FROM 
              city
            INNER JOIN address
              ON city.city_id = address.city_id
            INNER JOIN customer
              ON address.address_id = customer.address_id
            WHERE city.city = '", city, "' 
            ORDER BY customer.last_name ASC, customer.first_name ASC", sep=""
  )
  df <- dbGetQuery(con, query)
  return(df)
}


avg_amount_by_length<-function(length){
  #   Funkcja zwracająca wynik zapytania do bazy o średnią wartość wypożyczenia filmów dla zadanej długości length.
  #     Przykład wynikowej tabeli:
  #     |   |length |avg
  #     |0	|48	    |4.295389
  #     
  #     
  #     Jeżeli warunki wejściowe nie są spełnione to funkcja powinna zwracać wartość NULL.
  #         
  #     Parameters:
  #     length (integer,double): długość filmu dla którego mamy pożyczyć średnią wartość wypożyczonych filmów
  #     
  #     Returns:
  #     DataFrame: DataFrame zawierający wyniki zapytania
  if(typeof(length) != "integer" && typeof(length) != "double") {
    return(NULL)
  }
  
  query <- paste(
            "SELECT 
              film.length, AVG(payment.amount) AS avg
            FROM
              film
            INNER JOIN inventory
              ON film.film_id = inventory.film_id
            INNER JOIN rental
              ON inventory.inventory_id = rental.inventory_id
            INNER JOIN payment
              ON rental.rental_id = payment.rental_id
            WHERE film.length = ", length, 
            "GROUP BY film.length", sep=""
  )
  df <- dbGetQuery(con, query)
  return(df)
}


client_by_sum_length<-function(sum_min){
  #   Funkcja zwracająca wynik zapytania do bazy o sumaryczny czas wypożyczonych filmów przez klientów powyżej zadanej wartości .
  #     Przykład wynikowej tabeli:
  #     |   |first_name |last_name  |sum
  #     |0  |Brian	    |Wyman  	|1265
  #     
  #     Tabela wynikowa powinna być posortowane według sumy, imienia i nazwiska klienta.
  #     Jeżeli warunki wejściowe nie są spełnione to funkcja powinna zwracać wartość NULL.
  #         
  #     Parameters:
  #     sum_min (integer,double): minimalna wartość sumy długości wypożyczonych filmów którą musi spełniać klient
  #     
  #     Returns:
  #     DataFrame: DataFrame zawierający wyniki zapytania
  if(typeof(sum_min) != "integer" && typeof(sum_min) != "double") {
    return(NULL)
  }
  
  query <- paste(
            "SELECT
              customer.first_name, customer.last_name, SUM(film.length) AS sum
            FROM 
              customer
            INNER JOIN rental
              ON customer.customer_id = rental.customer_id
            INNER JOIN inventory
              ON rental.inventory_id = inventory.inventory_id
            INNER JOIN film
              ON inventory.film_id = film.film_id
            GROUP BY customer.first_name, customer.last_name
              HAVING SUM(film.length) >= ", sum_min,
            "ORDER BY sum ASC, customer.last_name ASC, customer.first_name ASC", sep=""
  )
  df <- dbGetQuery(con, query)
  return(df)
}


category_statistic_length<-function(name){
  #   Funkcja zwracająca wynik zapytania do bazy o statystykę długości filmów w kategorii o zadanej nazwie.
  #     Przykład wynikowej tabeli:
  #     |   |category   |avg    |sum    |min    |max
  #     |0	|Action 	|111.60 |7143   |47 	|185
  #     
  #     Jeżeli warunki wejściowe nie są spełnione to funkcja powinna zwracać wartość NULL.
  #         
  #     Parameters:
  #     name (character): Nazwa kategorii dla której ma zostać wypisana statystyka
  #     
  #     Returns:
  #     DataFrame: DataFrame zawierający wyniki zapytania
  if(typeof(name) != "character") {
    return(NULL)
  }
  
  query <- paste(
            "SELECT
              category.name AS category, AVG(film.length) AS avg, 
              SUM(film.length) AS sum, MIN(film.length) as min, 
              MAX(film.length) as max
            FROM 
              film
            INNER JOIN film_category
              ON film.film_id = film_category.film_id
            INNER JOIN category
              ON film_category.category_id = category.category_id
            WHERE category.name = '", name,
            "' GROUP BY category.name", sep=""
  )
  df <- dbGetQuery(con, query)
  return(df)
}


# NIE EDYTOWAĆ *****************************************************************
test_dir('tests/testthat')
# ******************************************************************************

# zad 1
query1 <- "SELECT
            film.length AS length, film.title
          FROM
            film
          WHERE length = 100"
df1 <- dbGetQuery(con, query1) # dla 100 minut
df1

# zad 2
df2 <- client_from_city("Aurora")
df2

# zad 3
query3 <- "SELECT 
              film.title, AVG(payment.amount) AS avg
            FROM
              film
            INNER JOIN inventory
              ON film.film_id = inventory.film_id
            INNER JOIN rental
              ON inventory.inventory_id = rental.inventory_id
            INNER JOIN payment
              ON rental.rental_id = payment.rental_id
            GROUP BY film.title"
df3 <- dbGetQuery(con, query3)
df3

# zad 4
query4 <- "SELECT 
              category.name AS category, COUNT(film.title) AS count
            FROM
              category
            INNER JOIN film_category
              ON category.category_id = film_category.category_id
            INNER JOIN film
              ON film_category.film_id = film.film_id
            GROUP BY 
              category.name"
df4 <- dbGetQuery(con, query4)
df4

# zad 5
query5 <- "SELECT 
              country.country, COUNT(customer.customer_id)
            FROM 
              country
            INNER JOIN city
              ON country.country_id = city.country_id
            INNER JOIN address
              ON city.city_id = address.city_id
            INNER JOIN customer
              ON address.address_id = customer.address_id
            GROUP BY
              country.country"
df5 <- dbGetQuery(con, query5)
df5

# zad 6
query6 <- "SELECT
            store.store_id, COUNT(DISTINCT customer.customer_id) AS no_of_customers
          FROM
            store
          INNER JOIN staff
            ON store.store_id = staff.store_id
          INNER JOIN payment
            ON staff.staff_id = payment.staff_id
          INNER JOIN customer
            ON payment.customer_id = customer.customer_id
          GROUP BY store.store_id
          HAVING COUNT(customer.customer_id) > 0 AND COUNT(customer.customer_id) < 300"
df6 <- dbGetQuery(con, query6)
df6

# zad 7
query7 <- "SELECT
            customer.first_name, customer.last_name, SUM(film.length) AS length
          FROM
            customer
          INNER JOIN rental
            ON customer.customer_id = rental.customer_id
          INNER JOIN inventory
            ON rental.inventory_id = inventory.inventory_id
          INNER JOIN film
            ON inventory.film_id = film.film_id
          GROUP BY customer.first_name, customer.last_name
          HAVING SUM(film.length) > 200*60"
df7 <- dbGetQuery(con, query7)
df7

# zad 8
query8 <- "SELECT
            AVG(payment.amount)
          FROM
            payment"
df8 <- dbGetQuery(con, query8)
df8

# zad 9
query9 <- "SELECT
            category.name, AVG(film.length)
          FROM
            category
          INNER JOIN film_category
            ON category.category_id = film_category.category_id
          INNER JOIN film
            ON film_category.film_id = film.film_id
          GROUP BY category.name"
df9 <- dbGetQuery(con, query9)
df9

# zad 10
query10 <- "SELECT
            DISTINCT category.name AS category, MAX(film.length) AS max_length
          FROM
            category
          INNER JOIN film_category
            ON category.category_id = film_category.category_id
          INNER JOIN film
            ON film_category.film_id = film.film_id
          GROUP BY category.name
          ORDER BY category.name ASC"
df10 <- dbGetQuery(con, query10)
df10

# zad 11
query11 <- "SELECT
              category.name AS category, film.length
            FROM 
              category
            INNER JOIN film_category
              ON category.category_id = film_category.category_id
            INNER JOIN film
              ON film_category.film_id = film.film_id
            ORDER BY film.length DESC
            LIMIT 1"
df11 <- dbGetQuery(con, query11)
df11 # zgadza sie z zadaniem 10
