library(DBI)
library(RPostgres)

dsn_database = "wbauer_adb_2023"   
dsn_hostname = "pgsql-196447.vipserv.org"  
dsn_port = "5432"                
dsn_uid = "wbauer_adb"        
dsn_pwd = "adb2020"  

con <- dbConnect(Postgres(), dbname = dsn_database, host=dsn_hostname, port=dsn_port, user=dsn_uid, password=dsn_pwd) 

df <- dbGetQuery(con, 'SELECT * FROM city') 
print(df)

# zadanie 1
inst1 <- 'SELECT DISTINCT name FROM category'
df1 <- dbGetQuery(con, inst1)
cat('Mamy', nrow(df1), 'kategorii filmów w wypożyczalni')

# zadanie 2
inst2 <- 'SELECT name FROM category ORDER BY category ASC'
df2 <- dbGetQuery(con, inst2)
df2

# zadanie 3
inst3 <- 'SELECT title, release_year FROM film ORDER BY release_year ASC'
df3 <- dbGetQuery(con, inst3)
cat('Najstarszym filmem jest', df3$title[1], ', a najnowszym jest', df3$title[nrow(df3)])

# zadanie 4
inst4 <- "SELECT rental_date FROM rental WHERE rental_date BETWEEN '2005-07-01' AND '2005-08-01'"
df4 <- dbGetQuery(con, inst4)
cat("Między 2005-07-01 a 2005-08-01 odbyło się", nrow(df4), "wypożyczeń")

# zadanie 5
inst5 <- "SELECT rental_date FROM rental WHERE rental_date BETWEEN '2010-01-01' AND '2011-02-01'"
df5 <- dbGetQuery(con, inst5)
cat("Między 2010-01-01 a 2011-02-01 odbyło się", nrow(df5), "wypożyczeń")

# zadanie 6
inst6 <- "SELECT amount FROM payment ORDER BY amount DESC LIMIT 1"
df6 <- dbGetQuery(con, inst6)
df6

# zadanie 7
inst7 <- "SELECT 
            first_name, last_name, country
          FROM 
            customer
          LEFT JOIN address 
            ON customer.address_id = address.address_id
          LEFT JOIN city 
            ON address.city_id = city.city_id
          LEFT JOIN country 
            ON city.country_id = country.country_id
          WHERE 
            country = 'Poland' OR country = 'Nigeria' OR country = 'Bangladesh'"
df7 <- dbGetQuery(con, inst7)
df7

# zadanie 8
inst8 <- "SELECT
            country
          FROM
            staff
          LEFT JOIN address 
            ON staff.address_id = address.address_id
          LEFT JOIN city 
            ON address.city_id = city.city_id
          LEFT JOIN country 
            ON city.country_id = country.country_id"
df8 <- dbGetQuery(con, inst8)
df8

# zadanie 9
inst9 <- "SELECT
            first_name, last_name
          FROM
            staff
          LEFT JOIN address ON staff.address_id = address.address_id
          LEFT JOIN city ON address.city_id = city.city_id
          LEFT JOIN country ON city.country_id = country.country_id
          WHERE country.country = 'Argentina' OR country.country = 'Spain'"
df9 <- dbGetQuery(con, inst9)
cat("W Argentynie lub Hiszpanii mieszka", nrow(df9), "pracowników")

# zadanie 10
inst10 <- "SELECT
            DISTINCT name
          FROM
            category
          LEFT JOIN film_category
            ON category.category_id = film_category.category_id
          LEFT JOIN film
            ON film_category.film_id = film.film_id
          LEFT JOIN inventory
            ON film.film_id = inventory.film_id
          LEFT JOIN rental
            ON inventory.inventory_id = rental.inventory_id
          WHERE rental.rental_date IS NOT NULL
          "
df10 <- dbGetQuery(con, inst10)
df10

# zadanie 11
inst11 <- "SELECT
            DISTINCT name
          FROM
            category
          LEFT JOIN film_category
            ON category.category_id = film_category.category_id
          LEFT JOIN film
            ON film_category.film_id = film.film_id
          LEFT JOIN inventory
            ON film.film_id = inventory.film_id
          LEFT JOIN rental
            ON inventory.inventory_id = rental.inventory_id
          LEFT JOIN customer
            ON rental.customer_id = customer.customer_id
          LEFT JOIN address 
            ON customer.address_id = address.address_id
          LEFT JOIN city 
            ON address.city_id = city.city_id
          LEFT JOIN country 
            ON city.country_id = country.country_id
          WHERE country.country = 'United States'"
df11 <- dbGetQuery(con, inst11)
df11

# zadanie 12
inst12 <- "SELECT
            title
          FROM
            film
          LEFT JOIN film_actor
            ON film.film_id = film_actor.film_id
          LEFT JOIN actor
            ON film_actor.actor_id = actor.actor_id
          WHERE (actor.first_name = 'Olympia' AND actor.last_name = 'Pfeiffer') 
            OR (actor.first_name = 'Julia' AND actor.last_name = 'Zellweger')
            OR (actor.first_name = 'Ellen' AND actor.last_name = 'Presley')"
df12 <- dbGetQuery(con, inst12)
df12
