##########################################################################
# Jose Cajide - @jrcajide
# Master Data Science: Reading data
##########################################################################

list.of.packages <- c("R.utils", "tidyverse", "doParallel", "foreach", "sqldf")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


# Base R: Do not run
# flights <- read.csv("data/flights/2007.csv")

airports <- read.csv("data/airports.csv")


# Reading data ------------------------------------------------------------

# readr
library(readr)

?read_csv

ptm <- proc.time()
flights <- read_csv('data/flights/2007.csv', progress = T)
proc.time() - ptm

print(object.size(get('flights')), units='auto')


# data.table
remove.packages("data.table")
# Notes:
# http://www.openmp.org/
# https://github.com/Rdatatable/data.table/wiki/Installation
# 
# Linux & Mac:
# install.packages("data.table", type = "source", repos = "http://Rdatatable.github.io/data.table")
# 
# install.packages("data.table")

library(data.table)

ptm <- proc.time()
flights <- fread("data/flights/2007.csv")
proc.time() - ptm


# Reading multiple files --------------------------------------------------


( data_path <- file.path('data','flights') )

( files <- list.files(data_path, pattern = '*.csv', full.names = T) )

system.time( flights <- lapply(files, fread) ) 

system.time( flights <- lapply(files, fread, nThread=4) )


# What is flights?
class(flights)

flights <- rbindlist(flights)



# Parallel reading --------------------------------------------------------

# library(parallel)
# system.time(flights <- mclapply(files, data.table::fread, mc.cores = 8))

library(doParallel)
registerDoParallel(cores = detectCores() - 1)

library(foreach)
system.time( flights <- foreach(i = files, .combine = rbind) %dopar% read_csv(i) )

system.time( flights <- data.table::rbindlist(foreach(i = files) %dopar% data.table::fread(i, nThread=8)))


print(object.size(get('flights')), units='auto')
unique(flights$Year)



# Reading big files -------------------------------------------------------

# Some times system commands are faster
system('head -5 data/flights/2008.csv')
readLines("data/flights/2008.csv", n=5)

# Num rows
length(readLines("data/flights/2008.csv")) # Not so big files

nrow(data.table::fread("data/flights/2008.csv", select = 1L, nThread = 2)) # Using fread on the first column


# Reading only what I neeed
library(sqldf)
jfk <- sqldf::read.csv.sql("data/flights/2008.csv", 
                           sql = "select * from file where Dest = 'JFK'")
head(jfk)

data.table::fread("data/flights/2008.csv", select = c("UniqueCarrier","Dest","ArrDelay" ))


# Using other tools
# shell: csvcut ./data/airlines.csv -c Code,Description

data.table::fread('/Library/Frameworks/Python.framework/Versions/2.7/bin/csvcut ./data/airports.csv -c iata,airport' )

# shell: head -n 100 ./data/flights/2007.csv | csvcut -c UniqueCarrier,Dest,ArrDelay | csvsort -r -c 3

data.table::fread('head -n 100 ./data/flights/2007.csv | /Library/Frameworks/Python.framework/Versions/2.7/bin/csvcut -c UniqueCarrier,Dest,ArrDelay | /Library/Frameworks/Python.framework/Versions/2.7/bin/csvsort -r -c 3')


# Dealing with larger than memory datasets


# Using a DBMS
# sqldf("attach 'flights_db.sqlite' as flights")
# sqldf("DROP TABLE IF EXISTS flights.delays")

read.csv.sql("./data/flights/2008.csv", 
             sql = c("attach 'flights_db.sqlite' as flights", 
                     "DROP TABLE IF EXISTS flights.delays",
                     "CREATE TABLE flights.delays as SELECT UniqueCarrier, TailNum, ArrDelay FROM file WHERE ArrDelay > 0"), 
             filter = "head -n 100000")

db <- dbConnect(RSQLite::SQLite(), dbname='flights_db.sqlite')
dbListTables(db)

delays.df <- dbGetQuery(db, "SELECT UniqueCarrier, AVG(ArrDelay) AS AvgDelay FROM delays GROUP BY UniqueCarrier")  
delays.df


unlink("flights_db.sqlite")
dbDisconnect(db)


# Chunks ------------------------------------------------------------------

# read_csv_chunked
library(readr)
f <- function(x, pos) subset(x, Dest == 'JFK')

jfk <- read_csv_chunked("./data/flights/2008.csv",
                        chunk_size = 50000,
                        callback = DataFrameCallback$new(f))


# Importing a file into a DBMS:
db <- DBI::dbConnect(RSQLite::SQLite(), dbname='flights_db.sqlite')
dbListTables(db)
dbWriteTable(db,"jfkflights",jfk) # Inserta en df en memoria en la base de datos
dbGetQuery(db, "SELECT count(*) FROM jfkflights")  
dbRemoveTable(db, "jfkflights")
rm(jfk)


##########################################################################
# Ex: Coding exercise: Using read_csv_chunked, read ./data/flights/2008.csv by chunks while sending data into a RSQLite::SQLite() database
##########################################################################

db <- DBI::dbConnect(RSQLite::SQLite(), dbname='flights_db.sqlite')
writetable <- function(df,pos) {
  dbWriteTable(db,"flights",df,append=TRUE)
}
readr::read_csv_chunked(file="./data/flights/2008.csv", callback=SideEffectChunkCallback$new(writetable), chunk_size = 50000)

# Check
num_rows <- dbGetQuery(db, "SELECT count(*) FROM flights")
num_rows == nrow(data.table::fread("data/flights/2008.csv", select = 1L, nThread = 2)) 

dbGetQuery(db, "SELECT * FROM flights LIMIT 6") 

dbRemoveTable(db, "flights")
dbDisconnect(db)

# sqlite3 /Users/jose/Documents/GitHub/master_data_science/flights_db.sqlite
# sqlite> .tables
# sqlite> SELECT count(*) FROM flights;



# Basic functions for data frames -----------------------------------------

names(flights)
str(flights)
nrow(flights)
ncol(flights)
dim(flights)
