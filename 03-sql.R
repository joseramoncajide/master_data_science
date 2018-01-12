##########################################################################
# Jose Cajide - @jrcajide
# Master Data Science: SQL with R
##########################################################################

list.of.packages <- c("R.utils", "tidyverse", "doParallel", "foreach", "sqldf", "broom", "DBI", "data.table")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


flights <- data.table::fread('data/flights/2008.csv')


# SQL con R ---------------------------------------------------------------


# https://cran.r-project.org/bin/macosx/
# https://github.com/ggrothendieck/sqldf#problem-involvling-tcltk
# capabilities()[["tcltk"]]
# options(gsubfn.engine = "R")

library(sqldf)

sqldf("select Year from flights group by Year order by Year desc")

carrier_delays <- sqldf("SELECT UniqueCarrier, COUNT(ArrDelay) AS ArrivalDelays FROM flights WHERE ArrDelay > 0 AND Dest = 'JFK' AND Year = 2008  GROUP BY UniqueCarrier ORDER BY 2 DESC");

carrier_distances <- sqldf("SELECT UniqueCarrier, COUNT(Distance) AS Flights FROM flights WHERE Distance > 0 AND Dest = 'JFK' AND Year = 2008 GROUP BY UniqueCarrier ORDER BY UniqueCarrier ASC");

# The goal is to determine whether the observed differences can be reasonnably explained by chance, or not


carrier_ratios <- sqldf("SELECT UniqueCarrier, Flights, ArrivalDelays AS Yes,  Flights - ArrivalDelays AS No, ROUND(CAST(ArrivalDelays AS float) / Flights,2)  AS Ratio FROM (
                        SELECT carrier_distances.UniqueCarrier, ArrivalDelays, Flights FROM carrier_delays LEFT JOIN carrier_distances ON carrier_delays.UniqueCarrier = carrier_distances.UniqueCarrier
) ORDER BY Ratio DESC ")

##########################################################################
# Ex: Import data/airlines.csv into the "airlines" object
# Using SQL add the a new column to carrier_ratios called "Carrier" with the airline name:
# 
# Carrier Flights   Yes    No Ratio
# 1    ExpressJet Airlines Inc.     195   124    71  0.64
# 2     Northwest Airlines Inc.    1930  1108   822  0.57
# 
##########################################################################

library(tidyverse)

airlines <- read_csv('data/airlines.csv')

sqldf("SELECT * FROM airlines LIMIT 10")

carrier_ratios <- sqldf("SELECT Description As Carrier, Flights, Yes, No, Ratio FROM carrier_ratios LEFT JOIN airlines ON carrier_ratios.UniqueCarrier =  airlines.Code")


# Tables and matrices

carrier_ratios.mat <- as.matrix(sqldf("SELECT Yes, No FROM carrier_ratios"))
rownames(carrier_ratios.mat) <- sqldf("SELECT Carrier FROM carrier_ratios")[ , "Carrier"]
carrier_ratios.mat
class(carrier_ratios.mat)


carrier_ratios.tbl <- as.table(carrier_ratios.mat) 
carrier_ratios.tbl
class(carrier_ratios.tbl)


( proportions <- round(prop.table(carrier_ratios.tbl, margin = 1),2) )
t(proportions)

barplot(t(proportions), beside=TRUE, legend=TRUE)

# Comparing over the total
round(prop.table(t(carrier_ratios.mat),margin=1), 2)
sum(prop.table(t(carrier_ratios.mat),margin=1)['Yes',])

barplot(prop.table(t(carrier_ratios.mat),margin=1), beside=TRUE, legend=TRUE)

# Basic statistics

# Test to see if we can statistically conclude that not all proportions are equal:
# To know whether UniqueCarrier made a difference in the chances of ArrivalDelays, lets carry out a proportion test. 
# This test tells how probable it is that proportions are the same. A low p-value tells you that  proportions probably differ from each other.
# The null hypothesis H0:  All the proportions are the same
# The alternative H1:  Some are different

prop.test(carrier_ratios.tbl)
# p-value < 0.05 (random chance probability) => we reject the null hypothesis so H1 is true

prop.test(carrier_ratios.tbl[c('Envoy Air', 'Delta Air Lines Inc.'),])
# p-value = 0.05728 > 0.05 => we cannot reject the null hypothesis so the proportins are the same, 

sqldf("SELECT Carrier, Yes, No, Ratio FROM carrier_ratios WHERE Carrier IN ('Envoy Air', 'Delta Air Lines Inc.')")

barplot(t(proportions[c('Envoy Air', 'Delta Air Lines Inc.'),]), beside=TRUE, legend=TRUE)

# Tesing all the airlnes at time:
test_result <- pairwise.prop.test(carrier_ratios.tbl ,p.adjust.method="none")
test_result

# When the p.values are smaller than the significance level for each pair-wise comparison we can reject the null hypothesis that the proportions are equal based on the available sample of data.

# Airlines 
test_result <- broom::tidy(test_result)
sqldf("SELECT group1, group2, ROUND(`p.value`, 2) AS `p.value` FROM test_result WHERE `p.value` > 0.05")
# The output includes a p-value. Conventionally, a p-value of less than 0.05 indicates that it is likely the groups’ proportions are different whereas a p-value exceeding 0.05 provides no such evidence.



# working with data bases -------------------------------------------------

library(DBI)

con <- dbConnect(RSQLite::SQLite(), path = ":memory:")

tmp <- tempfile()
con <- dbConnect(RSQLite::SQLite(), tmp)

dbWriteTable(con, "flights", flights)
dbListTables(con)
dbListFields(con, "flights")
dbSendQuery(con, "SELECT * FROM flights WHERE Dest = 'JFK' LIMIT 10")

query <- dbSendQuery(con, "SELECT * FROM flights WHERE Dest = 'JFK' LIMIT 10")
jfk <- dbFetch(query)

dbClearResult(query)
dbDisconnect(con)
unlink(tmp)
