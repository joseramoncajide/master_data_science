##########################################################################
# Jose Cajide - @jrcajide
# Master Data Science: Modeling data with dplyr and sql
##########################################################################

list.of.packages <- c("dplyr","ggplot2", "scales", "tibble", "foreach", "dbplyr", "broom", "DBI", "lubridate")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

devtools::install_github("edgararuiz/tidypredict")
library(tidypredict)

library(ggplot2)
library(scales)
library(tibble)
library(dbplyr)

flights_table <- readr::read_csv('data/flights/2008.csv') %>% 
  mutate(current_score = 0) %>%
  rowid_to_column("flight_id")


# Let's simulate we are using a data base backend
library(DBI)
con <- dbConnect(RSQLite::SQLite(), path = ":memory:")
db_fligths <- copy_to(con,flights_table )
rm(flights_table)

query <- db_fligths %>%
  mutate(DayOfWeek = as.character(DayOfWeek), 
         Month = as.character(Month)) %>% 
  select(DepDelay, Month, DayOfWeek, UniqueCarrier, Distance) 

query %>% 
  show_query()

flights <- query %>%
  collect() 

flights <- flights %>% sample_n(10000) 

model <- lm(DepDelay ~ ., data = flights)

summary(model)

tidypredict_sql(model, con = con)

update_statement <- build_sql("UPDATE flights_table SET current_score  = ", tidypredict_sql(model, con = con), con = con)
update_statement

dbSendQuery(con, update_statement)

scores.df <- db_fligths %>%
  filter(Dest == 'JFK') %>% 
  select(UniqueCarrier, Month, UniqueCarrier, DayOfWeek, current_score) %>%
  group_by(UniqueCarrier, Month, DayOfWeek) %>% 
  summarise(current_score = mean(current_score)) %>% 
  arrange(-current_score) %>% 
  collect()

scores.df %>% 
  mutate(current_score = scales::rescale(current_score)) %>%
  ggplot(aes(Month, DayOfWeek)) + 
  facet_wrap(~UniqueCarrier) +
  geom_tile(aes(fill = current_score), colour = "white") + 
  scale_fill_gradient(low = "white", high = "darkblue") + 
  theme_minimal(base_size = 9)


