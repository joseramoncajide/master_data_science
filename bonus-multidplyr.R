##########################################################################
# Jose Cajide - @jrcajide
# Master Data Science: Data wrangling with multidplyr
##########################################################################

list.of.packages <- c("R.utils", "tidyverse", "doParallel", "foreach", "sqldf", "broom", "DBI", "ggplot2", "tidyr", "lubridate")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)




# Operaciones en paralelo

library(dplyr)
library(data.table)
library(purrr) # For map() reduce()

# Pararllel multidplyr
data_path <- file.path('data','flights')
files <- dir(data_path, pattern = "*.csv")

flights <- files %>%
  # read in all the files, appending the path before the filename
  map(~ data.table::fread(file.path(data_path, .))) %>% 
  reduce(rbind)

flights


devtools::install_github("hadley/multidplyr")
library(multidplyr)

flights %>% 
  group_by(Month) %>% 
  summarize(cnt = n())


flights %>% 
  partition(Month) %>% 
  summarize(cnt = n()) %>% 
  collect() # Unordered results

# Creating 4-core cluster
cluster <- create_cluster(7)

flights %>% 
  partition(Month, cluster = cluster) %>% 
  summarize(cnt = n()) %>% 
  collect()

# Estimating how delays vary over the course of the year and within a day

common_dest <- flights %>%
  count(Dest) %>%
  filter(n >= 365) %>%
  semi_join(flights, .) %>% 
  mutate(yday = lubridate::yday(ISOdate(Year, Month, DayofMonth)))

common_dest %>% head

cluster <- create_cluster(2)
cluster

set_default_cluster(cluster)

by_dest <- common_dest %>% 
  partition(Dest, cluster = cluster)
by_dest

cluster_library(by_dest, "mgcv")
system.time({
  models <- by_dest %>% 
    do(mod = gam(DepDelay ~ s(yday) + s(DepTime), data = .))
})

unnest(models)


system.time({
  models <- common_dest %>% 
    group_by(Dest) %>% 
    do(mod = gam(DepDelay ~ s(yday) + s(DepTime), data = .))
})


results = map(fit, augment)




