##########################################################################
# Jose Cajide - @jrcajide
# Master Data Science: Tidy time series
##########################################################################


list.of.packages <- c("tibbletime", "tidyverse", "ggmap")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(tibbletime)
library(tidyverse)
library(ggmap)

( files <- list.files("data/airbnb/", pattern = '*.csv', full.names = T) )
airbnb <- lapply(files, read_csv)
airbnb <- bind_rows(airbnb)

airbnb <- airbnb %>% 
  as_tbl_time(last_modified) %>%
  arrange(last_modified) %>%
  select(last_modified, price, overall_satisfaction, latitude, longitude)

summary(airbnb)

airbnb %>%
  collapse_by(period = "1 year") %>%
  group_by(last_modified) %>%
  summarise(median_price = median(price, na.rm = T))

# Clean up
airbnb %>%
  collapse_by(period = "2 hour", clean = TRUE) %>%
  group_by(last_modified) %>%
  summarise(median_price = median(price)) %>% 
  head

# Start
airbnb %>%
  collapse_by(period = "2 hour", clean = TRUE, side = "start") %>%
  group_by(last_modified) %>%
  summarise(median_price = median(price)) %>% 
  head

airbnb %>%
  collapse_by(period = "2 hour", clean = TRUE, side = "start", start_date = "2014-08-01 15:00:00") %>%
  group_by(last_modified) %>%
  summarise(median_price = median(price)) %>% 
  head


# Viz: ggmap

airbnb_plot <- airbnb %>% 
  drop_na() %>% 
  as_tbl_time(index = last_modified) %>% 
  # Collapse and clean
  collapse_by(period = "hour", clean = TRUE, side = "start") %>%
  # Throw out a few outliers
  filter(between(price, quantile(price, .05), quantile(price, .95))) %>% 
  mutate(price = log10(price)) %>% 
  qmplot(longitude, latitude, data = ., geom = "blank") +
  geom_point(aes(color = price), alpha = .2, size = .3) +
  scale_color_continuous(low = "red", high = "blue")

airbnb_plot
