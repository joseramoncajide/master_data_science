##########################################################################
# Jose Cajide - @jrcajide
# Master Data Science: SQL with R
##########################################################################

list.of.packages <- c("sparklyr", "tidyverse", "dplyr", "dbplyr", "tidyr", "ggplot2", "readr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


# devtools::install_github("rstudio/sparklyr")
library(sparklyr)
spark_available_versions()
spark_installed_versions()
# spark_uninstall('2.1.0', '2.7')
# spark_install(version = "2.2.0", '2.7')

# sudo rm -fr /Library/Java/JavaVirtualMachines/jdk-9.jdk/
# sudo rm -fr /Library/Internet\ Plug-Ins/JavaAppletPlugin.plugin
# sudo rm -fr /Library/PreferencePanes/JavaControlPanel.prefPane
# http://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html
# export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.8.0_111.jdk/Contents/Home
# export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.8.0_{ should be your version }.jdk/Contents/Home


library(dplyr)
library(dbplyr)
library(tidyr)
library(readr)
library(ggplot2)


conf <- spark_config()
conf$`sparklyr.shell.driver-memory` <- "6G"
conf$`sparklyr.shell.executor-memory` <- "6G"
conf$`spark.yarn.executor.memoryOverhead` <- "1g"
conf$`spark.executor.cores` <- 3
conf$`spark.sql.shuffle.partitions` <- "5000"

sc <- spark_connect(master = "local", 
                    version = "2.2.0",
                    config = conf)

spark_web(sc)



# Leer del cluster
spark_read_csv(sc, 'flights', path = './data/flights/2008.csv', overwrite = T, memory = T)

src_tbls(sc)

flights_tbl <- tbl(sc, 'flights')

select(flights_tbl, Year:DayOfWeek, ArrDelay, DepDelay) %>% head()
filter(flights_tbl, DepDelay > 1000)



### Leer de local

flights <- read_csv('./data/flights/2008.csv')

flights_tbl <- copy_to(sc, flights, "flights", overwrite = T, memory = T)

flights_tbl %>% 
  count %>% 
  dbplyr::sql_render()


carrierhours <- flights_tbl %>% 
  filter(Dest == 'JFK', UniqueCarrier != "EV", ArrDelay > 0) %>% 
  # filter(Dest == 'JFK', UniqueCarrier %in% c('NW', 'YV', 'AA')) %>% 
  select(ArrDelay, UniqueCarrier) %>% collect()

flights_stats <- carrierhours %>%
  group_by(UniqueCarrier) %>%
  summarize(count = n(), MeanArrDelay = mean(ArrDelay, na.rm = T)) 

flights_tbl %>% 
  filter(Dest == 'JFK') %>% 
  group_by(UniqueCarrier) %>% 
  summarize(count = n(), MeanArrDelay = mean(ArrDelay, na.rm = T)) %>% arrange(-MeanArrDelay) %>% collect()%>%  View()

# ¿Is there any relationship between airlines and flight delays?
flights_stats

library(gplots) 
plotmeans(carrierhours$ArrDelay ~ carrierhours$UniqueCarrier, 
          digits=2, ccol="red", mean.labels=T, main="Mean Arrival Delays by Airline")


boxplot(carrierhours$ArrDelay ~ carrierhours$UniqueCarrier, 
        main="Arrival Delays by Airline", xlab="Airline", ylab="Arrival delays", col=rainbow(7), outline=F)
points(flights_stats$MeanArrDelay, col="black", pch=18)

# Anova
# Are the variations between the airlines means due to true differences about the populations means or just due to sampling variability?
# H0: The three means are statistically equal.
# There is no relationship between airlines and delays
# H1: Not all airlines means are equal
# There is a relationship between airlines and delays

( anova <- aov (carrierhours$ArrDelay ~ carrierhours$UniqueCarrier) )
summary(anova)

# Since p-value < 0.05 we can conclude that, for our confidence interval, 
# we reject the null hypothesis H0 and accept the alternative hypothesis H1, meaning
# that there is a significant relationship between airlines and delays at JFK.


# Differences between all groups: Which airlines are different from the others?
(tukey <- TukeyHSD(anova))

library(broom)

# There is no significant difference in arrival delays for this airlines:
tidy(tukey) %>% 
  select(-term) %>% 
  filter(adj.p.value > .05) %>% 
  arrange(-adj.p.value) %>% 
  head(1)
# Example: There is no significant difference in arrival delays betwwen YV and AA
flights_stats %>% 
  filter(UniqueCarrier %in% c("YV", "AA"))


# There is a significant difference in arrival delays for this airlines:
tidy(tukey) %>% 
  select(-term) %>% 
  filter(adj.p.value < .05) %>% 
  arrange(adj.p.value) %>% 
  head(1)
# Example: There is a significant difference in arrival delays betwwen DL and AA
flights_stats %>% 
  filter(UniqueCarrier %in% c("DL", "AA"))


plot(tukey, las=1, col="steelblue" )


# --- JOINS

airlines.df <- read_csv('./data/airlines.csv') 
airports.df <- read_csv('./data/airports.csv')


airlines_tbl <- copy_to(sc, airlines.df, "airlines", overwrite = T)
airports_tbl <- copy_to(sc, airports.df, "airports", overwrite = T)

# ---- MODEL

flights_tbl

model_data <- flights_tbl %>% 
  filter(!is.na(DepDelay)) %>% 
  mutate(IsWeekEnd = if_else(DayOfWeek == 5 | DayOfWeek == 6 | DayOfWeek == 7, 1, 0), 
         DepHour = floor(DepTime/100), 
         DelayLabeled = if_else(ArrDelay > 15, 1, 0)) %>% 
  select(ArrDelay, Month, DepHour, DepDelay,  IsWeekEnd, Origin, Dest, UniqueCarrier)

model_data <- sdf_sample(model_data, 0.1)


# Partition the data into training and validation sets
model_partition <- model_data %>% 
  sdf_partition(train = 0.8, valid = 0.2, seed = 5555)

# Fit a linear model
ml1 <- model_partition$train %>%
  ml_linear_regression(ArrDelay ~ .)

# Summarize the linear model
summary(ml1)

broom::tidy(ml1) %>% filter(grepl('^Origin', term) ) %>% arrange(desc(estimate)) %>% View()

top_delayed_destinations <- broom::tidy(ml1) %>%
  as_data_frame() %>% 
  filter(grepl('^Dest', term) ) %>% 
  tidyr::separate(term, c("var", "dest"), sep = "_", remove = TRUE, convert = FALSE) %>% 
  select(var, dest, estimate) %>% 
  left_join(airports.df, by = c("dest" = "iata")) %>% 
  arrange(desc(estimate)) %>% 
  mutate(in_top_10 = (min_rank(-estimate) <= 10))



# MAP VIZ
library(leaflet)
pal <- colorFactor(c("dodgerblue4", "firebrick3"), domain = top_delayed_destinations$in_top_10)


leaflet(top_delayed_destinations) %>% 
  addTiles('https://{s}.tile.openstreetmap.se/hydda/base/{z}/{x}/{y}.png',
           attribution = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') %>% 
  addCircleMarkers(lng = ~long,
             lat = ~lat,
             popup = ~paste(airport, "<br>",city),
             weight = ~estimate,
             radius = ~ifelse(in_top_10 == F, 2, 6),
             color = ~pal(top_delayed_destinations$in_top_10),
             stroke = F,
             fillOpacity = ~ifelse(in_top_10 == F, .8, 1)
  ) 



spark_disconnect(sc)

