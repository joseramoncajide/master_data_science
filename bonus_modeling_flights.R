library(randomForest)
library(tidyverse)


# Ejercicios
options(dplyr.width = Inf, dplyr.print_min = 4)


flights <- read_csv('data/flights/2008.csv',progress = T)

flights_model_data <- flights %>% 
  filter(UniqueCarrier %in% c("AA", "UA")) %>% 
  sample_n(10000) %>% 
  mutate(ArrDelay = if_else(ArrDelay > 0, 'Y', 'N'),
         UniqueCarrier = as_factor(UniqueCarrier)) %>% 
  select(UniqueCarrier, ArrDelay, DepDelay, Month, DayofMonth, DepTime) %>% 
  na.omit()

barplot(table(flights_model_data$ArrDelay))

# flights_model_data$ArrDelay <- as.factor(flights_model_data$ArrDelay)

formula <- factor(ArrDelay) ~ .

fit <- randomForest(formula, data = flights_model_data,
                          importance = TRUE,
                          ntree = 100)

fit
importance(fit)
varImpPlot(fit)

# Par cada compañía por separado

models_by_airline <- flights_model_data %>% 
  nest(-UniqueCarrier) %>% 
  mutate(rf = map(data, ~ randomForest(formula, data = .,
                                        importance = TRUE,
                                        ntree = 100)),
         importance = map(rf, importance)) 

models_by_airline$rf
models_by_airline$importance
