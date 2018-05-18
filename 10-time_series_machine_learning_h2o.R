##########################################################################
# Jose Cajide - @jrcajide
# Master Data Science: Time series forecasting using machine learning in H2O
##########################################################################

rm(list=ls()) 
cat("\014")

list.of.packages <- c("h2o", "tidyverse", "timetk", "tidyquant")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Load libraries
library(h2o)
library(tidyverse) 
library(timetk)  
library(tidyquant)

ipi.df <- read_csv('data/ipi.csv')
ipi.df %<>% 
  separate(date, into = c("year", "month"), sep = "M") %>% 
  mutate(date = as.Date(paste(year, month, 01, sep = "-"))) %>% 
  select(date, ipi)

ipi.df <- ipi.df %>% filter(date <= '2013-12-31')

summary(ipi.df)

ipi.df %>%
  ggplot(aes(date, ipi)) +
  # Train Region
  annotate("text", x = ymd("2002-01-01"), y = 150,
           color = palette_light()[[1]], label = "Train Region") +
  # Validation Region
  geom_rect(xmin = as.numeric(ymd("2012-01-01")), 
            xmax = as.numeric(ymd("2012-12-31")),
            ymin = 0, ymax = Inf, alpha = 0.02,
            fill = palette_light()[[3]]) +
  annotate("text", x = ymd("2012-07-01"), y = 150,
           color = palette_light()[[1]], label = "Validation\nRegion") +
  # Test Region
  geom_rect(xmin = as.numeric(ymd("2013-01-01")), 
            xmax = as.numeric(ymd("2013-12-31")),
            ymin = 0, ymax = Inf, alpha = 0.02,
            fill = palette_light()[[4]]) +
  annotate("text", x = ymd("2013-05-01"), y = 150,
           color = palette_light()[[1]], label = "Test\nRegion") +
  # Data
  geom_line(col = palette_light()[1]) +
  geom_point(col = palette_light()[1]) +
  geom_ma(ma_fun = SMA, n = 12, size = 1) +
  # Aesthetics
  theme_tq() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "IPI",
       subtitle = "Train, Validation, and Test Sets Shown") 

ipi.df %>% glimpse()

# Augment (adds data frame columns)
ipi.df_aug <- ipi.df %>%
  tk_augment_timeseries_signature()

ipi.df_aug %>% glimpse()

ipi.df_clean <- ipi.df_aug %>%
  select_if(~ !is.Date(.)) %>%
  select_if(~ !any(is.na(.))) %>%
  mutate_if(is.ordered, ~ as.character(.) %>% as.factor)

ipi.df_clean %>% glimpse()

# Split into training, validation and test sets
train_tbl <- ipi.df_clean %>% filter(year < 2012)
valid_tbl <- ipi.df_clean %>% filter(year == 2012)
test_tbl  <- ipi.df_clean %>% filter(year == 2013)

h2o.init() 

# Convert to H2OFrame objects
train_h2o <- as.h2o(train_tbl)
valid_h2o <- as.h2o(valid_tbl)
test_h2o  <- as.h2o(test_tbl)

# Set names for h2o
y <- "ipi"
x <- setdiff(names(train_h2o), y)

?h2o.automl

automl_models_h2o <- h2o.automl(
  x = x, 
  y = y, 
  training_frame = train_h2o, 
  validation_frame = valid_h2o, 
  leaderboard_frame = test_h2o, 
  max_runtime_secs = 60, 
  stopping_metric = "deviance")


# Extract leader model
automl_leader <- automl_models_h2o@leader

pred_h2o <- h2o.predict(automl_leader, newdata = test_h2o)

h2o.performance(automl_leader, newdata = test_h2o)

# Investigate test error
error_tbl <- ipi.df %>% 
  filter(lubridate::year(date) == 2013) %>%
  add_column(pred = pred_h2o %>% as.tibble() %>% pull(predict)) %>%
  rename(actual = ipi) %>%
  mutate(
    error     = actual - pred,
    error_pct = error / actual
  ) 
error_tbl

error_tbl %>%
  summarise(
    me   = mean(error),
    rmse = mean(error^2)^0.5,
    mae  = mean(abs(error)),
    mape = mean(abs(error_pct)),
    mpe  = mean(error_pct)
  ) %>%
  glimpse()

h2o.varimp(automl_leader)
h2o.varimp_plot(automl_leader, num_of_features = NULL)



h2o.shutdown()


