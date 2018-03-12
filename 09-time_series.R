##########################################################################
# Jose Cajide - @jrcajide
# Master Data Science: Time series manipulation, analysis and forecasting
##########################################################################

rm(list=ls()) 
cat("\014")

list.of.packages <- c("tibbletime", "tidyverse", "tidyquant", "zoo", "ggmap", "forecast", "smooth", "Metrics", "timetk", "sweep", "ggseas")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(tibbletime)
library(tidyverse)
library(tidyquant)
library(zoo)
library(ggmap)
library(forecast)
library(smooth)
library(Metrics)
library(timetk)
library(sweep)
library(ggseas)

ipi.df <- read_csv('data/ipi.csv')

#----------------------------------------------------------------------------
# Exercise 1. Transform ipi.df from:
# 
# date      ipi
# <chr>   <dbl>
# 2017M11 104 
# to:
# 
# date         ipi
# <date>     <dbl>
# 2017-11-01 104  
#----------------------------------------------------------------------------

# Sol: 
# ipi.df <- ip.df %>% 


#----------------------------------------------------------------------------
# Exercise 2. Plot the data as a time series using ggplot2
#----------------------------------------------------------------------------

# ipi.df %>% ggplot


#----------------------------------------------------------------------------
# Data as a vector
#----------------------------------------------------------------------------

(ipi <- ipi.df$ipi)
(ipi <- ipi.df %>% 
  pull(ipi))

#----------------------------------------------------------------------------
# Time series witb base R
#----------------------------------------------------------------------------

(ipi.ts <- ts(data = ipi,frequency = 12, start = c(2002, 1)))
start(ipi.ts)
end(ipi.ts)
frequency(ipi.ts)

summary(ipi.ts)
head(ipi.ts)
max(ipi.ts)

plot(ipi.ts, main =  "IPI")
autoplot(ipi.ts) + labs(title = "IPI")


stats::lag(ipi.ts, 1)

plot(ipi.ts,stats::lag(ipi.ts),xlab="IPI(t)",ylab="IPI(t+1)")
lag.plot(ipi.ts)
lag.plot(ipi.ts,lags=4,layout=c(2,2))

# Diferenciación
(ipi_diff.ts <- diff(ipi.ts,1))
plot(ipi_diff.ts, main="IPI")

(ipi_diff.ts <- diff(ipi.ts,12))
plot(ipi_diff.ts, main="IPI con una diferencia estacional")


#----------------------------------------------------------------------------
# XTS
#----------------------------------------------------------------------------
ipi.xts <- xts(ipi.df$ipi, as.Date(ipi.df$date, format='%Y-%m-%d'))
plot(ipi.xts)

#----------------------------------------------------------------------------
# zoo: includes irregular time series.
#----------------------------------------------------------------------------
library(zoo)
ipi.zoo <- zoo(ipi.df$ipi, order.by=ipi.df$date)
plot(ipi.zoo)


#----------------------------------------------------------------------------
# tibbletime
#----------------------------------------------------------------------------

# Tydyverse
class(ipi.df)

# Date must be ordered desc first
ipi.df <- ipi.df %>% 
  arrange(date) %>% 
  tbl_time(index = date)

class(ipi.df)

# Time filtering
ipi.df %>% 
  filter(date >= '2017-01-01',
         date <= '2017-03-01')

ipi.df %>% 
  filter_time('2017-01-01' ~ '2017-03-01')

ipi.df %>% 
  filter_time('start' ~ '2002-07-01')

ipi.df %>% 
  filter_time('2013' ~ 'end')

# Year filtering example
ipi.df %>% 
  filter_time('2017' ~ '2017')

ipi.df %>% 
  filter_time(~ '2017')

# Month filtering example
ipi.df %>% 
  filter_time(~ '2017-01')

# Keywords
ipi.df %>% 
  filter_time('start' ~ '2015') %>% 
  tail()

# Hint: group_by

ipi2.df <- ipi.df %>% 
  mutate(ipi_mom = (ipi - lag(ipi)) / ipi) %>% 
  gather(key = "indice", value ='val', -date) %>% 
  group_by(indice)

ipi2.df
class(ipi2.df)

ipi2.df %>% 
  filter_time('2017-01-01' ~ '2017-02-01')

# Aggregating data to a less granular level
as_period(ipi.df, '3 month')
as_period(ipi.df, 'y') 


rolling_mean <- rollify(mean, window = 5)
cor_roll <- rollify(~cor(.x, .y), window = 5)

ipi.df %>% 
  mutate(mean_5 = rolling_mean(ipi),
         cor = cor_roll(ipi , lag(ipi)))


short_term_mean <- rollify(mean, window = 5)
long_term_mean  <- rollify(mean, window = 50)

ipi.df %>% 
  mutate(short_mean = short_term_mean(ipi),
         long_term_mean = long_term_mean(ipi))

#----------------------------------------------------------------------------
# Exercise 3. Using ggplot, plot ipi.df IPI series with the the 
# short_term_mean and the long_term_mean
#----------------------------------------------------------------------------

# ipi.df %>% 



# Grouping
ipi.df %>% 
  collapse_by(period = "yearly") %>% 
  group_by(date) %>% 
  summarise(median_ipi = median(ipi))


# A better dataset :) -----------------------------------------------------


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

#----------------------------------------------------------------------------
# Modeling Time Series
#----------------------------------------------------------------------------

ipi.ts
class(ipi.ts)

# Average method
ipi.mean <- meanf(ipi.ts, h = 12)
summary(ipi.mean)
plot(ipi.mean)
autoplot(ipi.mean)


# Naive
ipi.naive <- naive(ipi.ts, 12)
summary(ipi.naive)
plot(ipi.naive)
autoplot(ipi.naive)

# Fitted Values and Residuals
# It is important to always check that the residuals are well-behaved
# Essential assumptions for an appropriate forecasting model include residuals being uncorrelated and centered at mean zero
checkresiduals(ipi.naive) # p<0.05 => Residuals are not white noise

# Evaluating Forecast Accuracy
# https://www.otexts.org/fpp/2/5

accuracy(ipi.naive)

# Training & Test Sets
(n <- length(ipi.ts))
n_test <- 12
(n_train <- n - n_test)

(ipi_train <- ts(data = ipi.ts[1:n_train],frequency = 12, start = c(2002, 1)) )
(ipi_test <- ts(data = ipi.ts[(n_train+1):n],frequency = 12, end = c(2017, 11)) )

autoplot(ipi_train) 
autoplot(ipi_test) 

ipi.naive_fc <- naive(ipi_train, h = length(ipi_test))
ipi.mean_fc <- meanf(ipi_train, h = length(ipi_test))
ipi.snaive_fc <- snaive(ipi_train, h = length(ipi_test))
forecast::accuracy(ipi.mean_fc, ipi_test)
forecast::accuracy(ipi.naive_fc, ipi_test)
forecast::accuracy(ipi.snaive_fc, ipi_test)

autoplot(ipi_train) +
  autolayer(ipi.mean_fc$mean, series="Mean") +
  autolayer(ipi.naive_fc$mean, series="Naïve") +
  autolayer(ipi.snaive_fc$mean, series="Seasonal naïve") +
  ggtitle("IPI Forecasts") +
  xlab("Year") + ylab("IPI") +
  guides(colour=guide_legend(title="Forecast"))

# Time Series Cross-validation
errors <- tsCV(ipi.ts, forecastfunction = naive, h = 12)
mean(errors^2, na.rm = TRUE) #MSE

# Compute and compare the MSE for different forecast horizons 
MSE <- vector("numeric", 10)
for(h in 1:10) {
  errors <- tsCV(ipi.ts, forecastfunction = naive, h = h)
  MSE[h] <- mean(errors^2, na.rm = TRUE)
}
MSE


# Simple Moving Average

# Predict IPI would be for next 12 months based on the the last 12 months
# Order 2
fit2<-sma(ipi.ts, order = 2, h = 12, holdout = T, level = .95)
round(fit2$forecast,0)
plot(forecast(fit2))
round(mae(fit2$holdout, fit2$forecast),2)

# Automatic
fitX<-sma(ipi.ts, h = 12, holdout = T, level = .95, ic = 'AIC')
plot(fitX)
round(fitX$forecast,0)
plot(forecast(fitX))
round(mae(fitX$holdout, fitX$forecast),2)

# Bonus library forecast

autoplot(ipi.ts, series = "Data") + 
  autolayer(ma(ipi.ts, 1), series = "1 yr MA") +
  autolayer(ma(ipi.ts, 6), series = "5 yr MA") +
  autolayer(ma(ipi.ts, 12), series = "10 yr MA") +
  xlab("Date") + 
  ylab("Savings Rate") +
  theme_tq()

# with zoo
ipi_12.df <- ipi.df %>% 
  mutate(ipi_tma = rollmean(ipi, k = 12, fill = NA, align = "right"))
ipi.df %>% tail()
ipi_12.df %>% tail()

ipi_12.df %>%
  gather(metric, value, -date) %>%
  ggplot(aes(date, value, color = metric)) +
  geom_line() +
  theme_tq()

# Other way.
ggplot(ipi.df, aes(date, ipi)) +
  geom_line()+ 
  geom_ma(ma_fun = SMA, n = 12, size = 1, color = "red",linetype = 1) +
  geom_ma(ma_fun = SMA, n = 6, size = 1, color = "blue",linetype = 1) +
  geom_ma(ma_fun = EMA, n = 6, size = 1, color = "green",linetype = 1) +
  coord_x_date(xlim = c(today() - years(3), today())) + 
  theme_tq()

# ts components -----------------------------------------------------------
# the trend is the long-term increase or decrease in the data
# the seasonal pattern occurs when a time series is affected by seasonal factors such as the time of the year or the day of the week
# the cycle occurs when the data exhibit rises and falls that are not of a fixed period
#----------------------------------------------------------------------------

ipi.decomp <- decompose(ipi.ts)
names(ipi.decomp)
plot(ipi.decomp)

ggseasonplot(ipi.ts)

ggseasonplot(ipi.ts , polar = TRUE)

ggsubseriesplot(ipi.ts)

# Autocorrelation -----------------------------------------------------------
acf(ipi.ts)
ggAcf(ipi.ts)
gglagplot(ipi.ts)



# Seasonally adjusted or deseasonalized data ------------------------------
# Obteining a seasonal stationary ts
ipi_des.ts  <- ipi.ts-ipi.decomp$seasonal
plot(ipi.ts,main="IPI. Serie normal Vs desestacionalizada")
lines(ipi_des.ts, col='red')


# Forecasting -------------------------------------------------------------

# Holt-Winters Forecasting or Triple Exponential Smoothing ----------------
# https://en.wikipedia.org/wiki/Exponential_smoothing#Triple_exponential_smoothing
# α is the data smoothing factor, 0 < α < 1, 
# β is the trend smoothing factor, 0 < β < 1, 
# and γ is the seasonal change smoothing factor, 0 < γ < 1.

# alpha = .3
fit_es1 <- HoltWinters(ipi_train,alpha = .3, beta = F, gamma = F)
plot(fit_es1)
fit_es1$SSE

fit_es <- HoltWinters(ipi_train, beta = F, gamma = F)
plot(fit_es)
fit_es$SSE
round(fit_es$alpha,4)


#----------------------------------------------------------------------------
# Exercise 4. Fit a Holt-Winters leaving all parameter in blank. 
# Get the proposed α, β and γ.
# Check the final sum of squared errors achieved in optimizing
#----------------------------------------------------------------------------
# fit_hw <- HoltWinters(ipi_train)



# Predictive performance of the model
pred_es <- predict(fit_es, n.ahead = 12, prediction.interval = T, level = .95)
round(head(pred_es),0)

pred_hw <- predict(fit_hw, n.ahead = 12, prediction.interval = T, level = .95)
round(head(pred_hw),0)

# Get RMSE on a test dataset: ipi_test

round(rmse(predicted = pred_es[, "fit"], actual  = ipi_test), 2)
round(rmse(predicted = pred_hw[, "fit"], actual  = ipi_test), 2)

plot(fit_es,predicted.values = pred_es)
plot(fit_hw,predicted.values = pred_hw)

# Forecasting out of sample: future prediction
fit_out_sample <- HoltWinters(ipi.ts)
forecast_out_sample <- forecast(fit_out_sample, h=12)
round(print(forecast_out_sample),2)
plot(forecast_out_sample)

# Chech prediction for Dec.2017:
# https://goo.gl/b3dGiB



# Forecasting the tidy way ------------------------------------------------

# Exponential smoothing
# Step 1: Modeling a time series
fit_ets <- ipi.df %>%
  tk_ts(start = 2002, freq = 12, silent = T) %>% 
  ets()

plot(fit_ets)

sw_tidy(fit_ets)
sw_glance(fit_ets)
augment_fit_ets <- sw_augment(fit_ets)
augment_fit_ets

augment_fit_ets %>%
  ggplot(aes(x = index, y = .resid)) +
  geom_hline(yintercept = 0, color = "grey40") +
  geom_point(color = palette_light()[[1]], alpha = 0.5) +
  geom_smooth(method = "loess") +
  scale_x_yearmon(n = 10) +
  labs(title = "IPI: ETS Residuals", x = "") + 
  theme_tq()

# TS decomposition
decomp_fit_ets <- sw_tidy_decomp(fit_ets)
decomp_fit_ets 

decomp_fit_ets %>%
  gather(key = key, value = value, -index) %>%
  mutate(key = forcats::as_factor(key)) %>%
  ggplot(aes(x = index, y = value, group = key)) +
  geom_line(color = palette_light()[[2]]) +
  geom_ma(ma_fun = SMA, n = 12, size = 1) +
  facet_wrap(~ key, scales = "free_y", ncol = 1) +
  scale_x_yearmon(n = 10) +
  labs(title = "IPI: ETS Decomposition", x = "") + 
  theme_tq() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Step 2: Forecasting the model
fcast_ets <- fit_ets %>%
  forecast(h = 12)

# Step 3: Tidy the forecast objec
sw_sweep(fcast_ets, fitted = TRUE) %>% tail(13)

sw_sweep(fcast_ets) %>%
  ggplot(aes(x = index, y = ipi, color = key)) +
  geom_ribbon(aes(ymin = lo.95, ymax = hi.95), 
              fill = "#D5DBFF", color = NA, size = 0) +
  geom_ribbon(aes(ymin = lo.80, ymax = hi.80, fill = key), 
              fill = "#596DD5", color = NA, size = 0, alpha = 0.8) +
  geom_line(size = 1) +
  labs(title = "IPI, ETS Model Forecast", x = "", y = "IPI",
       subtitle = "Regular Time Index") +
  scale_y_continuous() +
  scale_x_yearmon(n = 12, format = "%Y") +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq() 

sw_sweep(fcast_ets, timetk_idx = TRUE) %>%
  tail(13)



# More on TS --------------------------------------------------------------

monthplot(ipi.ts, main="IPI Monthly Patterns",
          xlab="Month", ylab="Count", col="darkblue", lwd=2, lty.base=2, lwd.base=1,
          col.base="gray40")

seasonplot(ipi.ts, main="IPI Seasonal Trends",
           col=rainbow(10), year.labels=TRUE, year.labels.left=TRUE, cex=0.7,
           cex.axis=0.8)



# Identifying possible breakpoints in a time series -----------------------
require(strucchange)
breakpoints(ipi.ts ~ 1)
plot(ipi.ts, main="IPI breakpoints", ylab="IPI", col="darkblue", lwd=1.5)
# Plot the line at the optimal breakpoint
lines(breakpoints(ipi.ts ~ 1), col="darkgreen")
# Plot a 90% confidence interval
lines(confint(breakpoints(ipi.ts ~ 1), level=0.90), col="darkgreen")
# Add breakpoint location text
text(2009, 125, "April 2011", cex=0.75, col="darkgreen", pos=4, font=3)
text(2014, 125, "March 2014", cex=0.75, col="darkgreen", pos=4, font=3)



# Evaluating quality with control charts ----------------------------------
require(qcc)
infections = c(6, 2, 5, 1, 3, 4, 2, 6, 3, 2, 4, 7, 1, 1, 4, 4, 1, 5, 2, 3, 5,
               2, 3, 2, 4)
patient_days = c(985, 778, 1010, 834, 750, 729, 1002, 639, 985, 578, 976, 540,
                 829, 723, 908, 1017, 1097, 1122, 1234, 1022, 1167, 1098, 1201, 1045, 1141)
month_name = month.abb[c(1:12, 1:12, 1:1)]

infection_control = qcc(infections, sizes=patient_days/1000, type="u",
                        labels=month_name, axes.las=2, xlab="Month", ylab="", digits=2,
                        title="Hospital Acquired Infections Rate per 1,000 Patient Days\n
                        u-chart for Jan 2012 - Jan 2014")
# Create a real date and move limits and rate into a data frame
ic_qcc = data.frame(Month = seq(as.Date("2012-01-01"),
                                as.Date("2014-01-01"), "months"), infection_control$limits,
                    Rate = (infections / patient_days)*1000)
ic_qcc$Violations = factor(ifelse(row.names(ic_qcc)
                                  == infection_control$violations$beyond.limits, "Violation", NA))

ggplot(ic_qcc, aes(x=Month, y=Rate)) +
  geom_line(aes(y=mean(ic_qcc$Rate)), color="gray50") +
  geom_line() +
  geom_point(color="darkblue") +
  geom_point(data=filter(ic_qcc, Violations=="Violation"),
             color="red", size=3) +
  geom_line(aes(y=LCL), linetype="dashed", color="gray50") +
  geom_line(aes(y=UCL), linetype="dashed", color="gray50") +
  xlab("Month") +
  ylab("Rate") +
  ggtitle("Hospital Acquired Infections Rate per 1,000 Patient Days
          u-chart for Jan 2012 - Jan 2014") +
  theme_minimal()
