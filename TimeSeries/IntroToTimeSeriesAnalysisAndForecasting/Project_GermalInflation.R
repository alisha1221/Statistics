# German Inflation Data
# https://www.statbureau.org/en/germany/inflation-tables
#-------------------------------------------

library(tidyverse)
library(forecast)


# import data
data <- scan()
inflation <- ts(data, start=2008, frequency=12)
inflation

tsdisplay(inflation)

#-------------------------------------------
# Seasonal decomposition
#-------------------------------------------
# divide data into its components - trend, seasonality, remainder (white noise)

plot(decompose(inflation))
# no trend
# seasonality present
# seasonal component stays constant over several cycles, so additive model is better

#-------------------------------------------
# Using the stl method (seasonal and trend decomposition with LOESS)
#-------------------------------------------
# decompose() have NA values for some initial obs, but there are new methods
# for seasonal decomposition like X11, Seats, STL that do not have NA values
# Also, the models allow the seasonal part to be adjusted over time, unlike the standard decomposition
# STL is robust to outliers
# STL is suited towards an additive model. For multiplicative models, data transformation is needed
# Seasonal and trend cycles may change over time in STL, unlike decompose

plot(stl(inflation, s.window=7)) # s.window = number of equired seasonal cycles to calculate changes for the seasonality, x>=7, x is odd

# stl forecasting
plot(stlf(inflation, method = "ets"))
ggplot2::autoplot(stlf(inflation), h=24) # using ggplot

# comparison with standard ets forecast
plot(forecast(ets(inflation), h=24))

#-------------------------------------------
# Seasonal ARIMA 
#-------------------------------------------
sarima <- auto.arima(inflation, stepwise = T, trace=T, approximation = F)
sarima

sarima_fore <- forecast(sarima)
plot(sarima_fore)
autoplot(inflation)+
  forecast::autolayer(sarima_fore)+
  theme_bw()
# There is no drift, which coincides with the fact that there is no trend
# Patterns avaiable in data are primarily of seasonal nature



#-------------------------------------------
# Exponential Smoothing with ets
#-------------------------------------------
ets_m <- ets(inflation)
ets_m
summary(ets_m)
ets_fore <- forecast(ets_m, h=60)
plot(ets_fore)

autoplot(inflation)+
  forecast::autolayer(ets_fore)

#-------------------------------------------
# Comparison with seasonal Holt Winters model
#-------------------------------------------
plot(hw(inflation, h=60))

#-------------------------------------------
## Cross Validation of 2 models
#-------------------------------------------
# This process is time-consuming
# Producing an error rate for each time point in the series
#  Actual  value vs calculated value
# Test set has 1 observayion and train set has all observations before the test
# data after test point not used for that time point
# So, error rates are computed one after another along the timeline, called rolling forecast origin
# Forecast accuracy - avg value of all the errors of the whole time series

germaninflets = ets(inflation)
germaninflarima = auto.arima(inflation, 
                             stepwise = T, 
                             approximation = F, 
                             trace = T)

forecastets = function(x, h) {
  forecast(ets(x), h = h)
}

forecastarima = function(x, h) {
  forecast(auto.arima(x), stepwise = T, approximation = F, h=h)
}

etserror = tsCV(inflation, forecastets, h=1)
arimaerror = tsCV(inflation, forecastarima, h=1)

mean(etserror^2, na.rm=TRUE)
mean(arimaerror^2, na.rm=TRUE)

# Better model is the one with lower error rates (MSE, RMSE)
# In this case, ARIMA performed better



# 
# # Cross validation
# 
# tstrain <- window(inflation, start=2008, end=c(2015, 12))
# tstest <- window(inflation, start=2016)
# 
# sarima_train <- auto.arima(tstrain)
# summary(sarima_train)
# sarima_train_fore <- forecast(sarima_train, h=22)
# accuracy(sarima_train_fore, tstest)
# 
# 
# 
# ets_train <- ets(tstrain)
# summary(ets_train)
# ets_train_fore <- forecast(ets_train, h=22)
# accuracy(ets_train_fore, tstest)




