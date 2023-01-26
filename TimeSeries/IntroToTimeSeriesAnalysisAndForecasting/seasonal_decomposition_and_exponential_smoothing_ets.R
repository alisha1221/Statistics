
# Seasonal Decomposition

### Decomposing Time Series (U)
library(forecast)
library(ggplot2)

plot(nottem)
frequency(nottem)
length(nottem)
decompose(nottem, type = "additive")
plot(decompose(nottem, type = "additive"))

# using ggplot
autoplot(decompose(nottem, type = "additive"))

# alternatively the function stl could be used
plot(stl(nottem, s.window="periodic"))
stl(nottem, s.window="periodic")

# seasonal adjustment
mynottem = decompose(nottem, "additive")
class(mynottem)

# we are subtracting the seasonal element
nottemadjusted = nottem - mynottem$seasonal

# getting a plot
plot(nottemadjusted)
plot(mynottem$seasonal)

# a stl forecast from the package forecast
plot(stlf(nottem, method = "arima"))


# Decomposition Exercise
air <- AirPassengers
class(air)
frequency(air)

plot(air)
# seems to have trend and seasonality

mymodel1 = decompose(air, type="additive")
plot(mymodel1)
mymodel2 <- decompose(air, type="multiplicative")
plot(mymodel2)

# Trend is constantly increasing over the whole plot
# Seasonal patterns that peak around summer time
# For additive, some pattern still left in the remainder and only the middle part
# around 1954-1959 seens random. This is not a good sign and shows that the model
# could be improved upon.
# Want all the patterns in the model, and only randomness or white noise in the remainder.

# For multiplicative, some pattern in the beginning till 1953, 
# but better plot for randomness than the one from additive. Still, model could be further 
# improved that allows additive and multiplicative component.


add_adj <- air - mymodel1$seasonal
mul_adj <- air - mymodel2$seasonal

plot(add_adj) # same as below
plot(mymodel1$trend + mymodel1$random)
# Still have pseudo seasonal spikes on the trend line and they are evenly spaced, which
# means there is a patternm, at least at the beginning and the end of the data set
# At least seasonal part of the model needs improvement

plot(air)
plot(mul_adj)
plot(mymodel2$trend + mymodel2$random)

# For this data, automated models might select multiplicative seasonality and additive trend


### SMOOTHING SMA

library("TTR")

# in order to identify trends, we can use smoothers
# like a simple moving avg
# n identfies the order or the SMA - you can experiment with this parameter

x = c(1,2,3,4,5,6,7)
SMA(x, n = 3) # SMA fro TTR package, 3rd order

lynxsmoothed = SMA(lynx, n = 9); lynxsmoothed

# we can compare the smoothed vs the original lynx data
plot(lynx)
plot(lynxsmoothed)



# Exponential Smoothing with ets
## ets
library(forecast)
# Smoothing coefs to manage the weighting based on the timestamp
# reactive model relies heavily on recent data - high coef ~1
# smooth model - low coef ~0

# Coefficients
# alpha: initial level (or error)
# beta: trend
# gamma: seasonality
# phi: damped parameter

# Using function ets
etsmodel = ets(nottem); etsmodel 
# for ets(), default for model='ZZZ', which means auto-selection of the three components
# additive A, multiplicative M, non-present N

# Plotting the model vs original
plot(nottem, lwd = 3)
lines(etsmodel$fitted, col = "red")
# fitted values quite close to dataset

# Plotting the forecast
plot(forecast(etsmodel, h = 12))

# Changing the prediction interval
plot(forecast(etsmodel, h = 12, level = 95))

# Manually setting the ets model
etsmodmult = ets(nottem, model ="MZM"); etsmodmult

# Plot as comparison
plot(nottem, lwd = 3)
lines(etsmodmult$fitted, col = "red")






