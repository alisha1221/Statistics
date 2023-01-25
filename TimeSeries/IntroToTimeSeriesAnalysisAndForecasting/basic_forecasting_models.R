library(forecast)

set.seed(54)
myts <- ts(c(rnorm(50, 34, 10), rnorm(67, 7, 1), runif(23, 3, 14)))

plot(myts)
# mean and variance are not constant
# Constant variance is a key indicator of stationarity.
# Transformation is one way to make a time series stationary.
# Can also use differencing using diff() or use auto.arima() to make it stationary

# Mean, Naive and Drift method
meanm <- meanf(myts, h=10)
naivem <- naive(myts, h=10)
driftm <- rwf(myts, h=10, drift=T)

plot(meanm, main="", bty="l")
lines(naivem$mean, col=123, lwd=2)
lines(driftm$mean, col=22, lwd=2)
legend("bottomleft", lty=1, col=c(4, 123, 22), cex=0.75,
       legend=c("Mean method", "Naive method", "Drift method"))


# Model comparison and accuracy
mytstrain <- window(myts, start=1, end=112)
mytstest <- window(myts, start=113)

meanma <- meanf(mytstrain, h=28)
naivema <- naive(mytstrain, h=28)
driftma <- rwf(mytstrain, h=28, drift=T)

accuracy(meanma, mytstest)
accuracy(naivema, mytstest)
accuracy(driftma, mytstest)
# naive has lesser RMSE, MAE, MAPE, MASE, so it's better than other models

# Residuals

var(naivem$residuals[-1])
mean(naivem$residuals[-1])
# mean=-0.344, var=84.537

hist(naivem$residuals)
shapiro.test(naivem$residuals)
# p<0.05, so reject H0. The residuals are not normal.

# Autocorrelation: correlation coefficient between different time points (lags) in a time series
# Partial autocorrelation: cor coef adjusted for all shorter lags in a time series
# acf() is used to identify the moving average (MA) part of the ARIMA model
# pacf() identifies the values for the autoregressive part (AR)
acf(naivem$residuals[-1]) # omit the 1st lag
# 4 lags outside the CI, so there is some autocorrelation.

pacf(naivem$residuals[-1]) # count the 1st lag too
# 8 lags outside the CI


# Stationarity
# ADF = augmented Dickey-Fuller test
# removes autocorrelation and tests for non-stationarity
library(tseries)
adf.test(myts)
# H0: non-stationary, Ha: stationary
# p>0.05, so no sufficient evidence to reject the null hypothesis of non-stationary


# With log transformation
logmyts <- log(myts)
plot(logmyts)

# Mean, Naive and Drift method
meanm <- meanf(logmyts, h=10)
naivem <- naive(logmyts, h=10)
driftm <- rwf(logmyts, h=10, drift=T)

plot(meanm, main="", bty='l')
lines(naivem$mean, col=123, lwd=2)
lines(driftm$mean, col=22, lwd=2)
legend("bottomleft", lty=1, col=c(4, 123, 22),
       legend=c("Mean method", "Naive method", "Drift method"))


# Model comparison and accuracy
mytstrain <- window(logmyts, start=1, end=112)
mytstest <- window(logmyts, start=113)

meanma <- meanf(mytstrain, h=28)
naivema <- naive(mytstrain, h=28)
driftma <- rwf(mytstrain, h=28, drift=T)

accuracy(meanma, mytstest)
accuracy(naivema, mytstest)
accuracy(driftma, mytstest)
# Other models improved as well, but naive is still better


# Residuals

var(naivem$residuals[-1])
mean(naivem$residuals[-1])
# mean=-0.017, var=0.207
# mean is closer to 0 now

hist(naivem$residuals)
shapiro.test(naivem$residuals)
# p<0.05, so we could not fix this issue

acf(naivem$residuals[-1])
# only 2 lags outside the bounds, so this is better than before
pacf(naivem$residuals[-1])
# 2 lags outside the bounds

# Stationarity
# ADF = augmented Dickey-Fuller test
# removes autocorrelation and tests for non-stationarity
adf.test(logmyts)
# H0: non-stationary, Ha: stationary
# p>0.05, so no sufficient evidence to reject the null hypothesis of non-stationary

