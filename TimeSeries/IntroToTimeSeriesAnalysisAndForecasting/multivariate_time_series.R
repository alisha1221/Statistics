
#--------------------------------------------------------------
### Multivariate Time Series Datasets
#--------------------------------------------------------------
set.seed(40)
# Generating a random dataframe
x = rnorm(100, 1)
y = rnorm(100, 30)
z = rnorm(100, 500)
xyz = data.frame(x, y, z)
class(xyz)

# Converting a data.frame into mts

mymts = ts(xyz, 
           frequency = 12,
           start = c(1940, 4))

# Standard exploratory tools
plot(mymts)
head(mymts)
class(mymts)

# Our further exercise dataset
class(EuStockMarkets)
head(EuStockMarkets)


# Main packages -vars and MTS.
# problem: both have different functions VAR()
# Need to specify VAR() from which package is being used
library(vars)
library(MTS)

#--------------------------------------------------------------
## Testing for stationarity
#--------------------------------------------------------------
library(tseries)

### tseries - standard test adt.test
apply(EuStockMarkets, 2, adf.test)
# non-stationary as all p>0.05
# But we need stationarity, so do differencing

# Alternative: lib fUnitRoots, function
# apply(EuStockMarkets, 2, adfTest, lags=0, type="c")


# Differencing the whole mts
library(MTS)
stnry = diffM(EuStockMarkets)

# Retest
apply(stnry, 2, adf.test)
# Now they are stationary

#--------------------------------------------------------------
## VAR (Vector Autoregressive) modeling
#--------------------------------------------------------------
# Requisites of VAR modeling
#   Endogenous data (inputs influence each other)
#   Continuous numeric variables
# Each variable is a linear function of past lags of itself
# and past lags of the other variables

plot.ts(stnry)

# Lag order identification
library(vars)

VARselect(stnry, type = "none", lag.max = 10)

# Creating a VAR model with vars

var.a <- vars::VAR(stnry,
                   lag.max = 10,
                   ic = "AIC",
                   type = "none")
summary(var.a)


#--------------------------------------------------------------
# Residual diagnostics
#--------------------------------------------------------------
# How much of the pattern is captured by the model?
# Measuring the correlation left in the residuals and optimize the 
# model as much as possible
# There must be no or very little correlation in the residuals
# Portmanteau test
serial.test(var.a)
# p<0.05, so there is still lots of correlation between residuals
# Need to tweak the model, change the VAR lags or change the model type
# e.g. VAR(9) model on differenced MTS
# Alternatives: add a 2nd step of differencing, get log() of whole data


#--------------------------------------------------------------
# Granger test for causality
#--------------------------------------------------------------
# Is there indeed a correlation between the components of a model?
causality(var.a, cause = c("DAX"))
# p<0.05, so causality
# If no causality (p>0.05), can drop the variable and simplify the model
# Requires stationary MTS
# The test should be performed on all variables of the MTS


#--------------------------------------------------------------
## Forecasting VAR models
#--------------------------------------------------------------
# Time series is differenced - no trend
# Forecasting complex models far into the future might provide 
# unreliable results
# Bringing back the forecast result onto the original scale

fcast = predict(var.a, n.ahead = 25)
plot(fcast)

# Forecasting the DAX index
DAX = fcast$fcst[1]; DAX # type list

# Extracting the forecast column
x = DAX$DAX[,1]; x

tail(EuStockMarkets)

# Inverting the differencing
x = cumsum(x) + 5473.72
plot.ts(x)


# Adding data and forecast to one time series

DAXinv =ts(c(EuStockMarkets[,1], x),
           start = c(1991,130), frequency = 260)
plot(DAXinv)
plot.ts(DAXinv[1786:1885])

## Creating an advanced plot with visual separation
library(lattice)
library(grid)
library(zoo)

# Converting to object zoo
x = zoo(DAXinv[1786:1885])

# Advanced xyplot from lattice

xyplot(x, grid=TRUE, panel = function(x, y, ...){
  panel.xyplot(x, y, col="red", ...)
  grid.clip(x = unit(76, "native"), just=c("right"))
  panel.xyplot(x, y, col="green", ...) })


