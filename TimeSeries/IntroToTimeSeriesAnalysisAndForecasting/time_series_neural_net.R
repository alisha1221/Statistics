
# Neural Network Autoregression Model (NNAR)
# Autregression based neural net
# A neural network simplifies input variables over one or more
# layers into one output value
# p lagged values as input, k nodes, output - NNAR(p, k)
# For seasonal model, also include P seasonal lag, output - NNAR(p, P, k)
# nnet do not perform well with trending data, so de-trend or differencing

#--------------------------------------------------------------

APTelectric <- read.csv("APTelectricity.csv")
myts = ts(APTelectric$watt, frequency = 288)
plot(myts)

set.seed(34)
library(forecast)

# fit neural net
fit = nnetar(myts)

nnetforecast <- forecast(fit, h = 400, PI = F)

library(ggplot2)
autoplot(nnetforecast)
# model takes last 16 lags, 1 seasonal lag, and simplifies these to
# 9 nodes or neurons in the hidden layer


#--------------------------------------------------------------
## Using an external regressor in a neural net
#--------------------------------------------------------------
# need numeric variables (not factor or character)

fit2 = nnetar(myts, xreg = APTelectric$appliances)

# Defining the vector which we want to forecast

y =rep(2, times = 12*10)

nnetforecast <- forecast(fit2, xreg = y, PI = F)
autoplot(nnetforecast)
