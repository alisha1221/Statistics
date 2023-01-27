# Time Series Project
# Singapore Labor Force Participation Rate
# data - https://docs.google.com/spreadsheets/d/1frieoKODnD9sX3VCZy5c3QAjBXMY-vN7k_I9gR-gcU8/pub#
#-------------------------------------------

library(tidyverse)
library(forecast)

# scan the data and load into workspace
data <- scan()
data

# convert to a time series object
myts <- ts(data, start=1980, frequency = 1)
myts
tsdisplay(myts)

# The data is trending, but there is no seasonality
# Can use Holt's linear trend model (or with damped parameter) or ARIMA

#---------------------------------
# Holt's Linear Trend
#---------------------------------
# Estimated forecast value = level value at time t + trend value at time t multiplied by h (# of steps in future)
# Y_{(t+h)|t} = l_t + h*b_t

holtm <- holt(myts, h=10, damped=FALSE)
summary(holtm)
plot(holtm)

# For this particular data, the trend cannot continue indefinitely
# Impossible for a labor force participation rate to cross 100% mark
# A person cannot participate indefinite times in the labor market
# Participation rate will likely come to a halt long before the 100% mark
# Think about people being handicapped, unhealthy, uneducated, or simply 
# unwilling to participate on the labor market
# That means the curve will likely flatten out in the range of 90-94% participation
# given the other participation rates on the sheet we obtained the data from
# It is clear that Singapore will encounter flat labor force participation as opposed
# to the steady growth it experienced over the last 30 years or so
# Need to incorporate this information in model
# Can use holt() + damped

#---------------------------------
# Holt's Damped Trend
#---------------------------------
# assumes trend cannot be constant forever
# damped parameter (phi) introduced for this
# if phi~=1, close to the original slope of the Holt trend model
# if phi~=0, flattened curve
# recommended range is 0.8 to 0.95
# phi ~0.8 makes sure that short run forecasts still contain the trend, whereas
# longer forecast lags are at a flat curve

# holtm_damp <- holt(myts, h=10, damped = TRUE, phi=0.8) # can also manually adjust phi
holtm_damp <- holt(myts, h=10, damped = TRUE)
summary(holtm_damp)
plot(holtm_damp)

#---------------------------------
# ARIMA
#---------------------------------
# trending data - so autocorrelation present (earlier obs influences later obs)
# Moving average not present
# no seasonality, so no differencing needed

arimam <- auto.arima(myts, stepwise = F, 
                     trace = T, approximation = F)
summary(arimam)
checkresiduals(arimam)
arimafore <- forecast(auto.arima(myts), h=10)

#---------------------------------
# Visualize results
#---------------------------------
autoplot(arimam$fitted)+
  forecast::autolayer(holtm$mean, series="Holt Linear")+
  forecast::autolayer(holtm_damp$mean, series="Holt Damped Trend")+
  forecast::autolayer(arimafore$mean, series="ARIMA")+
  xlab("Year")+
  ylab("Labor Force Participation Rate Age 25-54")+
  ggtitle("Singapore")+
  theme_bw()+
  theme(
    plot.title=element_text(hjust=0.5, 
                            face="bold", 
                            size=15),
    legend.position = c(0.8, 0.2))+
  guides(color=guide_legend(title="Forecast Method"))
  

autoplot(arimam$fitted, series = "ARIMA", size=1.5)+
  forecast::autolayer(holtm$fitted, series="Holt Linear Trend", size=1.5)+
  forecast::autolayer(holtm_damp$fitted, series="Holt Damped Trend", size=1.5)+
  autolayer(myts, color = "black", size=2)+
  xlab("Year")+
  ylab("Labor Force Participation Rate Age 25-54")+
  ggtitle("Singapore")+
  theme_bw()+
  theme(
    plot.title=element_text(hjust=0.5, 
                            face="bold", 
                            size=15),
    legend.position = c(0.8, 0.2))+
  guides(color=guide_legend(title="Forecast Method"))


  

