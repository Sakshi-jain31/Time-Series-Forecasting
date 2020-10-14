setwd("C:/Users/Sakshi/Desktop/Github Projects/Time Series Project")

###############Installing Libraries######################

library(ggplot2)
install.packages("tseries")
library(tseries)
install.packages("forecast")
library(forecast)

#################Data Exploration##################################

View(gas)
au.gas=ts(gas, start=c(1956, 1), frequency = 12)
head(au.gas)
tail(au.gas)
any(is.na(au.gas))  ##Check for any missing values
class(au.gas)
ts.plot(au.gas, xlab="Year", ylab="Gas Production", main="Australian Gas Production 1956 - 1995")
plot(decompose(au.gas, type="additive"))

########################Stationarity Test###################

adf.test(au.gas)


## decomposing the AU series 
decomposed = stl(au.gas, s.window = "periodic")
seasonal = decomposed$time.series[,1]  ## seasonal component
trend = decomposed$time.series[,2]    ## trend component
remainder = decomposed$time.series[,3]  ## random component 


# removing the seasonality
des.data = au.gas - seasonal
plot(des.data, ylab= "Production units", main = "De-Seasonalized Series") ## plotting the de-seasonlized data

########################ARIMA Modelling and Forecasting###############

train.AUgas = window(au.gas, start=c(1956,1), end = c(1993,12), frequency=12)
test.AUgas = window(au.gas, start=c(1994,1), frequency=12)
## Plotting the train and Test set 
autoplot(train.AUgas, series="Train") +
  autolayer(test.AUgas, series="Test") +
  ggtitle("AU gas  Traning and Test data") +
  xlab("Year") + ylab("Production") +
  guides(colour=guide_legend(title="Forecast"))

################Differencing the Series############################

Augas.diff = diff(au.gas, differences = 2)

adf.test(Augas.diff)   ###Stationarity Test

####################Checking ACF and PACF######################

par(mfrow=c(1,2))
acf(au.gas, lag.max = 50)
pacf(au.gas,lag.max = 50)



par(mfrow=c(1,2))
acf(Augas.diff, lag.max = 50)
pacf(Augas.diff,lag.max = 50)

#################Manual Arima Model#####################
man.arima = arima(train.AUgas, order = c(1,1,1), seasonal = c(1,1,1), method = 'ML')
man.arima


plot(forecast(man.arima, h=12), shadecols = "oldstyle")

Box.test(man.arima$residuals, type = "Ljung-Box", lag = 350)
acf(man.arima$residuals, lag.max = 50)
hist(man.arima$residuals) ## checking the normal distribution of residuals 



################Auto Fit Arima Model##################
auto.fit = auto.arima(train.AUgas, trace = F, seasonal = T)
auto.fit
plot(forecast(auto.fit, h=12), ylab = "Gas Production", xlab = "Year")
Box.test(auto.fit$residuals, type = "Ljung-Box", lag = 350)


Vec1<- cbind(test.AUgas ,as.data.frame(forecast(man.arima, h=20))[,1])
ts.plot(Vec1, col=c("blue", "red"), main="Gas Production: Actual vs Forecast")
legend("bottomright", legend=c("Actual", "Forecast"),col=c("blue", "red"), cex=0.8, lty= 1:1)

## Accuracy of the manual arima model 
accuracy(forecast(man.arima, 24), test.AUgas)

## Accuracy of the Auto arima model 
accuracy(forecast(auto.fit, 24), test.AUgas) 

