#loading necessary libraries
library(zoo)
library(reshape2)
library(forecast)
library(tseries)

data = read.csv("C:/Users/BIU LGA/Documents/Rain_Data.csv")

#Reshaping data for easy analysis

long_data = melt(data, id.vars = "YEAR", variable.name = "Month", value.name = "Rainfall")

long_data$Date = as.Date(paste(long_data$YEAR, long_data$Month, "01", sep = "-"), format="%Y-%B-%d")

long_data = long_data[order(long_data$Date), ]

rainfall_zoo = zoo(long_data$Rainfall, order.by=long_data$Date)

aggregated_data = aggregate(Rainfall ~ Date, data = long_data, sum)

rainfall_zoo = zoo(aggregated_data$Rainfall, order.by=aggregated_data$Date)

rainfall_ts = ts(rainfall_zoo, frequency=12)

#Checking for seasonality in data...

plot(rainfall_ts, main="Monthly Rainfall Time Series", xlab="Date", ylab="Rainfall (mm)", col="blue")

ggtsdisplay(rainfall_ts)

#we observe that the original data exhibits a wave like pattern evidence of seasonality 
#and no trend is observed which implies that the Time series is stationary'''
#From the trend of the data there is decrease in rainfall with some months having zero values.


#Performing Augmented Dickey-Fuller test
Adf_test = adf.test(rainfall_ts)
print(Adf_test)

##Alternative hypothesis: stationary
##Conclusion: Since p-value < α, we reject H0 and conclude that the series is stationary


#performing KPSS test for level stationarity
kpss_test = kpss.test(rainfall_ts)
print(kpss_test)

##Conclusion: since p-value > α we do not reject H0. 
##Hence, we conclude that the series is stationary.

# Plot ACF and PACF
par(mfrow=c(2,1))
acf(rainfall_ts, main="ACF of Rainfall Time Series")
pacf(rainfall_ts, main="PACF of Rainfall Time Series")
par(mfrow=c(1,1))

##The sinusoidal or periodic pattern in the ACF plot is again suggesting 
##that the series has a strong seasonal effect also, the PACF plot is neither suggesting otherwise.

#Fitting a Sarima Model
sarima_model = auto.arima(rainfall_ts, seasonal = TRUE)
 
#print summary
summary(sarima_model)

#checking for residuals
checkresiduals(sarima_model)

##Residuals shows the time series of the standardized residuals. The plot depicts no pattern, a clear 
##indication that the standardized residuals behave like a typical white noise process as required.

#forecasting values
forecast = forecast(sarima_model,h=24)
plot(forecast, main= "sarima model forecast")
print(forecast)

##As we can see from the forecast, May – September has the highest predicted 
##values which indicates the beginning and peak of raining season in Jos.
