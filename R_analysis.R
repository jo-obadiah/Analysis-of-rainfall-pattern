library(zoo)
library(reshape2)
library(forecast)
library(tseries)

data = read.csv("C:/Users/BIU LGA/Documents/Rain_Data.csv")

long_data = melt(data, id.vars = "YEAR", variable.name = "Month", value.name = "Rainfall")

long_data$Date = as.Date(paste(long_data$YEAR, long_data$Month, "01", sep = "-"), format="%Y-%B-%d")

long_data = long_data[order(long_data$Date), ]

rainfall_zoo = zoo(long_data$Rainfall, order.by=long_data$Date)

aggregated_data = aggregate(Rainfall ~ Date, data = long_data, sum)

rainfall_zoo = zoo(aggregated_data$Rainfall, order.by=aggregated_data$Date)

rainfall_ts = ts(rainfall_zoo, frequency=12)

plot(rainfall_ts, main="Monthly Rainfall Time Series", xlab="Date", ylab="Rainfall (mm)", col="blue")

ggtsdisplay(rainfall_ts)

#Performimg Augmented Dickey-Fuller test
Adf_test = adf.test(rainfall_ts)
print(Adf_test)

#performing KPSS test for level stationarity
kpss_test = kpss.test(rainfall_ts)
print(kpss_test)

# Plot ACF and PACF
par(mfrow=c(2,1))
acf(rainfall_ts, main="ACF of Rainfall Time Series")
pacf(rainfall_ts, main="PACF of Rainfall Time Series")
par(mfrow=c(1,1))

#Fitting a SArima Model
sarima_model = auto.arima(rainfall_ts, seasonal = TRUE)
 #print summary
summary(sarima_model)

#check residua;s
checkresiduals(sarima_model)

#forcasting values
forecast = forecast(sarima_model,h=24)
plot(forecast, main= "sarima model forecast")
print(forecast)
