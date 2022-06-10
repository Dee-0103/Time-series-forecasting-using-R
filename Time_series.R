# Time series on Air Passengers data

install.packages('timeSeries')
install.packages('forecast')
library(timeSeries)
library(forecast)

# data
df = AirPassengers
df

# In case data is not time series format use ts(data, frequency = time interval) 
# to convert it into time series

# start year
start(AirPassengers)

# end year
end(AirPassengers)

# frequency
frequency(AirPassengers)

# Plot
plot(AirPassengers)

# decomposing into different components

decom_data = decompose(AirPassengers, "multiplicative")
decom_data

plot(decom_data)

# additive decomposition (default)
decom_add = decompose(AirPassengers)
plot(decom_add)

plot(decom_data$trend)
plot(decom_data$random)
plot(decom_data$seasonal)

# Box-plot
boxplot(df~cycle(df), main = 'Month against number of passengers travelling in the airlines',xlab = 'Month no.', ylab = 'No. of Passengers') 

# Stationary data
stationary_data =df - decom_add$seasonal-decom_add$trend
plot(stationary_data)



# Auto ARIMA model
f_model = auto.arima(stationary_data)

f = forecast(f_model, h = 10*12, level = c(95)) # h =10*12 means 10 years data
f

plot(f)


# original data
f_model = auto.arima(df)

f = forecast(f_model, h = 10*12, level = c(95)) # h =10*12 means 10 years data
f

plot(f)






