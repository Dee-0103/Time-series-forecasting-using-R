library(timeSeries)
library(forecast)

data = read.csv('C:/Users/Dell/Downloads/daily-total-female-births.csv')
head(data)

data$Date = as.Date(data$Date)
data['year'] = strftime(data$Date,'%Y')
data['mon'] = strftime(data$Date,'%b')
data['day'] = strftime(data$Date,'%d')

data

df_new = subset(data, data$day!=31, c('Births'))
df_new

plot(df_new$Births)

# converting data to time series
data = ts(data = df_new, frequency = 30)
head(df_new)
plot(data)

# start
start(data)

# end
end(data)

# frequency
frequency(data)

#decomposing the data
decomp_data = decompose(data)

plot(decomp_data)

plot(decomp_data$trend)

plot(decomp_data$random)

plot(decomp_data$seasonal)

boxplot(data~cycle(data))

# Removing the seasonality and trend
stationary_2 = data - decomp_data$seasonal 
station_1 = data - decomp_data$trend

plot(stationary_2)
plot(station_1)

# Apply ARIMA
arima_mod = auto.arima(stationary_2)

f_am = forecast(arima_mod,h = 5, level = c(95))
f_am
plot(f_am)


arima_mod2 = auto.arima(station_1)

f_org = forecast(arima_mod2, h= 5, level = c(95))
f_org

plot(f_org)





