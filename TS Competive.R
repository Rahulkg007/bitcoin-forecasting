setwd("~/Google Drive/RMIT/Semester 3/Time Series/Final Project - Competitive")

data = read.csv('Bitcoin_Historical_Price.csv')

str(data)
data$Date = as.Date(data$Date,'%Y-%m-%d')

plot(data)

require(tidyverse)
class(data)
ggplot(data,aes(Date,Close)) + geom_line()
data2$Close = log(data$Close)
ggplot(data2,aes(Date,Close)) + geom_line()
data.2017 = data[data$Date > as.Date("2017-10-01"),]
require(zoo)
require(forecast)
data.zoo <- zoo(data.2017$Close, data.2017$Date)
#data.zoo <- zoo(data$Close, data$Date)

autoplot(log(data.zoo))

fit = auto.arima(data.zoo)
summary(fit)
plot(forecast(fit,h=10))
ggtsdisplay(data.zoo, lag.max = 48)

bc = BoxCox.ar(data.zoo)
bc$ci
data.log = log(data.zoo)
require(TSA)
autoplot(data.log)

# data.diff = diff(data.log)

lambda = sum(bc$ci)/length(bc$ci)
data.box = (data.zoo^lambda-1)/lambda
data.diff = diff(data.box)

autoplot(data.diff)
ggtsdisplay(data.diff, lag.max = 96)
adf.test(data.diff)

eacf(data.diff)

fit_111 = Arima(data.zoo, order = c(2,1,2), lambda = -.55)
summary(fit_111)
autoplot(forecast(fit_111,h=10))

plot(armasubsets(y=data.diff,nar=10,nma=10,y.name='test',ar.method='ols'))
fit_413 = Arima(data.zoo, order = c(4,1,5), lambda = 0)
summary(fit_413)
autoplot(forecast(fit_413,h=10)) 


# ----
library(zoo)
require(ggfortify)
require(forecast)

setwd("~/Google Drive/RMIT/Semester 3/Time Series/Final Project Regular")
data = read.csv('Melbourne_housing_FULL 2.csv')
str(data)

data$Date = as.Date(data$Date,'%d/%m/%Y')

data$month.year = as.Date(paste('01-',format(data$Date, format = "%m-%Y")),format = '%d-%m-%Y')

data.monthly <- data %>% 
  filter(!is.na(Price) & Rooms == 3) %>%
  group_by(month.year) %>% 
  summarise_at(vars(Price), mean) %>%
  arrange(month.year)

data.ts = zoo(data.monthly$Price, data.monthly$month.year)
autoplot(data.ts) + geom_point() +
  scale_x_date(date_breaks = '4 months', date_labels = '%b %Y')

fit = auto.arima(data.ts)
autoplot(forecast(fit,h=5))


df <- data %>% 
  filter(!is.na(Price) & Rooms == 3 & Type == 'h')
df.zoo = zoo(data$Price, data$Date)
df2 <- df %>% 
  group_by(Date) %>% 
  summarise_at(vars(Price), mean) 

data.zoo = zoo(df$Price, df$Date)
autoplot(data.zoo)
