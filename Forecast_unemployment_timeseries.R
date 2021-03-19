##Tiem series
#The data is taken from Federal Reserve Economic Data

#The data has 574 rows and 6 variables

#date - represents date
#psavert - personal saving rate
#pce - personal consumption expenditures, in billions of dollars
#uempmed - median duration of unemployment in thousands
#pop - total population , in thousands
#unemploy - number of unemployed in thousands (dependent variable)

#we will focus on date and unemploy

#Loading the required libraries

library(readr)
library(ggplot2)
library(forecast)
library(fpp2)
library(TTR)
library(dplyr)


#Reading the data and having a glimpse
dat <- read.csv("economics.csv")
glimpse(dat)

#Data Partitioning
#creating test and train data sets for model validation
#achieved by using class which already has Train and Test

dat_train <- subset(dat, class == 'Train')
dat_test <- subset(dat, class == 'Test')
nrow(dat_train); nrow(dat_test)
glimpse(dat_train)


#3Preparing the time series object
dat[574,1] #end date
dat[1,1] #start date

#converting the data into a time series object
#training datasetr is coverted into a time series object
data_ts <- ts(dat_train[,6], start = c(1967,1), end = c(2014, 12), frequency = 12)

#creating a utility function for calculating mean absolute percentage error (or MAPE)
#MAPE used for evaluating the performance of the forecasting model

mape <- function(actual, pred){
  mape<- mean(abs((actual-pred)/actual))*100
  print(mape)
}
#when the function is called by giving the value, it gives the p[ercentage error


#Naive Forecasting Model
#most recent observation is used to forecast 
#naive() function is used

naive_model <- naive(data_ts, h = 12) # h = number of values i want to forecast
summary(naive_model)

#testing for the mean abosolute percentage error(mape)
dat_test$naive <- 2768 #2768 is the forecasted value
mape(dat_test$unemploy, dat_test$naive)

###Simple Exponential Smoothing
#ES sre an extension of the naive method 
se_model <- ses(data_ts, h = 12 )
summary(se_model)


##

df_fc = as.data.frame(se_model)
dat_test$simplexp = df_fc$`Point Forecast`
mape(dat_test$unemploy, dat_test$simplexp) #adding point foreact to dat_test dataframe
print(dat_test)

df_fc$`Point Forecast`



##Holts's Trend Method
##extension to the simple exponential smoothening
holt_model <- holt(data_ts, h = 12)
summary(holt_model)

##
df_holt <- as.data.frame(holt_model)
dat_test$holt = df_holt$`Point Forecast`
mape(dat_test$unemploy, dat_test$holt)

##ARIMA model test
arima_model <- auto.arima(data_ts)
summary(arima_model)


#The output shows 2.53 % mape error for training set

#for test set
fore_arima = forecast::forecast(arima_model, h = 12)
print(fore_arima)
df_arima = as.data.frame(fore_arima)
dat_test$arima = df_arima$`Point Forecast`
mape(dat_test$unemploy, dat_test$arima)
print(dat_test)

plot(data_ts)


##tbats
model_tbats <- tbats(data_ts)
summary(model_tbats)


for_tbats <- forecast::forecast(model_tbats, h = 12)
df_tbats = as.data.frame(for_tbats)
dat_test$tbats = df_tbats$`Point Forecast`
mape(dat_test$unemploy, dat_test$tbats) 

png(file = "timeseries12.png")
plot(dat_train)