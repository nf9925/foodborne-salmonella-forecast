install.packages("readxl")
library(readxl)
library(tidyverse)
library(forecast)

###Importing the dataset first

library(readxl)
NationalOutbreakPublicDataTool=read_excel("F:/COPH HPMT 52173 Decision Analytics/Final Project/NationalOutbreakPublicDataTool.xlsx")
View(NationalOutbreakPublicDataTool)

#Shortening the title of the dataset
main_data=read_excel("F:/COPH HPMT 52173 Decision Analytics/Final Project/NationalOutbreakPublicDataTool.xlsx")

### (1) Creating a new data frame from the source data 
library("dplyr")
library(tidyverse)

# I am renaming one of the variables as I am facing problems while creating the new data frame with "Primary Mode"
main_data=main_data %>% rename(pri_mode = "Primary Mode")
view(main_data)

new_dataframe=main_data %>% select(Year, Month, pri_mode, Etiology, Illnesses)
view(new_dataframe)

### (2) Counting the number of missing values
missing=colSums(is.na(new_dataframe))

### (3) Removing missing values
new_dataframe=na.omit(new_dataframe)
view(new_dataframe)


### (4) Filtering dataframe based on "Food" related outbreaks
library("dplyr")
outbreaks_based_food=new_dataframe %>% filter(pri_mode == "Food")
view(outbreaks_based_food)


### (5) Filtering “Etiology” to any row that starts with the word “Salmonella”
outbreaks_based_salmonella=new_dataframe %>%filter(grepl("^Salmonella", Etiology))
view(outbreaks_based_salmonella)

### (6) Creating a timeseries data based on months for the number of illnesses related to Salmonella outbreaks. 

#I am creating a new variable "Date" from "Year" and "Month" first
Date=new_dataframe %>% mutate(Date = as.Date(paste(Year, Month, "01", sep = "-")))
view(Date)

ts_salmonella=outbreaks_based_salmonella %>% mutate(Date = as.Date(paste(Year, Month, "01", sep = "-"))) %>% select(Date, Illnesses) %>% group_by(Date) %>% summarise(total_illnesses = sum(Illnesses)) %>% ungroup()

#the timeseries
ts_salmonella=ts(ts_salmonella$total_illnesses, frequency = 12, start = c(1998, 1))
plot(ts_salmonella)


### (7) Developing a monthly seasonal plot
library(forecast)

ggseasonplot(ts_salmonella, year.labels = TRUE, year.labels.left = TRUE) + labs(title = "Monthly Seasonal Plot of Salmonella-related Illnesses",x = "Month", y = "Number of Illnesses")


### (8) Applying multiple forecasting methods to make predictions of future number of Salmonella-related illnesses. 

#For example, I plan to predict future number of Salmonella-related illnesses for the next 5 years so h = 5*12 = 60

autoplot(ts_salmonella)+autolayer(naive(ts_salmonella,h=60),PI=FALSE,series="naive")+ autolayer(meanf(ts_salmonella,h=60),PI=FALSE,series="Mean")+ autolayer(snaive(ts_salmonella,h=60),PI=FALSE,series = "snaive")+ autolayer(rwf(ts_salmonella,h=60,drift=TRUE),PI=FALSE,series = "Drift")


#Naive Method: fit1
fit1=naive(ts_salmonella,h = 60) 

#Average Method: fit2
fit2=meanf(ts_salmonella,h = 60)

#Seasonal Naive Method
fit3=snaive(ts_salmonella,h = 60)

#Drift Method
fit4=rwf(ts_salmonella,h=60,drift=TRUE)

library(forecast)

#ARIMA Method
fit5=auto.arima(ts_salmonella)
forecast_arima=forecast(fit5, h = 60)
plot(forecast_arima, xlab = "Time", ylab = "Number of Illnesses", main = "Forecast using ARIMA")

#Exponential Smoothing Method
fit6=ets(ts_salmonella)
forecast_ets=forecast(fit6, h = 60)
plot(forecast_ets, xlab = "Time", ylab = "Number of Illnesses", main = "Forecast using Exponential Smoothing")

library(forecast)

#Neural Networks
fit7=nnetar(ts_salmonella)
forecast_nn=forecast(fit7, h = 60)  
plot(forecast_nn, xlab = "Time", ylab = "Number of Illnesses", main = "Forecast using Neural Network")

#TBATS model
fit8=tbats(ts_salmonella)
forecast_tbats=forecast(fit8, h = 60)
plot(forecast_tbats, xlab = "Time", ylab = "Number of Illnesses", main = "Forecast using TBATS Method")

#BATS model
fit9=bats(ts_salmonella)
forecast_bats=forecast(fit9, h = 60)
plot(forecast_bats, xlab = "Time", ylab = "Number of Illnesses", main = "Forecast using BATS Method")

#Holt's Linear Method
fit10=HoltWinters(ts_salmonella, beta = FALSE, gamma = FALSE)
forecast_horizon_holt=60  
forecast_holt=forecast(fit10, h = 60)
plot(forecast_holt, xlab = "Time", ylab = "Values", main = "Forecast using Holt's Linear Method ")


### (9) Checking the accuracy
library(forecast)

accuracy(fit1)
accuracy(fit2)
accuracy(fit3)
accuracy(fit4)
accuracy(fit5)
accuracy(fit6)
accuracy(fit7)
accuracy(fit8)
accuracy(fit9)
accuracy(fit10)

###Conclusion regarding which forecasting methods has the highest accuracy###
#Firstly, I am considering fit 6 i.e., the BATS model to have the highest accuracy because of its lowest RMSE values compared with that of rest of the models. Secondly, fit 6 has the second-lowest MAPE value. Last but not the least, in comparison with the metrics of fit1 (naive method), fit 6 has a lower MASE value, much lesser than 1. 



