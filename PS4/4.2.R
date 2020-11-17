library(tidyr) 
library(dplyr) 
library(ggplot2)

# Read the hourly data 
BaoAn_Data   <- read.csv(file = "2281305.csv", header = T)

#1.Construct a time series
BaoAn_Data_tb <- as_tibble(BaoAn_Data)
Tem_set<-BaoAn_Data_tb %>%
  mutate(Tem_new = ifelse(substr(TMP,7,7) == "1" |substr(TMP,1,5) != '+9999',
                      as.numeric(substr(TMP,1,5))*0.1,NA)) %>%
  mutate(YM = paste(substr(DATE,1,4),substr(DATE,6,7),sep = "")) %>% 
  filter(YM >= 201001 & YM <= 202008) %>%
  group_by(YM) %>% 
  summarise(Monthly_Mean=mean(Tem_new,na.rm = T))

Monthly_Mean <- ts(Tem_set$Monthly_Mean, start=c(2010,1),end = c(2020,08) , frequency=12)
plot(Monthly_Mean, type="l")

#2.Decompose the time series&Check whether the error part follows a white noise distribution
Monthly_Mean_components <- decompose(Monthly_Mean)
plot(Monthly_Mean_components)
# Plot hist
hist(Monthly_Mean_components$random, prob=TRUE)
# Add pdf
curve(dnorm(x, mean=mean(Monthly_Mean_components$random,na.rm=T),
            sd=sd(Monthly_Mean_components$random,na.rm=T)),
      add=TRUE, col="red")

#3.Fit an ARIMA model
Monthly_Mean_diff1 <- diff(Monthly_Mean, differences=2)
plot.ts(Monthly_Mean_diff1)
Monthly_Mean_diff2 <- diff(Monthly_Mean, differences=12)
plot.ts(Monthly_Mean_diff2)
# plot a correlogram
acf(Monthly_Mean_diff1)  
# plot a partial correlogram
pacf(Monthly_Mean_diff1) 
model <- auto.arima(Monthly_Mean)
summary(model)

#4.Predict monthly-averaged temperatures with the ARIMA model
# fit an ARIMA(3,1,1) model
Monthly_Meanarima <- arima(Monthly_Mean, order=c(3,1,1)) 
Monthly_Meanarima

# load the "forecast" R library
library(forecast)

Monthly_Meanforecasts <- forecast(Monthly_Meanarima, h=12)
Monthly_Meanforecasts
plot(Monthly_Meanforecasts)
acf(Monthly_Meanforecasts$residuals)
Box.test(Monthly_Meanforecasts$residuals, type="Ljung-Box")

# make time plot of forecast errors
plot.ts(Monthly_Meanforecasts$residuals)
# make a histogram
plot(Monthly_Meanforecasts$residuals)

plot(forecast(Monthly_Meanarima,h=12))

#获取9月数据真值，10月数据没有
Tem_set<-BaoAn_Data_tb %>%
  mutate(Tem_new = ifelse(substr(TMP,7,7) == "1" |substr(TMP,1,5) != '+9999',
                          as.numeric(substr(TMP,1,5))*0.1,NA)) %>%
  mutate(YM = paste(substr(DATE,1,4),substr(DATE,6,7),sep = "")) %>% 
  filter(YM == 202009) %>%
  group_by(YM) %>% 
  summarise(Monthly_Mean=mean(Tem_new,na.rm = T)) %>% 
  pull()

