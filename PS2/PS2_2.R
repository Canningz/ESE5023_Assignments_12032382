setwd("D:/Document/SUSTech/¿Î³Ì/HM&Project/ESE5023/repository/ESE5023_Assignments/PS2/RECALL")
library(tidyr)
library(dplyr)
library(ggplot2)

shengzhen <- as_tibble(read.csv("2281305.csv",header = T))

shengzhen %>% 
  
  select(DATE,WND) %>% 
  
  mutate(wind1 = as.numeric(substr(WND,1,3)),
         
         wind_2 = substr(WND,5,5),wind_3 = substr(WND,7,7),
         
         wind4 = as.numeric(substr(WND,9,12)),wind_5 = substr(WND,14,14)) %>% 
  
  mutate(date=as.Date(DATE),month_year=substr(DATE,1,7),year=substr(DATE,1,4) ,
         
         winspeed1 =ifelse(wind1>0 & wind1 <360,wind1,NA),
         
         speed2 =ifelse(wind2 =='1',wind2,NA),speed3=ifelse(wind3=="N",wind3,NA),
         
         speed4 =ifelse(wind4>0 &wind4<900,wind4,NA),speed5=ifelse(wind5=='1',wind5,NA)) %>%
  
  group_by(month_year) %>% 
  
  summarise(date,month_year,month_mean=mean(speed4,na.rm = TRUE)) 

  ggplot(aes(dat,month_mean,color = year)) +
  
  geom_line() +
  
  labs(title = "Monthly average wind speed from 2010 to 2020")+
  
  ylab("Monthly average wind speed(m/s)")+
  
  xlab("Month")
  
  #refer»ÆïÇ