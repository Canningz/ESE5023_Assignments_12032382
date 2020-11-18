library(tidyr)
library(dplyr)
library(ggplot2)

shengzhen <- as_tibble(read.csv("2281305.csv",header = T))

Shenzhen_Data <- shengzhen %>%   
  select(DATE,WND) %>%
  
  mutate(wind1 = as.numeric(substr(WND,1,3)),
         
         wind2 = substr(WND,5,5),wind3 = substr(WND,7,7),
         
         wind4 = as.numeric(substr(WND,9,12)),wind5 = substr(WND,14,14)) %>% 
  
  mutate(date=as.Date(DATE),month_year=substr(DATE,1,7),year=substr(DATE,1,4) ,
         wind_1_new =ifelse(wind1>0 & wind1 <360,wind1,NA),
         wind_2_new =ifelse(wind2 =='1',wind2,NA),
         wind_3_new=ifelse(wind3=="N",wind3,NA),
         wind_4_new =ifelse(wind4>0 &wind4<900,wind4,NA),
         wind_5_new=ifelse(wind5=='1',wind5,NA)) %>%
  
  group_by(month_year) %>% 
  summarise(date,month_year,month_mean=mean(wind_4_new,na.rm = TRUE)) 
  
  Shenzhen_Data %>%                 
  ggplot(aes(date,month_mean)) + 
  geom_line() 

  #referª∆Ô«