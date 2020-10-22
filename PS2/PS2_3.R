setwd("D:/Document/SUSTech/¿Î³Ì/HM&Project/ESE5023/repository/ESE5023_Assignments/PS2/RECALL")
library(tidyr)
library(dplyr)
library(ggplot2)

Trading_Value <- as_tibble(read.csv("Trading.csv",header = T))

Trading_Value %>% 
  
  filter(Partner.Country.Code == 96) %>% 
  
  ggplot(aes(Year,Value*1000))+
  
  geom_line()+
  
  labs(title = "Total Trading Value")+
  
  xlab("Year")+
  
  ylab('value($)')