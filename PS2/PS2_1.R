setwd('D:/Document/SUSTech/课程/HM&Project/ESE5023/参考资料')
library(tidyr)
library(dplyr)
library(ggplot2)


#1.1

Sig_Eqs <- as_tibble(read.table(file ="signif.txt",sep = "\t",header = TRUE,stringsAsFactors = F,quote = ""))


#1.2
Sig_Eqs  %>%
  
  filter(YEAR >= -2150) %>% 
  
  select(COUNTRY,DEATHS) %>% 
  
  group_by(COUNTRY) %>% 
  
  summarize(total_death = sum(DEATHS,na.rm = T)) %>% 
  
  arrange(desc(total_death)) %>% 
  
  head(10)


#1.3
Sig_Eqs %>%
  
  select(YEAR,EQ_PRIMARY) %>%
  
  filter(EQ_PRIMARY >= 6) %>%
  
  group_by(YEAR) %>%
    
  summarize(sum = n()) %>%
    
  ggplot(aes(YEAR,sum)) +
  
  geom_line()


#1.4
CountEq_LargestEq <- function(country){
  
  CountEq = Sig_Eqs %>%
    
    select(COUNTRY,EQ_PRIMARY) %>%  
    
    filter(COUNTRY == country) 
  
  CountEq_num = as.numeric(nrow(CountEq))  
  
  Date_Eq = Sig_Eqs %>%
    
    filter(COUNTRY == country) %>%  
    
    select(COUNTRY,EQ_PRIMARY,DAY,MONTH,YEAR) %>%   
    
    mutate(Eq_date = as.character(paste0(YEAR,"-",MONTH,"-",DAY))) %>%
    
    arrange(desc(EQ_PRIMARY)) 
  
  country_date = Date_Eq$date[(Date_Eq$EQ_PRIMARY == max(Date_Eq$EQ_PRIMARY,na.rm = TRUE )) ]
 
  country_date=country_date[!is.na(country_date)]
 
  return(c(CountEq_num, country_date))
}
