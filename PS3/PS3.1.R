setwd("E:/SUSTech/¿Î³Ì/HM&Project/ESE5023/repository/ESE5023_Assignments/PS3")
library(tidyr)
library(dplyr)
library(ggplot2)

#create datasets
Rainfall_unseed <- c(1202.6, 830.1, 372.4, 345.5, 321.2, 244.3, 163.0, 
            147.8, 95.0, 87.0, 81.2, 68.5, 47.3, 41.1, 36.6, 
            29.0, 28.6, 26.3, 26.0, 24.4, 21.4, 17.3, 11.5, 4.9, 4.9, 1.0)

Rainfall_seed <- c(2745.6, 1697.1, 1656.4, 978.0, 703.4, 489.1, 430.0, 
          334.1, 302.8, 274.7, 274.7, 255.0, 242.5, 200.7, 198.6, 
          129.6, 119.0, 118.3, 115.3, 92.4, 40.6, 32.7, 31.4, 17.5, 7.7, 4.1)
 
#coefficient caculation         
Rainfall_unseed %>%
  summarise(
    count = n(),
    mean_Rainfall_unseed = mean(Rainfall_unseed, na.rm = TRUE),
    sd_Rainfall_unseed = sd(Rainfall_unseed, na.rm = TRUE)
  )
Rainfall_seed %>%
  summarise(
    count = n(),
    mean_Rainfall_seed = mean(Rainfall_seed, na.rm = TRUE),
    sd_Rainfall_seed = sd(Rainfall_seed, na.rm = TRUE)
  )
  
#Plot two box plots side-by-side of the data.
Rainfall<-cbind(Rainfall_unseed,Rainfall_seed)
rf<-as_tibble(Rainfall)
boxplot(rf$Rainfall_unseed,rf$Rainfall_seed,
        xlab = "unseed_and_seed",
        ylab = "rainfall",
        main = "Rainfall",
        cex = 1)

t.test(Rainfall_unseed, Rainfall_seed)

