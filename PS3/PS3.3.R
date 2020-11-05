setwd("E:/SUSTech/¿Î³Ì/HM&Project/ESE5023/repository/ESE5023_Assignments/PS3")
library(tidyr)
library(dplyr)
library(ggplot2)

vegetarian<-c(185,197,189,181,150,176,171,174,202,171,207,125,189,179,163,174,184,186)
non_vegetarian<-c(210,139,172,198,177,rep(NA,13))

one_way_anova<- aov(vegetarian ~ non_vegetarian)
summary(one_way_anova) 