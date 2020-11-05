setwd("E:/SUSTech/¿Î³Ì/HM&Project/ESE5023/repository/ESE5023_Assignments/PS3")
library(tidyr)
library(dplyr)
library(ggplot2)

BigBang <- read.csv("Big Bang.csv",header = T)

Vel <- BigBang$Velocity
Dis <- BigBang$Distance
Dis <- Distance * 1e6 * 1e12 * 30.9

plot(Vel,Dis,xlab = "Velocity(km per second)", ylab = "Distance(km)")

fit1 <- lm(Dis~Vel)
abline(fit1, lwd = 2,col = 'red')
summary(fit1)

fit2 <- lm(Dis~Vel-1)
abline(fit2, lwd = 2,col = 'blue')
summary(fit2)
