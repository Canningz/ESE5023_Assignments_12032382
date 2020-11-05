setwd("E:/SUSTech/¿Î³Ì/HM&Project/ESE5023/repository/ESE5023_Assignments/PS3")
library(tidyr)
library(dplyr)
library(ggplot2)
library(MASS)
library(leaps)
data(cpus)
str(cpus)

cpus%>%
  mutate(mean_perf=mean(perf))
sample_index <- sample(nrow(cpus),nrow(cpus)*0.80)
cpus_train <- cpus[sample_index,]
cpus_test  <- cpus[-sample_index,]

subset_result <- regsubsets(perf ~ syct+ mmin + mmax + cach +
                                  chmin + chmax, data=cpus_train, nbest=2, nvmax = 6)
plot(subset_result, scale="bic")
summary(subset_result)


model1=lm(perf ~ syct+ mmin + mmax + cach +
               chmin + chmax, data=cpus_train)
model_step_b <- step(model1,direction='backward')
summary(model1)

perf_predict <- predict(model1,cpus_test)
plot(cpus_test$mean_perf, perf_predict)

mean(cpus_test$perf_predict)