library(MASS)
library(leaps)
library(tidyr)
library(ggplot2)
library(dplyr)


#7-1
hist(forbes$pres,main="barometric pressure in inches of mercury",xlab = "Pres")
hist(forbes$bp,main="boiling point (degrees Farenheit)",xlab = "Bp")
t.test(forbes$pres, forbes$bp)


#7-2
SH <- as_tibble(ships) %>%
  mutate(type = factor(type, ordered = TRUE)) %>% 
  mutate(year = factor(year, ordered = TRUE)) %>% 
  mutate(period = factor(period, ordered = TRUE))   
# Quick check
glimpse(SH)
levels(SH$type)
SH %>%
  group_by(type) %>%
  summarise(
    count = n(),
    mean_incidents = mean(incidents, na.rm = TRUE),
    sd_incidents = sd(incidents, na.rm = TRUE)
  )
ggplot(SH, aes(x = type, y = incidents, fill = type)) +
  geom_boxplot() +
  theme_classic()
anova_one_way <- aov(incidents ~ type, data = SH)
summary(anova_one_way)


#7-3
str(ships)
# plotting the data to determine the linearity
plot(ships, main="Matrix Scatterplot")
model <- lm(incidents ~ type + year + period + service, data = ships)
coef(model)
summary(model)
cor(ships$year, ships$service)
model2 <- lm(incidents ~ type + year + period + type:year, data = ships)
summary(model2)