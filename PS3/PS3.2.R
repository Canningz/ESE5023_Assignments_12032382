setwd("E:/SUSTech/¿Î³Ì/HM&Project/ESE5023/repository/ESE5023_Assignments/PS3")
library(tidyr)
library(dplyr)
library(ggplot2)

Tyrannosaurus_bone <- read.csv("PS3.2.csv",header = T)
Tyrannosaurus_bone <- as_tibble(Tyrannosaurus_bone)
ggplot(Tyrannosaurus_bone, aes(x = Bone, y = Oxygen, fill = Bone)) +
  geom_boxplot() +
  theme_classic()

anova_one_way <- aov(Oxygen~factor(Bone), data = Tyrannosaurus_bone)
summary(anova_one_way)