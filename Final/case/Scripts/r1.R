setwd("E:/SUSTech/课程/HW_Project/ESE5023/Final project/global_mean_sea_level")
require(data.table)
dat <- read.table("GMSL_TPJAOS_4.2_199209_202004.txt")
names(dat) <- c("Altimeter Type", "GSML-南海", "GSML-波斯湾", "GSML-墨西哥湾", "Weighted Obs", "GMSL", "GSML-北海", "GSML-挪威海", "GSML-孟加拉湾", "GSML-日本海", "GSML-加利福尼亚湾", "GSML-阿拉伯海")
dat[dat == 99000] <- NA
dat$`Altimeter Type` <- factor(dat$`Altimeter Type`, levels = c(0,999), labels = c("dual frequency", "signle frequency"))
require(ggplot2)
library(ggcorrplot)
library(ggExtra)
ggplot(data = dat, aes(x=`Altimeter Type`, y=`GSML-南海`, fill = `Altimeter Type`)) +
  geom_boxplot(width = 0.4)
ggplot(data = dat, aes(x=`Altimeter Type`, y=`GSML-波斯湾`, fill = `Altimeter Type`)) +
  geom_boxplot(width = 0.4)
ggplot(data = dat, aes(x=`Altimeter Type`, y=`GSML-墨西哥湾`, fill = `Altimeter Type`)) +
  geom_boxplot(width = 0.4)
ggplot(data = dat, aes(x=`Altimeter Type`, y=`GSML-北海`, fill = `Altimeter Type`)) +
  geom_boxplot(width = 0.4)
ggplot(data = dat, aes(x=`Altimeter Type`, y=`GSML-挪威海`, fill = `Altimeter Type`)) +
  geom_boxplot(width = 0.4)
ggplot(data = dat, aes(x=`Altimeter Type`, y=`GSML-孟加拉湾`, fill = `Altimeter Type`)) +
  geom_boxplot(width = 0.4)
ggplot(data = dat, aes(x=`Altimeter Type`, y=`GSML-日本海`, fill = `Altimeter Type`)) +
  geom_boxplot(width = 0.4)
ggplot(data = dat, aes(x=`Altimeter Type`, y=`GSML-加利福尼亚湾`, fill = `Altimeter Type`)) +
  geom_boxplot(width = 0.4)
ggplot(data = dat, aes(x=`Altimeter Type`, y=`GSML-阿拉伯海`, fill = `Altimeter Type`)) +
  geom_boxplot(width = 0.4)

