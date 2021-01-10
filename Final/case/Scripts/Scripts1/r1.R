setwd("E:/SUSTech/�γ�/HW_Project/ESE5023/Final project/global_mean_sea_level")
require(data.table)
dat <- read.table("GMSL_TPJAOS_4.2_199209_202004.txt")
names(dat) <- c("Altimeter Type", "GSML-�Ϻ�", "GSML-��˹��", "GSML-ī������", "Weighted Obs", "GMSL", "GSML-����", "GSML-Ų����", "GSML-�ϼ�����", "GSML-�ձ���", "GSML-������������", "GSML-��������")
dat[dat == 99000] <- NA
dat$`Altimeter Type` <- factor(dat$`Altimeter Type`, levels = c(0,999), labels = c("dual frequency", "signle frequency"))
require(ggplot2)
library(ggcorrplot)
library(ggExtra)
ggplot(data = dat, aes(x=`Altimeter Type`, y=`GSML-�Ϻ�`, fill = `Altimeter Type`)) +
  geom_boxplot(width = 0.4)
ggplot(data = dat, aes(x=`Altimeter Type`, y=`GSML-��˹��`, fill = `Altimeter Type`)) +
  geom_boxplot(width = 0.4)
ggplot(data = dat, aes(x=`Altimeter Type`, y=`GSML-ī������`, fill = `Altimeter Type`)) +
  geom_boxplot(width = 0.4)
ggplot(data = dat, aes(x=`Altimeter Type`, y=`GSML-����`, fill = `Altimeter Type`)) +
  geom_boxplot(width = 0.4)
ggplot(data = dat, aes(x=`Altimeter Type`, y=`GSML-Ų����`, fill = `Altimeter Type`)) +
  geom_boxplot(width = 0.4)
ggplot(data = dat, aes(x=`Altimeter Type`, y=`GSML-�ϼ�����`, fill = `Altimeter Type`)) +
  geom_boxplot(width = 0.4)
ggplot(data = dat, aes(x=`Altimeter Type`, y=`GSML-�ձ���`, fill = `Altimeter Type`)) +
  geom_boxplot(width = 0.4)
ggplot(data = dat, aes(x=`Altimeter Type`, y=`GSML-������������`, fill = `Altimeter Type`)) +
  geom_boxplot(width = 0.4)
ggplot(data = dat, aes(x=`Altimeter Type`, y=`GSML-��������`, fill = `Altimeter Type`)) +
  geom_boxplot(width = 0.4)
