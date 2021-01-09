setwd("E:/SUSTech/课程/HW_Project/ESE5023/Final project/global_mean_sea_level")
require(data.table)
dat <- read.table("GMSL_TPJAOS_4.2_env_parameter.txt")
names(dat) <- c("Altimeter Type", "Timeline", "Mid-cycle", "Obs", "Weighted Obs", "GMSL", "Variation", "60-day Gaussian Type Filter", "GIA", "10 SD od GMSL", "GSML (GIA)", "GSML (GIA) Annual and Semi-annual")
dat[dat == 99000] <- NA
dat$`Altimeter Type` <- factor(dat$`Altimeter Type`, levels = c(0,999), labels = c("dual frequency", "signle frequency"))
require(ggplot2)
library(ggcorrplot)
library(ggExtra)

#Mid-cycle变化趋势
ggplot(data = dat, aes(x=`Timeline`, y=`Mid-cycle`)) +
  geom_point(aes(col = `Altimeter Type`)) +
  facet_grid(~`Altimeter Type`)

#平均海平面变化趋势
ggplot(data = dat, aes(x=`Timeline`, y=`GMSL`, col =`Altimeter Type`)) +
  geom_point() +
  geom_smooth(method = "loess") +
  facet_grid(~`Altimeter Type`)


#经过高斯滤波器处理60天海平面数据走向趋势
ggplot(data = dat, aes(x=`Timeline`, y=`60-day Gaussian Type Filter`, col =`Altimeter Type`)) +
  geom_point() +
  geom_smooth(method = "loess") +
  facet_grid(~`Altimeter Type`)


#全球海平面年度增量方差变化

ggplot(data = dat, aes(x=`Timeline`, y=Variation, col =`Altimeter Type`)) +
  geom_point() +
  geom_smooth(method = "loess") +
  facet_grid(~`Altimeter Type`)

#海平面高度变化的同时，冰山脱离冰架的情况
ggplot(data = dat, aes(x=`Timeline`, y=`GIA`, col =`Altimeter Type`)) +
  geom_point() +
  geom_smooth(method = "loess") +
  facet_grid(~`Altimeter Type`)


#冰川架附近海域海平面高度变化情况
ggplot(data = dat, aes(x=`Timeline`, y=`GSML (GIA)`, col =`Altimeter Type`)) +
  geom_point() +
  geom_smooth(method = "loess") +
  facet_grid(~`Altimeter Type`)

#冰川架附近海域半年度海平面高度变化情况
ggplot(data = dat, aes(x=`Timeline`, y=`GSML (GIA) Annual and Semi-annual`, col =`Altimeter Type`)) +
  geom_point() +
  geom_smooth(method = "loess") +
  facet_grid(~`Altimeter Type`)

#卫星测高
ggplot(data = dat, aes(x=`Timeline`, y=`10 SD od GMSL`, col =`Altimeter Type`)) +
  geom_point() +
  geom_smooth(method = "loess") +
  facet_grid(~`Altimeter Type`)

corr <- cor(dat[, 2:12])
ggcorrplot(corr, hc.order = TRUE, type = "lower",
           outline.col = "white",
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"))


#海平面高度变化的同时，冰川架附近水面高度演化
p <- ggplot(dat, aes(x=`Mid-cycle`, y=`GMSL`, color=`Altimeter Type`)) +
  geom_point() +
  theme(legend.position="none")
ggMarginal(p, type="histogram")
ggMarginal(p, type="density")
ggMarginal(p, type="boxplot")
my_cols <- c("#00AFBB", "#E7B800", "#FC4E07")  
pairs(dat[,6:8], pch = 19,  cex = 0.5,
      col = my_cols[dat$`Altimeter Type`],
      lower.panel=NULL)
hist(dat$Variation, probability = TRUE)
lines(density(dat$Variation, adjust=2), lty="dotted", col="darkgreen", lwd=2) 

