setwd("E:/SUSTech/课程/HW_Project/ESE5023/repository/ESE5023_Assignments/PS5")
library("sp")
library("rgdal")
library("sf")
library("raster")
library("maps")

#5.1.1

#load Solar-radiation, 2.5 minutes in R
rlist_srad=list.files("E:/SUSTech/课程/HW_Project/ESE5023/repository/ESE5023_Assignments/PS5/wc2.1_2.5m_srad", pattern="tif$", full.names=T) 
srad_01 <- raster("E:/SUSTech/课程/HW_Project/ESE5023/repository/ESE5023_Assignments/PS5/wc2.1_2.5m_srad/wc2.1_2.5m_srad_01.tif")
srad <- srad_01
for(i in rlist_srad){ 
  srad_mon <- raster(i) 
  srad = srad_mon + srad
}
srad = srad-srad_01
srad_mean <- srad/12
srad_mean

#load Precipitation, 2.5 minutes in R
rlist_prec=list.files("E:/SUSTech/课程/HW_Project/ESE5023/repository/ESE5023_Assignments/PS5/wc2.1_2.5m_prec", pattern="tif$", full.names=T) 
prec_01 <- raster("E:/SUSTech/课程/HW_Project/ESE5023/repository/ESE5023_Assignments/PS5/wc2.1_2.5m_prec/wc2.1_2.5m_prec_01.tif")
prec <- prec_01
for(i in rlist_prec){ 
  prec_mon <- raster(i) 
  prec = prec_mon + prec
}
prec = prec-prec_01
prec_mean <- prec/12
prec_mean

#load Wind speed, 2.5 minutes in R
rlist_wind=list.files("E:/SUSTech/课程/HW_Project/ESE5023/repository/ESE5023_Assignments/PS5/wc2.1_2.5m_wind", pattern="tif$", full.names=T) 
wind_01 <- raster("E:/SUSTech/课程/HW_Project/ESE5023/repository/ESE5023_Assignments/PS5/wc2.1_2.5m_wind/wc2.1_2.5m_wind_01.tif")
wind <- wind_01
for(i in rlist_wind){ 
  wind_mon <- raster(i) 
  wind = wind_mon + wind
}
wind = wind-wind_01
wind_mean <- wind/12
wind_mean

#5.1.2

#Plot the above data sets over China. 
China <- readOGR("China_map", "bou2_4p")

srad_crop <- crop(srad_mean,China)
srad_crop2 <- mask(srad_crop,China,na.rm=T)
plot(srad_crop2, main="Solar radiation, 2.5 minutes in China")

prec_crop <- crop(prec_mean,China)
prec_crop2 <- mask(prec_crop,China,na.rm=T)
plot(prec_crop2, main="Precipitation, 2.5 minutes in China")

wind_crop <- crop(wind_mean,China)
wind_crop2 <- mask(wind_crop,China,na.rm=T)
plot(wind_crop2, main="Wind speed, 2.5 minutes in China")

#5.1.3

wind_crop3 <- wind_crop2
wind_crop3[which(wind_crop3@data@values < 4)] <- NA
plot(wind_crop3,main ='My favorite spots to build wind farms')
maps::map('world',add=T)
#西部比较适合建风电场，内蒙古中部地区也适合

#5.1.4

srad_crop3 <- srad_crop2
#光伏发电站的设置为年降雨量小于400ml，太阳辐射大于16000kJ/m^2/day
srad_crop3[which(prec_CN_m@data@values > 400|srad_crop3@data@values < 16000)] <- NA
plot(srad_crop3,main =' My favorite spots of PV farms')
maps::map('world',add=T)

#refer to胡兆平同学




