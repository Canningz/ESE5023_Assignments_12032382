# Read the hourly data 
Met_Data <- read.csv(file = "2281305.csv")
DV <- Met_Data$VIS
print(DV)
num=length(DV)
print(num)
# Get variation
range <- list()
dqc <- list()
vc <- list()
qvc <- list()
del_label <-c()
# Split the variation string
for(i in 1:num){
  range()[i] <- as.numeric(substr(DV[i:i],1,6))
  dqc[i] <- substr(DV[i:i],8,8)
  vc[i] <- substr(DV[i:i],10,10)
  qvc[i] <-substr(DV[i:i],12,12)
  
}
# Get the values of del_label
for (j in 1:num){
  if (range[j] <0 | range[j]>160000 ){ 
    del_label= c(del_label,j)
  }
}
for (t in 1:num){
  if (dqc[t] != '1' | vc[t] !='N' | qvc[t] != '1'){ 
    del_label= c(del_label,t)
  }
}
# Get the values of data_all_filter
data_vis3=DV[del_label]   
data_all_filter=data[del_label]
print(length(data_all_filter))
# Split the variation string then Convert character to double
filter_dis <- list()
filter_num=length(data_vis3)
for (d in 1: filter_num ){
  filter_dis[d] <- as.numeric(substr(data_vis3[d:d],1,6))
}
print(filter_num)
# Convert the character vector to date vector
Obs_Time = data_all_filter$DATE
print(length(Obs_Time))
Obs_Time <- Obs_Time[-del_label]
print(length(Obs_Time))
Obs_Time2=as.Date(Obs_Time)
# Plot
plot(Obs_Time2,filter_dis,lwd=0.5,type='l',col='blue')
#春天冬天的能见度较差，夏天秋天不好。可能是由于春冬季节供暖排放所致
#三年的数据成周期变化



#第二题
vis20100_5 <- 0
vis20105_10 <- 0
vis201010_15 <- 0
vis201015_20 <- 0
vis201020_25 <- 0
vis201025_30 <- 0
vis2010_30_ <- 0

vis20110_5 <- 0
vis20115_10 <- 0
vis201110_15 <- 0
vis201115_20 <- 0
vis201120_25 <- 0
vis201125_30 <- 0
vis2011_30_ <- 0

vis20120_5 <- 0
vis20125_10 <- 0
vis201210_15 <- 0
vis201215_20 <- 0
vis201220_25 <- 0
vis201225_30 <- 0
vis2012_30_ <- 0

vis20130_5 <- 0
vis20135_10 <- 0
vis201310_15 <- 0
vis201315_20 <- 0
vis201320_25 <- 0
vis201325_30 <- 0
vis2013_30_ <- 0

for(p in 1:98133){
  
  if(2010 %in% as.numeric(substr(Obs_Time2[p],1,4)) == TRUE ){
    if(filter_dis[p] < 5000){
      vis20100_5 = vis2010_5 +1
      
    }else if(filter_dis[p] < 10000){
      vis20105_10 = vis201005_10 +1
      
    }else if(filter_dis[p] < 15000){
      vis201010_15 = vis201010_15 +1
      
    }else if(filter_dis[p] < 20000){
      vis201015_20 = vis201015_20 +1
      
    }else if(filter_dis[p] < 25000){
      vis201020_25 = vis201020_25 +1
      
    }else if(filter_dis[p] < 30000){
      vis201025_30 = vis201025_30 +1
      
    }else {
      vis2010_30_ = vis2010_30_ +1
      
    }
    
  }else if(2011 %in% as.numeric(substr(Obs_Time2[p],1,4)) == TRUE ){
    if(filter_dis[p] < 5000){
      vis20110_5 = vis20110_5 +1
      
    }else if(filter_dis[p] < 10000){
      vis20115_10 = vis20115_10 +1
      
    }else if(filter_dis[p] < 15000){
      vis201110_15 = vis201110_15 +1
      
    }else if(filter_dis[p] < 20000){
      vis201115_20 = vis201115_20 +1
      
      
    }else if(filter_dis[p] < 25000){
      vis201120_25 = vis201120_25 +1
      
    }else if(filter_dis[p] < 30000){
      vis201125_30 = vis201125_30 +1
      
    }else {
      vis2011_30_ = vis2011_30_ +1
      
    }
    
  }else if(2012 %in% as.numeric(substr(Obs_Time2[p],1,4)) == TRUE ){
    if(filter_dis[p] < 5000){
      vis20120_5 = vis20120_5 +1
      
    }else if(filter_dis[p] < 10000){
      vis20125_10 = vis20125_10 +1
      
    }else if(filter_dis[p] < 15000){
      vis201210_15 = vis201210_15 +1
      
    }else if(filter_dis[p] < 20000){
      vis201215_20 = vis201215_20 +1
      
    }else if(filter_dis[p] < 25000){
      vis201220_25 = vis201220_25 +1
      
    }else if(filter_dis[p] < 30000){
      vis201225_30 = vis201225_30 +1
      
    }else {
      vis2012_30_ = vis2012_30_ +1
      
    }
    
  }else if(2013 %in% as.numeric(substr(Obs_Time2[p],1,4)) == TRUE ){
    if(filter_dis[p] < 5000){
      vis20130_5 = vis20130_5 +1
      
    }else if(filter_dis[p] < 10000){
      vis20135_10 = vis20135_10 +1
      
    }else if(filter_dis[p] < 15000){
      vis201310_15 = vis201310_15 +1
      
    }else if(filter_dis[p] < 20000){
      vis201315_20 = vis201315_20 +1
      
    }else if(filter_dis[p] < 25000){
      vis201320_25 = vis201320_25 +1
      
    }else if(filter_dis[p] < 30000){
      vis201325_30 = vis201325_30 +1
      
    }else {
      vis2013_30_ = vis2013_30_ +1
    }
    
  }
  
}


colors <- c("pink","red","brown",'yellow','blue','green','orange')
years <- c("2010","2011","2012","2013")
regions <- c("vis0_5","vis5_10","vis10_15",'vis15_20','vis20_25','vis25_30','vis_30_')


# Create the matrix of the values.
Values <- matrix(c(vis20100_5,vis20110_5,vis20120_5,vis20130_5,
                   vis20105_10,vis20115_10,vis20125_10,vis20135_10,
                   vis201010_15,vis201110_15,vis201210_15,vis201310_15,
                   vis201015_20,vis201115_20,vis201215_20,vis201315_20,
                   vis201020_25,vis201120_25,vis201220_25,vis201320_25,
                   vis201025_30,vis201125_30,vis201225_30,vis201325_30,
                   vis2010_30_,vis2011_30_,vis2012_30_,vis2013_30_),
                 nrow=7,ncol=4,byrow=TRUE)

png(file = "2010-2013变化图.png")
barplot(Values,main="visibility of 2010-2013",names.arg=years,xlab="years",ylab="m",col=colors)
legend("topright", regions, cex=1.3, fill=colors)
dev.off()
#参考CSDN
#https://blog.csdn.net/g863402758/article/details/53389564
#https://zhu-group.github.io/ese5023/Section_03.html#Matrix_and_array
#黄锴