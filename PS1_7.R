#7.1
Met_Data <- read.csv(file = '20162020',header = T)
colnames(pressureindex)
head(pressureindex)
data_nyu <- pressureindex $NYU
data_caltech <- pressureindex $CALTech
data_mit <- pressureindex $MIT
data_nyu[which(data_nyu==--10000)] <- NA
data_caltech[which(data_caltech==-10000)] <- NA
data_mit[which(data_mit==-10000)] <- NA

#7.2
time_data <- pressureindex $Time
time_data1 <- as.Date(time_data)
plot(time_data1,data_710,col='blue',type='l')