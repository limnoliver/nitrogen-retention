
library(RcppRoll)
source('R/AddAlpha.R')

list.files(getwd())
setwd("E:/Dropbox/ArcGIS")

# data<-read.table('UMR_AllDays_Route.txt', header=TRUE, sep="," ,skip=0)
data<-read.table('UMR_AllDays_Route2.txt', header=TRUE, sep="," ,skip=0)
data$riverkm<-data$MEAS/1000

data<-data[order(data$MEAS, decreasing=FALSE),]

data2<-data[data$NITRATEM>0,]
data3<-data2[data2$TurbFNU>0,]

plot(data2$MEAS,  data2$NITRATEM, type="p" )
plot(data2$MEAS,  data2$TurbFNU, type="p" )
plot(data2$MEAS,  data2$CH4uM_t, type="p" )
plot(data2$MEAS,  data2$ChlARFU, type="p" )

dams<-read.csv('DamsAlongRoute3.csv', sep=",", header=TRUE)
dams$riverkm<-dams$MEAS/1000
dams<-dams[order(dams$MEAS, decreasing=FALSE),]
dams$name<-c('SAF-U', 'SAF-L', 1,2,3,4,5,'5a', 6,7,8,9,10,11,12,13,14,15,16,17,18, 19, 20, 21, 22, 24, 25, '26')

dams2<-dams[dams$riverkm>10,]

tribs<-read.csv('TribsAlongRoute.csv', sep=",", header=TRUE)
tribs$riverkm<-tribs$MEAS/1000
tribs<-tribs[order(tribs$MEAS, decreasing=FALSE),]

tribs$name2<-c("MN", "SC", "Ch", "Bl", "Rt", "WI", "Rk", "IA", "DM", "IL", "MO", "OH")
tribs2<-tribs[tribs$NAME!="Black River",]

list.files('ChannelWidth')
width<-read.csv('ChannelWidth/MainChannelWidth2_per100m.csv', header=T, stringsAsFactors = F)
widthtable<-read.csv('ChannelWidth/UMRMainChannelWidthTable2.csv', header=T, stringsAsFactors = F)
widthtable$riverkm<-round(widthtable$MEAS/1000, 2)

head(widthtable)
head(data2)

plot(widthtable$riverkm, widthtable$WIDTH)
points(data2$riverkm, data2$NITRATEU, col=2, type='p')

#Sort based on time
data3<-data2[order(data2$ltime),]

#Rolling means
data3$rollx<-roll_mean(data3$riverkm, 3, fill=NA)
data3$rollNO3<-roll_mean(data3$NITRATEM, 3, fill=NA)

#slopes
data3$dt<-c(NA,diff(data3$ltime))
data3$dx<-c(NA,diff(data3$rollx))
data3$dno3<-c(NA,diff(data3$rollNO3))

data4<-data3[which(data3$dx>0 & data3$dx<1),]
data5<-data4[which(data4$dt<40),]

plot(data5$riverkm, col=add.alpha('black', 0.1), pch=16)
abline(h=c(250,280,990,1050))
head(data5)

