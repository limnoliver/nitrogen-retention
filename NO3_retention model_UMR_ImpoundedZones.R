
# Workflow to calculate N retention prediction for each pool of the Upper Mississippi River
# Sample dates - Aug 1-13, 2015 

library(zoo)
library(dataRetrieval)
library(gtools)

#Download USGS gauge data for UMR rivers and select tributaries

parameterCd <- c("00060", "99133") # Discharge, NO3
startDate <- "2015-08-01"
endDate <- "2015-08-14"

# Get USGS gauge data for all UMR stations.
siteNumbers<-c('05288930', # Franklin
               '05331000', # St. Paul *** (pools 1)
               '05331580', # Hastings (LD 2) *** (pools 2-LakePepin)
               '05344980', # Red Wing (LD 3)
               '05355341', # Below Lake Pepin
               '05378500', # Winona (LD 5a) *** (pools 4-9)
               '05389500', # McGregor
               '05411500', # Clayton
               '05420400', # Fulton(LD 13)
               '05420500', # Clinton *** (pools 10-15)
               '05474500', # Keokuk (LD 19) *** (pools 16-19)
               '05501600', # Hannibal
               '05587450', # At Grafton *** (pools 20-25)
               '05587455', # Below Grafton 
               '05587498', # Alton (LD 26)
               '07010000', # St. Louis ***
               '07020500', # Chester ***
               '07020850', # Cape Girardeau
               '07022000', # Thebes ***
               '370000089122601', # Above Cairo
               '365730089063001' # Below Cairo
               ) 

siteINFO<-readNWISsite(siteNumbers)
dailyDataAvailable <- whatNWISdata(siteNumbers, service="uv")

dischargeUnit <- readNWISdv(siteNumbers, parameterCd, startDate, endDate)
dischargeUnit <- renameNWISColumns(dischargeUnit)

# Convert to cubic meter per second 
dischargeUnit$Flow_cms<-dischargeUnit$Flow /35.3147


#Illinois River gauge data
ILsiteNumbers<-c("05586100", #Valley City IL
                 "05586300") #Florence, IL


ILDischarge<-readNWISdv(ILsiteNumbers, parameterCd, startDate, endDate)
ILDischarge <- renameNWISColumns(ILDischarge)
ILDischarge$Flow_cms<-ILDischarge$Flow /35.3147

#Calculate Discharge for pool 26 (Miss River at Grafton plus IL)
AltonDischarge<-merge(dischargeUnit[which(dischargeUnit$site_no=="05587450"),], ILDischarge[which(ILDischarge$site_no=="05586100"),], by="Date")
AltonDischarge$Flow_cms<- (AltonDischarge$Flow_cms.x + AltonDischarge$Flow_cms.y)
AltonDischarge$site_no<-9999

#Check to make sure it looks correct
plot(AltonDischarge$Flow_cms, ylim=c(0, max(AltonDischarge$Flow_cms)))
points(AltonDischarge$Flow_cms.x, col="red")
points(AltonDischarge$Flow_cms.y, col="blue")

dischargeUnit<-smartbind(dischargeUnit, AltonDischarge)

#Load Flame data
setwd("E:/Dropbox/ArcGIS")

data<-read.table('UMR_AllDays_Route2.txt', header=TRUE, sep="," ,skip=0)
data$riverkm<-data$MEAS/1000
data<-data[order(data$MEAS),]
data[data==0] <- NA
data$ltime<-as.POSIXct(data$ltime, format="%Y-%m-%d %H:%M:%S", tz="America/Chicago")

NO3data<-data[!is.na(data$NITRATEM),]
NO3data$rollNO3<-rollmean(NO3data$NITRATEM, k=25, align='center', fill=NA)

plot(NO3data$NITRATEM)
lines(NO3data$rollNO3, type="l", col="red")

#Load Impoundment data
setwd("E:/Dropbox/FLAME_MississippiRiver")


impound<-read.csv('ImpoundedAreas.csv', header=TRUE, stringsAsFactors = F)
impound2<-impound[impound$Impound.Length>0,]

# separate dataframe for each impoundment area
i=1
frames<-list()
for (i in 1:nrow(impound2)){
  interval<-c(impound2$Impound.Start[i], impound2$ImpoundEnd[i])
  newdata<-subset(NO3data,NO3data$MEAS>=interval[1] & NO3data$MEAS<=interval[2])
  if (nrow(newdata)>1){
    plot(newdata$riverkm, newdata$NITRATEM)
    mtext(paste("Pool", as.character(impound2$Pool[i])), 3, 0.1, cex=0.8)
  }
  frames[[i]]<-newdata
}

#plot each impoundment no3
png("dNO3PerImpound.png", res=200, width=8,height=6, units="in")
par(mfrow=c(3,5))
par(mar=c(2,1,2,1), oma=c(3,3,0,0))

j=2
for (j in 1:length(frames)){

  newdata<-frames[[j]]
  if (nrow(newdata)>1){
    plot(newdata$riverkm, newdata$NITRATEM)
    mtext(paste("Pool", as.character(impound2$Pool[j])), 3, 0.1, cex=0.8)
    }
}
mtext("Nitrate (mg/L)", 2, 1.5, outer=T)
mtext("River km", 1, 1, outer=T)
dev.off()

# # Plot Lake Pepin Only
# pepin<-frames[[2]]
# 
# png("dNO3Pepin.png", res=200, width=4,height=3, units="in")
# par(mfrow=c(1,1))
# par(mar=c(4,4,1,1), oma=c(0,0,0,0))
# plot(pepin$riverkm, pepin$NITRATEM, ylab="", xlab="", las=1)
# mtext("Nitrate (mg/L)", 2, 3)
# mtext("River km", 1, 2)
# dev.off()
# 
# par(mfrow=c(1,1))
# 
# plot(pepin$riverkm, pepin$NITRATEM)
# points(pepin$riverkm, pepin$rollNO3, type="l", col="red")
# 
# pepin$dx<-c(NA,diff(pepin$MEAS))
# pepin$dNO3<-c(NA, diff(pepin$NITRATEM))
# 
# 
# 
# Q<-17400*0.028316847
# w<-2736
# 
# pepin$U<- pepin$dNO3/pepin$dx*Q/w*(-3600)*24	*1000
# pepin_v2<-pepin[pepin$dx>10,]
# 
# plot(pepin$riverkm, pepin$NITRATEM)
# points(pepin$riverkm, pepin$rollNO3, type="l", col="red")
# 
# plot(pepin_v2$U)
# summary(pepin_v2$U)
# Pepin_U<-mean(pepin_v2$U)
# Pepin_sd<-sd(pepin_v2$U, na.rm=T)
# Pepin_se<-Pepin_sd/sqrt(length(pepin_v2$U))
# 
# hist(pepin_v2$U, breaks=20)


# Make a datatable of Q and w for each impoundment
impound2$nearGauge<-c(rep('05331580', 2),rep('05378500', 6), rep('05420500',4), rep('05474500',1), rep('05587450',1), "9999")
impound2$Date<-as.Date(NA)
k=1
for (k in 1:length(frames)){
  impound2$Date[k]<-strptime(mean(frames[[k]]$ltime), format="%Y-%m-%d")
}

impound2$Q<-as.numeric(NA)
l<-1
for (l in 1:nrow(impound2)){
  
  impound2$Q[l]<-dischargeUnit[which(dischargeUnit$site_no==impound2$nearGauge[l] & dischargeUnit$Date==impound2$Date[l]),'Flow_cms']
}

impound2$width<-impound2$Impound.Area.Sum/impound2$Impound.Length

#Calculate dNO3 and U for each pool
# This occurs three ways. 
# 1) uses rate of change through impounded area (All Data)
# 2) uses NO3 start and NO3 End from uploaded csv
# 3) uses mean of first 20 observations and last 20 observations

impound2$Umean<-NA
impound2$Usd<-NA
impound2$Use<-NA
impound2$N_Start<-NA
impound2$N_End<-NA

m<-1
for (m in 1:nrow(impound2)){
  table<-frames[[m]]
  Q<-impound2$Q[m]
  w<-impound2$width[m]
  
  table$dx<-c(NA,diff(table$MEAS))
  table_v1<-table[table$dx>10,]
  # table_v1<-table
  table_v1$dNO3<-c(NA, diff(table_v1$rollNO3))
  table_v1$U<-table_v1$dNO3/table_v1$dx*Q/w*(-3600)*24	*1000 # Convert to mg N m-2 day-1
  table_v2<-table_v1[which(is.finite(table_v1$U)),]
  
#   plot(table_v2$riverkm, table_v2$NITRATEM)
#   points(table_v2$riverkm, table_v2$rollNO3, type="l", col="red")

  summary(table_v2$U)
  impound2$Umean[m]<-mean(table_v2$U)
  impound2$Usd[m]<-sd(table_v2$U, na.rm=T)
  impound2$Use[m]<-impound2$Usd[m]/sqrt(length(table_v2$U))
  
  impound2$N_Start[m]<-mean(head(table_v1$rollNO3, n=20), na.rm=T)
  impound2$N_End[m]<-mean(tail(table_v1$rollNO3, n=20), na.rm=T)

}

impound2$U2mean<-(impound2$NO3_Start-impound2$NO3_End)*impound2$Q/impound2$Impound.Area.Sum*(86400000)*14.007/1000

impound2$U3mean<-(impound2$N_Start-impound2$N_End)*impound2$Q/impound2$Impound.Area.Sum*(86400000)

impound2$Nremoval_MgD1<-impound2$Umean*impound2$Impound.Area.Sum/1000/1000/1000
impound2$Nremoval_MgD2<-impound2$U2mean*impound2$Impound.Area.Sum/1000/1000/1000
impound2$Nremoval_MgD3<-impound2$U3mean*impound2$Impound.Area.Sum/1000/1000/1000

impound2[,c(1, 7, 14:24)]
plot(impound2[,c(18,23,24)])
plot(impound2[,23]~impound2[,24])
lines(c(-1000000, 1000000), c(-1000000, 1000000))
abline(h=0)
abline(v=0)

hist(impound2[,21], breaks=15)



# New Code
# Calculate predicted N retention based on WRT and mean depth
# Based on Harrison et al 2009 and Wollheim 2006
# Uses N_retention Model code

impound3<-impound2[which(!is.na(impound2$PoolVolume_10.6_m3)),]

#Possible Settling Velocities
# Vf = c(-13.66, -9.92, -5.66) 
Vf = c(-35, -13.66, -5.66) 
z = impound3$PoolMeanDepth_m
WRT<-impound3$PoolVolume_10.6_m3*1000000/impound3$Q/31536000

colors<-matlab.like(length(z))
lty<-seq(1, length(Vf), 1)

i=1
j=1

for (i in 1:length(z)){
  z1 = z[i]
  WRT1<-WRT[i]
  for (j in 1:length(Vf)){
    Vf1 = Vf[j]
    
    if (i ==1 & j==1){
      curve(1-(exp((Vf1*x)/z1)), .001, 100, log = "x", type="n", ylab="N Retention (%)", xlab="WRT (y)")
      
      abline(v=c(1,7,30,365)/365, col="darkgrey", lty=1, lwd=0.5)
      mtext(c("Day", "Week", "Month", "Year"), side=3, outer=FALSE, at = c(1,7,30,365)/365, col="darkgrey", cex=1)
    }
    
curve(1-(exp((Vf1*x)/z1)), .001, 100, log = "x", lwd=2, col=colors[i], lty=lty[j], add=T)
points(x= WRT1, 1-(exp((Vf1*WRT1)/z1)), pch=8, col=colors[i], cex=2, lwd=2)

  }
}

