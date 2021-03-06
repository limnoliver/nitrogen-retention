# ###################################################
# Script to calculate NO3 retention in Pool 8
# Code will run for all of the Pool 8 sampling events
# Inputs include discharge (Q) and [NO3] (mg N / L)
# Inputs include Dam 7, Root and Lacrosse Rivers
# Output is condition at Dam 8
# ###################################################

# load packages
library(dataRetrieval)
library(lubridate)
library(MASS)

# set start and end date
StartDate<-"2014-01-01"
EndDate<-"2016-12-31"

setwd("E:/Dropbox/FLAME_MississippiRiver")

# Get Dam 7 Discharge Data
Dam7Q<-read.table('DischargeData/ld7_Q_2014.txt', header=T, skip=5, stringsAsFactors = F, sep="")
Dam7Q$DATE<-as.Date(Dam7Q$DATE, format="%d%b%Y")
Dam7Q<-Dam7Q[!is.na(Dam7Q$DATE),]
Dam7Q$Flow_cms<-Dam7Q$INST.VAL/35.3147

# Get Dam 8 Discharge Data
Dam8Q<-read.table('DischargeData/ld8_Q_2014.txt', header=T, skip=5, stringsAsFactors = F, sep="")
Dam8Q$DATE<-as.Date(Dam8Q$DATE, format="%d%b%Y")
Dam8Q<-Dam8Q[!is.na(Dam8Q$DATE),]
Dam8Q$Flow_cms<-Dam8Q$INST.VAL/35.3147

# ==================================
# Get UMESC Water Chem Data
# ==================================

WaterChemData<-read.csv('ltrm_water_data.csv', header=T, stringsAsFactors = F)
WaterChemData$DATE<-as.Date(WaterChemData$DATE, format="%m/%d/%Y")

#remove bad reads
WaterChemData$NOX[WaterChemData$NOXQF %in% c(64)]<-NA
WaterChemData<-subset(WaterChemData, !is.na(NOX) & DATE>=StartDate & DATE<=EndDate)

allsites<-unique(WaterChemData$LOCATCD)

site_names<-c('BK01', 'LX00', 'R000', 'M701', 'M679')

dam8samples<-grep(site_names[5], WaterChemData$LOCATCD)
dam7samples<-grep(site_names[4], WaterChemData$LOCATCD)
rootsamples<-grep(site_names[3], WaterChemData$LOCATCD)
blacksamples<-grep(site_names[1], WaterChemData$LOCATCD)
lacrossesamples<-grep(site_names[2], WaterChemData$LOCATCD)

SampleDates<-unique(WaterChemData$DATE[c(dam8samples, dam7samples, rootsamples, blacksamples, lacrossesamples)])
SampleDates<-SampleDates[order(SampleDates)]

SampleChemData1<-WaterChemData[WaterChemData$DATE %in% SampleDates, ]
SampleChemData1<-SampleChemData1[order(SampleChemData1$DATE),]

SampleChemData<-SampleChemData1[c("DATE", "LOCATCD", "TEMP", "NOX")]

#Figure out which days are part of the same survey
today=2
SampleChemData$group<-c(1, rep(NA, nrow(SampleChemData)-1))
for (today in 2:nrow(SampleChemData)){
  if(SampleChemData$DATE[today] - SampleChemData$DATE[today-1] <7){
    SampleChemData$group[today]<-SampleChemData$group[today-1]}
  else {
    SampleChemData$group[today]<-SampleChemData$group[today-1]+1
  }
}

#aggregate dates
# maybe<-aggregate(SampleChemData, by=list(SampleChemData$group, SampleChemData$Sample.Notes), FUN=mean)


dam8Data<-SampleChemData[grep(site_names[5], SampleChemData$LOCATCD),]
dam7Data<-SampleChemData[grep(site_names[4], SampleChemData$LOCATCD),]
rootData<-SampleChemData[grep(site_names[3], SampleChemData$LOCATCD),]
blackData<-SampleChemData[grep(site_names[1], SampleChemData$LOCATCD),]
lacrosseData<-SampleChemData[grep(site_names[2], SampleChemData$LOCATCD),]


# ==================================
# Get Tributary Water Chemistry and Discharge Data
# ==================================

#Triburary River gauge data
TsiteNumbers<-c("05382000",#Black River Upper
                "05383075",#LaCrosse River
                "05385000",#Root River Main
                "05385500",#Root River South
                "05379500",#Trempeleau
                "05378500", # UMR Winona (LD 5a) 
                "05420500" # UMR Clinton (LD 13) 
)

TribNames<-c('Black River Upper',
             'LaCrosse River',
             'Root River Main',
             'Root River South',
             'Trempeleau',
             'UMR Winona (LD 5a)', 
             'UMR Clinton (LD 13)')

parameterCd <- c("00060") # Discharge

tribtable<-cbind(TribNames, TsiteNumbers)

trib_list<-list()
for (trib in 1:length(TsiteNumbers)){
  TDischarge<-readNWISdv(TsiteNumbers[trib], parameterCd, StartDate, EndDate)
  TDischarge <- renameNWISColumns(TDischarge)
  TDischarge$Flow_cms<-TDischarge$Flow /35.3147
  
  trib_list[[trib]]<-TDischarge
  names(trib_list)[trib]<-TribNames[trib]
}

Rootmerge<-merge(trib_list[c('Root River Main')][[1]], trib_list[c('Root River South')][[1]], by='Date', all=T)
Rootmerge$Flow_cms<-rowSums(data.frame(Rootmerge$Flow_cms.x, Rootmerge$Flow_cms.y), na.rm=T)
trib_list[[length(trib_list)+1]]<-Rootmerge
names(trib_list)[[length(trib_list)]]<-'Root River'

LD7merge1<-merge(trib_list[c('UMR Winona (LD 5a)')][[1]], trib_list[c('Trempeleau')][[1]], by='Date', all=T)
LD7merge2<-merge(LD7merge1, trib_list[c('Black River Upper')][[1]], by='Date', all=T)
LD7merge2$Flow_cms<-rowSums(data.frame(LD7merge2$Flow_cms.x, LD7merge2$Flow_cms.y, LD7merge2$Flow_cms), na.rm=T)
trib_list[[length(trib_list)+1]]<-LD7merge2
names(trib_list)[[length(trib_list)]]<-'UMR_ALD7'

# ==================================
# Loop through dates and make mass balance
# ==================================


# start Loop here
# Make list and data frame to fill with data
pool_summary<-as.data.frame(matrix(nrow=length(unique(SampleChemData$group)), ncol=10), stringsAsFactors = F)
names(pool_summary)<-(c("Date", "NO3_start", "NO3_end", "dNO3", "RNO3", "Dam7_Q", "Dam8_Q", "RR_Q", "USGS_Q", "Temp"))

row<-2
for (row in unique(SampleChemData$group)){
  GroupChemData<-SampleChemData[SampleChemData$group==row,]
  day<-round_date(median(GroupChemData$DATE), 'day')

  # Get NO3 values for ins and outs
  # Mississippi In, Mississippi out, Root, Black, and LaCrosse Rivers)
  
  MR_in<-mean(dam7Data$NOX[dam7Data$group==row])
  MR_out<-mean(dam8Data$NOX[dam8Data$group==row])
  RR_in<-mean(rootData$NOX[rootData$group==row])
  BR_in<-mean(blackData$NOX[blackData$group==row])
  LR_in<-mean(lacrosseData$NOX[lacrosseData$group==row])
  
  
  #Substitue mean if NA
  if (is.na(LR_in)){
    LR_in<-mean(lacrosseData$NOX, na.rm=T)}
  if (is.na(RR_in)){  
    RR_in<-mean(rootData$NOX, na.rm=T)}
  if (is.na(BR_in)){  
    BR_in<-mean(blackData$NOX, na.rm=T)}

  # Get Q for Tributaries
  RR_in_Q<-trib_list[[c("Root River")]][trib_list[[c("Root River")]]$Date==day,c('Flow_cms')]
  LR_in_Q<-trib_list[[c("LaCrosse River")]][trib_list[[c("LaCrosse River")]]$Date==day,c('Flow_cms')]
  
  # Set Black River to specific flow (1500 cfs in summer, 500 cfs in winter)
  if (month(day) %in% c(11,12,1,2,3)){
    BR_in_Q<-500/35.3147}
  if (!month(day) %in% c(11,12,1,2,3)){
    BR_in_Q<-1500/35.3147}
  
  #  Black River to 0
  # BR_in_Q<-0

  # Get Q for Main Channel
  MR_in_Q<-Dam7Q$Flow_cms[Dam7Q$DATE==day]-BR_in_Q
  MR_out_Q<-Dam8Q$Flow_cms[Dam8Q$DATE==day]
  if (length(MR_out_Q)!=1){  
    MR_out_Q<-MR_in_Q+RR_in_Q+LR_in_Q+BR_in_Q}
  
  USGS_Q<-trib_list[[c("UMR_ALD7")]][trib_list[[c("UMR_ALD7")]]$Date==day,c('Flow_cms')]
  
  # NO3 mass balance
  In_NO3<-((MR_in*MR_in_Q) + (RR_in*RR_in_Q) + (LR_in*LR_in_Q) + (BR_in*BR_in_Q) ) / (MR_in_Q+RR_in_Q+LR_in_Q+BR_in_Q)
  Out_NO3<-MR_out
  delta_NO3<-In_NO3-Out_NO3
  R_NO3<-delta_NO3/In_NO3
  
  # export to summary table
  pool_summary[row,1]<-as.character(day)
  if (length(R_NO3)>0){
    pool_summary[row,2]<-In_NO3
    pool_summary[row,3]<-Out_NO3
    pool_summary[row,4]<-delta_NO3
    pool_summary[row,5]<-R_NO3
    pool_summary[row,6]<-MR_in_Q    
    pool_summary[row,7]<-MR_out_Q
    pool_summary[row,8]<-RR_in_Q
    pool_summary[row,9]<-USGS_Q
    pool_summary[row,10]<-mean(GroupChemData$TEMP, na.rm=T)
  }
  else {print('Missing Data')}
}
pool_summary$Date<-as.Date(pool_summary$Date)
pool_summary$RR_pct<-pool_summary$RR_Q/pool_summary$MR_Q
pool_summary

png("E:/Dropbox/FLAME_MississippiRiver/N_Model/N_retention_Drivers_USGS_Intrapool.png", res=200, width=4.2,height=4, units="in")
cex=0.8
cexpt=1.5
par(cex=cex)
par(ps=12)

cex=0.8
par(cex=cex)
cexpt=1
ps=12
par(mfrow=c(2,2))
par(mar=c(3,1,0.5,0.5), oma=c(0,2.5,0,0))
par(mgp=c(3,.5,0))
par(tck=-0.03)
par(pch=16)

plot(pool_summary$Date, pool_summary$RNO3, type="o", las=1, cex.axis=cex, cex=cexpt)
mtext("Date", 1, 2, cex=cex)
abline(h=0)

plot(pool_summary$Dam8_Q, pool_summary$RNO3, yaxt="n", cex.axis=cex, cex=cexpt)
mtext(expression(paste("Discharge (m"^"3", " s"^"-1", ")", sep="")), 1, 2, cex=cex)
axis(2, labels=NA)
abline(h=0)

plot(pool_summary$Temp, pool_summary$RNO3, cex.axis=cex, las=1, cex=cexpt)
mtext(expression(paste("Temperature (", degree, "C)", sep="")), 1, 2, cex=cex)
abline(h=0)

plot(pool_summary$NO3_start, pool_summary$RNO3, yaxt="n", cex.axis=cex, cex=cexpt)
mtext(expression(paste('Incoming ', NO[3], " (mg N L"^"-1", ")")),1,2, cex=cex)
axis(2, labels=NA)
abline(h=0)

mtext(expression(paste(NO[3], ' Retention (%)')),2,1, outer=T, cex=cex)


dev.off()

#Single Linear Models
Q_model<-lm(pool_summary$RNO3~pool_summary$Dam8_Q)
summary(Q_model)
Temp_model<-lm(pool_summary$RNO3~pool_summary$Temp)
summary(Temp_model)             
NO3_model<-lm(pool_summary$RNO3~pool_summary$NO3_start)
summary(NO3_model)

#Multiple Linear Models with stepwise selection (AIC - Both)
null_model<-lm(pool_summary$RNO3~1)
full_model<-lm(pool_summary$RNO3~pool_summary$Dam8_Q + pool_summary$Temp + pool_summary$NO3_start)
summary(full_model)
step_model<-step(null_model, scope=list(lower=null_model, upper=full_model), direction='both')
anova(step_model)
summary(step_model)

#Another Function but gives same result
step_model2<-stepAIC(full_model, direction='both')
anova(step_model2)
summary(step_model2)

plot(residuals(Q_model)~ pool_summary$Temp)
abline(h=0)

plot(residuals(Temp_model)~ pool_summary$Dam8_Q)
abline(h=0)

plot(dam7Data$NOX~dam7Data$DATE, type="l", ylim=c(0,8), lwd=2)
points(dam8Data$NOX~dam8Data$DATE, type="l", col="red", lwd=2)
points(lacrosseData$NOX~lacrosseData$DATE, type="l", col="brown")
points(rootData$NOX~rootData$DATE, type="l", col="lightblue")
points(blackData$NOX~blackData$DATE, type="l", col="green")


