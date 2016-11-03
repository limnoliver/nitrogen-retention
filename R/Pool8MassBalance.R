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

# set start and end date
StartDate<-"2014-01-01"
EndDate<-"2016-11-03"

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
# Get FLAME and LTER Water Chem Data
# ==================================

WaterChemData<-read.csv('MissRiverFlameLabMerged.csv', header=T, stringsAsFactors = F)
WaterChemData$Date<-as.Date(WaterChemData$DateTime, format="%Y-%m-%d")

WaterChemData$NO3AVG<-rowMeans(data.frame(WaterChemData$NO3.NO2/1000, WaterChemData$NITRATEMG), na.rm=T)

site_names<-c('D8', 'D7', 'Root', 'Black', 'Crosse')

dam8samples<-grep(site_names[1], WaterChemData$Sample.Notes)
dam7samples<-grep(site_names[2], WaterChemData$Sample.Notes)
rootsamples<-grep(site_names[3], WaterChemData$Sample.Notes)
blacksamples<-grep(site_names[4], WaterChemData$Sample.Notes)
lacrossesamples<-grep(site_names[5], WaterChemData$Sample.Notes)

SampleDates<-unique(WaterChemData$Date[c(dam8samples, dam7samples, rootsamples, blacksamples, lacrossesamples)])
SampleDates<-SampleDates[order(SampleDates)]

SampleChemData<-WaterChemData[WaterChemData$Date %in% SampleDates, ]
SampleChemData<-SampleChemData[order(SampleChemData$Date),]

#Figure out which days are part of the same survey
today=2
SampleChemData$group<-c(1, rep(NA, nrow(SampleChemData)-1))
for (today in 2:nrow(SampleChemData)){
  if(SampleChemData$Date[today] - SampleChemData$Date[today-1] <7){
    SampleChemData$group[today]<-SampleChemData$group[today-1]}
  else {
    SampleChemData$group[today]<-SampleChemData$group[today-1]+1
  }
}

#aggregate dates
# maybe<-aggregate(SampleChemData, by=list(SampleChemData$group, SampleChemData$Sample.Notes), FUN=mean)


dam8Data<-SampleChemData[grep(site_names[1], SampleChemData$Sample.Notes),]
dam7Data<-SampleChemData[grep(site_names[2], SampleChemData$Sample.Notes),]
rootData<-SampleChemData[grep(site_names[3], SampleChemData$Sample.Notes),]
blackData<-SampleChemData[grep(site_names[4], SampleChemData$Sample.Notes),]
lacrosseData<-SampleChemData[grep(site_names[5], SampleChemData$Sample.Notes),]


# ==================================
# Get Tributary Water Chemistry and Discharge Data
# ==================================

#Triburary River gauge data
TsiteNumbers<-c("05382000",#Black River Upper
                "05383075",#LaCrosse River
                "05385000",#Root River Main
                "05385500",#Root River South
                "05378500", # UMR Winona (LD 5a) 
                "05420500" # UMR Clinton (LD 13) 
)

TribNames<-c('Black River Upper',
             'LaCrosse River',
             'Root River Main',
             'Root River South', 
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

# ==================================
# Loop through dates and make mass balance
# ==================================


# start Loop here
# Make list and data frame to fill with data
pool_summary<-as.data.frame(matrix(nrow=length(unique(SampleChemData$group)), ncol=8), stringsAsFactors = F)
names(pool_summary)<-(c("Date", "NO3_start", "NO3_end", "dNO3", "RNO3", "Dam7_Q", "Dam8_Q", "RR_Q"))

row<-2
for (row in unique(SampleChemData$group)){
  GroupChemData<-SampleChemData[SampleChemData$group==row,]
  day<-round_date(median(GroupChemData$Date), 'day')

  # Get NO3 values for ins and outs
  # Mississippi In, Mississippi out, Root, Black, and LaCrosse Rivers)
  
  MR_in<-mean(dam7Data$NO3AVG[dam7Data$group==row])
  MR_out<-mean(dam8Data$NO3AVG[dam8Data$group==row])
  RR_in<-mean(rootData$NO3AVG[rootData$group==row])
  BR_in<-mean(blackData$NO3AVG[blackData$group==row])
  LR_in<-mean(lacrosseData$NO3AVG[lacrosseData$group==row])
  
  # Use USGS NO3 data for one date. Samples analyzed in Boulder, CO
  if (day=='2014-10-02'){
    MR_in<-66.297*14.007/1000
    MR_out<-64.059*14.007/1000
  }
  
  #Substitue mean if NA
  if (is.na(LR_in)){
    LR_in<-mean(lacrosseData$NO3AVG, na.rm=T)}
  if (is.na(RR_in)){  
    RR_in<-mean(rootData$NO3AVG, na.rm=T)}
  if (is.na(BR_in)){  
    BR_in<-mean(blackData$NO3AVG, na.rm=T)}

  # Get Q for Tributaries
  RR_in_Q<-trib_list[[c("Root River")]][trib_list[[c("Root River")]]$Date==day,c('Flow_cms')]
  LR_in_Q<-trib_list[[c("LaCrosse River")]][trib_list[[c("LaCrosse River")]]$Date==day,c('Flow_cms')]
  BR_in_Q<-1500/35 # Set Black River to specific flow (1500 cfs in summer, 500 cfs in winter)
  BR_in_Q<-0 # or set Black River to zero
  
  # Get Q for Main Channel
  MR_in_Q<-Dam7Q$Flow_cms[Dam7Q$DATE==day]-BR_in_Q
  MR_out_Q<-Dam8Q$Flow_cms[Dam8Q$DATE==day]
  
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
  }
  else {print('Missing Data')}
}
pool_summary$Date<-as.Date(pool_summary$Date)
pool_summary$RR_pct<-pool_summary$RR_Q/pool_summary$MR_Q
pool_summary


plot(pool_summary$MR_Q, pool_summary$RNO3)
plot(pool_summary$Date, pool_summary$RNO3)
abline(h=0)




    sub<-NO3data[NO3data$riverkm<dam_km[dam_nu],]
    sub2<-Turbdata[Turbdata$riverkm<dam_km[dam_nu],]
  }
  else {
    sub<-NO3data[NO3data$riverkm<dam_km[dam_nu] & NO3data$riverkm>dam_km[dam_nu-1], ]
    sub2<-Turbdata[Turbdata$riverkm<dam_km[dam_nu] & Turbdata$riverkm>dam_km[dam_nu-1], ] 
  }
  flame_date<-median(as.Date(sub$ltime))
  flamedata_list[[dam_nu]]<-sub
  flamedata_list2[[dam_nu]]<-sub2
  names(flamedata_list)[[dam_nu]]<-dam_name[dam_nu]
  names(flamedata_list2)[[dam_nu]]<-dam_name[dam_nu]
  
  #Distance
  pool_summary[dam_nu,1]<-dam_name[dam_nu]
  pool_summary[dam_nu,2:3]<-range(sub$riverkm)
  pool_summary[dam_nu,4]<-pool_summary[dam_nu,3] - pool_summary[dam_nu,2]
  
  MR_Q_out<-AugQDaily[AugQDaily$Date==flame_date,c(dam_name[dam_nu])]
  
  # If Triburary exists in pool
  if (dam_nu %in% InputChemistry$poolInterval){
    
    #Miss River Metrics
    MR_NO3in<-median(sub$NITRATEM[1:10], na.rm=T)
    MR_Turbin<-median(sub2$TurbFNU[1:20], na.rm=T)
    MR_Q_in<- pool_summary[dam_nu-1,13]
    
    #Use Water Chem table for Pool 8 metrics
    #flame data are bad because we sampled this stretch 3 times over 2 days
    if (dam_name[dam_nu]=='8'){
      MR_NO3in<-0.78
      MR_Turbin<-9.01
    }   
    
    #Tributary Metrics
    Trib_NO3in<- InputChemistry$NITRATEMG[InputChemistry$poolInterval==dam_nu]
    Trib_Turbin<- InputChemistry$TurbFNU[InputChemistry$poolInterval==dam_nu]
    Trib_Q<- InputChemistry$Q[InputChemistry$poolInterval==dam_nu]
    
    #NO3 initial
    pool_summary[dam_nu,5] <- ((Trib_NO3in*Trib_Q) + (MR_NO3in*MR_Q_in)) / (MR_Q_in+Trib_Q)
    #Turb initial
    pool_summary[dam_nu,8]<- ((Trib_Turbin*Trib_Q) + (MR_Turbin*MR_Q_in)) / (MR_Q_in+Trib_Q)
    
  }
  else{
    #NO3 initial
    pool_summary[dam_nu,5]<-median(sub$NITRATEM[1:10], na.rm=T)
    #Turb initial
    pool_summary[dam_nu,8]<-median(sub2$TurbFNU[1:20], na.rm=T)
  }
  
  #NO3 final
  pool_summary[dam_nu,6]<-median(sub$NITRATEM[(length(sub$NITRATEM)-9):length(sub$NITRATEM)], na.rm=T)
  #Turb final
  pool_summary[dam_nu,9]<-median(sub2$TurbFNU[(length(sub2$TurbFNU)-19):length(sub2$TurbFNU)], na.rm=T)
  
  if (dam_name[dam_nu]=='8'){
    pool_summary[dam_nu,6]<-0.768
    pool_summary[dam_nu,9]<-6.46
  } 
  #NO3 change
  pool_summary[dam_nu,7]<- pool_summary[dam_nu,5] - pool_summary[dam_nu,6]
  #NO3 Retention (0-1)
  pool_summary[dam_nu,11]<-pool_summary[dam_nu,7]/pool_summary[dam_nu,5]
  
  #Turb change
  pool_summary[dam_nu,10]<- pool_summary[dam_nu,8] - pool_summary[dam_nu,9]
  #Turb Retention (0-1)
  pool_summary[dam_nu,12]<-pool_summary[dam_nu,10]/pool_summary[dam_nu,8]
  #Discharge out (cms)
  if (length(MR_Q_out)==1){
    pool_summary[dam_nu,13]<-MR_Q_out}
  print(dam_name[dam_nu])
}

pool_summary$Pool[pool_summary$Pool!='Pepin'& !is.na(pool_summary$Pool)]<-paste("p", pool_summary$Pool[pool_summary$Pool!='Pepin'& !is.na(pool_summary$Pool)], sep="")

print(pool_summary)
hist(pool_summary$RNO3, breaks=100)





