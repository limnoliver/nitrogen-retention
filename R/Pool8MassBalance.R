# ###################################################
# Script to calculate NO3 retention in Pool 8
# Code will run for all of the Pool 8 sampling events
# Inputs include discharge (Q) and [NO3] (mg N / L)
# Inputs include Dam 7, Root and Lacrosse Rivers
# Output is condition at Dam 8
# ###################################################

# load packages
library(dataRetrieval)

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
WaterChemData$DateTime<-as.POSIXct(WaterChemData$DateTime, format="%Y-%m-%d %H:%M:%S", tz="America/Chicago")

sites<-unique(WaterChemData$Sample.Notes)
dam8samples<-grep("D8", WaterChemData$Sample.Notes)
dam7samples<-grep("D7", WaterChemData$Sample.Notes)
rootsamples<-grep("Root", WaterChemData$Sample.Notes)
blacksamples<-grep("Black", WaterChemData$Sample.Notes)
lacrossesamples<-grep("Crosse", WaterChemData$Sample.Notes)

dam8Data<-WaterChemData[dam8samples,]
dam7Data<-WaterChemData[dam7samples,]
rootData<-WaterChemData[rootsamples,]
blackData<-WaterChemData[blacksamples,]
lacrosseData<-WaterChemData[lacrossesamples,]

SampleDates<-unique(as.Date(c(dam8Data$Date, dam7Data$Date, rootData$Date, blackData$Date, lacrosseData$Date)))

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

Rootmerge<-merge(trib_list[c('Root River Main')][[1]], trib_list[c('Root River South')][[1]], by='Date')
Rootmerge$Flow_cms<-Rootmerge$Flow_cms.x+Rootmerge$Flow_cms.y
trib_list[[length(trib_list)+1]]<-Rootmerge
names(trib_list)[[length(trib_list)]]<-'Root River'

# ==================================
# Loop through dates and make mass balance
# ==================================







