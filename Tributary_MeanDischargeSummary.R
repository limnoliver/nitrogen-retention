library(dataRetrieval)

# ==================================
# Get Tributary Water Chemistry and Discharge Data
# ==================================
parameterCd <- c("00060", "99133") # Discharge, NO3
startDate <- "2015-01-01"
endDate <- "2015-12-31"

# Get USGS gauge data for all UMR stations.
siteNumbers<-c('05331000', # St. Paul *** (pools 1)
               '05331580', # Hastings (LD 2) *** (pools 2-LakePepin)
               '05344500', # Prescot (LD 3) - below St. Croix
               '05378500', # Winona (LD 5a) *** (pools 4-9)
               '05420500', # Clinton (LD 13) *** (pools 10-15)
               '05474500', # Keokuk (LD 19) *** (pools 16-19)
               '05587450', # At Grafton (LD 26)*** (pools 20-25)
               '07010000', # St. Louis ***
               '07020500', # Chester ***
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



# ==================================
# Get Tributary Water Chemistry and Discharge Data
# ==================================

#Triburary River gauge data
TsiteNumbers<-c("05331000",#Mississippi River
                "05330920",#Minnesota River
                "05344490",#St Croix River
                "05369500",#Chippewa River
                "05382000",#Black River Upper
                "05383075",#LaCrosse River
                "05385000",#Root River Main
                "05385500",#Root River South
                "05388250",#Upper Iowa River
                "05407000",#Wisconsin River
                "05418720",#Maquoketa River
                "05446500",#Rock River
                "05465700",#Iowa River
                "05490600",#Des Moines River
                "05586100",#Illinois River
                "06935965",#Missouri River
                "03612600"#Ohio River
)


TribNames<-c('Mississippi River',
             'Minnesota River',
             'St. Croix River',
             'Chippewa River',
             'Black River Upper',
             'LaCrosse River',
             'Root River Main',
             'Root River South',
             'Upper Iowa River',
             'Wisconsin River',
             'Maquoketa River',
             'Rock River',
             'Iowa River Confluence',
             'Des Moines River',
             'Illinois River',
             'Missouri River',
             'Ohio River')

tribtable<-data.frame(TribNames, TsiteNumbers)
tribtable$Mean_Annual_Flow<-NA
tribtable$Median_Annual_Flow<-NA
trib_list<-list()

for (trib in 1:length(TsiteNumbers)){
  TDischarge<-readNWISdv(TsiteNumbers[trib], parameterCd, startDate, endDate)
  TDischarge <- renameNWISColumns(TDischarge)
  TDischarge$Flow_cms<-TDischarge$Flow /35.3147
  
  trib_list[[trib]]<-TDischarge
  names(trib_list)[trib]<-TribNames[trib]
  tribtable$Mean_Annual_Flow[trib]<-mean(TDischarge$Flow_cms, na.rm=T)
  tribtable$Median_Annual_Flow[trib]<-median(TDischarge$Flow_cms, na.rm=T)
}

Rootmerge<-merge(trib_list[c('Root River Main')][[1]], trib_list[c('Root River South')][[1]], by='Date')
Rootmerge$Flow_cms<-Rootmerge$Flow_cms.x+Rootmerge$Flow_cms.y
trib_list[[length(trib_list)+1]]<-Rootmerge
names(trib_list)[[length(trib_list)]]<-'Root River'



