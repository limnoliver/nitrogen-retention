rm(list = ls())
options(digits=4)
# library(foreign)
library(rgdal)
library(rgeos)
library(plyr)
library(maptools)
library(dataRetrieval)
library(zoo)

# Get Dam Discharge Data
setwd("E:/Dropbox/FLAME_MississippiRiver")
DamQ<-read.csv('USACE_Discharge_StPaulDams2015.csv', header=T, skip=6, stringsAsFactors = F)
DamQ$DateTime<-as.POSIXct(DamQ$X, format="%d%b%Y  %H%M", tz="America/Chicago")
DamQ<-DamQ[!is.na(DamQ$DateTime),]
DamQ$Date<-as.Date(DamQ$X, format="%d%b%Y")

DamQDaily1<-aggregate(DamQ[,3:13], by=list(DamQ$Date), FUN="mean")
names(DamQDaily1)[1]<-"Date"

DamQ_Rock<-read.csv('USACE_Discharge_RockIslandDams2015.csv', header=T, skip=0, stringsAsFactors = F)
DamQ_Rock$Date<-as.Date(DamQ_Rock$Date, format="%Y-%m-%d")

DamQ_StLouis<-read.csv('USACE_Discharge_StLouisDams2015.csv', header=T, skip=0, stringsAsFactors = F)
DamQ_StLouis$DateTime<-as.POSIXct(DamQ_StLouis$X, format="%d%b%Y  %H%M", tz="America/Chicago")
DamQ_StLouis<-DamQ_StLouis[!is.na(DamQ_StLouis$DateTime),]
DamQ_StLouis$Date<-as.Date(DamQ_StLouis$X, format="%d%b%Y")

DamQDaily3<-aggregate(DamQ_StLouis[,2:4], by=list(DamQ_StLouis$Date), FUN="mean")
names(DamQDaily3)[1]<-"Date"

DamQDaily<-merge(DamQDaily1, DamQ_Rock, by="Date", all=T)
DamQDaily<-merge(DamQDaily,DamQDaily3, by="Date", all=T)

#convert to cms
DamQDaily[,2:ncol(DamQDaily)]<-DamQDaily[,2:ncol(DamQDaily)]/35.3147

# ================================
# Code to calculate surface area, volume, for each of the Upper Mississippi River pools
# ================================

# Get Lake Pepin Shapefile
setwd("E:/Dropbox/FLAME/basemaps/shapefiles")
Pepinshape<-readOGR(getwd(), "LakePepin", stringsAsFactors = F)


dir1<-(paste(getwd(), "/USGS_AquaticAreas", sep=""))
dir1_files<-list.files(dir1)

#Figure out file names
dir1_dbf<-dir1_files[grep(".dbf", dir1_files)]
dir1_shape<-sub(".dbf", "", dir1_dbf)

#Make dataframe and vector to fill with information
summary_df<-as.data.frame(matrix(nrow=length(dir1_shape)+1, ncol=8))
names(summary_df)<-c("Pool", "TotalArea", "MC_Area", "SC_Area", "I_Area", "BWc_Area", "LP_Area", "BWi_Area" )
all_codes<-c()
all_desc<-c()

#loop through all pool shapefiles
for (file in 1:length(dir1_shape)){
i<-dir1_shape[file]

name_i<-sub("aqa_1989_", "", i)
name_i<-sub("_z15n83", "", name_i)
summary_df[file,1]<-name_i

shape_i<-readOGR(dir1, i, stringsAsFactors = F)
shape_i<-shape_i[!shape_i$AQUA_CODE %in% c("N", "NOPH"),]

shape_i$Area_Calc<-(sapply(shape_i@polygons, function(x) x@Polygons[[1]]@area))/1000000
all_codes<-c(all_codes, unique(shape_i$AQUA_CODE))
all_desc<-c(all_desc, unique(shape_i$AQUA_DESC))

summary_df[file, 2]<-sum(shape_i$Area_Calc)

shape_i_MC<-shape_i@data[shape_i$AQUA_CODE %in% c('MCB','MNC'), ]
shape_i_SC<-shape_i@data[shape_i$AQUA_CODE %in% c('SC', 'TC', 'TRC', 'EC'), ]
shape_i_I<-shape_i@data[shape_i$AQUA_CODE %in% c('CIMP'), ]
shape_i_LP<-shape_i@data[shape_i$AQUA_CODE %in% c('CTDL'), ]
shape_i_BWc<-shape_i@data[shape_i$AQUA_CODE %in% c('CACL', 'CFDL', 'CMML', 'CFSA', 'CBP', 'CLLL'), ]
shape_i_BWi<-shape_i@data[shape_i$AQUA_CODE %in% c('IACL', 'IFDL', 'IMML', 'IBP', 'ILLL', 'ITDL', 'IFSA', 'ISCL'), ]

if (name_i=="p04"){
  summary_df[file, 2]<-summary_df[file, 2]-sum(shape_i_LP$Area_Calc)
  summary_df[file, 7]<-sum(shape_i_LP$Area_Calc)/summary_df[file, 2]
  summary_df[file, 6]<-sum(shape_i_BWc$Area_Calc)/summary_df[file, 2]
  summary_df[nrow(summary_df), 1]<-"Pepin"
  summary_df[nrow(summary_df), 2]<-sum(shape_i_LP$Area_Calc)
}
else {
  summary_df[file, 7]<-NA
  summary_df[file, 6]<-(sum(shape_i_LP$Area_Calc)+sum(shape_i_BWc$Area_Calc))/summary_df[file, 2]
}

summary_df[file, 3]<-sum(shape_i_MC$Area_Calc)/summary_df[file, 2]
summary_df[file, 4]<-sum(shape_i_SC$Area_Calc)/summary_df[file, 2]
summary_df[file, 5]<-sum(shape_i_I$Area_Calc)/summary_df[file, 2]

summary_df[file, 8]<-sum(shape_i_BWi$Area_Calc)/summary_df[file, 2]
print(summary_df[file,])

}
#Change pool names to match final merge and omit empty data
summary_df$Pool<-sub("5a", "5A", summary_df$Pool)
summary_df$Pool<-sub("p0", "p", summary_df$Pool)
summary_df<-summary_df[!is.na(summary_df$TotalArea),]
summary_df<-subset(summary_df, select=-LP_Area)
summary_df<-summary_df[-grep("or", summary_df$Pool),]

unique_codes<-unique(all_codes)
unique_desc<-unique(all_desc)
unique_table<-data.frame(unique_codes, unique_desc)

#Save outputs
write.table(summary_df, "UMR_Pool_Areas.csv", sep=",", row.names=F, col.names=T)
setwd("E:/Git_Repo/nitrogen-retention")
saveRDS(summary_df, file = "UMR_Pool_Areas.rds")


# ===========================================
# Step 2
# Calculate Volume for pools with Bathy data
# ===========================================
setwd("E:/Dropbox/FLAME/basemaps/shapefiles")

dir2<-(paste(getwd(), "/USGS_Bathy", sep=""))
dir2_files<-list.files(dir2)

#Figure out file names
dir2_dbf<-dir2_files[grep("z15n83.dbf", dir2_files)]
dir2_shape<-sub(".dbf", "", dir2_dbf)


#Make dataframe and vector to fill with information
bathy_df<-as.data.frame(matrix(nrow=length(dir2_shape)+1, ncol=2))
names(bathy_df)<-c("Pool", "Volume")
grid_codes<-c()
depths<-c()

bathy=1
#loop through all bathy shapefiles
for (bathy in 1:length(dir2_shape)){
  i<-dir2_shape[bathy]
  
  name_i<-sub("bath_", "", i)
  name_i<-sub("_z15n83", "", name_i)
  name_i<-strsplit(name_i, split="_", fixed = T)[[1]][2]
  bathy_df[bathy,1]<-name_i

  shape_i<-readOGR(dir2, i, stringsAsFactors = F)
  shape_i<-shape_i[!shape_i$GRID_CODE %in% c(9999,-9999),]
  shape_i<-shape_i[!is.na((shape_i$DEPTH_M_)),]
  
  grid_codes<-c(grid_codes, unique(shape_i$GRID_CODE))
  depths<-c(depths, unique(shape_i$DEPTH_M_)) 
  
  l<-strsplit(shape_i$DEPTH_M_, split=" ", fixed = T)
  df<-rbind.fill(lapply(l,function(y){as.data.frame(t(y),stringsAsFactors=FALSE)}))
  df <- data.frame(sapply(df, function(x) as.numeric(as.character(x))))
  
  #Calculate mean depth for each polygon. Input shapefile displays depth as a range (e.g., "0.2 - 0.4"). Code selects for middle of range. 
  df$V4<-NA
  row=2
  for (row in 1:nrow(df)){
    if (is.finite(df$V2[row])){
      df[row,4]<-df$V2[row]}
    else {
    df[row,4]<-mean(unlist(df[row,c(1,3)]))
    }
  }

  
  shape_i$AREA<-(sapply(shape_i@polygons, function(x) x@Polygons[[1]]@area))/1000000
  shape_i$Volume<-NA
  shape_i$Depth_Max_<-NA
  shape_i$Depth_Mean<-df[,4]
  # shape_i$DoverSA<-NA
  
  shape_i$Depth_Max_<-shape_i$GRID_CODE/100
  shape_i$Volume<-shape_i$Depth_Mean*shape_i$AREA

  bathy_df[bathy,2]<-sum(shape_i$Volume)
  
  # For Pool 4, clip Lake Pepin and summarize it separately
  if (name_i=="p4"){
    
    #find polygons that fall within lake pepin and select those. 
    clip<-gIntersects(Pepinshape, shape_i, byid=T)
    clip2<-as.vector(clip)
    clipped<-shape_i[clip2,]
  
    bathy_df[nrow(bathy_df),2]<-sum(clipped$Volume)
    bathy_df[nrow(bathy_df),1]<-"Pepin"
    
    # Remove Pepin metrics from Pool 4 summary
    bathy_df[bathy,2]<-sum(shape_i$Volume)-sum(clipped$Volume)
  }
    
  print(bathy_df[bathy,])
}
bathy_df[,2]<-round(bathy_df[,2], digits=1)

print(bathy_df)

#Save outputs
write.table(bathy_df, "UMR_Pool_Volumes.csv", sep=",", row.names=F, col.names=T)
setwd("E:/Git_Repo/nitrogen-retention")
saveRDS(bathy_df, file = "UMR_Pool_Volumes.rds")


# ==============================
# Step 3
# Determine discharge for sample events
# ==============================

parameterCd <- c("00060", "99133") # Discharge, NO3
startDate <- "2015-08-01"
endDate <- "2015-08-31"

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
                "06935965"#Missouri River
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
             'Missouri River')

tribtable<-cbind(TribNames, TsiteNumbers)

trib_list<-list()
for (trib in 1:length(TsiteNumbers)){
  TDischarge<-readNWISdv(TsiteNumbers[trib], parameterCd, startDate, endDate)
  TDischarge <- renameNWISColumns(TDischarge)
  TDischarge$Flow_cms<-TDischarge$Flow /35.3147
  
  trib_list[[trib]]<-TDischarge
  names(trib_list)[trib]<-TribNames[trib]
}

Rootmerge<-merge(trib_list[c('Root River Main')][[1]], trib_list[c('Root River South')][[1]], by='Date')
Rootmerge$Flow_cms<-Rootmerge$Flow_cms.x+Rootmerge$Flow_cms.y
trib_list[[length(trib_list)+1]]<-Rootmerge
names(trib_list)[[length(trib_list)]]<-'Root River'

#Get Tributary locations
setwd("E:/Dropbox/ArcGIS")
tribs<-read.csv('TribsAlongRoute.csv', sep=",", header=TRUE, stringsAsFactors = F)
tribs$riverkm<-tribs$MEAS/1000
tribs<-tribs[order(tribs$MEAS, decreasing=FALSE),]

tribs$name2<-c("MN", "SC", "Ch", "Bl", "Rt", "WI", "Rk", "IA", "DM", "IL", "MO", "OH")
tribs2<-tribs[tribs$NAME!="Black River",]
tribs_add<-tribs2[1,]
tribs_add[1,]<-NA
tribs_add[1,c('riverkm')]<-c(491)
tribs_add[1, c('NAME', 'name2')]<-c("Maquoketa River", "MA")
tribs2<-rbind(tribs2, tribs_add)

setwd('E:/Dropbox/FLAME_MississippiRiver/Data/2015_UMR_AllDays')
TribChemistry<-read.csv('UMR2015_AllWaterChemSamples.csv', header=T, stringsAsFactors = F)

TribChemistry2<-  TribChemistry[TribChemistry$Sample.Notes %in%  names(trib_list),]
TribChemistry2$DateTime<-as.Date(TribChemistry2$DateTime, format="%m/%d/%Y")

TribChemistry2$Q<-NA
sample=1
for (sample in 1:nrow(TribChemistry2)){
  site<-TribChemistry2$Sample.Notes[sample]
  date<-TribChemistry2$DateTime[sample]
  table<-trib_list[[site]]
  TribChemistry2$Q[sample]<-table$Flow_cms[table$Date==date]
}

TribChemistry2$Sample.Notes<-sub("St.", "Saint", TribChemistry2$Sample.Notes)
TribChemistry2$Sample.Notes<-sub("River Confluence", "River", TribChemistry2$Sample.Notes)

Inputs<-intersect(tribs2$NAME, TribChemistry2$Sample.Notes)
InputChemistry<-TribChemistry2[TribChemistry2$Sample.Notes %in% Inputs,]

InputChemistry$riverkm[match(Inputs,InputChemistry$Sample.Notes)]<-tribs2$riverkm[match(Inputs,tribs2$NAME)]

#Make Table of flow for each pool

AugQDaily<-subset(DamQDaily, Date>=startDate & Date<=endDate)
names(AugQDaily)<-sub("DAM", "", names(AugQDaily))


Pool1<-dischargeUnit[dischargeUnit$site_no=='05331000', c('Date', 'Flow_cms')]
Pool2<-dischargeUnit[dischargeUnit$site_no=='05331580', c('Date', 'Flow_cms')]
Pool3<-dischargeUnit[dischargeUnit$site_no=='05344500', c('Date', 'Flow_cms')]
Pool5A<-dischargeUnit[dischargeUnit$site_no=='05378500', c('Date', 'Flow_cms')]
Pool13<-dischargeUnit[dischargeUnit$site_no=='05420500', c('Date', 'Flow_cms')]
Pool19<-dischargeUnit[dischargeUnit$site_no=='05474500', c('Date', 'Flow_cms')]
Pool26<-dischargeUnit[dischargeUnit$site_no=='05587450', c('Date', 'Flow_cms')]

AugQDaily$'1' <-Pool1$Flow_cms[match(AugQDaily$Date, Pool1$Date)]
AugQDaily$'2' <-Pool2$Flow_cms[match(AugQDaily$Date, Pool2$Date)]
AugQDaily$'3' <-Pool3$Flow_cms[match(AugQDaily$Date, Pool3$Date)]
AugQDaily$'5A' <-Pool5A$Flow_cms[match(AugQDaily$Date, Pool5A$Date)]
AugQDaily$'13' <-Pool13$Flow_cms[match(AugQDaily$Date, Pool13$Date)]
AugQDaily$'19' <-Pool19$Flow_cms[match(AugQDaily$Date, Pool19$Date)]
AugQDaily$'26' <-Pool26$Flow_cms[match(AugQDaily$Date, Pool26$Date)]

AugQDaily$Pepin<-AugQDaily$'3'
AugQDaily$'15'<-AugQDaily$'14'

setwd("E:/Git_Repo/nitrogen-retention")
saveRDS(AugQDaily, file = "UMR_AugQDaily.rds")


# =================================
# Step 4
# Calculate Change in NO3/Turb for each pool
# Flow weighted
# =================================

#Load Flame data
setwd("E:/Dropbox/ArcGIS")

data<-read.table('UMR_AllDays_Route2.txt', header=TRUE, sep="," ,skip=0)
data$riverkm<-data$MEAS/1000
data<-data[order(data$MEAS),]
data[data==0] <- NA
data$ltime<-as.POSIXct(data$ltime, format="%Y-%m-%d %H:%M:%S", tz="America/Chicago")

NO3data<-data[!is.na(data$NITRATEM),]
NO3data$rollNO3<-rollmean(NO3data$NITRATEM, k=25, align='center', fill=NA)

Turbdata<-data[!is.na(data$TurbFNU),]
Turbdata$rollTurb<-rollmean(Turbdata$TurbFNU, k=10, align='center', fill=NA)

plot(NO3data$NITRATEM)
lines(NO3data$rollNO3, type="l", col="red")

#Load dam data
dams<-read.csv('DamsAlongRoute3.csv', sep=",", header=TRUE)
dams$riverkm<-dams$MEAS/1000
dams<-dams[order(dams$MEAS, decreasing=FALSE),]
dams$name<-c('SAF-U', 'SAF-L', 1,2,3,4,5,'5A', 6,7,8,9,10,11,12,13,14,15,16,17,18, 19, 20, 21, 22, 24, 25, '26')

dams1<-dams[dams$riverkm>10,]
Pepindam<-as.data.frame(matrix(nrow=1, ncol=ncol(dams1)))
names(Pepindam)=names(dams1)
Pepindam[1,c(2,(ncol(Pepindam)-1):ncol(Pepindam))]<-c(148500 , 148.5, "Pepin")

dams2<-rbind(dams1, Pepindam)
dams2$riverkm<-as.numeric(dams2$riverkm)
dams2$MEAS<-as.numeric(dams2$MEAS)
dams2<-dams2[order(dams2$riverkm, decreasing=FALSE),]

dam_km<-dams2$riverkm
dam_name<-dams2$name

dams$name



#Indicate which rows have tributaries entering
InputChemistry$poolInterval<-findInterval(InputChemistry$riverkm, vec=c(0,dam_km) )


# start Loop here
# Make list and data frame to fill with data
flamedata_list<-list()
flamedata_list2<-flamedata_list
pool_summary<-as.data.frame(matrix(nrow=length(dam_name), ncol=13))
names(pool_summary)<-(c("Pool", "RiverKM_start", "RiverKM_end","Pool_length", "NO3_start", "NO3_end", "dNO3", "Turb_start", "Turb_end", "dTurb", "RNO3", "RTurb", "Q"))

dam_nu<-1
for (dam_nu in 1:length(dam_km)){
  if (dam_nu==1){
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

setwd("E:/Git_Repo/nitrogen-retention")
saveRDS(pool_summary, file = "UMR_R_dNO3_estimates.rds")


# ==============================
# Step 5 
# Merge tables and output single file
# bathy_df - Volume
# summary_df - Area and BW/I percentages
# pool_summary - Q, R, location
# ==============================

bathy_df$Pool
summary_df$Pool
pool_summary$Pool
intersect(pool_summary$Pool, intersect(bathy_df$Pool,summary_df$Pool))

merge1<-merge(summary_df, bathy_df, by='Pool', all=T)
merge2<-merge(merge1, pool_summary, by='Pool', all=T)
merge2<-merge2[order(merge2$RiverKM_start),]

#Summarize Pool Areas


AllPools<-merge2[1,]
AllPools[1,]<-NA
AllPools$Pool<-"All Pools"
AllPools$TotalArea<-sum(merge2$TotalArea, na.rm=T)
AllPools$I_Area<-sum(merge2$TotalArea*merge2$I_Area, na.rm=T)/sum(merge2$TotalArea, na.rm=T)
AllPools$BWc_Area<-sum(merge2$TotalArea*merge2$BWc_Area, na.rm=T)/sum(merge2$TotalArea, na.rm=T)
AllPools$RTurb<-(-0.833)
AllPools$RNO3<-0.087
AllPools$Q<-merge2$Q[merge2$Pool=='p26']

merge3<-rbind(merge2, AllPools)

merge3$WRT_d<-merge3$Volume/merge3$Q*(1000000/3600/24) #days
merge3$Z_mean_m<-merge3$Volume/merge3$TotalArea
merge3$H<-merge3$Q/merge3$TotalArea*31.536

# merge3$Vf<-((-1)*merge3$Z_mean_m/merge3$WRT_d*365 * log(1-merge3$RNO3))
# merge3$vf50<-((-1)*merge3$Z_mean_m/merge3$WRT_d*365 * log(1-0.5))
# merge3$vf20<-((-1)*merge3$Z_mean_m/merge3$WRT_d*365 * log(1-0.2))
# merge3$vf10<-((-1)*merge3$Z_mean_m/merge3$WRT_d*365 * log(1-0.1))

merge3$Vf<-((-1)*merge3$H * log(1-merge3$RNO3))
merge3$vf50<-((-1)*merge3$H * log(1-0.5))
merge3$vf20<-((-1)*merge3$H * log(1-0.2))
merge3$vf10<-((-1)*merge3$H * log(1-0.1))




names(merge3)[names(merge3) == 'Q'] <- 'Q_cms'

setwd('E:/Dropbox/FLAME_MississippiRiver')
write.table(merge3, "UMR_Pool_Summary_Table.csv", sep=",", row.names=F, col.names=T)

setwd("E:/Git_Repo/nitrogen-retention")
saveRDS(merge3, file = "UMR_Pool_Summary_Table.rds")


