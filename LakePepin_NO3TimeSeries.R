
#Get Discharge Data

parameterCd <- c("00060", "99133") # Discharge, NO3
startDate <- "2007-01-01"
endDate <- Sys.Date()
siteNumbers<-c('05378500') # Winona (LD 5a)

siteINFO<-readNWISsite(siteNumbers)
dailyDataAvailable <- whatNWISdata(siteNumbers, service="uv")

dischargeUnit <- readNWISdv(siteNumbers, parameterCd, startDate, endDate)
dischargeUnit <- renameNWISColumns(dischargeUnit)

# Convert to cubic meter per second 
dischargeUnit$Flow_cms<-dischargeUnit$Flow /35.3147


list.files()
PepinLTRM<-read.csv("LTRMP_WQ_DATA_0924173851.txt", sep=",", header=T, stringsAsFactors = F)
head(PepinLTRM)

PepinLTRM$DATE<-as.Date(PepinLTRM$DATE, format="%m/%d/%Y")
PepinLTRM<-PepinLTRM[,c(2, 4:6)]

PepinLTRM<-subset(PepinLTRM, DATE>"2007-01-01" & !is.na(NOX) & NOXQF!=64)
Dates<-as.Date(c("2007-01-01", "2008-01-01", "2009-01-01", "2010-01-01", "2011-01-01", "2012-01-01", "2013-01-01", "2014-01-01", "2015-01-01", "2016-01-01"))

aggregated<-aggregate(PepinLTRM$NOX, by=list(PepinLTRM$DATE, PepinLTRM$LOCATCD), FUN="mean")

names(aggregated)<-c("DATE", "LOCATCD", "NOX")
sites<-as.character(unique(aggregated$LOCATCD))

site1<-subset(aggregated, LOCATCD==sites[1])
site2<-subset(aggregated, LOCATCD==sites[2])
site3<-subset(aggregated, LOCATCD==sites[3])
site4<-subset(aggregated, LOCATCD==sites[4])
site5<-subset(aggregated, LOCATCD==sites[5])
site6<-subset(aggregated, LOCATCD==sites[6])
site7<-subset(aggregated, LOCATCD==sites[7])

colors<-matlab.like2(n=length(sites))

par(mfrow=c(2,1))
par(oma=rep(0, 4))
par(mar=c(3, 4, 1, 1))

plot(site1$NOX ~site1$DATE, col=colors[1], type="l", ylab="", xlab="",yaxt="n")
points(site2$NOX ~site2$DATE, col=colors[2], type="l")
points(site3$NOX ~site3$DATE, col=colors[3], type="l")
points(site4$NOX ~site4$DATE, col=colors[4], type="l")
points(site5$NOX ~site5$DATE, col=colors[5], type="l")
points(site6$NOX ~site6$DATE, col=colors[6], type="l")
points(site7$NOX ~site7$DATE, col=colors[7], type="l")

axis(2)
mtext("Nitrate (mgN/L)", 2, 3)

mtext("Date", 1, 2)

abline(v=Dates, lty=3)


plot(dischargeUnit$Date, dischargeUnit$Flow_cms, type="l", ylab="", xlab="", lwd=0.5, col="grey", yaxt="n")
axis(2)
mtext("Discharge", 2, 3)





merged<-merge(site1, site2, by="DATE")
names(merged)<-(c("DATE", "Site1", "NO3_Site1", "Site2", "NO3_Site2"))
merged$NO3_Diff<-merged$NO3_Site2-merged$NO3_Site1
plot(merged$NO3_Diff~merged$DATE, type="b")
abline(h=0)


