
library(lubridate)
library(zoo)
library(dataRetrieval)
library(gtools)
library(graphics)

#Get Discharge Data

parameterCd <- c("00010", "00060", "99133") # Discharge, NO3
startDate <- "2015-01-01"
endDate <- Sys.Date()
siteNumbers<-c('05378500') # Winona (LD 5a)

siteINFO<-readNWISsite(siteNumbers)
dailyDataAvailable <- whatNWISdata(siteNumbers, service="uv")

dischargeUnit <- readNWISdv(siteNumbers, parameterCd, startDate, endDate)
dischargeUnit <- renameNWISColumns(dischargeUnit)

# Convert to cubic meter per second 
dischargeUnit$Flow_cms<-dischargeUnit$Flow /35.3147


list.files()
PepinLTRM<-read.csv("LTRMP_WQ_DATA_0926141843.txt", sep=",", header=T, stringsAsFactors = F)
head(PepinLTRM)

PepinLTRM$DATE<-as.Date(PepinLTRM$DATE, format="%m/%d/%Y")
PepinLTRM<-PepinLTRM[,c("DATE", "LOCATCD", "TEMP", "NOX", "NOXQF")]

PepinLTRM<-subset(PepinLTRM, DATE>startDate & !is.na(NOX) & NOXQF!=64)
Dates<-as.Date(c("2007-01-01", "2008-01-01", "2009-01-01", "2010-01-01", "2011-01-01", "2012-01-01", "2013-01-01", "2014-01-01", "2015-01-01", "2016-01-01"))

aggregated<-aggregate(PepinLTRM[,c("TEMP", "NOX")], by=list(PepinLTRM$DATE, PepinLTRM$LOCATCD), FUN="mean")

names(aggregated)[1:2]<-c("DATE", "LOCATCD")
aggregated$month<-month(aggregated$DATE)
sites<-as.character(unique(aggregated$LOCATCD))

site1<-subset(aggregated, LOCATCD==sites[1])
site2<-subset(aggregated, LOCATCD==sites[2])
site3<-subset(aggregated, LOCATCD==sites[3])
site4<-subset(aggregated, LOCATCD==sites[4])
site5<-subset(aggregated, LOCATCD==sites[5])
site6<-subset(aggregated, LOCATCD==sites[6])
site7<-subset(aggregated, LOCATCD==sites[7])

colors<-matlab.like2(n=length(sites))


png("PepinNO3LTRM.png", res=200, width=5,height=4, units="in")

par(mfrow=c(2,1))
par(oma=rep(0, 4))
par(mar=c(2, 3.5, 0.5, 3.5))
cex=0.7
par(cex=cex)

plot(site1$NOX ~site1$DATE, col=colors[1], type="p", ylab="", xlab="",yaxt="n", ylim=range(aggregated$NOX), xlim=range(aggregated$DATE), lwd=3, xaxt="n", pch=16)
points(site2$NOX ~site2$DATE, col=colors[2], type="p", pch=16)
points(site3$NOX ~site3$DATE, col=colors[3], type="p", pch=16)
points(site4$NOX ~site4$DATE, col=colors[4], type="p", pch=16)
points(site5$NOX ~site5$DATE, col=colors[5], type="p", pch=16)
points(site6$NOX ~site6$DATE, col=colors[6], type="p", pch=16)
points(site1$NOX ~site1$DATE, col=colors[1], type="o", lwd=3, pch=16)
points(site7$NOX ~site7$DATE, col=colors[7], type="o", lwd=3, pch=16)

xdates<-floor_date(as.POSIXct(range(aggregated$DATE)), "month")


axis.Date(1, at = seq(xdates[1], xdates[2], "quarter"), labels=F, tck=-0.02)
axis.Date(1, at = seq(xdates[1], xdates[2], "year"), labels=F, tck=-0.05)
axis.Date(1, at = seq(xdates[1], xdates[2], "year")+181*3600*24, labels=T, tck=0)

axis(2, las=1)
mtext("Nitrate (mgN/L)", 2, 2.5, cex=cex)

legend("topleft", inset=0.01, c("Pepin inlet", "Pepin outlet"), lwd=2, col=c(colors[c(length(colors),1)], lty=1), bty="n")



# mtext("Date", 1, 2)

abline(v=Dates, lty=3)
abline(v=as.Date("2015-08-02"), lty=1)

plot(dischargeUnit$Date, dischargeUnit$Flow_cms, type="l", ylab="", xlab="", lwd=1, col="darkgrey", yaxt="n", xlim=range(aggregated$DATE), xaxt="n")
axis(2)
mtext("Discharge (cms)", 2, 2.5, cex=cex)

axis.Date(1, at = seq(xdates[1], xdates[2], "quarter"), labels=F, tck=-0.02)
axis.Date(1, at = seq(xdates[1], xdates[2], "year"), labels=F, tck=-0.05)
axis.Date(1, at = seq(xdates[1], xdates[2], "year")+181*3600*24, labels=T, tck=0)
abline(v=Dates, lty=3)

par(new=T)

plot(site7$TEMP ~site7$DATE, col="blue", type="l", lwd=1, xlim=range(aggregated$DATE), xaxt="n", ylab="", xlab="", yaxt="n")
axis(4, col="blue", col.axis="blue", las=1)
mtext("Temperature (Deg C)", 4, 2, cex=cex, col="blue")
box(which='plot')

dev.off()


merged<-merge(site1, site7, by="DATE")
names(merged)<-(c("DATE", "Site1", "Temp_Site1", "NO3_Site1", "Site2", "Temp_Site2", "NO3_Site2"))
merged$NO3_Diff<-merged$NO3_Site2-merged$NO3_Site1



PepinMerged<-merge(merged, dischargeUnit, by.x="DATE", by.y="Date", all.x=T)

par(mfrow=c(1,1))
par(oma=rep(0, 4))
par(mar=c(3, 4, 1, 4))
flowcut<-median(dischargeUnit$Flow_cms)

plot(PepinMerged$NO3_Diff[PepinMerged$Flow_cms<flowcut]~PepinMerged$DATE[PepinMerged$Flow_cms<flowcut], type="p", pch=16, col="black", ylab="", xlab="", las=1, ylim=range(PepinMerged$NO3_Diff), xlim=range(merged$DATE))
points(PepinMerged$NO3_Diff[PepinMerged$Flow_cms>flowcut]~PepinMerged$DATE[PepinMerged$Flow_cms>flowcut], type="p", pch=16, col="red")
axis(2, las=1)
mtext("NO3 Diff (mgN/L)", 2, 2.5)

abline(h=c(0, mean(PepinMerged$NO3_Diff[PepinMerged$Flow_cms>flowcut]), mean(PepinMerged$NO3_Diff[PepinMerged$Flow_cms<flowcut])), lty=c(1,2,2), col=c("black", "red", "black" ))

par(new=T)
plot(dischargeUnit$Date, dischargeUnit$Flow_cms, type="l", ylab="", xlab="", lwd=0.5, col="blue", yaxt="n", xlim=range(merged$DATE))
axis(4, col="blue", col.axis="blue")
axis(4, col="blue", col.axis="blue", at=flowcut, labels=c("*MF"), las=1)
mtext("Discharge (cms)", 4, 2, col="blue")
axis(1, at=Dates, labels=F)

legend("topleft", inset=0.02, c("Flow<MedianFlow", "Flow>MedianFlow"), col=c("black", "red"), pch=16)

#End plot


w<-2736
dx<-(787.6-764.3)*1000


PepinMerged$U<-PepinMerged$NO3_Diff/dx*PepinMerged$Flow_cms/w *(3600)*24	*1000

plot(PepinMerged$U[PepinMerged$Flow_cms<flowcut]~PepinMerged$DATE[PepinMerged$Flow_cms<flowcut], type="p", pch=16, col="black", ylab="", xlab="", las=1, ylim=range(PepinMerged$U), xlim=range(merged$DATE))
points(PepinMerged$U[PepinMerged$Flow_cms>flowcut]~PepinMerged$DATE[PepinMerged$Flow_cms>flowcut], type="p", pch=16, col="red")

abline(h=mean(PepinMerged$U[PepinMerged$Flow_cms>flowcut]), col="red", lty=2)
abline(h=mean(PepinMerged$U[PepinMerged$Flow_cms<flowcut]), col="black", lty=2)


#End U plot



plot(PepinMerged$NO3_Diff~PepinMerged$Flow_cms, type="p")
model<-lm(PepinMerged$NO3_Diff~PepinMerged$Flow_cms)
summary(model)
abline(model)

model2<-lm(PepinMerged$NO3_Diff~PepinMerged$Flow_cms*PepinMerged$Temp_Site1)
summary(model2)

PepinSummer<-subset(PepinMerged, Temp_Site1>15)
plot(PepinSummer$NO3_Diff~PepinSummer$Flow_cms, type="p")
model<-lm(PepinSummer$NO3_Diff~PepinSummer$Flow_cms)
summary(model)
abline(model)

model2<-lm(PepinSummer$NO3_Diff~PepinSummer$Flow_cms*PepinSummer$Temp_Site1)
summary(model2)

plot(PepinMerged$NO3_Diff~PepinMerged$Flow_cms, type="p", pch=16)
points(PepinSummer$NO3_Diff~PepinSummer$Flow_cms, type="p", pch=16, cex=1, col="red")
