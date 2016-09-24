
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

plot(site1$NOX ~site1$DATE, col=colors[1], type="l")
points(site2$NOX ~site2$DATE, col=colors[2], type="l")
points(site3$NOX ~site3$DATE, col=colors[3], type="l")
points(site4$NOX ~site4$DATE, col=colors[4], type="l")
points(site5$NOX ~site5$DATE, col=colors[5], type="l")
points(site6$NOX ~site6$DATE, col=colors[6], type="l")
points(site7$NOX ~site7$DATE, col=colors[7], type="l")

abline(v=Dates, lty=3)

merged<-merge(site1, site2, by="DATE")
names(merged)<-(c("DATE", "Site1", "NO3_Site1", "Site2", "NO3_Site2"))
merged$NO3_Diff<-merged$NO3_Site2-merged$NO3_Site1
plot(merged$NO3_Diff~merged$DATE, type="b")
abline(h=0)


