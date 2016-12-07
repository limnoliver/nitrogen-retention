
rm(list=ls(all=TRUE))

#Get Discharge Data
# install.packages('dataRetrieval')
library(dataRetrieval)
library(colorRamps)
library(sp)
library(gtools)
library(sensorQC)

# Load image scale function
source('R/ImageScale.R')

#  get discahrge data for Mississippi River (at Winona)
dischargeUnit<-readRDS('Data/missriver_discharge.rds')
dischargeUnit<-dischargeUnit[dischargeUnit$site_no=='05378500',]
dischargeUnit$dateTime<-as.POSIXct(paste(dischargeUnit$Date, ' 12:00:00', sep=""), tz='America/Chicago')

TribDischarge<-readRDS('Data/trib_discharge.rds')
RootDischarge<-TribDischarge['Root River'][[1]]
RootDischarge$dateTime<-as.POSIXct(paste(RootDischarge$Date, ' 12:00:00', sep=""), tz='America/Chicago')
                       
#Get LTRMP NO3 data and plot
a<-read.csv('Data/N_turb_p8fss_2015.csv', header=T, stringsAsFactors = F)

a[,1]<-as.Date(a[,1], format="%m/%d/%Y")
a$Date.Time<-as.POSIXct(paste(a[,1], a[,2], sep=' '), format="%Y-%m-%d %H:%M", tz="America/Chicago")

a<-a[which(a$NOXQF!=64),]

sites<-unique(a$site)
sites<-sites[c(1:2,4,3,5:length(sites))]
names<-c('Lawrence Lake N', 'Lawrence Lake S', 'Target Lake', 'Stoddard Island',  'MC Center', 'MC East', 'MC West', 'MC Mean')

colors1<-colorRampPalette(c("blue4", "blue","slategray1", "orange", "brown4", "black"))(length(sites)+3)
colors<-colors1[2:(length(colors1)-1)]
# colors<-bpy.colors(length(sites)+2, cutoff.tails=0.0, alpha=1)
# colors<-colors[1:(length(colors)-1)]
pch<-c(rep(1, 4), rep(15, 3),NA)
lty<-c(rep(0,length(sites)), 1)

a$match<-match(a$site, sites, nomatch=NA)
a$col<-colors[a$match]
a$symbol<-pch[a$match]

main<-subset(a, match>=5)
main_avg<-as.data.frame(aggregate(main, by=list(main$date), FUN=mean, na.rm=T))

mainvars<-names(main)
main_avg2<-main_avg[mainvars]

jittertime<-as.POSIXct(jitter(as.numeric(a$Date.Time), 150), tz="America/Chicago", origin = "1970-01-01")
plot(jittertime~ a$Date.Time)
df<-data.frame(a$Date.Time, jittertime)
df$diff<-df[,1]-df[,2]
# hist(as.numeric(df$diff))
mean(as.numeric(df$diff))

#Plot LTRM and Discharge Data

png("Figures/LTRMP_NO3_2015.png", width=5, height=3, units='in', res=600)

cex=0.6
par(cex=cex, cex.axis=cex)
par(mar=c(2,2,0,0))
par(oma=c(.5,.5,.5,.5))
par(mgp=c(3,.5,0))
par(tck=c(-.03))
xlim=range(main_avg2$Date.Time)
layout(matrix(c(1,2), nrow=2, ncol=1), widths=c(1), heights=c(6,4))

plot(jittertime, a$NOX, col=a$col, pch=a$symbol, lwd=2, xlab="", ylab="", las=1, xlim=xlim, cex=cex)
lines(main_avg2$Date.Time, main_avg2$NOX, col=colors[length(colors)], type="l", lwd=2)

legend("topright", inset=0.02, rev(c(names)), col=rev(colors), pch=rev(pch), bty = "o", ncol=1, lwd=2, lty=rev(lty), cex=cex)

mtext(expression(paste(NO[3], " (mg N L"^"-1", ")")), 2, 1.5, cex=cex)

plot(dischargeUnit$dateTime, dischargeUnit$Flow_cms, type="l", col='black', lty=1, lwd=1, ylab="", xlab="", xlim=xlim)

mtext(expression(paste("Discharge (m"^"3", " s"^"-1", ")")), 2, 1.5, cex=cex)
mtext("2015", 1, -.5, cex=cex, outer=T)

dev.off()


# get FLAME and LTER data for sample sites
AllMissMerged<-readRDS('Data/MissRiver_WaterChem_FlameLTER.rds')

dfNO3<-AllMissMerged[which(!is.na(AllMissMerged$`NO3 NO2`) & !is.na(AllMissMerged$NITRATEMG)),]
dfNO3$NO3_MG<-dfNO3$'NO3 NO2'/1000
plot(dfNO3$NITRATEMG~(dfNO3$NO3_MG))
model<-lm(dfNO3$NITRATEMG~ dfNO3$NO3_MG)
abline(model)
lines(c(-1, 10), c(-1, 10), col="red")
summary(model)
n<-nrow(dfNO3)


# Plot FLAME (SUNA) vs LTER no3
B<-100 #Number of color breaks

# Turbidity color ramp
colors<-colorRampPalette(c("blue4", "blue","slategray1", "orange", "brown4", "black"))( 120 )[10:110]

dfNO3$Turbpoint<-log10(dfNO3$TurbFNU)
dfNO3$Col <- as.numeric(cut(dfNO3$Turbpoint,breaks = B))
dfNO3$Color<-colors[dfNO3$Col]
dfNO3<-dfNO3[order(dfNO3$TurbFNU),]

#SUNA vs lab figure
png("Figures/LTER_SUNA_NO3.png", width=3, height=3.8, units='in', res=600)

cex=0.9
par(cex=cex, cex.axis=cex)
par(mar=c(2.5,2.5,0.5,0.5))
par(oma=rep(0,4))
par(mgp=c(3,.5,0))
par(tck=c(-.02))
lim=range(c(dfNO3$NITRATEMG, dfNO3$NO3_MG), na.rm=T)

layout(matrix(c(1,2), nrow=2, ncol=1), widths=c(3), heights=c(3,.8))
breaks <- seq(min(dfNO3$Turbpoint, na.rm = TRUE), max(dfNO3$Turbpoint, na.rm = TRUE) ,length.out=100)
# par(mar=c(1,1,1,1))


# plot(dfNO3$NITRATEMG~dfNO3$NO3_MG, xlim=lim, ylim=lim, las=1, ylab="", xlab="", axes=F, col=dfNO3$Color, pch=16, cex=0.6+dfNO3$Turbpoint/1.5)
plot(dfNO3$NITRATEMG~dfNO3$NO3_MG, xlim=lim, ylim=lim, las=1, ylab="", xlab="", axes=F, col=dfNO3$Color, pch=16, cex=1.6)
box(which='plot')
axis(1, mgp=c(3,.3,0))
axis(2, las=1)

mtext(expression(paste('SUNA ', NO[3], " (mg N L"^"-1", ")")), 2, 1, cex=cex)
mtext(expression(paste('Lab ', NO[3], " (mg N L"^"-1", ")")), 1, 1.5, cex=cex)

abline(model)
lines(c(-1, 10), c(-1, 10), lty=2)

legend("topleft", inset=0.02, c("Least Squares", "1:1"), bty = "n", ncol=1, lty=c(1,2), cex=cex)
legend("bottomright", inset=0.02, c(paste("n=",n, sep="")), bty = "n",cex=cex)

#Add scale
par(mar=c(2,1,1,1), bg=NA, mgp=c(3, .1, 0))
image.scale((dfNO3$Turbpoint), col=colors[1:(B-1)], breaks=breaks-1e-8, axis.pos=1)
mtext(expression(paste(log[10], " Turbidity (FNU)", sep="")), 1, 1.1, cex=cex)
#abline(v=levs)
box()

dev.off()




# Get FLAME data from sites that match LTRMP data

Flamesites<-unique(allcsv$Sample.Notes)
allcsv2<-allcsv[which(is.finite(allcsv$NITRATEMG)),]

MCsites<-Flamesites[grep("7", Flamesites)]
MCdata<-allcsv2[which(allcsv2$Sample.Notes %in% MCsites),]

Lawsites<-Flamesites[grep("Law", Flamesites)]
Lawdata<-allcsv2[which(allcsv2$Sample.Notes %in% Lawsites),]

Tarsites<-Flamesites[grep("Tar", Flamesites)]
Tardata<-allcsv2[which(allcsv2$Sample.Notes %in% Tarsites),]

Tursites<-Flamesites[grep("Tur", Flamesites)]
Turdata<-allcsv2[which(allcsv2$Sample.Notes %in% Tursites),]

Consites<-Flamesites[grep("Cone", Flamesites)]
Condata<-allcsv2[which(allcsv2$Sample.Notes %in% Consites),]

Rensites<-Flamesites[grep("Reno", Flamesites)]
Rendata<-allcsv2[which(allcsv2$Sample.Notes %in% Rensites),]

subsites<-c("BW_LLN", "BW_TL", "BW_SI", "MC_C")
LLNdata<-a[a$site==subsites[1],]
TLdata<-a[a$site==subsites[2],]
SIdata<-a[a$site==subsites[3],]
MCCdata<-a[a$site==subsites[4],]

UMR_Dates <- as.POSIXct(c("2015-08-01 00:00:00", "2015-08-14 00:00:00"), tz="America/Chicago")



# Plot of NO3 over time in Pool 8.
# This will merge with Pool 8 maps (2015)
png("Figures/FlameSites_LTRMP_NO3_2015.png", width=5, height=3, units='in', res=600)

TScolors<-c(colors[95], 'tan3', 'deepskyblue', 'blue', 'blue4')
cex=0.6
# par(ps=8)
par(cex=cex, cex.axis=cex)
par(mar=c(.5,2,0,0))
par(oma=c(1.5,.5,.5,6.5))
par(mgp=c(3,.3,0))
par(tck=c(-.03))
xlim=range(main_avg2$Date.Time)
ylim=range(c(TLdata$NOX, LLNdata$NOX, SIdata$NOX,main_avg2$NOX), na.rm=T)
layout(matrix(c(1,2), nrow=2, ncol=1), widths=c(1), heights=c(5,5))


plot(main_avg2$Date.Time, main_avg2$NOX, col=colors[length(colors)], type="n", lwd=1, xlab="", ylab="", xlim=xlim, cex=cex, ylim=ylim, pch=16, yaxt="n", xaxt="n", bty="L")
axis(1, at=seq(as.POSIXct('2015-03-01'), as.POSIXct('2015-11-01'), by='month')[c(1,3,5,7,9)], labels=NA)

points(main_avg2$Date.Time, main_avg2$NOX, col=TScolors[1], type="o", lwd=1, cex=cex, ylim=ylim, pch=16)
axis(2, mgp=c(3,.5,0), las=1)

points(TLdata$Date.Time, TLdata$NOX, col=TScolors[4], lwd=1, cex=cex, type="o", pch=16)
points(SIdata$Date.Time, SIdata$NOX, col=TScolors[3], lwd=1, cex=cex, type="o", pch=16)
points(LLNdata$Date.Time, LLNdata$NOX, col=TScolors[5], lwd=1, cex=cex, type="o", ylim=ylim, pch=16)

points(Tardata$DateTime, Tardata$NITRATEMG, col=TScolors[3], pch=8, cex=cex)
points(Lawdata$DateTime, Lawdata$NITRATEMG, col=TScolors[5], pch=8, cex=cex)
points(MCdata$DateTime, MCdata$NITRATEMG, col=TScolors[1], pch=8, cex=cex)
points(Turdata$DateTime, Turdata$NITRATEMG, col=TScolors[2], pch=8, cex=cex)

mtext(expression(paste(NO[3], " (mg N L"^"-1", ")")), 2, 1.5, cex=cex)

box(which='plot')

# turn off clipping and plot legend outside of plot window
par(xpd=F)
dimensions<-par('usr')


legend(dimensions[2],dimensions[4], c("Main Channel", "Side Channel", "Stoddard Island",  "Target Lake",  "Lawrence Lake"), col=TScolors, text.col= TScolors , bty = "n", ncol=1, cex=cex, title="Sample Site", title.col="Black", xpd=NA)


legend(dimensions[2]+1000000,dimensions[4]-2, c("FLAME", "LTRM"), col='black', bty = "n", ncol=1, lty=c(0,1), pch=c(8, 16), cex=cex, title="Sample Type", title.col="Black", xpd=NA)



plot(dischargeUnit$dateTime, dischargeUnit$Flow_cms, type="l", col=TScolors[1], lty=1, lwd=1, ylab="", xlab="", xlim=xlim, yaxt="n", xaxt="n")
axis(2, mgp=c(3,.5,0), las=1)
axis(1, at=seq(as.POSIXct('2015-03-01'), as.POSIXct('2015-11-01'), by='month')[c(1,3,5,7,9)], labels=c('Mar 1', 'May 1', 'Jul 1', 'Sep 1', 'Nov 1'))
mtext(expression(paste("Discharge (m"^"3", " s"^"-1", ')')), 2, 1.5, cex=cex)

#Plot Root River Discharge
par(new=T)
plot(RootDischarge$dateTime, RootDischarge$Flow_cms, type="l", col='grey40', lty=2, lwd=1, ylab="", xlab="", xlim=xlim, yaxt="n", xaxt="n")
axis(4, mgp=c(3,.5,0), las=1)
mtext(expression(paste("Root River")), 4, 1.1, cex=cex)
mtext(expression(paste("Discharge (m"^"3", " s"^"-1", ')')), 4, 1.8, cex=cex)


mtext(expression(paste("Discharge (m"^"3", " s"^"-1", ')')), 2, 1.5, cex=cex)
mtext("2015", 1, .5, cex=cex, outer=T)

dev.off()


