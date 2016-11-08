###################################
# Code merges LTER water chemistry with FLAME
###################################

library(gtools)
library(colorRamps)

source('R/ImageScale.R')


setwd("E:/Dropbox/FLAME_MississippiRiver")

#get LTER chemlab data

allfiles<-list.files()
waterchemfile<-allfiles[grep('LTER_waterchem', allfiles, ignore.case=T)]

LTERdata<-do.call("rbind", lapply(waterchemfile, read.table, sep="," ,skip=0,  header = TRUE, fill=TRUE, stringsAsFactors = F))

str(LTERdata)

tests<-unique(LTERdata$Test)
samplesID<-unique(LTERdata$Sample.ID)

# longtable<-LTERdata[,4:6]
# shorttable<-longtable[!duplicated(longtable[,"Sample.ID"]),]

chemtable<-data.frame(matrix(ncol=1, nrow=length(samplesID)))
names(chemtable)[1]<-c('Sample.ID')
chemtable$Sample.ID<-samplesID


for (test in tests){
  testtable<-subset(LTERdata,Test==test)
  smalltable<-testtable[,c(6,12)]
  names(smalltable)[2]<-test
  chemtable<-merge(chemtable, smalltable, by='Sample.ID', all.x=T)
}

str(chemtable)
summary(chemtable)

# Get Mississippi River Data
# And merge with LTER

directories<-list.files('Data')
# allcsv<-data.frame()
k<-1
for (k in 1:length(directories)){
  dir<-directories[k]
  list<-list.files(paste('Data/', dir, sep=""))
  download<-list[grep('_Samples', list, ignore.case=T)]
  if(length(download)>=1){
    csv<-read.csv(paste('Data', dir, download, sep="/"), header=T, stringsAsFactors = F)
    
    if (k==1){
      allcsv<-csv}
    if (k>1){
      allcsv<-smartbind(allcsv, csv, fill=NA)}
  }
}

str(allcsv)

allcsv$DateTime<-as.POSIXct(allcsv$DateTime, format= "%Y-%m-%d %H:%M:%S", tz="America/Chicago")

AllMissMerged<- merge(allcsv, chemtable, by.x='Sample.Number', by.y='Sample.ID', all.x=T)

str(AllMissMerged)

write.table(AllMissMerged, file="MissRiverFlameLabMerged.csv", row.names = F, sep=",")

dfNO3<-AllMissMerged[which(!is.na(AllMissMerged$`NO3 NO2`) & !is.na(AllMissMerged$NITRATEMG)),]
dfNO3$NO3_MG<-dfNO3$'NO3 NO2'/1000
plot(dfNO3$NITRATEMG~(dfNO3$NO3_MG))
model<-lm(dfNO3$NITRATEMG~ dfNO3$NO3_MG)
abline(model)
lines(c(-1, 10), c(-1, 10), col="red")
summary(model)
n<-nrow(dfNO3)

B<-100 #Number of color breaks
colors<-bpy.colors(n=B, cutoff.tails=0.1, alpha=1)
colors<-(blue2green2red(n=B))
# 
colors<-colorRampPalette(c("blue4", "blue","slategray1", "orange", "brown4", "black"))( 120 )[10:110]

dfNO3$Turbpoint<-log10(dfNO3$TurbFNU)
dfNO3$Col <- as.numeric(cut(dfNO3$Turbpoint,breaks = B))
dfNO3$Color<-colors[dfNO3$Col]

dfNO3<-dfNO3[order(dfNO3$TurbFNU),]
#SUNA vs lab figure
png("LTER_SUNA_NO3.png", width=3, height=3.8, units='in', res=600)

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
