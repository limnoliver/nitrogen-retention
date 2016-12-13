

# Get Mississippi River Flame Data
# Look in Loken Desktop Dropbox/FLAME_MississippiRiver folder

directories<-list.files('E:/Dropbox/FLAME_MississippiRiver/Data')
# allcsv<-data.frame()
k<-1
for (k in 1:length(directories)){
  dir<-paste('E:/Dropbox/FLAME_MississippiRiver/Data/', directories[k], sep="")
  list<-list.files(paste(dir, sep=""))
  download<-list[grep('cleaned.csv', list, ignore.case=T)]
  if(length(download)>=1){
    csv<-read.csv(paste(dir, download, sep="/"), header=T, stringsAsFactors = F)
    
    if (k==1){
      allcsv<-csv}
    if (k>1){
      allcsv<-smartbind(allcsv, csv, fill=NA)}
  }
}

str(allcsv)
summary(allcsv$NITRATEMG, na.rm=T)
hist(allcsv$NITRATEMG)
NO3csv<-allcsv[which(!is.na(allcsv$NITRATEMG)),]
summary(NO3csv$NITRATEMG)

