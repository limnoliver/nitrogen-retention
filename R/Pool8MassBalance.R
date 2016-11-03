#####################################################
# Script to calculate NO3 retention in Pool 8
# Code will run for all of the Pool 8 sampling events
# Inputs include discharge (Q) and [NO3] (mg N / L)
# Inputs include Dam 7, Root and Lacrosse Rivers
# Output is condition at Dam 8
#####################################################

# load packages
library(dataRetrieval)

# Get Dam 7 Discharge Data
# setwd("E:/Dropbox/FLAME_MississippiRiver")
# Dam7Q<-read.table('ld7_Q_2014.txt', header=T, skip=5, stringsAsFactors = F, sep="")
# Dam7Q$DATE<-as.Date(Dam7Q$DATE, format="%d%b%Y")
# Dam7Q<-Dam7Q[!is.na(Dam7Q$DATE),]


# Get Dam 8 Discharge Data
setwd("E:/Dropbox/FLAME_MississippiRiver")
Dam8Q<-read.table('ld8_Q_2014.txt', header=T, skip=5, stringsAsFactors = F, sep="")
Dam8Q$DATE<-as.Date(Dam8Q$DATE, format="%d%b%Y")
Dam8Q<-Dam8Q[!is.na(Dam8Q$DATE),]

