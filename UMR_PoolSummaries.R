
# library(foreign)
library(rgdal)
library(rgeos)
# Code to calculate surface area, volume, for each of the Upper Mississippi River pools


setwd("E:/Dropbox/FLAME/basemaps/shapefiles")

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

summary_df

unique_codes<-unique(all_codes)
unique_desc<-unique(all_desc)
unique_table<-data.frame(unique_codes, unique_desc)

write.table(summary_df, "UMR_Pool_Areas.csv", sep=",", row.names=F, col.names=T)


# ===========================================
# Step 2
# Calculate Volume for pools with Bathy data
# ===========================================


dir2<-(paste(getwd(), "/USGS_Bathy", sep=""))
dir2_files<-list.files(dir2)

#Figure out file names
dir2_dbf<-dir2_files[grep("z15n83.dbf", dir2_files)]
dir2_shape<-sub(".dbf", "", dir2_dbf)


#Make dataframe and vector to fill with information
summary_df<-as.data.frame(matrix(nrow=length(dir1_dbf)+1, ncol=8))
names(summary_df)<-c("Pool", "TotalArea", "MC_Area", "SC_Area", "I_Area", "BWc_Area", "LP_Area", "BWi_Area" )
all_codes<-c()
all_desc<-c()

#loop through all bathy shapefiles
for (bathy in 1:length(dir2_shape)){
  i<-dif1_shape[file]
  
  name_i<-sub("aqa_1989_", "", i)
  name_i<-sub("_z15n83", "", name_i)
  summary_df[file,1]<-name_i

