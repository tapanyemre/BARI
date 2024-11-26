##### PREP 0: Load Neccesary Libraries and Functions#### 
# List of required packages
packages <- c("sf", "magrittr", "tidyverse")

# Install and load packages
lapply(packages, function(pkg) {
  if (!require(pkg, character.only = TRUE)) install.packages(pkg, dependencies = TRUE)
  library(pkg, character.only = TRUE, quietly = TRUE)
})

# Set global options
options(scipen = 100000000)

######### # PREP 0: Read Files ###########

# Input Paths
input_dir <- "~/BARI/Permits/Inputs"
permits_path = file.path(record_path)  #obtained from the first syntax
properties_path = file.path(input_dir, "Properties_2023.csv") # from BARI dataverse
landParcels_path = file.path(input_dir, "LandParcels_2023.csv")  # from BARI dataverse
landParcelsShpPath = file.path(input_dir, "LandParcels_2023.shp") # from BARI dataverse
blockGroupsShpPath_10 = file.path(input_dir, "Census Block Groups.shp") # from BARI dataverse
tractsShpPath_10 = file.path(input_dir, "Tracts_Boston BARI.shp") # from BARI dataverse
blockGroupsShpPath_20 = file.path(input_dir, "Block_Groups_Boston_2020_BARI.shp") # from BARI dataverse
tractsShpPath_20 = file.path(input_dir, "Tracts_2020.shp") # from BARI dataverse


# Output Path
output_dir <- "~/BARI/Permits/Outputs"
lpEcometricsPath = file.path(output_dir, "Parcels/")
csv = "csv/"
long = "longitudinal/"
shp = "shp/"
lpEcometricsName = "Permits.Ecometrics.LP"
cbgEcometricsPath = file.path(output_dir, "CBG/")
files_10 = "2010/" 
files_20 = "2020/" 
cbgEcometricsName = "Permits.Ecometrics.CBG"
ctEcometricsPath = file.path(output_dir, "CT/")
ctEcometricsName = "Permits.Ecometrics.CT"

# Read Files
permits = read.csv(permits_path,stringsAsFactors=FALSE)
landParcels = read.csv(landParcels_path, stringsAsFactors=F)
lps_shp = read_sf(landParcelsShpPath, stringsAsFactors=F)
cts_shp_10 = read_sf(tractsShpPath_10, stringsAsFactors=F)
bgs_shp_10 = read_sf(blockGroupsShpPath_10, stringsAsFactors=F)
cts_shp_20 = read_sf(tractsShpPath_20, stringsAsFactors=F)
bgs_shp_20 = read_sf(blockGroupsShpPath_20, stringsAsFactors=F)
propertyAssessment = read.csv(properties_path,stringsAsFactors=F)

#---- PREP BEFORE AGGREGATION ----
# Check data completeness
cat("Percentage of non-NA BG_ID_10:", sum(!is.na(permits$BG_ID_10)) / length(permits$BG_ID_10), "\n") #0.977 #0.9825709
cat("Percentage of non-NA CT_ID_10:", sum(!is.na(permits$CT_ID_10)) / length(permits$CT_ID_10), "\n") #0.977 0.9825709
cat("Percentage of non-NA ISSUED_DATE:", sum(!is.na(permits$ISSUED_DATE)) / length(permits$ISSUED_DATE), "\n")  #1

#this doesn't depend on the year so i can do it outside the loop
#highlights the fact that we are using the same number of parcels for each year

# Aggregate land parcels by Block Group and Census Tract for both 2010 and 2020
lps_perBG = aggregate(Land_Parcel_ID ~ BG_ID_10, data=landParcels,length)
names(lps_perBG)[2]="numLandParcels"

lps_perBG_20 = aggregate(Land_Parcel_ID ~ BG_ID_20, data=landParcels,length)
names(lps_perBG_20)[2]="numLandParcels"
##there are 40 block groups not present in the permit data, but this is because they are trivial spaces at the border of the city that technically belong to other municipalities##

lps_perCT = aggregate(Land_Parcel_ID ~ CT_ID_10, data=landParcels,length)
names(lps_perCT)[2]="numLandParcels"

lps_perCT_20 = aggregate(Land_Parcel_ID ~ CT_ID_20, data=landParcels,length)
names(lps_perCT_20)[2]="numLandParcels"

# Merge land parcel counts with shapefiles
cbgs_agg_long_10 <- lps_perBG
cts_agg_long_10 <- lps_perCT
cbgs_agg_long_20 <- lps_perBG_20
cts_agg_long_20 <- lps_perCT_20

names(cbgs_agg_long_10)[2] <- 'nLP'
names(cts_agg_long_10)[2] <- 'nLP'
names(cbgs_agg_long_20)[2] <- 'nLP'
names(cts_agg_long_20)[2] <- 'nLP'

cbgs_agg_long_shp_10 = merge(bgs_shp_10,lps_perBG,by.x = "GEOID10", by.y="BG_ID_10",all.x=T)
cts_agg_long_shp_10 = merge(cts_shp_10,lps_perCT,by.x = "GEOID10", by.y="CT_ID_10",all.x=T)
cbgs_agg_long_shp_20 = merge(bgs_shp_20,lps_perBG_20,by.x = "GEOID20", by.y="BG_ID_20",all.x=T)
cts_agg_long_shp_20 = merge(cts_shp_20,lps_perCT_20,by.x = "GEOID20", by.y="CT_ID_20",all.x=T)

# Prepare long format data for merging
lps_agg_long <- landParcels %>% select(Land_Parcel_ID)
# Merge the aggregated long format data with land parcel information
lps_agg_long = merge(lps_agg_long,landParcels[c("Land_Parcel_ID","BG_ID_10","CT_ID_10","BG_ID_20","CT_ID_20")],by="Land_Parcel_ID",all.x=T)


#### Aggregation at all levels ####
#loops through for each year and does 2010 and 2020 geographies separately
# this loop is ~300 lines

for (i in c(2010:2023)){ #put the latest year here
  print(i)
  permits_sub = permits[year(permits$ISSUED_DATE)==i,]
  permits_sub$DECLARED_VALUATION_c99 = ifelse(permits_sub$DECLARED_VALUATION<0, 0,
                                              ifelse(permits_sub$DECLARED_VALUATION>quantile(permits_sub$DECLARED_VALUATION,.99, na.rm = TRUE),
                                                     quantile(permits_sub$DECLARED_VALUATION,.99, na.rm = TRUE),
                                                     permits_sub$DECLARED_VALUATION))
  temp = permits_sub[ permits_sub$newcon==1,]
  permits_sub_nd =  rbind(permits_sub[ permits_sub$newcon!=1, ],
                          temp[!duplicated(paste(temp$DECLARED_VALUATION,temp$Land_Parcel_ID)),])
  rm(temp)
  
  permits_sub_nd = permits_sub_nd[ !duplicated(permits_sub_nd$PermitNumber),]
  
  
  #aggregating
  
  types_byParcel  = aggregate( cbind(demo,newcon,addition,reno) ~ Land_Parcel_ID,data =permits_sub, FUN=sum, na.rm=T)
  
  
  names(types_byParcel) = c("Land_Parcel_ID","DEMO","NEWCON","ADD","RENO")
  types_byParcel2 = aggregate(cbind(((demo+newcon+addition+reno)*DECLARED_VALUATION_c99),
                                    ((demo+newcon+addition+reno)*DECLARED_VALUATION_c99*government))~Land_Parcel_ID, data = permits_sub_nd,FUN=sum,na.rm=T)
  names(types_byParcel2) = c("Land_Parcel_ID","DV","DV_public")
  types_byParcel = merge(types_byParcel,types_byParcel2,by="Land_Parcel_ID",all=T)
  
  types_byParcel$NEWCON[types_byParcel$NEWCON > 0 ]=1
  types_byParcel$DEMO = ifelse(types_byParcel$NEWCON == 1,0,ifelse(types_byParcel$DEMO>0,1,0))
  types_byParcel$ADD = ifelse(types_byParcel$NEWCON == 1 | types_byParcel$DEMO == 1,0,ifelse(types_byParcel$ADD>0,1,0))
  types_byParcel$RENO = ifelse(types_byParcel$NEWCON == 1 | types_byParcel$DEMO == 1 | types_byParcel$ADD == 1,0,ifelse(types_byParcel$RENO > 0,1,0))
  
  
  types_byParcel = types_byParcel[!is.na(types_byParcel$Land_Parcel_ID) & (!types_byParcel$Land_Parcel_ID==0),]
  
  types_byParcel = merge(types_byParcel,landParcels[c("Land_Parcel_ID","BG_ID_10","CT_ID_10","BG_ID_20","CT_ID_20")],by="Land_Parcel_ID",all.x=T)
  
  # output this year's file out:
  write.csv(types_byParcel,paste(lpEcometricsPath,csv, lpEcometricsName,".",i,".csv", sep = ""),row.names=F)
  
  # rename the data frame so it persists
  assign(paste("types_byParcel",i,sep="_"), types_byParcel)
  
  # append years to colnames for wide longitudinal format:
  names(types_byParcel)[2:7] = paste(names(types_byParcel)[2:7], i, sep=".")
  lps_agg_long <- merge(lps_agg_long,types_byParcel[1:7],by='Land_Parcel_ID',all.x=TRUE)
  lps_agg_long[is.na(lps_agg_long[ncol(lps_agg_long)]),(ncol(lps_agg_long)-5):ncol(lps_agg_long)] <-0
  lps_shp = merge(lps_shp,types_byParcel[1:7],by.x ="Ln_P_ID", by.y ="Land_Parcel_ID", all.x=T)
  
  # aggregating to CBG level ####
  #2010#
  print("aggregating to CBG level")
  types_byParcel_i <- get(paste0("types_byParcel_", i))
  cbgs_agg_10 = aggregate(cbind(DEMO, NEWCON, ADD, RENO,
                                DEMO*DV, NEWCON*DV, ADD*DV, RENO*DV,DV_public)~BG_ID_10, data= types_byParcel_i, FUN=sum,na.rm=T)
  names(cbgs_agg_10)[2:ncol(cbgs_agg_10)] = c("DEMO_count","NEWCON_count","ADD_count","RENO_count",
                                              "DEMO_DV","NEWCON_DV","ADD_DV","RENO_DV","Public_DV")
  
  # merge on the number of lps in the cbg
  cbgs_agg_10 = merge(cbgs_agg_10, lps_perBG, by="BG_ID_10",all=T)
  # make 0s for any CT ids that were not in the aggregation
  cbgs_agg_10[is.na(cbgs_agg_10$DEMO_count),c("DEMO_count","NEWCON_count","ADD_count","RENO_count",
                                              "DEMO_DV","NEWCON_DV","ADD_DV","RENO_DV","Public_DV")]=0
  
  # divide by number of LPs
  cbgs_agg_10[,c("DEMO_PP","NEWCON_PP","ADD_PP","RENO_PP",
                 "DEMO_DV_PP","NEWCON_DV_PP","ADD_DV_PP","RENO_DV_PP","Public_DV_PP")] =
    cbgs_agg_10[,c("DEMO_count","NEWCON_count","ADD_count","RENO_count",
                   "DEMO_DV","NEWCON_DV","ADD_DV","RENO_DV","Public_DV")] / cbgs_agg_10$numLandParcels
  
  
  # make the alteration to existing structure var
  cbgs_agg_10[,c("ALTSTRUCT_count","ALTSTRUCT_PP","ALTSTRUCT_DV_PP")] =
    cbgs_agg_10[,c("ADD_count","ADD_PP","ADD_DV_PP")] +
    cbgs_agg_10[,c("DEMO_count","DEMO_PP","DEMO_DV_PP")] +
    cbgs_agg_10[,c("RENO_count","RENO_PP","RENO_DV_PP")]
  
  
  cbgs_agg_10[,c("TOTAL_count","TOTAL_PP","TOTAL_DV_PP")] =
    cbgs_agg_10[,c("NEWCON_count","NEWCON_PP","NEWCON_DV_PP")] +
    cbgs_agg_10[,c("ALTSTRUCT_count","ALTSTRUCT_PP","ALTSTRUCT_DV_PP")]
  
  # reorder the variables and save the CSV
  cbgs_agg_10 = cbgs_agg_10[,c("BG_ID_10","numLandParcels",
                               "NEWCON_count", "NEWCON_PP","NEWCON_DV_PP", "DEMO_count", "DEMO_PP", "DEMO_DV_PP",
                               "ADD_count","ADD_PP","ADD_DV_PP","RENO_count","RENO_PP","RENO_DV_PP",
                               "ALTSTRUCT_count","ALTSTRUCT_PP","ALTSTRUCT_DV_PP",
                               "TOTAL_count","TOTAL_PP","TOTAL_DV_PP",
                               "Public_DV_PP")]
  names(cbgs_agg_10)
  
  write.csv(cbgs_agg_10,paste(cbgEcometricsPath,files_10, csv, cbgEcometricsName,"2010.",i,".csv", sep = ""),row.names=F)
  
  # rename the data frame so it persists
  assign(paste("cbgs_agg_10",i,sep="_"), cbgs_agg_10)
  cbgs_agg_long_10<-merge(cbgs_agg_long_10,cbgs_agg_10[,-2],by='BG_ID_10',all.x=TRUE)
  names(cbgs_agg_long_10)[(ncol(cbgs_agg_long_10)-18):ncol(cbgs_agg_long_10)]<-paste(names(cbgs_agg_long_10)[(ncol(cbgs_agg_long_10)-18):ncol(cbgs_agg_long_10)],i,sep=".")
  
  # change names for the shp
  names(cbgs_agg_10) = c("BG_ID_10","nLP","NC_c","NC_PP", "NC_DV", "D_c",  "D_PP" , "D_DV",
                         "A_c", "A_PP","A_DV","R_c", "R_PP","R_DV","AS_c", "AS_pp", "AS_DV","T_c","T_PP","T_DV",
                         "P_DV")
  
  
  cbgs_agg_10 <- subset(cbgs_agg_10, select = -c(nLP))
  
  bgs_shp_10 = merge(bgs_shp_10[1:23], cbgs_agg_10,by.x="GEOID10", by.y="BG_ID_10",all.x=T)
  names(bgs_shp_10)
  write_sf(bgs_shp_10, dsn = paste(cbgEcometricsPath, files_10, shp, sep = ""),layer = paste("Permits.Ecometrics.CBG2010",i,sep="."), driver="ESRI Shapefile", append=F)
  
  # rename for long shapefile
  names(cbgs_agg_10)[2:ncol(cbgs_agg_10)] = paste(names(cbgs_agg_10)[2:ncol(cbgs_agg_10)],gsub(pattern = "20","",as.character(i)),sep=".")
  # merge for long shapefile and dropping numLandParcels column
  
  cbgs_agg_long_shp_10 = merge(cbgs_agg_long_shp_10,cbgs_agg_10,by.x="GEOID10",by.y="BG_ID_10", all.x=T)
  
  #2020#
  print("CBG-2020")
  cbgs_agg_20 = aggregate(cbind(DEMO, NEWCON, ADD, RENO,
                                DEMO*DV, NEWCON*DV, ADD*DV, RENO*DV,DV_public)~BG_ID_20, data= types_byParcel_i, FUN=sum,na.rm=T)
  names(cbgs_agg_20)[2:ncol(cbgs_agg_20)] = c("DEMO_count","NEWCON_count","ADD_count","RENO_count",
                                              "DEMO_DV","NEWCON_DV","ADD_DV","RENO_DV","Public_DV")
  
  # merge on the number of lps in the cbg
  cbgs_agg_20 = merge(cbgs_agg_20, lps_perBG_20, by="BG_ID_20",all=T)
  # make 0s for any CT ids that were not in the aggregation
  cbgs_agg_20[is.na(cbgs_agg_20$DEMO_count),c("DEMO_count","NEWCON_count","ADD_count","RENO_count",
                                              "DEMO_DV","NEWCON_DV","ADD_DV","RENO_DV","Public_DV")]=0
  
  # divide by number of LPs
  cbgs_agg_20[,c("DEMO_PP","NEWCON_PP","ADD_PP","RENO_PP",
                 "DEMO_DV_PP","NEWCON_DV_PP","ADD_DV_PP","RENO_DV_PP","Public_DV_PP")] =
    cbgs_agg_20[,c("DEMO_count","NEWCON_count","ADD_count","RENO_count",
                   "DEMO_DV","NEWCON_DV","ADD_DV","RENO_DV","Public_DV")] / cbgs_agg_20$numLandParcels
  
  
  # make the alteration to existing structure var
  cbgs_agg_20[,c("ALTSTRUCT_count","ALTSTRUCT_PP","ALTSTRUCT_DV_PP")] =
    cbgs_agg_20[,c("ADD_count","ADD_PP","ADD_DV_PP")] +
    cbgs_agg_20[,c("DEMO_count","DEMO_PP","DEMO_DV_PP")] +
    cbgs_agg_20[,c("RENO_count","RENO_PP","RENO_DV_PP")]
  
  
  cbgs_agg_20[,c("TOTAL_count","TOTAL_PP","TOTAL_DV_PP")] =
    cbgs_agg_20[,c("NEWCON_count","NEWCON_PP","NEWCON_DV_PP")] +
    cbgs_agg_20[,c("ALTSTRUCT_count","ALTSTRUCT_PP","ALTSTRUCT_DV_PP")]
  
  # reorder the variables and save the CSV
  cbgs_agg_20 = cbgs_agg_20[,c("BG_ID_20","numLandParcels",
                               "NEWCON_count", "NEWCON_PP","NEWCON_DV_PP", "DEMO_count", "DEMO_PP", "DEMO_DV_PP",
                               "ADD_count","ADD_PP","ADD_DV_PP","RENO_count","RENO_PP","RENO_DV_PP",
                               "ALTSTRUCT_count","ALTSTRUCT_PP","ALTSTRUCT_DV_PP",
                               "TOTAL_count","TOTAL_PP","TOTAL_DV_PP",
                               "Public_DV_PP")]
  names(cbgs_agg_20)
  
  write.csv(cbgs_agg_20,paste(cbgEcometricsPath, files_20, csv, cbgEcometricsName,"2020.",i,".csv", sep = ""),row.names=F)
  
  # rename the data frame so it persists
  assign(paste("cbgs_agg_20",i,sep="_"), cbgs_agg_20)
  cbgs_agg_long_20<-merge(cbgs_agg_long_20,cbgs_agg_20[-c(2)],by='BG_ID_20',all.x=TRUE)
  names(cbgs_agg_long_20)[(ncol(cbgs_agg_long_20)-18):ncol(cbgs_agg_long_20)]<-paste(names(cbgs_agg_long_20)[(ncol(cbgs_agg_long_20)-18):ncol(cbgs_agg_long_20)],i,sep=".")
  
  
  # change names for the shp
  names(cbgs_agg_20) = c("BG_ID_20","nLP","NC_c","NC_PP", "NC_DV", "D_c",  "D_PP" , "D_DV",
                         "A_c", "A_PP","A_DV","R_c", "R_PP","R_DV","AS_c", "AS_pp", "AS_DV","T_c","T_PP","T_DV",
                         "P_DV")
  
  
  cbgs_agg_20 <- subset(cbgs_agg_20, select = -c(nLP))
  
  bgs_shp_20 = merge(bgs_shp_20[1:14], cbgs_agg_20,by.x="GEOID20", by.y="BG_ID_20",all.x=T)
  names(bgs_shp_20)
  write_sf(bgs_shp_20, dsn = paste(cbgEcometricsPath, files_20, shp, sep = ""),layer = paste("Permits.Ecometrics.CBG2020",i,sep="."), driver="ESRI Shapefile", append=F)
  
  # rename for long shapefile
  names(cbgs_agg_20)[2:ncol(cbgs_agg_20)] = paste(names(cbgs_agg_20)[2:ncol(cbgs_agg_20)],gsub(pattern = "20","",as.character(i)),sep=".")
  # merge for long shapefile and dropping numLandParcels column
  
  cbgs_agg_long_shp_20 = merge(cbgs_agg_long_shp_20,cbgs_agg_20,by.x="GEOID20",by.y="BG_ID_20", all.x=T)
  
  
  ### aggregating to CT level ####
  print("aggregating to CT level")
  
  #2010#
  cts_agg_10 = aggregate(cbind(DEMO, NEWCON, ADD, RENO,
                               DEMO*DV, NEWCON*DV, ADD*DV, RENO*DV,DV_public)~CT_ID_10, data= types_byParcel_i, FUN=sum,na.rm=T)
  names(cts_agg_10)[2:ncol(cts_agg_10)] = c("DEMO_count","NEWCON_count","ADD_count","RENO_count",
                                            "DEMO_DV","NEWCON_DV","ADD_DV","RENO_DV","Public_DV")
  
  # merge on the number of lps in the ct
  cts_agg_10 = merge(cts_agg_10, lps_perCT, by="CT_ID_10",all=T)
  # make 0s for any CT ids that were not in the aggregation
  cts_agg_10[is.na(cts_agg_10$DEMO_count),c("DEMO_count","NEWCON_count","ADD_count","RENO_count",
                                            "DEMO_DV","NEWCON_DV","ADD_DV","RENO_DV","Public_DV")]=0
  
  # divide by number of LPs
  cts_agg_10[,c("DEMO_PP","NEWCON_PP","ADD_PP","RENO_PP",
                "DEMO_DV_PP","NEWCON_DV_PP","ADD_DV_PP","RENO_DV_PP","Public_DV_PP")] =
    cts_agg_10[,c("DEMO_count","NEWCON_count","ADD_count","RENO_count",
                  "DEMO_DV","NEWCON_DV","ADD_DV","RENO_DV","Public_DV")] / cts_agg_10$numLandParcels
  
  
  # make the alteration to existing structure var
  cts_agg_10[,c("ALTSTRUCT_count","ALTSTRUCT_PP","ALTSTRUCT_DV_PP")] =
    cts_agg_10[,c("ADD_count","ADD_PP","ADD_DV_PP")] +
    cts_agg_10[,c("DEMO_count","DEMO_PP","DEMO_DV_PP")] +
    cts_agg_10[,c("RENO_count","RENO_PP","RENO_DV_PP")]
  
  
  cts_agg_10[,c("TOTAL_count","TOTAL_PP","TOTAL_DV_PP")] =
    cts_agg_10[,c("NEWCON_count","NEWCON_PP","NEWCON_DV_PP")] +
    cts_agg_10[,c("ALTSTRUCT_count","ALTSTRUCT_PP","ALTSTRUCT_DV_PP")]
  
  # reorder the variables and save the CSV
  cts_agg_10 = cts_agg_10[,c("CT_ID_10","numLandParcels",
                             "NEWCON_count", "NEWCON_PP","NEWCON_DV_PP", "DEMO_count", "DEMO_PP", "DEMO_DV_PP",
                             "ADD_count","ADD_PP","ADD_DV_PP","RENO_count","RENO_PP","RENO_DV_PP",
                             "ALTSTRUCT_count","ALTSTRUCT_PP","ALTSTRUCT_DV_PP",
                             "TOTAL_count","TOTAL_PP","TOTAL_DV_PP",
                             "Public_DV_PP")]
  names(cts_agg_10)
  
  write.csv(cts_agg_10,paste(ctEcometricsPath, files_10, csv, ctEcometricsName,"2010.",i,".csv", sep = ""),row.names=F)
  
  # rename the data frame so it persists
  assign(paste("cts_agg_10",i,sep="_"), cts_agg_10)
  cts_agg_long_10<-merge(cts_agg_long_10,cts_agg_10[-c(2)],by='CT_ID_10',all.x=TRUE)
  names(cts_agg_long_10)[(ncol(cts_agg_long_10)-18):ncol(cts_agg_long_10)]<-paste(names(cts_agg_long_10)[(ncol(cts_agg_long_10)-18):ncol(cts_agg_long_10)],i,sep=".")
  
  
  # change names for the shp
  names(cts_agg_10) = c("CT_ID_10","nLP","NC_c","NC_PP", "NC_DV", "D_c",  "D_PP" , "D_DV",
                        "A_c", "A_PP","A_DV","R_c", "R_PP","R_DV","AS_c", "AS_pp", "AS_DV","T_c","T_PP","T_DV",
                        "P_DV")
  
  
  cts_agg_10 <- subset(cts_agg_10, select = -c(nLP))
  
  cts_shp_10 = merge(cts_shp_10[1:16], cts_agg_10,by.x="GEOID10", by.y="CT_ID_10",all.x=T)
  names(cts_shp_10)
  write_sf(cts_shp_10, dsn = paste(ctEcometricsPath, files_10, shp, sep = ""),layer = paste("Permits.Ecometrics.CT2010",i,sep="."), driver="ESRI Shapefile", append=F)
  
  # rename for long shapefile
  names(cts_agg_10)[2:ncol(cts_agg_10)] = paste(names(cts_agg_10)[2:ncol(cts_agg_10)],gsub(pattern = "20","",as.character(i)),sep=".")
  # merge for long shapefile and dropping numLandParcels column
  
  cts_agg_long_shp_10 = merge(cts_agg_long_shp_10,cts_agg_10,by.x="GEOID10",by.y="CT_ID_10", all.x=T)
  
  #2020#
  print("CT-2020")
  cts_agg_20 = aggregate(cbind(DEMO, NEWCON, ADD, RENO,
                               DEMO*DV, NEWCON*DV, ADD*DV, RENO*DV,DV_public)~CT_ID_20, data= types_byParcel_i, FUN=sum,na.rm=T)
  names(cts_agg_20)[2:ncol(cts_agg_20)] = c("DEMO_count","NEWCON_count","ADD_count","RENO_count",
                                            "DEMO_DV","NEWCON_DV","ADD_DV","RENO_DV","Public_DV")
  
  # merge on the number of lps in the ct
  cts_agg_20 = merge(cts_agg_20, lps_perCT_20, by="CT_ID_20",all=T)
  # make 0s for any CT ids that were not in the aggregation
  cts_agg_20[is.na(cts_agg_20$DEMO_count),c("DEMO_count","NEWCON_count","ADD_count","RENO_count",
                                            "DEMO_DV","NEWCON_DV","ADD_DV","RENO_DV","Public_DV")]=0
  
  # divide by number of LPs
  cts_agg_20[,c("DEMO_PP","NEWCON_PP","ADD_PP","RENO_PP",
                "DEMO_DV_PP","NEWCON_DV_PP","ADD_DV_PP","RENO_DV_PP","Public_DV_PP")] =
    cts_agg_20[,c("DEMO_count","NEWCON_count","ADD_count","RENO_count",
                  "DEMO_DV","NEWCON_DV","ADD_DV","RENO_DV","Public_DV")] / cts_agg_20$numLandParcels
  
  
  # make the alteration to existing structure var
  cts_agg_20[,c("ALTSTRUCT_count","ALTSTRUCT_PP","ALTSTRUCT_DV_PP")] =
    cts_agg_20[,c("ADD_count","ADD_PP","ADD_DV_PP")] +
    cts_agg_20[,c("DEMO_count","DEMO_PP","DEMO_DV_PP")] +
    cts_agg_20[,c("RENO_count","RENO_PP","RENO_DV_PP")]
  
  
  cts_agg_20[,c("TOTAL_count","TOTAL_PP","TOTAL_DV_PP")] =
    cts_agg_20[,c("NEWCON_count","NEWCON_PP","NEWCON_DV_PP")] +
    cts_agg_20[,c("ALTSTRUCT_count","ALTSTRUCT_PP","ALTSTRUCT_DV_PP")]
  
  # reorder the variables and save the CSV
  cts_agg_20 = cts_agg_20[,c("CT_ID_20","numLandParcels",
                             "NEWCON_count", "NEWCON_PP","NEWCON_DV_PP", "DEMO_count", "DEMO_PP", "DEMO_DV_PP",
                             "ADD_count","ADD_PP","ADD_DV_PP","RENO_count","RENO_PP","RENO_DV_PP",
                             "ALTSTRUCT_count","ALTSTRUCT_PP","ALTSTRUCT_DV_PP",
                             "TOTAL_count","TOTAL_PP","TOTAL_DV_PP",
                             "Public_DV_PP")]
  names(cts_agg_20)
  
  write.csv(cts_agg_20,paste(ctEcometricsPath, files_20, csv, ctEcometricsName,"2020.",i,".csv", sep = ""),row.names=F)
  
  # rename the data frame so it persists
  assign(paste("cts_agg_20",i,sep="_"), cts_agg_20)
  cts_agg_long_20<-merge(cts_agg_long_20,cts_agg_20[-c(2)],by='CT_ID_20',all.x=TRUE)
  names(cts_agg_long_20)[(ncol(cts_agg_long_20)-18):ncol(cts_agg_long_20)]<-paste(names(cts_agg_long_20)[(ncol(cts_agg_long_20)-18):ncol(cts_agg_long_20)],i,sep=".")
  
  
  
  # change names for the shp
  names(cts_agg_20) = c("CT_ID_20","nLP","NC_c","NC_PP", "NC_DV", "D_c",  "D_PP" , "D_DV",
                        "A_c", "A_PP","A_DV","R_c", "R_PP","R_DV","AS_c", "AS_pp", "AS_DV","T_c","T_PP","T_DV",
                        "P_DV")
  
  
  cts_agg_20 <- subset(cts_agg_20, select = -c(nLP))
  
  cts_shp_20 = merge(cts_shp_20[1:14], cts_agg_20,by.x="GEOID20", by.y="CT_ID_20",all.x=T)
  names(cts_shp_20)
  write_sf(cts_shp_20, dsn = paste(ctEcometricsPath, files_20, shp, sep = ""),layer = paste("Permits.Ecometrics.CT2020",i,sep="."), driver="ESRI Shapefile", append=F)
  
  # rename for long shapefile
  names(cts_agg_20)[2:ncol(cts_agg_20)] = paste(names(cts_agg_20)[2:ncol(cts_agg_20)],gsub(pattern = "20","",as.character(i)),sep=".")
  # merge for long shapefile and dropping numLandParcels column
  
  cts_agg_long_shp_20 = merge(cts_agg_long_shp_20,cts_agg_20,by.x="GEOID20",by.y="CT_ID_20", all.x=T)
  
}

############### SAVE Longtiduinal Files ########

#write longitudinal LPs
write.csv(lps_agg_long,paste(lpEcometricsPath, long, lpEcometricsName,".Longitudinal.csv", sep = ""),row.names=F)

# write longitudinal CBG 

names(cbgs_agg_long_10)[2] <- "numLandParcels"
names(cbgs_agg_long_20)[2] <- "numLandParcels"

write.csv(cbgs_agg_long_10,paste(cbgEcometricsPath, files_10, long, cbgEcometricsName,"10.Longitudinal.csv", sep = ""),row.names=F)
write.csv(cbgs_agg_long_20,paste(cbgEcometricsPath, files_20, long, cbgEcometricsName,"20.Longitudinal.csv", sep = ""),row.names=F)
write_sf(cbgs_agg_long_shp_10, dsn = paste0(cbgEcometricsPath, files_10, long, "Permits.Ecometrics.CBG2010.Longitudinal.gpkg"),layer = "Permits.Ecometrics.CBG2010.Longitudinal", driver="GPKG", append=F)
write_sf(cbgs_agg_long_shp_20, dsn = paste0(cbgEcometricsPath, files_20, long, "Permits.Ecometrics.CBG2020.Longitudinal.gpkg"),layer = "Permits.Ecometrics.CBG2020.Longitudinal", driver="GPKG", append=F)


names(cts_agg_long_10)[2] <- "numLandParcels"
names(cts_agg_long_20)[2] <- "numLandParcels"

# write longitudinal CT 
write.csv(cts_agg_long_10,paste(ctEcometricsPath, files_10, long, ctEcometricsName,"10.Longitudinal.csv", sep = ""),row.names=F)
write.csv(cts_agg_long_20,paste(ctEcometricsPath, files_20, long, ctEcometricsName,"20.Longitudinal.csv", sep = ""),row.names=F)
write_sf(cts_agg_long_shp_10, dsn = paste0(ctEcometricsPath, files_10, long, "Permits.Ecometrics.CT2010.Longitudinal.gpkg"),layer = "Permits.Ecometrics.CT2010.Longitudinal", driver="GPKG", append=F)
write_sf(cts_agg_long_shp_20, dsn = paste0(ctEcometricsPath, files_20, long, "Permits.Ecometrics.CT2020.Longitudinal.gpkg"),layer = "Permits.Ecometrics.CT2020.Longitudinal", driver="GPKG", append=F)

