
###############################################################################

### Building clusters from LU for streets 2018

### This script is using kmeans clustering for LU per streets from parcel data.
### We are only using the k centroids in order to add clusters for the data labeled 0 initially
### This is using the result of ParcelsToStreets script

###############################################################################

## read library ##
library(plyr)
library(sp)
library(sf)
library(raster)
library(rgdal)
library(rgeos)
library(dplyr)
library(dbplyr)
library(stringr)
library(tidyverse)
library(lwgeom)
library(units)
library(reshape)
library(reshape2)

options(scipen=999)

###############################################################################
## read data ##
###############################################################################

# read parcels resulted from 03 scripts
#parcel_final_2018_csv<-read.csv("D:\\GoogleDrive\\BARI Research Team Data Library\\Geographical Infrastructure\\Boston Geographical Infrastructure 2018\\Outputs\\Parcel_final_2018_10292019.csv")
#parcel_final_2019_csv<-read.csv("C:\\Users\\alina.ristea\\Google Drive\\BARI Research Team Data Library\\Geographical Infrastructure\\Boston Geographical Infrastructure 2018\\Outputs\\Parcel_final_2018_10292019.csv")
parcel_final_2023_csv<-read.csv("~/Desktop/BARI/GI-2023/drive/Parcel_final_10032023_YET.csv")

#parcel_final_2018_shp <- st_read("D:\\GoogleDrive\\BARI Research Team Data Library\\Geographical Infrastructure\\Boston Geographical Infrastructure 2018\\Outputs\\BostonParcels2018_10252019\\Parcels2018_10252019.shp")
#parcel_final_2021_shp <- st_read("../Outputs/BARIParcels2021.shp")

# join the info from the csv in the shp
#parcel_final_2021_shp$Land_Parcel_ID <- parcel_final_2021_shp$Ln_P_ID
#parcels <- left_join(parcel_final_2021_shp, parcel_final_2023_csv, by = "Land_Parcel_ID")
#coordinates(parcels) <- ~X + Y
#proj4string(parcels) = CRS("+init=epsg:4326")
parcels <- st_as_sf(parcels, coords = c("X", "Y"), crs = 4326)
#parcels <- st_transform(parcels, 26986)

#### read only shapefile from parcel when we will have the final version
#parcels <- st_read("C:\\Users\\alina.ristea\\Google Drive\\Work\\NEU_BARI\\Projects\\Geographical_Infrastructure\\Data\\Outputs\\Parcel_final_2018_shp_shorter.shp")
#parcels2018_full <- st_read("C:\\Users\\alina.ristea\\Google Drive\\BARI Research Team Data Library\\Geographical Infrastructure\\Boston Geographical Infrastructure 2019\\BostonParcels2019\\Parcels2019_full_attrib.shp")

# read street final data (result of ParcelToStreets script)
#streets_final <- st_read("D:\\GoogleDrive\\Work\\NEU_BARI\\Projects\\Geographical_Infrastructure\\Data\\Outputs\\roads_fin_2018_10202019.shp")
streets_final <- st_read("~/Desktop/BARI/GI-2023/drive/ParcelToStreets/roads_fin_clust_YET_10162023.shp")

###############################################################################
## transformations ##
###############################################################################

# create a pivot table with counts of LU class per each parcel tlid (e.g. tlid 001 has 2 R1 land use and 3 A land use) 
#parcels_LU_count <- dplyr::summarise(group_by(parcels,TLID,LU),count =n()) # did not work in 2023
parcels_LU_count <- dplyr::summarise(group_by(parcels, TLID, LU), 
                                     count = n(), 
                                     .groups = "drop")
length(unique(parcels_LU_count$TLID)) #13836 because only 13836 street segments have associated min one parcel #13890 new #13885 in 2019 #13827 in 2020 # 13817 in 2021 # 13866 in 2023

# count of LU type per TLID
parcels_LU_count_1 <- dcast(parcels_LU_count, TLID ~ LU, value.var = "count")
# remove the last row which inludes NA tlid's
#parcels_LU_count_1 <- parcels_LU_count_1[-c(13885),] 
#parcels_LU_count_1 <- parcels_LU_count_1[-c(13833),] 
## mz
parcels_LU_count_1 = parcels_LU_count_1[!is.na(parcels_LU_count_1$TLID),]

###############################################################################
## standardization ##
###############################################################################

#delete unneccesary columns (such as NA column)
parcels_LU_count_1[,19:19] <- NULL

# add 0's instead of NA's
parcels_LU_count_1[is.na(parcels_LU_count_1)] <- 0
# remove the TLID column: it will not be used in the clustering;
#parcels_LU_count_2 <- parcels_LU_count_2[,1:17]

# sum number of parcels per TLID from all the land use types
parcels_LU_count_1 <-   parcels_LU_count_1 %>% 
  mutate(count_tot = dplyr::select(., 2:18) %>% rowSums(na.rm = TRUE)) # if we have error, then add dplyr::select

# divide each land use count to the sum of parcels (with all land uses) per TLID
parcels_LU_count_2 <- parcels_LU_count_1[, 2:18]/parcels_LU_count_1[, 19]
parcels_LU_count_2 <- cbind(parcels_LU_count_2, parcels_LU_count_1$TLID)
colnames(parcels_LU_count_2)[18] <- "TLID"

# df including land use std, tlid, cluster old
parcels_LU_count_3 <- left_join(parcels_LU_count_2, streets_final[c("Cluster", "TLID")], by = "TLID") #13889obs #13826 in 2020
parcels_LU_count_3$geometry <- NULL


###############################################################################
## compare k means centroids with each street ##
###############################################################################
streets = parcels_LU_count_3[,1:17] #all the 17 land use types
clust = parcels_LU_count_3[,19] #old clusters

LabelingZeros <- function(streets,clust){
  centr1 <- matrix(0, nrow=7, ncol=17)
  centr <- as.data.frame(centr1)
  conta <- matrix(0, nrow=7, ncol=1)
  for (i in 1:dim(streets)[1]){
    if (clust[i] != 0){
      centr[clust[i],]<-centr[clust[i],]+streets[i,]
      conta[clust[i]]<-conta[clust[i]]+1
    }
  }
  for (j in 1:dim(conta)[1]){
    centr[j,]<-centr[j,]/conta[j]
  }
  
  clustnew <- matrix(0, nrow=dim(streets)[1], ncol=1)
  dist <- matrix(0, nrow=7, ncol=1)
  for (i in 1:dim(streets)[1]){
    #if (clust[i] != 0){
    #  clustnew[i]=clust[i]
    #} else {
    for (j in 1:7){
      dist[j]=sqrt(sum((centr[j,]-streets[i,])^2))
    }
    I=which.min(dist)
    clustnew[i]=I
  } 
  #}
  return(clustnew)
}


clusternew <- LabelingZeros(streets,clust)

###############################################################################
## add the new cluster values where we had 0 before ##
###############################################################################
parcels_LU_count_4 <- cbind(parcels_LU_count_3, clusternew)

# join the TLID with the cluster numbers 
a <- parcels_LU_count_4 %>%
  select("TLID","clusternew")

# join the clusters with the streets
streets_clust <- left_join(streets_final, a, by = "TLID")

sum(is.na(streets_clust$cluster)) #11002 #11007 in 2019 #11059 in 2020 # 11075 in 2021 #11026 in 2023
sum(is.na(streets_clust$clusternew))
#11056 street segments are not associated to parcels, so they do not have LU #11026 in 2023
sum(!is.na(streets_clust$clusternew)) #13889 #13884 in 2019 #13832 in 2020 # 13816 in 2021 # 13865 in 2023
sum(!is.na(streets_clust$cluster))
#13835 street segments associated to parcels #13826 in 2020 #13816 in 2021 #13865 in 2023

# add 0's instead of NA's for the streets without clusters
streets_clust[is.na(streets_clust)] <- 0
sum(streets_clust$clusternew == 0) # 11056 #11002 new #11007 in 2019 #11059 in 2020 # 11075 in 2021 # 11026 in 2023

# change names
streets_clust$Cluster <- streets_clust$clusternew
streets_clust$clusternew <- NULL

# make sure column names are correct and the order is the same as in the documentation
colnames(streets_clust)

# rename because some names were reduced for the shapefile
#streets_clust <- streets_clust %>%
#  dplyr::rename(#"COUNTYFP" = "COUNTYF", 
#"LFROMADD" = "LFROMAD",
#"RFROMADD" = "RFROMAD",
#"BG_ID_10" = "BG_ID_1",
#"CT_ID_10" = "CT_ID_1",
#"unit_N_orig" = "unt_N_r",
#"property_N" = "prprt_N",
#"parcel_N" = "parcl_N")

# rearrange col order
#streets_clust$BG_ID_10_ = NULL

streets_clust <- streets_clust[, c("TLID", 
                                   "STATEFP",     
                                   "COUNTYFP",    
                                   "TFIDL",
                                   "TFIDR",       
                                   "MTFCC",       
                                   "FULLNAM",
                                   "SMID",  
                                   "LFROMADD",
                                   "LTOADD",
                                   "RFROMADD",
                                   "RTOADD",
                                   "ZIPL",   
                                   "ZIPR",
                                   "Length",
                                   "CLASS", 
                                   "RDTYPE",
                                   "Main",
                                   "DeadEnd",
                                   "Cluster",
                                   "unit_N",
                                   "unit_N_orig",
                                   "property_N",
                                   "parcel_N",
                                   "BG_ID_10",
                                   "CT_ID_10",
                                   "geometry")]


# save file as shapefile and csv
streets_clust2 <- streets_clust
streets_clust2$geometry <- NULL
write.csv(streets_clust2,"roads_fin_clust_2023_kmeans.csv")
st_write(streets_clust,"roads_fin_clust_2023_kmeans.shp")


# Calculate percentages for the documentation 
# first add the parcel data
parcels_2021 <- read.csv("../Outputs/Parcel_final_2021_08172021.csv") #98445

streets_Cluster1 <- streets_clust[streets_clust$Cluster == 1,]
parcels_Cluster1 <- parcels_2021[parcels_2021$TLID %in% streets_Cluster1$TLID,]
nrow(parcels_Cluster1) #17441 in 2021
table(parcels_Cluster1$LU)

streets_Cluster2 <- streets_clust[streets_clust$Cluster == 2,]
parcels_Cluster2 <- parcels_2021[parcels_2021$TLID %in% streets_Cluster2$TLID,]
nrow(parcels_Cluster2) 
table(parcels_Cluster2$LU)

streets_Cluster3 <- streets_clust[streets_clust$Cluster == 3,]
parcels_Cluster3 <- parcels_2021[parcels_2021$TLID %in% streets_Cluster3$TLID,]
nrow(parcels_Cluster3)
table(parcels_Cluster3$LU)

streets_Cluster4 <- streets_clust[streets_clust$Cluster == 4,]
parcels_Cluster4 <- parcels_2021[parcels_2021$TLID %in% streets_Cluster4$TLID,]
nrow(parcels_Cluster4) 
table(parcels_Cluster4$LU)

streets_Cluster5 <- streets_clust[streets_clust$Cluster == 5,]
parcels_Cluster5 <- parcels_2021[parcels_2021$TLID %in% streets_Cluster5$TLID,]
nrow(parcels_Cluster5) 
table(parcels_Cluster5$LU)

streets_Cluster6 <- streets_clust[streets_clust$Cluster == 6,]
parcels_Cluster6 <- parcels_2021[parcels_2021$TLID %in% streets_Cluster6$TLID,]
nrow(parcels_Cluster6) 
table(parcels_Cluster6$LU)

streets_Cluster7 <- streets_clust[streets_clust$Cluster == 7,]
parcels_Cluster7 <- parcels_2021[parcels_2021$TLID %in% streets_Cluster7$TLID,]
nrow(parcels_Cluster7) 
table(parcels_Cluster7$LU)

# Percentages general
nrow(streets_Cluster1)*100/nrow(streets_clust[streets_clust$Cluster > 0,])
nrow(streets_Cluster2)*100/nrow(streets_clust[streets_clust$Cluster > 0,])
nrow(streets_Cluster3)*100/nrow(streets_clust[streets_clust$Cluster > 0,])
nrow(streets_Cluster4)*100/nrow(streets_clust[streets_clust$Cluster > 0,])
nrow(streets_Cluster5)*100/nrow(streets_clust[streets_clust$Cluster > 0,])
nrow(streets_Cluster6)*100/nrow(streets_clust[streets_clust$Cluster > 0,])
nrow(streets_Cluster7)*100/nrow(streets_clust[streets_clust$Cluster > 0,])