if (!requireNamespace("wk", quietly = TRUE))
  install.packages("wk")
# libraries
library(tidyverse)
library(glue)
library(sf)
library(wk)
options(scipen=100000000)

# set path
BARI <- "D:/Google Drive/BARI Research Team Data Library/"

# # loading in functions
# source(paste0(BARI,"/HG-Functions/Cleaning_functions_20190326.R"))
# source(paste0(BARI,"/HG-Functions/Geocoding_functions_20190326.R"))
# source(paste0(BARI,"/HG-Functions/Helper_functions_20190326.R"))
source("~/Desktop/BARI/PA-2023/scripts /functions/Cleaning_functions_20190326.R")
source("~/Desktop/BARI/PA-2023/scripts /functions/Geocoding_functions_20190326.R")
source("~/Desktop/BARI/PA-2023/scripts /functions/Helper_functions_20190326.R")


#---- INPUT PATHS ----
# permits_path = "Permits/Outputs/6ddcd912-32a0-43df-9908-63574f8c7e77.csv" # downloaded from Boston City data portal: https://data.boston.gov/dataset/approved-building-permits
# ID_walkover_path = "Geographical Infrastructure/Boston Geographical Infrastructure 2018/Data/IDConnector2020.csv" # from BARI dataverse
# properties_path = "Geographical Infrastructure/Boston Geographical Infrastructure 2020/Outputs/properties_geo_2020_08182020.csv" # from BARI dataverse
# 
# landParcels_path = "Geographical Infrastructure/Boston Geographical Infrastructure 2020/Outputs/Parcel_final_2020_08182020.csv" # from BARI dataverse
# landParcelsShpPath = "Geographical Infrastructure/Boston Geographical Infrastructure 2020/Outputs/BostonParcels2020_06262020"
# landParcelsShpName = "Parcels2020_06262020"
# 
# roadsCSVPath  = "Geographical Infrastructure/Boston Geographical Infrastructure 2020/Outputs/Streets/Drafts/roads2020_cleanaddress20200617.csv"
# roadsShpPath = "Geographical Infrastructure/Boston Geographical Infrastructure 2018/Data/BostonRoads2018"
# roadsShpName = "Roads2018"
# 
# blkShpPath = "Geographical Infrastructure/Boston Geographical Infrastructure 2019/Data/BostonBlocks2010"
# blkShpName = "BlocksBostonBARI"
# 
# bgShpPath = "Geographical Infrastructure/Boston Geographical Infrastructure 2019/Data/BostonBlockGroups2010"
# bgShpName = "BostonBlockGroups2010"
# 
# ctShpPath = "Geographical Infrastructure/Boston Geographical Infrastructure 2019/Data/BostonTracts2010"
# ctShpName = "BostonTracts2010"

#---- OUTPUT PATHS ----
permits2024_path <- "~/Desktop/BARI/Permits/2024/dataverse2/Permits.Records.Geocoded.06182024.csv"

#---- READ IN FILES ----
# permits <- read.csv(paste0(BARI,permits_path)) # already has "lat" "long" 
# IDconnector <- read.csv(paste0(BARI,ID_walkover_path))
# properties <- read.csv(paste0(BARI,properties_path))
# landParcels <- read.csv(paste0(BARI,landParcels_path))

#### ALTERNATIVE LOADING ####
permits_2024 <- read.csv("Permits/2024/inputs/tmphixlgnsx.csv")
permits_2023 <- read.csv("~/Desktop/BARI/Permits/2024/inputs/Permits Records 2023.csv")
#permits_processed <- st_read("/Users/yet/Desktop/BARI/Permits/2024/inputs/permits_processed.shp")
#permits_draft <- read.csv("~/Desktop/BARI/Permits/Geocoded Permits Apr 2024.csv")
properties <- read.csv("~/Desktop/BARI/dataverse_GI/Properties_2023.csv")
landParcels <- read.csv("~/Desktop/BARI/dataverse_GI/LandParcels_2023.csv")

######################################################################
#Filter the dataset until 2024
#Ensure the issued_date is in POSIXct format
#permits_2024$issued_date <- ymd_hms(permits_2024$issued_date)
#Filter the dataset to include only dates until the end of 2024
#permits_2024_filtered <- permits_2024 %>% 
#  filter(issued_date <= ymd_hms("2023-12-31 23:59:59+00"))
permits <- permits_2024 %>% filter(year(ymd_hms(issued_date)) != 2024)

#------------------------------- ADDING GEOGRAPHICAL DATA BASED ON ID ----------------------------#

## Old method via Henry geocoder:
# first must clean
permits = standardizeGeoNames(permits)

temp = clean_address(permits$address)
permits$num1 = temp[,2]
permits$num2 = temp[,3]
permits$street_c = temp[,4]
permits$suffix_c = temp[,5]
rm(temp)
permits$city_c <- clean_city(permits$city)
permits$zip_c <- clean_zip(permits$zip)

Permits_orig <- permits #28, 550065 #619953 obs. of  28 variables in 2024 with NA # 605445 obs. of  31 variables without NA

########## taking care of geographical identifiers ##############
#rename lat long variables
# permits <- permits %>% rename("lat" = "gpsy",
#                               "long" = "gpsx")
#permits$long <- -permits$long
# how many NA values do we have for lat/lon
sum(is.na(permits$long)) #60711 #14305 #14373 in 2024
sum(!is.na(permits$long))/nrow(permits) # 0.89 has lat/lon info #0.974691 #0.9765179 in 2024

# no geo-coding for now
# next, match to parcels shape files to get parcel_id and other geographic info

# Filter out empty strings and potentially invalid WKB strings
# Preserve indices to facilitate matching back to the original dataframe
valid_wkb_indices <- which(permits$geom_point_4326 != "")
permits_sf <- permits[valid_wkb_indices,]
valid_wkb <- permits$geom_point_4326[valid_wkb_indices]
permits_sf$wkb_raw <- NA

# Convert Hexadecimal WKB to Raw
permits_sf$wkb_raw <- lapply(valid_wkb, function(wkb) {
  as.raw(strtoi(paste0("0x", substring(wkb, seq(1, nchar(wkb) - 1, 2), seq(2, nchar(wkb), 2)))))
})

# Convert raw binary data to 'sfc' object
#geom <- st_as_sfc(wkb_raw, crs = 4326, EWKB = TRUE)
permits_sf$geometry <- st_as_sfc(permits_sf$wkb_raw, EWKB = TRUE)

# Combine the geometry with the subset data frame to create an sf data frame
permits_sf <- st_sf(permits_sf)
permits_sf <- st_transform(permits_sf, 4326)
permits_sf <- permits_sf %>% 
                  select(-c(wkb_raw, geom_point_2249, geom_point_4326, num1, num2, street_c, suffix_c, city_c, zip_c))
permits_sf <- permits_sf %>% rename( "gpsy" = "lat",
                               "gpsx" = "long")

permits_sf <- permits_sf %>% 
                    mutate(long = st_coordinates(geometry)[, 'X'], lat = st_coordinates(geometry)[, 'Y'])
permits_sf <- permits_sf %>% 
  select(-c(gpsy, gpsx))

#############################################


properties_merge <- properties[,c("PID", "GIS_ID", "Land_Parcel_ID", "TLID", 
                                  "Blk_ID_10", "BG_ID_10", "CT_ID_10",  "Blk_ID_20", "BG_ID_20", "CT_ID_20",
                                  "NSA_Name", "BRA_PD", "X", "Y")]
properties_merge <- properties_merge  %>% rename( "parcel_num" = "PID")

# Convert properties_merge to data.table if it's not already
setDT(properties_merge)

# Remove duplicates by keeping the first occurrence
properties_merge <- properties_merge[!duplicated(parcel_num)]


permits_sf_2 <- merge(permits_sf, properties_merge[!duplicated(parcel_num)], by = "parcel_num", all.x = TRUE)


permits_misgeo <- permits[which(permits$geom_point_4326 == ""),]
permits_misgeo <- permits_misgeo %>% 
  select(-c(geom_point_2249, geom_point_4326, num1, num2, street_c, suffix_c, city_c, zip_c))

permits_misgeo_2 <- merge(permits_misgeo, properties_merge, by = "parcel_num", all.x = TRUE)




# read in parcels shape files
#parcels <- st_read("~/Desktop/BARI/Permits/2024/inputs/LandParcels.2023/LandParcels.2023.shp") #98857
#parcels <- st_read("~/Downloads/LandParcels_2023/LandParcels_2023.shp")
#parcels <- st_read("~/Desktop/BARI/Permits/2024/inputs/LandParcels_2023/LandParcels_2023.shp")
#parcels <- landParcels
#parcels <- st_transform(parcels, 4326) 
#parcels <- st_transform(parcels, 4326) # for spatial operations
#parcels <- st_transform(parcels, st_crs(permits_sf))
#parcels <- st_set_crs(parcels, st_crs(permits_sf))
# overlap permits needing geo info on parcel shapes:
# permits2 <- st_join(permits_sf,parcels,join=st_nearest_feature)
# permits2 <- st_transform(permits2, 4326) # back to long/lat

permits3 <- permits_sf_2 %>%
  as.data.frame() %>%
  dplyr::select(-geometry) %>%
  cbind(st_coordinates(permits_sf_2))
# # Assuming you want to add the Ln_P_ID from permits3 to permits based on matching permitnumber
# # First, ensure that permitnumber is of the same type in both data frames for accurate matching
# permits_sf$permitnumber <- as.character(permits_sf$permitnumber)
# permits3$permitnumber <- as.character(permits3$permitnumber)
# 
# permits <- permits_sf
# ## add back to main permits dataset with all observations:
# #permits$GIS_ID <- permits3$GIS_ID[match(permits$permitnumber,permits3$permitnumber)]
# permits$Land_Parcel_ID <- permits3$Ln_P_ID[match(permits$permitnumber,permits3$permitnumber)]
# 
# # Check if the operation has succeeded without introducing NAs due to mismatches
# summary(permits$Land_Parcel_ID)


# bring in parcel csv to add census identifiers:
# landParcels$Land_Parcel_ID <- as.numeric(landParcels$Land_Parcel_ID) #98445 #97894 in 2024
# 
# landParcels$CT_ID_10 <- format(landParcels$CT_ID_10, scientific = F)
# landParcels$Blk_ID_10 <- format(landParcels$Blk_ID_10, scientific = F)
# landParcels$BG_ID_10 <- format(landParcels$BG_ID_10, scientific = F)
# landParcels$CT_ID_20 <- format(landParcels$CT_ID_20, scientific = F)
# landParcels$Blk_ID_20 <- format(landParcels$Blk_ID_20, scientific = F)
# landParcels$BG_ID_20 <- format(landParcels$BG_ID_20, scientific = F)
# 
# 
# permits$TLID <- landParcels$TLID[match(permits$Land_Parcel_ID,landParcels$Land_Parcel_ID)]
# permits$Blk_ID_10 <- landParcels$Blk_ID_10[match(permits$Land_Parcel_ID,landParcels$Land_Parcel_ID)]
# permits$BG_ID_10 <- landParcels$BG_ID_10[match(permits$Land_Parcel_ID,landParcels$Land_Parcel_ID)]
# permits$CT_ID_10 <- landParcels$CT_ID_10[match(permits$Land_Parcel_ID,landParcels$Land_Parcel_ID)]
# permits$Blk_ID_20 <- landParcels$Blk_ID_20[match(permits$Land_Parcel_ID,landParcels$Land_Parcel_ID)]
# permits$BG_ID_20 <- landParcels$BG_ID_20[match(permits$Land_Parcel_ID,landParcels$Land_Parcel_ID)]
# permits$CT_ID_20 <- landParcels$CT_ID_20[match(permits$Land_Parcel_ID,landParcels$Land_Parcel_ID)]
# permits$NSA_Name <- landParcels$NSA_Name[match(permits$Land_Parcel_ID,landParcels$Land_Parcel_ID)]
# permits$BRA_PD <- landParcels$BRA_PD[match(permits$Land_Parcel_ID,landParcels$Land_Parcel_ID)]
# 
# 
# permits$GIS_ID <- properties$GIS_ID[match(permits$Land_Parcel_ID,properties$Land_Parcel_ID)]
  
sum(is.na(permits3$long)) # 60711 #0
sum(!is.na(permits3$long))/nrow(permits3) # 88.9% #1
sum(is.na(permits3$Land_Parcel_ID)) # 62310 #32768
sum(is.na(permits3$BG_ID_10)) # 62311 #32768
sum(!is.na(permits3$BG_ID_10))/nrow(permits3)# 88.6%  #0.9451774
sum(is.na(permits3$CT_ID_10)) # 62311 #32768
sum(!is.na(permits3$CT_ID_10))/nrow(permits3)# 88.6% #0.9451774
sum(!is.na(permits3$GIS_ID))/nrow(permits3) #0.9451774

#### merge missing permits from 2023
# - permits[!valid_wkb_indices,]
# > nrow(permits[which(permits$geom_point_4326 == ""),])
# [1] 14373
# is.na(permits3$GIS_ID) CT_ID_10 BG_ID_10 Land_Parcel_ID ..... 32768
# > nrow(permits3[is.na(permits3$GIS_ID),])
# [1] 32768

##### splitting into subsets for filling NAs

permits_full <- permits3[!is.na(permits3$Land_Parcel_ID),]



# permits[is.na(permits$Land_Parcel_ID) & permits$Property_ID != 0 & !is.na(permits$Property_ID),]
# permits_0probID <- permits[is.na(permits$Land_Parcel_ID) & (permits$Property_ID == 0 | is.na(permits$Property_ID)),]
# permits_0probID_NA <- permits[is.na(permits$Land_Parcel_ID) & permits$Property_ID == 0 &







# geo enrichment
# SSH - adding parcel id, X and Y using last year's data to fill in the NAs
# dataset number 1

# geo2021 <- read.csv("/Users/saina/Desktop/311 analysis results/paper/First paper/new muni data/Permits.Records.Geocoded.SSH.01142021.csv") # 486277 rows, last year's data
# 
# geo2021$CT_ID_10 <- format(geo2021$CT_ID_10, scientific = F)
# geo2021$Blk_ID_10 <- format(geo2021$Blk_ID_10, scientific = F)
# geo2021$BG_ID_10 <- format(geo2021$BG_ID_10, scientific = F)
# 
# permits_full <- permits[!is.na(permits$Land_Parcel_ID),]
# permits_wpropID <- permits[is.na(permits$Land_Parcel_ID) & permits$Property_ID != 0 & !is.na(permits$Property_ID),]
# permits_0probID <- permits[is.na(permits$Land_Parcel_ID) & (permits$Property_ID == 0 | is.na(permits$Property_ID)),]
# # permits_0probID_NA <- permits[is.na(permits$Land_Parcel_ID) & permits$Property_ID == 0 &
# # is.na(permits$Property_ID),]
# # filling NAs - permits_wpropID
# permits_wpropID$Land_Parcel_ID <- geo2021$Land_Parcel_ID[match(permits_wpropID$Property_ID, 
#                                                                geo2021$Property_ID)]
# permits_wpropID$TLID <- geo2021$TLID[match(permits_wpropID$Property_ID, geo2021$Property_ID)]
# permits_wpropID$Blk_ID_10 <- geo2021$Blk_ID_10[match(permits_wpropID$Property_ID, geo2021$Property_ID)]
# permits_wpropID$BG_ID_10 <- geo2021$BG_ID_10[match(permits_wpropID$Property_ID, geo2021$Property_ID)]
# permits_wpropID$CT_ID_10 <- geo2021$CT_ID_10[match(permits_wpropID$Property_ID, geo2021$Property_ID)]
# permits_wpropID$NSA_Name <- geo2021$NSA_Name[match(permits_wpropID$Property_ID, geo2021$Property_ID)]
# permits_wpropID$BRA_PD <- geo2021$BRA_PD[match(permits_wpropID$Property_ID, geo2021$Property_ID)]
# permits_wpropID$lat <- geo2021$Y[match(permits_wpropID$Property_ID, geo2021$Property_ID)]
# permits_wpropID$long <- geo2021$X[match(permits_wpropID$Property_ID, geo2021$Property_ID)]
# 
# # filling NAs - permits_wpropID
# permits_0probID[!is.na(permits_0probID$parcel_id),]$Land_Parcel_ID <- geo2021$Land_Parcel_ID[match(permits_0probID[!is.na(permits_0probID$parcel_id),]$parcel_id, geo2021[!is.na(geo2021$parcel_num),]$parcel_num)]
# 
# permits_0probID[!is.na(permits_0probID$parcel_id),]$TLID <- geo2021$TLID[match(permits_0probID[!is.na(permits_0probID$parcel_id),]$parcel_id, geo2021[!is.na(geo2021$parcel_num),]$parcel_num)]
# 
# permits_0probID[!is.na(permits_0probID$parcel_id),]$Blk_ID_10 <- geo2021$Blk_ID_10[match(permits_0probID[!is.na(permits_0probID$parcel_id),]$parcel_id, geo2021[!is.na(geo2021$parcel_num),]$parcel_num)]
# 
# permits_0probID[!is.na(permits_0probID$parcel_id),]$CT_ID_10 <- geo2021$CT_ID_10[match(permits_0probID[!is.na(permits_0probID$parcel_id),]$parcel_id, geo2021[!is.na(geo2021$parcel_num),]$parcel_num)]
# 
# permits_0probID[!is.na(permits_0probID$parcel_id),]$BG_ID_10 <- geo2021$BG_ID_10[match(permits_0probID[!is.na(permits_0probID$parcel_id),]$parcel_id, geo2021[!is.na(geo2021$parcel_num),]$parcel_num)]
# 
# permits_0probID[!is.na(permits_0probID$parcel_id),]$NSA_Name <- geo2021$NSA_Name[match(permits_0probID[!is.na(permits_0probID$parcel_id),]$parcel_id, geo2021[!is.na(geo2021$parcel_num),]$parcel_num)]
# 
# permits_0probID[!is.na(permits_0probID$parcel_id),]$BRA_PD <- geo2021$BRA_PD[match(permits_0probID[!is.na(permits_0probID$parcel_id),]$parcel_id, geo2021[!is.na(geo2021$parcel_num),]$parcel_num)]

#integer64 to numeric
# permits_0probID$Blk_ID_10 <- as.numeric(permits_0probID$Blk_ID_10)
# permits_0probID$Blk_ID_10 <- format(permits_0probID$Blk_ID_10, scientific = FALSE)
# permits_full$Blk_ID_10 <- as.numeric(permits_full$Blk_ID_10)

# dt <- bind_rows(permits_0probID, permits_full, permits_wpropID) #36, 550065
# 
# dt$BG_ID_10 <- as.numeric(dt$BG_ID_10)
# dt$CT_ID_10 <- as.numeric(dt$CT_ID_10)
# dt$Blk_ID_10 <- as.numeric(dt$Blk_ID_10)
# 
# # filling NAs
# pad <- read.csv("/Users/Shared/Files From d.localized/School/Semester 6/BARI PAD update/PAD/PADCross.Record.DRFT1.06202022.csv") # property assessment data
# parcel2020 <- read.csv("/Users/saina/Downloads/Parcel_final_2020_06302020.csv") # parcel data 2020
# 
# dt$Land_Parcel_ID <- ifelse(is.na(dt$Land_Parcel_ID) & !is.na(dt$GIS_ID), dt$GIS_ID, dt$Land_Parcel_ID)
# #parcel2020
# dt$CT_ID_10[is.na(dt$CT_ID_10) & !is.na(dt$Land_Parcel_ID)] <- parcel2020$CT_ID_10[match(dt$Land_Parcel_ID[is.na(dt$CT_ID_10) & !is.na(dt$Land_Parcel_ID)], parcel2020$Land_Parcel_ID)]
# 
# dt$BG_ID_10[is.na(dt$BG_ID_10) & !is.na(dt$Land_Parcel_ID)] <- parcel2020$BG_ID_10[match(dt$Land_Parcel_ID[is.na(dt$BG_ID_10) & !is.na(dt$Land_Parcel_ID)], parcel2020$Land_Parcel_ID)]
# 
# dt$Blk_ID_10[is.na(dt$Blk_ID_10) & !is.na(dt$Land_Parcel_ID)] <- parcel2020$Blk_ID_10[match(dt$Land_Parcel_ID[is.na(dt$Blk_ID_10) & !is.na(dt$Land_Parcel_ID)], parcel2020$Land_Parcel_ID)]
# 
# dt$Blk_ID_10[is.na(dt$Blk_ID_10) & !is.na(dt$Land_Parcel_ID)] <- parcel2020$Blk_ID_10[match(dt$Land_Parcel_ID[is.na(dt$Blk_ID_10) & !is.na(dt$Land_Parcel_ID)], parcel2020$Land_Parcel_ID)]
# #pad
# dt$BG_ID_10[is.na(dt$BG_ID_10) & !is.na(dt$CT_ID_10) & !is.na(dt$Land_Parcel_ID)] <- pad$BG_ID_10[match(dt$Land_Parcel_ID[is.na(dt$BG_ID_10) & !is.na(dt$CT_ID_10) & !is.na(dt$Land_Parcel_ID)], pad$Land_Parcel_ID)]
# #geo2021
# parcel_num_NArec <- dt$parcel_num[is.na(dt$Land_Parcel_ID)]
# parcel_num_NArec <- parcel_num_NArec[!is.na(parcel_num_NArec)]
# parcel_num_NArec <- geo2021[geo2021$parcel_num %in% parcel_num_NArec,] # using BP from last year
# parcel_num_NArec$CT_ID_10 <- as.numeric(parcel_num_NArec$CT_ID_10)
# parcel_num_NArec$BG_ID_10 <- as.numeric(parcel_num_NArec$BG_ID_10)
# parcel_num_NArec$Blk_ID_10 <- as.numeric(parcel_num_NArec$Blk_ID_10)
# parcel_num_NArec <- parcel_num_NArec[!is.na(parcel_num_NArec$Land_Parcel_ID),] #47310
# 
# dt$parcel_num <- as.numeric(dt$parcel_num)
# 
# cover <- dt[is.na(dt$CT_ID_10),] #24325
# dt$CT_ID_10[is.na(dt$CT_ID_10)] <- parcel_num_NArec$CT_ID_10[match(dt$parcel_num[is.na(dt$CT_ID_10)], parcel_num_NArec$parcel_num)]
# 
# dt$BG_ID_10[is.na(dt$BG_ID_10)] <- parcel_num_NArec$BG_ID_10[match(dt$parcel_num[is.na(dt$BG_ID_10)], parcel_num_NArec$parcel_num)]
# 
# dt$Blk_ID_10[is.na(dt$Blk_ID_10)] <- parcel_num_NArec$Blk_ID_10[match(dt$parcel_num[is.na(dt$Blk_ID_10)], parcel_num_NArec$parcel_num)]
# 
# dt$Land_Parcel_ID[is.na(dt$Land_Parcel_ID)] <- parcel_num_NArec$Land_Parcel_ID[match(dt$parcel_num[is.na(dt$Land_Parcel_ID)], parcel_num_NArec$parcel_num)]
# 
# dt$lat[is.na(dt$lat)] <- parcel_num_NArec$Y[match(dt$parcel_num[is.na(dt$lat)], parcel_num_NArec$parcel_num)]
# dt$long[is.na(dt$long)] <- parcel_num_NArec$X[match(dt$parcel_num[is.na(dt$long)], parcel_num_NArec$parcel_num)]
# 
# TLID <- read.csv("~/Desktop/BARI/GI-2023/Outputs_Feb24/parcels_updatedTLID_2023.csv")
# cover_tlid_ct <- TLID[!is.na(TLID$CT_ID_10),]
# 
# dt$CT_ID_10[is.na(dt$CT_ID_10) & !is.na(dt$TLID)] <- 
#   cover_tlid_ct$CT_ID_10[match(dt$TLID[is.na(dt$CT_ID_10) & !is.na(dt$TLID)], cover_tlid_ct$TLID)]
# 
# dt$BG_ID_10[is.na(dt$BG_ID_10) & !is.na(dt$TLID)] <- 
#   cover_tlid_ct$BG_ID_10[match(dt$TLID[is.na(dt$BG_ID_10) & !is.na(dt$TLID)], cover_tlid_ct$TLID)]
# 
# dt$Blk_ID_10[is.na(dt$Blk_ID_10) & !is.na(dt$TLID)] <- 
#   cover_tlid_ct$Blk_ID_10[match(dt$TLID[is.na(dt$Blk_ID_10) & !is.na(dt$TLID)], cover_tlid_ct$TLID)]
# 
# # parcel 2021
# dt$TLID[is.na(dt$TLID) & !is.na(dt$Land_Parcel_ID)] <- 
#   landParcels$TLID[!is.na(landParcels$Land_Parcel_ID)][match(dt$Land_Parcel_ID[is.na(dt$TLID) & !is.na(dt$Land_Parcel_ID)], landParcels$Land_Parcel_ID[!is.na(landParcels$Land_Parcel_ID)])]
# 
# dt$NSA_Name[is.na(dt$NSA_Name) & !is.na(dt$Land_Parcel_ID)] <- 
#   landParcels$NSA_Name[!is.na(landParcels$Land_Parcel_ID)][match(dt$Land_Parcel_ID[is.na(dt$NSA_Name) & !is.na(dt$Land_Parcel_ID)], landParcels$Land_Parcel_ID[!is.na(landParcels$Land_Parcel_ID)])]
# 
# dt$BRA_PD[is.na(dt$BRA_PD) & !is.na(dt$Land_Parcel_ID)] <- 
#   landParcels$BRA_PD[!is.na(landParcels$Land_Parcel_ID)][match(dt$Land_Parcel_ID[is.na(dt$BRA_PD) & !is.na(dt$Land_Parcel_ID)], landParcels$Land_Parcel_ID[!is.na(landParcels$Land_Parcel_ID)])]
# 
# parcel2020$NSA_Name[parcel2020$NSA_Name == ""] <- NA
# 
# # parcel 2020, NSA_Name, BRA_PD
# dt$NSA_Name[is.na(dt$NSA_Name) & !is.na(dt$Land_Parcel_ID)] <- 
#   parcel2020$NSA_Name[!is.na(parcel2020$Land_Parcel_ID) & !is.na(parcel2020$NSA_Name)][match(dt$Land_Parcel_ID[is.na(dt$NSA_Name) & !is.na(dt$Land_Parcel_ID)], parcel2020$Land_Parcel_ID[!is.na(parcel2020$Land_Parcel_ID) & !is.na(parcel2020$NSA_Name)])]
# 
# dt$BRA_PD[is.na(dt$BRA_PD) & !is.na(dt$Land_Parcel_ID)] <- 
#   parcel2020$BRA_PD[!is.na(parcel2020$Land_Parcel_ID) & !is.na(parcel2020$BRA_PD)][match(dt$Land_Parcel_ID[is.na(dt$BRA_PD) & !is.na(dt$Land_Parcel_ID)], parcel2020$Land_Parcel_ID[!is.na(parcel2020$Land_Parcel_ID) & !is.na(parcel2020$BRA_PD)])]
# 
# # parcels, gis_id
# dt$GIS_ID[is.na(dt$GIS_ID) & !is.na(dt$Land_Parcel_ID)] <-
#   parcels$GIS_ID[!is.na(parcels$GIS_ID)][match(dt$Land_Parcel_ID[is.na(dt$GIS_ID) & !is.na(dt$Land_Parcel_ID)], parcels$Ln_P_ID[!is.na(parcels$GIS_ID)])]
# 
# sum(is.na(dt$long)) # 14332
# sum(!is.na(dt$long))/nrow(dt) # 97.4%
# sum(is.na(dt$Land_Parcel_ID)) # 14096
# sum(is.na(dt$BG_ID_10)) # 12839
# sum(!is.na(dt$BG_ID_10))/nrow(dt) # %97.6
# sum(is.na(dt$CT_ID_10)) # 12839
# sum(!is.na(dt$CT_ID_10))/nrow(dt) # %97.6
# sum(is.na(dt$Blk_ID_10)) # 12849
# sum(!is.na(dt$Blk_ID_10))/nrow(dt) # %97.6
# sum(is.na(dt$GIS_ID)) # 16861
# sum(!is.na(dt$GIS_ID))/nrow(dt) # %96.9
# 
# length(unique(dt$Land_Parcel_ID)) #70061
# length(unique(dt$CT_ID_10)) #181
# length(unique(dt$Blk_ID_10)) #5110
# length(unique(dt$BG_ID_10)) #559
# 
# dt$Blk_ID_10[!is.na(dt$Blk_ID_10)] <- format(dt$Blk_ID_10[!is.na(dt$Blk_ID_10)], scientific = FALSE)
# dt$BG_ID_10[!is.na(dt$BG_ID_10)] <- format(dt$BG_ID_10[!is.na(dt$BG_ID_10)], scientific = FALSE)
# dt$CT_ID_10[!is.na(dt$CT_ID_10)] <- format(dt$CT_ID_10[!is.na(dt$CT_ID_10)], scientific = FALSE)

## this is the raw permit data + geo characteristics. Reduced NAs as possible
# internal file
#write.csv(dt, "/Users/saina/Downloads/permits/buildingpermitsgeoJuly10_SSH.csv", row.names = F) 

#-------------------------------------------#
#       Making new vars                     #
#-------------------------------------------#
#This syntax comes from 2014 students

############### categorizing permit types ###########################
# permits <- dt
summary(permits_full)
permits <- permits_full
#Permits_Records_2023 <- read.csv("Permits/2023/Outputs/Permits.Records.Geocoded.DOB.20230801.csv")

permits <- permits %>%
  rename(
    PermitNumber = permitnumber,       # Different
    WORKTYPE = worktype,             # Same, different case
    #permittypedescr = permittype,    # Different
    #description = descriptio,        # Missing 'n'
    NOTES = comments, #different
    APPLICANT = applicant,           # Same, different case
    DECLARED_VALUATION = declared_valuation, # Different
    ISSUED_DATE = issued_date,        # Missing 'e'
    EXPIRATION_DATE = expiration_date,    # Missing '_DATE'
    STATUS = status,
    OCCUPANCY = occupancytype,          # Missing 'ype'
    ADDRESS = address,               # Same, different case
    CITY = city,                     # Same, different case
    STATE = state,                   # Same, different case
    ZIP = zip,                       # Same, different case
    #Property_ID = property_id,        # Same, different case
    NSA_NAME = NSA_Name             # Different case
  ) %>% 
  #mutate(X = st_coordinates(geometry)[, 'X'], Y = st_coordinates(geometry)[, 'Y']) 
  rename("Y" = "lat",  "X" = "long") 
  #%>% select(-geom_point_2249, -geom_point_4326, -wkb_raw, -num1, -num2, -street_c, -suffix_c, -city_c, -zip_c) 
  # %>% st_set_geometry(NULL)



permits$permittypedescr = trim(str_replace_all(permits$permittypedescr,"\xe4\xf3\xf1",""))

# Step 4: Categorize Permit Types
# permits <- permits %>%
#   mutate(
#     newcon = ifelse(
#       (!is.na(description) & description %in% c("New construction", "Erect")) |
#         (!is.na(permittypedescr) & permittypedescr %in% c("Erect/New Construction", "Foundation Permit")),
#       1, 0
#     ),
#     demo = ifelse(
#       !newcon & (!is.na(description) & description %in% c("Demolition - Exterior", "Demolition - Interior")),
#       1, 0
#     ),
#     addition = ifelse(
#       !newcon & !demo & (!is.na(description) & description == "Addition") |
#         (!is.na(permittypedescr) & permittypedescr %in% c("Amendment to a Long Form", "Long Form/Alteration Permit")),
#       1, 0
#     ),
#     reno = ifelse(
#       !newcon & !demo & !addition & (
#         (!is.na(description) & description %in% c("Electrical", "Gas", "Plumbing", "Renovations - Exterior", "Renovations - Interior NSC")) |
#           (!is.na(permittypedescr) & permittypedescr %in% c("Electrical Permit", "Gas Permit", "Plumbing Permit", "Short Form Bldg Permit", "Use of Premises", "Electrical Fire Alarms", "Electrical Low Voltage", "Electrical Temporary Service"))
#       ),
#       1, 0
#     ),
#     government = ifelse(
#       owner %in% c(
#         "CITY OF BOSTON", "BOSTON REDEVELOPMNT AUTH", "BOSTON REDEVELOPMENT AUTH", "BOSTON REDEVELOPMENT",
#         "BOSTON HOUSING AUTHORITY", "BOSTON HOUSING AUTH", "BOSTON POLICE POST 1018 VFW", "CITY OF BOSTON SCHOOL DEPT",
#         "CITY OF BOSTON PUBLIC HEALTH", "CITY OF BOSTON SCHOOL DEPT", "CITY OF BOSTON BY FCL", "CITY OF BOSTON PUB FACIL",
#         "BOSTON REDEVLOPMENT AUTHORIT", "BOSTON POLICE DETECTIVE", "CITY OF BOSTON PARKS", "BOSTON REDEVELOP AUTHORITY",
#         "CITY OF BOSTON PARKS AND", "THE BOSTON REDEVELOPMENT", "BOSTON REDEVOPMENT AUTH", "BOSTON REDEVLPMNT AUTHOR",
#         "BOSTON REDEVLOPMENT AUTHOR", "MBTA", "BOSTON PUBLIC HEALTH COMM", "CITY OF BOSTON PUBLIC HEALTH",
#         "CITY OB BOSTON PUBLIC HEALTH", "PUBLIC FACILITIES COMM", "BOSTON DEVELOPMENT", "BOSTON FIRE DEPARTMENT",
#         "BOSTON HOUSING", "BOSTON MUNRCIPA", "BOSTON POLICE DEPARTMENT", "BOSTON PORT * S", "BOSTON PUBLIC HEALTH COMM",
#         "BOSTON REDEVELO", "BOSTON REDEVELOPMENT AUTH", "BOSTON REDEVELP", "CITY OF BOSTON", "CITY OF BOSTON - DND",
#         "CITY OF BOSTON - PUB FAC", "CITY OF BOSTON (REO)", "CITY OF BOSTON BY FCL", "CITY OF BOSTON PROP MGMT DEPT",
#         "CITY OF BOSTON-GEORGE WHITE FUND", "COMMONWLTH OF M", "COMMWLTH OF MAS", "M B T A", "MASS BAY TRANSP",
#         "MASS BAY TRANSPORTATION AUTH", "MASS PORT AUTHO", "MASS PORT AUTHORITY", "MASS TURNPIKE A",
#         "MASS TURNPIKE AUTHORITY", "MASSACHUSETTS BAY TRANS AUTH", "MASSACHUSETTS PORT AUTHORITY", "MASSPORT AUTHOR",
#         "MBTA", "MSS PORT AUTHOR", "COMMMONWEALTH O", "COMMONWEALTH FL", "COMMONWEALTH OF", "UNITED STATES OF AMER",
#         "FEDERAL HOME LOAN MORTGAGE", "FEDERAL HOME LOAN MTG CORP", "FEDERAL MORTGAGE ASSOC", "FEDERAL NATIONAL MORTGAGE ASSO",
#         "FEDERAL NATIONAL MTG ASSOC"
#       ),
#       1, 0
#     )
#   )

NEWCON = (!is.na(permits$description) & 
            (permits$description == "New construction" | 
               permits$description == "Erect")) | 
  (!is.na(permits$permittypedescr) & 
     (permits$permittypedescr=="Erect/New Construction" |
        permits$permittypedescr == "Foundation Permit"))

DEMO = (!is.na(permits$description) & 
          (permits$description =="Demolition - Exterior" | permits$description=="Demolition - Interior")) 


ADD = (!is.na(permits$description) & 
         (permits$description == "Addition")) | 
  (!is.na(permits$permittypedescr) & 
     (permits$permittypedescr == "Amendment to a Long Form" | 
        permits$permittypedescr == "Long Form/Alteration Permit"))



RENO = (!is.na(permits$description) & 
          (permits$description == "Electrical" | 
             permits$description == "Gas" | 
             permits$description == "Plumbing" |
             permits$description == "Renovations - Exterior"|
             permits$description == "Renovations - Interior NSC"|
             permits$description == "Renovations - Exterior")) |
  (!is.na(permits$permittypedescr) & 
     (permits$permittypedescr == "Electrical Permit" | 
        permits$permittypedescr == "Gas Permit" | 
        permits$permittypedescr == "Plumbing Permit" | 
        permits$permittypedescr == "Short Form Bldg Permit" | 
        permits$permittypedescr == "Use of Premises" |
        permits$permittypedescr== "Electrical Fire Alarms" | 
        permits$permittypedescr== "Electrical Low Voltage" | 
        permits$permittypedescr== "Electrical Temporary Service"))


#combining the two categorizations
permits$newcon<-ifelse(NEWCON, 1,0)
permits$demo<-ifelse(!NEWCON & DEMO, 1,0)
permits$addition<-ifelse(!NEWCON & !DEMO & ADD, 1,0)
permits$reno<-ifelse(!NEWCON & !DEMO & !ADD & RENO, 1,0)

##“Government” Variable
permits$government = ifelse(permits$owner == "CITY OF BOSTON" |
                              permits$owner == "BOSTON REDEVELOPMNT AUTH" |
                              permits$owner == "BOSTON REDEVELOPMENT AUTH" |
                              permits$owner == "BOSTON REDEVELOPMENT" |
                              permits$owner == "BOSTON HOUSING AUTHORITY" |
                              permits$owner == "BOSTON HOUSING AUTH" |
                              permits$owner == "BOSTON POLICE POST 1018 VFW" |
                              permits$owner == "CITY OF BOSTON SCHOOL DEPT" |
                              permits$owner == "CITY OF BOSTON PUBLIC HEALTH" |
                              permits$owner == "CITY OF BOSTON SCHOOL DEPT" |
                              permits$owner == "CITY OF BOSTON BY FCL" |
                              permits$owner == "CITY OF BOSTON PUB FACIL" |
                              permits$owner == "BOSTON REDEVLOPMENT AUTHORIT" |
                              permits$owner == "BOSTON POLICE DETECTIVE" |
                              permits$owner == "CITY OF BOSTON PARKS" |
                              permits$owner == "BOSTON REDEVELOP AUTHORITY" |
                              permits$owner == "CITY OF BOSTON PARKS AND" |
                              permits$owner == "THE BOSTON REDEVELOPMENT" |
                              permits$owner == "BOSTON REDEVOPMENT AUTH" |
                              permits$owner == "BOSTON REDEVLPMNT AUTHOR" |
                              permits$owner == "BOSTON REDEVLOPMENT AUTHOR" |
                              permits$owner == "MBTA" |
                              permits$owner == "BOSTON PUBLIC HEALTH COMM" |
                              permits$owner == "CITY OF BOSTON PUBLIC HEALTH" |
                              permits$owner == "CITY OB BOSTON PUBLIC HEALTH" |
                              permits$owner == "PUBLIC FACILITIES COMM" |
                              permits$owner== "BOSTON DEVELOPMENT" |
                              permits$owner== "BOSTON FIRE DEPARTMENT" |
                              permits$owner== "BOSTON HOUSING" |
                              permits$owner== "BOSTON MUNRCIPA" |
                              permits$owner== "BOSTON POLICE DEPARTMENT" |
                              permits$owner== "BOSTON PORT * S" |
                              permits$owner== "BOSTON PUBLIC HEALTH COMM" |
                              permits$owner== "BOSTON REDEVELO" |
                              permits$owner== "BOSTON REDEVELOPMENT AUTH" |
                              permits$owner== "BOSTON REDEVELP" |
                              permits$owner== "CITY OF BOSTON" |
                              permits$owner== "CITY OF BOSTON - DND" |
                              permits$owner== "CITY OF BOSTON - PUB FAC " |
                              permits$owner== "CITY OF BOSTON (REO)" |
                              permits$owner== "CITY OF BOSTON BY FCL" |
                              permits$owner== "CITY OF BOSTON PROP MGMT DEPT" |
                              permits$owner== "CITY OF BOSTON-GEORGE WHITE FUND" |
                              permits$owner== "COMMONWLTH OF M" |
                              permits$owner== "COMMWLTH OF MAS" |
                              permits$owner== "M B T A" |
                              permits$owner== "MASS BAY TRANSP" |
                              permits$owner== "MASS BAY TRANSPORTATION AUTH" |
                              permits$owner== "MASS PORT AUTHO" |
                              permits$owner== "MASS PORT AUTHORITY" |
                              permits$owner== "MASS TURNPIKE A" |
                              permits$owner== "MASS TURNPIKE AUTHORITY" |
                              permits$owner== "MASSACHUSETTS BAY TRANS AUTH" |
                              permits$owner== "MASSACHUSETTS PORT AUTHORITY" |
                              permits$owner== "MASSPORT AUTHOR" |
                              permits$owner== "MBTA" |
                              permits$owner== "MSS PORT AUTHOR" |
                              permits$owner== "COMMMONWEALTH O" |
                              permits$owner== "COMMONWEALTH FL" |
                              permits$owner== "COMMONWEALTH OF" |
                              permits$owner== "UNITED STATES OF AMER" |
                              permits$owner== "FEDERAL HOME LOAN MORTGAGE" |
                              permits$owner== "FEDERAL HOME LOAN MTG CORP" |
                              permits$owner== "FEDERAL MORTGAGE ASSOC" |
                              permits$owner== "FEDERAL NATIONAL MORTGAGE ASSO" |
                              permits$owner== "FEDERAL NATIONAL MTG ASSOC",1,0)



# dropped industry religous school etc categories from last years



#lubridating ISSUED_DATE
require(lubridate)
#getting rid of ""
# permits$ISSUED_DATE[permits$ISSUED_DATE ==""]=NA
# sum(is.na(permits$ISSUED_DATE)) #0
permits$ISSUED_DATE = ymd_hms(permits$ISSUED_DATE)
sum(is.na(permits$ISSUED_DATE))

permits_backup <- permits
#lubridating EXPIRATION_DATE
permits$EXPIRATION_DATE[permits$EXPIRATION_DATE==""|permits$EXPIRATION_DATE==" "]=NA
sum(is.na(permits$EXPIRATION_DATE)) #22257 #24926 #24645 in 2024
permits$EXPIRATION_DATE = ymd_hms(permits$EXPIRATION_DATE)
sum(is.na(permits$EXPIRATION_DATE))

permits$PermitDuration = permits$EXPIRATION_DATE - permits$ISSUED_DATE
permits$PermitDuration[!is.na(permits$PermitDuration) & permits$PermitDuration<0]= NA

#permits = rename(permits,
                 #OCCUPANCY = occupancytype, NOTES = comments)
#permits = rename(permits, PermitNumber = permitnumber)

names(permits)
varnames = c( "PermitNumber","WORKTYPE","permittypedescr","description","NOTES",
              "APPLICANT", "DECLARED_VALUATION","total_fees","ISSUED_DATE","EXPIRATION_DATE",
              "STATUS","owner","OCCUPANCY","sq_feet", "ADDRESS", "CITY","STATE","ZIP",
              "Property_ID","GIS_ID","parcel_num","X","Y","Land_Parcel_ID","TLID","Blk_ID_10","BG_ID_10","CT_ID_10", "Blk_ID_20" ,"BG_ID_20", "CT_ID_20", 
              "NSA_NAME","BRA_PD","newcon","addition","demo","reno","PermitDuration", "government")

permits <- permits[,varnames]
#"Location", doesn't exist in 2021 data
# could vary by year - fixing the column names

# names(permits)[16] <- "CITY"
# names(permits)[17] <- "STATE"
# names(permits)[18] <- "ZIP"
# names(permits)[22] <- "X"
# names(permits)[21] <- "Y"
# names(permits)[35] <- "NSA_NAME"
# names(permits)[2] <- "WORKTYPE"
# names(permits)[1] <- "PermitNumber"
# names(permits)[6] <- "APPLICANT"
# names(permits)[7] <- "DECLARED_VALUATION"
# names(permits)[9] <- "ISSUED_DATE"
# names(permits)[10] <- "EXPIRATION_DATE"
# names(permits)[11] <- "STATUS"
# names(permits)[15] <- "ADDRESS"

setdiff(names(permits),varnames)
setdiff(varnames,names(permits))
# permits <- permits[,-1]

#permits = permits[,varnames]
permits$PermitDuration <- as.numeric(permits$PermitDuration)/86400 #NA's   :24739
summary(permits) 

# permits$DECLARED_VALUATION <- as.numeric(permits$DECLARED_VALUATION)
# permits$total_fees <- as.numeric(permits$total_fees)
# summary(permits$total_fees)
#permits_backup <- permits

permits <- permits %>%
  mutate(
    DECLARED_VALUATION = as.numeric(gsub("[\\$,]", "", DECLARED_VALUATION)),
    total_fees = as.numeric(gsub("[\\$,]", "", total_fees))
  )

permits$"total_fees" = ifelse(!is.na(permits$"total_fees") & permits$"total_fees"<0,NA, permits$"total_fees")

#get the lost ones from last year data:
###reincorporating these lost cases by 


#(1) identifying PermitNumbers occurring in last year's data that do not occur in this year's data, 
#missing_permits <- anti_join(permits_2023, permits[,1], by = "PermitNumber")
#missing_permits <- anti_join(permits_2023, permits_vector, by = "PermitNumber")
permits_vector <- data.table(PermitNumber = permits[, PermitNumber])
missing_permits <- permits_2023[!PermitNumber %in% permits_vector$PermitNumber]

#(2) subsetting them, 
#We would need to add 2020 geographies to these cases between steps 2 & 3 to ensure that all information is present.
missing_permits$Blk_ID_20 <- properties$Blk_ID_20[match(missing_permits$GIS_ID,properties$GIS_ID)]
missing_permits$BG_ID_20 <- properties$BG_ID_20[match(missing_permits$GIS_ID,properties$GIS_ID)]
missing_permits$CT_ID_20 <- properties$CT_ID_20[match(missing_permits$GIS_ID,properties$GIS_ID)]

missing_permits = missing_permits[,..varnames]
#(3) rbind them with the new data. 
missing_permits[, ISSUED_DATE := as.POSIXct(ISSUED_DATE, format = "%Y-%m-%d %H:%M:%S")]
missing_permits[, EXPIRATION_DATE := as.POSIXct(paste(EXPIRATION_DATE, "00:00:00"), format = "%Y-%m-%d %H:%M:%S")]

permits_merged <- rbind(permits, missing_permits)

permits_missingmerged <- permits3[which(permits3$permitnumber %in% setdiff(permits3$permitnumber, permits_merged$PermitNumber)),]
permits_missingorig <- Permits_orig[which(Permits_orig$permitnumber %in% setdiff(Permits_orig$permitnumber, permits_merged$PermitNumber)),]

permits_misafmerge <- permits3[is.na(permits3$Land_Parcel_ID),]
permits_misgeo_2 
missing_pernum_23 <-  c(permits_misafmerge$permitnumber, permits_misgeo_2$permitnumber)
setDT(permits_2023)
permits_2023 <- permits_2023[!duplicated(PermitNumber)]

missing_23 <- permits_2023[which(permits_2023$PermitNumber %in% unique(missing_pernum_23)),]
summary(missing_23) 





bp_2024 <- permits_merged

bp_2024_dedupe<-bp_2024[!duplicated(cbind(bp_2024$PermitNumber,bp_2024$WORKTYPE,bp_2024$permittypedescr,bp_2024$description,bp_2024$NOTES,
                                          bp_2024$APPLICANT, bp_2024$DECLARED_VALUATION, bp_2024$total_fees, bp_2024$ISSUED_DATE, bp_2024$EXPIRATION_DATE,
                                          bp_2024$STATUS, bp_2024$owner, bp_2024$OCCUPANCY, bp_2024$sq_feet)),]
#save!
write.csv(bp_2024_dedupe, permits2024_path,row.names=F)

permits_check <- permits_check %>%
  mutate(
    ISSUED_DATE = as.POSIXct(ISSUED_DATE, format = "%Y-%m-%d %H:%M:%S"),
    EXPIRATION_DATE = as.POSIXct(EXPIRATION_DATE, format = "%Y-%m-%d %H:%M:%S")
  )

