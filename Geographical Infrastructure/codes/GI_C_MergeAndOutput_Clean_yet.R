# 0. Preliminary Stuff
  
# Load Libraries}

library(tidyverse)
library(sf)



# Read BARI data}
# uploading the property assessment data + units 2020
PAD_unit <- read.csv("~/Desktop/Academic/BARI/Scripts/GI/Inputs/PAD.Record.wUnit.csv")
dim(PAD_unit) # 180627 67 in 2023  #182242     71 in 2023
#172266,81 in 2018, 174074 in 2019, 175004 80 in 2020, 177091 70 in 2021, 177091, 80 in July, 2022

MtchedTLID <- read.csv("~/Desktop/Academic/BARI/Scripts/GI/Outputs/parcels_updatedTLID_2024.csv")
sum(is.na(MtchedTLID$TLID))#43 in 2018 #66 in 2019  #66 in 2019 #65 in 2020 #48 in 2021 #45 in July, 2022, # 44 in March, 2023, #96 in Sep 2023 #124 in 2024

### to add in some variables
prpty_to_prcl <- read.csv("~/Desktop/Academic/BARI/Scripts/GI/Outputs/PropertiesToParcels-(11-2024).csv")
# 206958 in 2018 # 198269 in 2019 #176925 in July 2022, #176925 in March 2023, #180448 in August 2023

# special parcels from 2019
prcl <- read.csv("~/Desktop/Academic/BARI/Scripts/GI/Inputs/all_specialparcels_10282019.csv") # 98903 in 2018 #98930 in October, 2019



# Rename the Dataset}
PAD_unit <- PAD_unit %>% dplyr::rename(AV_TOTAL = TOTAL_VALUE,
                                       AV_LAND = LAND_VALUE,
                                       AV_BLDG = BLDG_VALUE,
                                       YR_REMOD = YR_REMODEL,
                                       NUM_FLOORS = RES_FLOOR)               



#}
## adding geo variables
MtchedTLID$Land_Parcel_ID <- prpty_to_prcl$Land_Parcel_ID[match(MtchedTLID$GIS_ID,prpty_to_prcl$GIS_ID)] 
length(unique(MtchedTLID$Land_Parcel_ID))
# 97894 in Sep 23, 98857 Unique in March, 2023 #97766 in 2024
#97954 unique before
nrow(MtchedTLID)
#98639 in Sep 23  98964 in Aug 23 [1] 99957 in March, 2023 # [1] 98930 in the last version #98529 in 2024
length(unique(MtchedTLID$MAP_PAR_ID))
# [1]98529 in 2024, 98822 in March 2023 # 98645 in Aug 23 #98639 in Sep 23
# [1]  #98903 in 2020, 98902 in 2021
length(unique(MtchedTLID$Land_Parcel_ID))
# 97766 in 2024, 98060 in March 2023 # 97907 in Aug 23 # 97894 in Sep 23
# [1] 98098 # 98051 in 2019 #97954 in 2020 # 97912 in 2021

# NA increased, due to change from 10 to 20. Comment by Toshi Feb 2023. 
sum(is.na(MtchedTLID$Land_Parcel_ID)) #0 in 2024, 0 in Sep 23, 99 in 2023 #157 in 2019 #277 in 2020 # 337 in 2021, 13 missing in 2022
sum(is.na(MtchedTLID$BG_ID_20)) #3 in 2024, 3 in Sep 23, 6 in 2023 #10 in 2020 #10 in 2021 #13 missing in 2022
sum(is.na(MtchedTLID$CT_ID_20)) #3 in 2024, 3 in Sep 23, 6 in  2023 # 8 in 2021 # 8 missing in 2022
sum(is.na(MtchedTLID$Blk_ID_20)) #3 in 2024, 3 in Sep 23, 6 in 2023 #8 in 2021 #8 missing in 2022
sum(is.na(MtchedTLID$TLID)) # 124 in 2024, 96 in Sep 23, 44 in March 2023 # 66 in 2019 #65 in 2020 #48 in 2021 # 43 missing in 2022
length(unique(PAD_unit$GIS_ID)) #98531 in 2024, 98639 in Aug 23 #98873 in Feb 2023# 98907 in 2019 #99777 in 2020 #98872 in 2021 #98943 in 2022 
length(unique(prpty_to_prcl$GIS_ID)) #98531 in 2024, 98639 in 2023 #98907 in 2019 #99777 in 2020 #98873 in 2021 #98943 in 2022

MtchedTLID <- MtchedTLID %>%
  dplyr::select("GIS_ID", "TLID", "Land_Parcel_ID", "CT_ID_20", "BG_ID_20", "Blk_ID_20","Blk_ID_10", "BG_ID_10" , "CT_ID_10" 
                ,"X","Y"
  )


new <- PAD_unit #%>%

new$Land_Parcel_ID <- prpty_to_prcl$Land_Parcel_ID[match(new$GIS_ID, prpty_to_prcl$GIS_ID)]
PAD_unit$Land_Parcel_ID <- prpty_to_prcl$Land_Parcel_ID[match(PAD_unit$GIS_ID, prpty_to_prcl$GIS_ID)]

new <- new %>%
  dplyr::select("Land_Parcel_ID", "GIS_ID", 
                "ST_NUM","ST_NAME","UNIT_NUM","ZIP_CODE", # remove "ST_NAME_SUF", in 2021
                "AV_TOTAL", "AV_LAND", "AV_BLDG", #these are not part of 2021 
                "LAND_SF", 
                "GROSS_AREA", "LIVING_AREA", 
                "unit_N", 
                "unit_N_orig", 
                #"unit_N_org", # we do not remove this even if originally gets removed in the PAD imputation
                "New_unit", # we do not remove this even if originally gets removed in the PAD imputation
                "LU",
                "OWN_OCC","OWNER",
                "MAIL_STREET_ADDRESS", #  was OWNER.MAIL.ADDRESS in 2023
                #"ZIP_CODE",  put in above 
                "YR_BUILT","YR_REMOD",
                "NUM_FLOORS"
                #"R_BLDG_STYL"# does not exist in 2021
  ) 

new$OWN_OCC <- as.numeric(new$OWN_OCC=="Y")
new$YR_REMOD[new$YR_REMOD==0] <- NA
new$YR_BUILT[new$YR_BUILT==0] <- NA

new$owner_address <- paste(new$MAIL_STREET_ADDRESS,new$ZIP_CODE)
# 2021 remove new$ST_NAME_SUF from between st name and unit num 
new$full_address <- paste(new$ST_NUM,new$ST_NAME,new$UNIT_NUM,new$ZIP_CODE)

# Convert relevant columns to numeric after removing non-numeric characters
new <- new %>%
  mutate(
    AV_TOTAL = as.numeric(gsub("[^0-9.]", "", AV_TOTAL)),
    AV_LAND = as.numeric(gsub("[^0-9.]", "", AV_LAND)),
    AV_BLDG = as.numeric(gsub("[^0-9.]", "", AV_BLDG)),
    LAND_SF = as.numeric(gsub("[^0-9.]", "", LAND_SF))
  )

new1 <- new %>% # self-entered unit numbers
  filter(!is.na(unit_N_orig)) %>%
  group_by(Land_Parcel_ID) %>%
  dplyr:: summarise(
    AV_TOTAL = sum(AV_TOTAL,na.rm=T),
    AV_LAND = sum(AV_LAND,na.rm=T), 
    AV_BLDG = sum(AV_BLDG,na.rm=T), 
    LAND_SF = sum(LAND_SF,na.rm=T), 
    GROSS_AREA = sum(GROSS_AREA,na.rm=T),
    LIVING_AREA = sum(LIVING_AREA,na.rm=T), 
    OWN_OCC = max(OWN_OCC),
    NUM_FLOORS = max(NUM_FLOORS),
    unit_N = sum(unit_N, na.rm=TRUE), 
    unit_N_orig = sum(unit_N_orig, na.rm=TRUE),
    property_N = n())

new2 <- new %>% # from SAM
  filter(!is.na(New_unit)) %>% 
  group_by(Land_Parcel_ID) %>%
  dplyr:: mutate(
    AV_TOTAL = sum(AV_TOTAL,na.rm=T),
    AV_LAND = sum(AV_LAND,na.rm=T), 
    AV_BLDG = sum(AV_BLDG,na.rm=T), 
    LAND_SF = sum(LAND_SF,na.rm=T), 
    GROSS_AREA = sum(GROSS_AREA,na.rm=T),
    LIVING_AREA = sum(LIVING_AREA,na.rm=T), 
    OWN_OCC = max(OWN_OCC),
    NUM_FLOORS = max(NUM_FLOORS),
    unit_N = unit_N,
    unit_N_orig = unit_N_orig,
    property_N = n()) %>%
  dplyr::select("Land_Parcel_ID","AV_TOTAL","AV_LAND","AV_BLDG" ,"LAND_SF","GROSS_AREA","LIVING_AREA","OWN_OCC","NUM_FLOORS","property_N", "unit_N","unit_N_orig",)

new3 <- new %>% # imputed
  filter(is.na(unit_N_orig)) %>%
  group_by(Land_Parcel_ID) %>%
  dplyr:: summarise(
    AV_TOTAL = sum(AV_TOTAL,na.rm=T),
    AV_LAND = sum(AV_LAND,na.rm=T), 
    AV_BLDG = sum(AV_BLDG,na.rm=T), 
    LAND_SF = sum(LAND_SF,na.rm=T), 
    GROSS_AREA = sum(GROSS_AREA,na.rm=T),
    LIVING_AREA = sum(LIVING_AREA,na.rm=T), 
    OWN_OCC = max(OWN_OCC),
    NUM_FLOORS = max(NUM_FLOORS),
    unit_N = sum(unit_N, na.rm=TRUE),
    unit_N_orig = sum(unit_N_orig, na.rm=TRUE),
    property_N = n())

new4 <- rbind(as.data.frame(new1),as.data.frame(new2),as.data.frame(new3))
new4 <- new4[!duplicated(new4),]

new5 <- new4 %>%
  group_by(Land_Parcel_ID) %>%
  dplyr:: mutate(
    AV_TOTAL = mean(AV_TOTAL,na.rm=T),
    AV_LAND = mean(AV_LAND,na.rm=T), 
    AV_BLDG = mean(AV_BLDG,na.rm=T), 
    LAND_SF = mean(LAND_SF,na.rm=T), 
    GROSS_AREA = mean(GROSS_AREA,na.rm=T),
    LIVING_AREA = mean(LIVING_AREA,na.rm=T), 
    OWN_OCC = max(OWN_OCC),
    NUM_FLOORS = max(NUM_FLOORS),
    unit_N = max(unit_N, na.rm = T),
    unit_N_orig = max(unit_N_orig, na.rm = T),
    property_N = max(property_N)) %>%
  dplyr::select("Land_Parcel_ID","AV_TOTAL","AV_LAND","AV_BLDG" ,"LAND_SF","GROSS_AREA","LIVING_AREA","OWN_OCC","NUM_FLOORS","property_N", "unit_N","unit_N_orig" )

new_parcels <- new5 # 110430 in Sep 23, ?177091 in Feb?, 2023 #109,843 in 2020, #111,153  in 2021
summary(new_parcels)
# View(new_parcels)

# some property_N larger than unit_N which normally is not possible; 
# this is because unit_N can have 0, so sum of 0's = 0; WE ARE NOT CHANGING IT (decision @Dan)
# if unit_N is smaller than property_N, add the value of property_N to unit_N
new_parcels$unit_N <- ifelse(new_parcels$unit_N < new_parcels$property_N, new_parcels$property_N, new_parcels$unit_N)
# check if property N is larger, this should be 0; there are some exceptions!!! when we have sum of 0's
View(new_parcels[new_parcels$property_N > new_parcels$unit_N,]) #this has 9,087 rows before using the ifelse before

# remove duplicates from parcels
new_parcels <- new_parcels[!duplicated(new_parcels),] # 97768 in 2024, #97894 in Sep 23,  #98150 in Feb 2023
#98179 in 2019 #98445 in 2020 #98136 in 2021

# remove any new parcels with NA Land_Parcel_ID
new_parcels = new_parcels[!is.na(new_parcels$Land_Parcel_ID),]

# rename columns for the special parcels if neccessary 
colnames(prcl)[which(names(prcl) == "comcntr")] <- "comcenter"
colnames(prcl)[which(names(prcl) == "suprmkt")] <- "supermkt"
colnames(prcl)[colnames(prcl)=="Ln_P_ID"] <- "Land_Parcel_ID"
# keep only the special parcels that do not have duplicated the land parcel id
prcl <- prcl[!duplicated(prcl$Land_Parcel_ID),] #98098 from Riley's special parcels for 2018 and 2019

new_parcels_sp <- left_join(x = new_parcels, y = prcl[ , c("Land_Parcel_ID", "lib", "police", "fire", "bps", "private", "comcenter", "medhos", "supermkt", "parking", "vacant", "rel", "mbta_stop", "college", "hlth_cntr", "sub_house")], by= "Land_Parcel_ID") #all.x=TRUE) # 98136 # 98179 in 2019

# length(unique(new_parcels_sp$Land_Parcel_ID)) # 98136 # 98179 in 2019
# length(unique(new$Land_Parcel_ID)) # 98136 # 98179 in 2019 
# length(unique(PAD_unit$Land_Parcel_ID)) # 98136 # 98179 in 2019

## Add back in the character vars from property data:
start1 <- Sys.time()
new_parcels_sp$LU <- NA
new_parcels_sp$YR_BUILT <- NA
new_parcels_sp$YR_REMOD <- NA
#new_parcels_sp$R_BLDG_STYL <- NA
new_parcels_sp$owner_address <- NA
new_parcels_sp$full_address <- NA
pb <- progress_estimated(nrow(new_parcels_sp),0)
for(i in 1:nrow(new_parcels_sp)){
  # subset to this parcel in prop data:
  this_parcel <- subset(new,Land_Parcel_ID == new_parcels_sp$Land_Parcel_ID[i])
  new_parcels_sp$LU[i] <- names(table(this_parcel$LU)[which.max(table(this_parcel$LU))])
  if(sum(!is.na(this_parcel$YR_BUILT))==0){
    new_parcels_sp$YR_BUILT[i] <- NA
  } else{
    new_parcels_sp$YR_BUILT[i] <- as.numeric(names(table(this_parcel$YR_BUILT)[which.max(table(this_parcel$YR_BUILT))]))
  }
  if(sum(!is.na(this_parcel$YR_REMOD))==0){
    new_parcels_sp$YR_REMOD[i] <- NA
  } else{
    new_parcels_sp$YR_REMOD[i] <- as.numeric(names(table(this_parcel$YR_REMOD)[which.max(table(this_parcel$YR_REMOD))]))
  }
  ## comment out 2021 -- no building style information provided
  # if(sum(!is.na(this_parcel$R_BLDG_STYL))==0){
  # 	new_parcels_sp$R_BLDG_STYL[i] <- NA
  # } else{
  # 	new_parcels_sp$R_BLDG_STYL[i] <- names(table(this_parcel$R_BLDG_STYL)[which.max(table(this_parcel$R_BLDG_STYL))])
  #	}
  new_parcels_sp$owner_address[i] <- names(table(this_parcel$owner_address)[which.max(table(this_parcel$owner_address))])
  if("CM" %in% this_parcel$LU){
    # if this parcel has a main condo building, take that address:
    new_parcels_sp$full_address[i] <- this_parcel$full_address[this_parcel$LU=="CM"]
  } else{
    new_parcels_sp$full_address[i] <- names(table(this_parcel$full_address)[which.max(table(this_parcel$full_address))])
  }
  pb$tick()$print() # update progress bar
}
end1 <- Sys.time()
end1-start1 
#Time difference of 7.323345764 mins in 2024
# 33 minutes #Time difference of 11.91908 mins in Sep 23

new_parcels_sp_geo <- new_parcels_sp  #97894 ın Sep 23, 98150 in Feb 2023 #98445 in 2020, #98136 in 2021

new_parcels_sp_geo$TLID <- MtchedTLID$TLID[match(new_parcels_sp_geo$Land_Parcel_ID,MtchedTLID$Land_Parcel_ID)]
# 171 tlid missing in 2019 #66 missing after script 02 in 2019 

# added to have both 10 and 20, Toshi 
new_parcels_sp_geo$CT_ID_10 <- MtchedTLID$CT_ID_10[match(new_parcels_sp_geo$Land_Parcel_ID,MtchedTLID$Land_Parcel_ID)]
new_parcels_sp_geo$BG_ID_10 <- MtchedTLID$BG_ID_10[match(new_parcels_sp_geo$Land_Parcel_ID,MtchedTLID$Land_Parcel_ID)]
new_parcels_sp_geo$Blk_ID_10 <- MtchedTLID$Blk_ID_10[match(new_parcels_sp_geo$Land_Parcel_ID,MtchedTLID$Land_Parcel_ID)]

new_parcels_sp_geo$CT_ID_20 <- MtchedTLID$CT_ID_20[match(new_parcels_sp_geo$Land_Parcel_ID,MtchedTLID$Land_Parcel_ID)]
new_parcels_sp_geo$BG_ID_20 <- MtchedTLID$BG_ID_20[match(new_parcels_sp_geo$Land_Parcel_ID,MtchedTLID$Land_Parcel_ID)]
new_parcels_sp_geo$Blk_ID_20 <- MtchedTLID$Blk_ID_20[match(new_parcels_sp_geo$Land_Parcel_ID,MtchedTLID$Land_Parcel_ID)]


new_parcels_sp_geo$X <- MtchedTLID$X[match(new_parcels_sp_geo$Land_Parcel_ID,MtchedTLID$Land_Parcel_ID)]
new_parcels_sp_geo$Y <- MtchedTLID$Y[match(new_parcels_sp_geo$Land_Parcel_ID,MtchedTLID$Land_Parcel_ID)]


sum(is.na(new_parcels_sp_geo$lib))#485 in sep23 #475 missing in Feb 2023, 
#253 missing in 2019 #658 missing in 2020 # 44 missing in 2021
new_parcels_sp_geo$lib[is.na(new_parcels_sp_geo$lib)] <- 0
new_parcels_sp_geo$police[is.na(new_parcels_sp_geo$police)] <- 0
new_parcels_sp_geo$fire[is.na(new_parcels_sp_geo$fire)] <- 0
new_parcels_sp_geo$bps[is.na(new_parcels_sp_geo$bps)] <- 0
new_parcels_sp_geo$private[is.na(new_parcels_sp_geo$private)] <- 0
new_parcels_sp_geo$comcenter[is.na(new_parcels_sp_geo$comcenter)] <- 0
new_parcels_sp_geo$medhos[is.na(new_parcels_sp_geo$medhos)] <- 0
new_parcels_sp_geo$supermkt[is.na(new_parcels_sp_geo$supermkt)] <- 0
new_parcels_sp_geo$parking[is.na(new_parcels_sp_geo$parking)] <- 0
new_parcels_sp_geo$vacant[is.na(new_parcels_sp_geo$vacant)] <- 0
new_parcels_sp_geo$rel[is.na(new_parcels_sp_geo$rel)] <- 0
new_parcels_sp_geo$mbta_stop[is.na(new_parcels_sp_geo$mbta_stop)] <- 0
new_parcels_sp_geo$college[is.na(new_parcels_sp_geo$college)] <- 0
new_parcels_sp_geo$hlth_cntr[is.na(new_parcels_sp_geo$hlth_cntr)] <- 0
new_parcels_sp_geo$sub_house[is.na(new_parcels_sp_geo$sub_house)] <- 0

sum(is.na(new_parcels_sp_geo$TLID)) #120 in 2024 #93 in sep 23 #104 in 2023  #171 missing in 2019 #534 missing in 2020 #81 missing in 2021
sum(is.na(new_parcels_sp_geo$Blk_ID_20)) #4 in 2024 #2 in sep 23 65 in 2023 #137 missing in 2019 #500 in 2020 #47 missing in 2021
sum(is.na(new_parcels_sp_geo$BG_ID_20))  #4 in 2024 #2 in sep 23 65 in 2023 #139 missing in 2019 #502 in 2020 #49 missing in 2021
sum(is.na(new_parcels_sp_geo$CT_ID_20)) #4 in 2024 #2 in sep 23 65 in 2023 #137 missing in 2019 #500 in 2020  #47 missing in 2021
sum(is.na(new_parcels_sp_geo$owner_address)) # 0 missing


# reorder variables:
new_parcels_sp_geo <- new_parcels_sp_geo %>%
  dplyr::select(Land_Parcel_ID,
                full_address,
                property_N,
                unit_N, 
                unit_N_orig, 
                AV_LAND,
                AV_BLDG,
                AV_TOTAL,
                LAND_SF,
                GROSS_AREA,
                LIVING_AREA,
                LU,
                OWN_OCC,
                NUM_FLOORS,
                YR_BUILT,
                YR_REMOD,
                #R_BLDG_STYL,
                owner_address,
                comcenter, 
                medhos,
                supermkt,
                parking,
                vacant,
                rel,
                lib, 
                bps, 
                police, 
                fire, 
                private, 
                mbta_stop, # added new
                college, # added new
                hlth_cntr, # added new 
                sub_house, # added new
                X,
                Y,
                TLID,
                Blk_ID_10,
                BG_ID_10,
                CT_ID_10,
                Blk_ID_20,
                BG_ID_20,
                CT_ID_20
  )




# again tons of things in one chunk

#}
# @Alina: we run the before chunk code on Discovery and we read here the new_parcels_sp_geo; the sf package did not work this is why i run only partialy on the cluster; hopefully this will not be needed in the future

# @Michael: In 2021, I did not run the chunk prior to this on discovery; so I do not need to read this file here.
#new_parcels_sp_geo <- read.csv("C:\\Users\\bariuser2\\Google Drive\\BARI #Research Team Data Library\\Geographical Infrastructure\\Boston Geographical #Infrastructure 2020\\Outputs\\Prelim\\new_parcels_sp_geo_1.csv")

blocks <- st_read("~/Desktop/Academic/BARI/GI-2023/new datasets/Blocks_Boston_2010_BARI/Blocks_Boston BARI.shp")
blocks_2020 <- st_read("~/Desktop/Academic/BARI/GI-2023/new datasets/Block_Groups_Boston_2020/Block_Groups_Boston_2020.shp")

## adding NSA_Name and BRA_PD
## Toshi: merging by Blk_ID_10 because block 2020 version data is not available as of April 5th, 2023. 
## although block group is availalbe (block group and block are  different level of data)

new_parcels_sp_geo$NSA_Name <- NA
new_parcels_sp_geo$BRA_PD <- NA
new_parcels_sp_geo$NSA_Name <- blocks$NSA_NAME[match(new_parcels_sp_geo$Blk_ID_10,blocks$Blk_ID_10)]
new_parcels_sp_geo$BRA_PD <- blocks$BRA_PD[match(new_parcels_sp_geo$Blk_ID_10,blocks$Blk_ID_10)]

# check if we have correct number of unique Blk_ID_10 (5480 for 2018)
# if not run new_parcels_sp_geo$Blk_ID_10 <- format(new_parcels_sp_geo$Blk_ID_10, scientific = F) ## this does not change anything in 2020
#View(table(new_parcels_sp_geo$Blk_ID_10))
length(unique(new_parcels_sp_geo$Blk_ID_20)) #5156 in 2024 #5155 in Sep 23, 5156 in 2023
#5479 in 2019 #5479 in 2020 #5480 in 2021 (some pb here initialy)
length(unique(new_parcels_sp_geo$NSA_Name)) #69 in 2020 #69 in Sep 23 #69 in 2024
length(unique(new_parcels_sp_geo$BRA_PD))  #17 in 2020 #17 in Sep 23 #17 in 2024

# corrections: some unit_N are too large; manually changed; new_parcels_sp_geo$unit_N == 1271 changed to 7 because of 2019 data
#new_parcels_sp_geo[new_parcels_sp_geo$unit_N == 1742,]$unit_N <- 500
#new_parcels_sp_geo[new_parcels_sp_geo$unit_N == 1271,]$unit_N <- 7
#new_parcels_sp_geo[new_parcels_sp_geo$unit_N == 1271,]$unit_N <- 7 # we do not change this one because in the original PAD data has 1271, it is not inferred unit
#new_parcels_sp_geo[new_parcels_sp_geo$unit_N == 1040,]$unit_N <- 500
#new_parcels_sp_geo[new_parcels_sp_geo$unit_N == 1000,]$unit_N <- 0
#new_parcels_sp_geo[new_parcels_sp_geo$Land_Parcel_ID == 1303445000,]$unit_N <- 1271 # this has unit_N_orig == 1271, and unit_N is 1271 in 2018

# check if property_N>unit_N -> every property has at least one unit, so it cannot happen 
# if we check PAD unit we notice that the O's are not count for unit_N while they should be counted; this is why errors can appear
# this should be solved while creating new1 variable; in case not run here:
new_parcels_sp_geo$unit_N <- ifelse(new_parcels_sp_geo$unit_N < new_parcels_sp_geo$property_N, new_parcels_sp_geo$property_N, new_parcels_sp_geo$unit_N)

# 2020: we notice that address full name includes NA for street number and for state; we remove them;
new_parcels_test <- new_parcels_sp_geo 
new_parcels_test$full_address <- gsub(" NA ", " ", new_parcels_test$full_address) # this removes the state NA
t <- startsWith(new_parcels_test$full_address, "NA ")
new_parcels_test <- as_tibble(data.frame(cbind(new_parcels_test, t=t)))
# remove the "NA " from the beginning
new_parcels_test$full_address <- ifelse(new_parcels_test$t == TRUE, gsub("^.{0,3}", "", new_parcels_test$full_address), new_parcels_test$full_address)
new_parcels_test$t <- NULL

# 2020: we add 0 for the zipcode in fullname to be able to be geocoded in Arc (for Greg)
# in order to do that we split address and zipcode
#library(tidyr)
new_parcels_test <- new_parcels_test %>% separate(full_address, into = c('address', 'zip'), sep = -4, convert = TRUE) #we have three cases without ZIP from pad unit and this puts wrong data on the zip column # to clean and add zip were possible after checks online

# needed some cleaning
new_parcels_test$address <- gsub("49-51 NORFOLK S", "49-51 NORFOLK ST", new_parcels_test$address) 
new_parcels_test$address <- gsub("361 CENTRE S", "361 CENTRE ST", new_parcels_test$address) 
#new_parcels_test$address <- gsub("249 RUGGLE", "249 RUGGLES ST", new_parcels_test$address[which(new_parcels_test$Land_Parcel_ID ==902718013)]) # because there are 2 with same name but not st name or code
new_parcels_test$address <- ifelse(new_parcels_test$Land_Parcel_ID ==902718013, "249 RUGGLES ST", new_parcels_test$address)
new_parcels_test$zip <- ifelse(new_parcels_test$address == "49-51 NORFOLK ST", "2124", new_parcels_test$zip)
new_parcels_test$zip <- ifelse(new_parcels_test$address == "249 RUGGLES ST", "2120", new_parcels_test$zip)
new_parcels_test$zip <- ifelse(new_parcels_test$address == "361 CENTRE ST", "2130", new_parcels_test$zip)

# add 0 in zip
new_parcels_test$zip <- sprintf("0%s", new_parcels_test$zip)
new_parcels_test$full_address <- paste(new_parcels_test$address, new_parcels_test$zip)
new_parcels_test$address <- NULL
new_parcels_test$zip <- NULL

#reorder by column index
#new_parcels_test <- new_parcels_test[c(1,41,2:40)]
new_parcels_sp_geo <- new_parcels_test

# additional changes
new_parcels_sp_geo$YR_REMOD <- ifelse(new_parcels_sp_geo$Land_Parcel_ID == "601186000", "1920", new_parcels_sp_geo$YR_REMOD)
new_parcels_sp_geo$YR_REMOD <- as.numeric(new_parcels_sp_geo$YR_REMOD)

# check column types
str(new_parcels_sp_geo)

# save parcel data csv
write.csv(new_parcels_sp_geo, "~/Desktop/Academic/BARI/Scripts/GI/Outputs/Parcel_final_2024_YET.csv", row.names = FALSE, na='') #97894in Sep 23, 98150 in Feb 2023, #98179 in 2019 #98445 in 2020, # 98136, 37 in 2021
# we add na='' when writing the csv so it can be used in ArcGis (for numeric fields)


# Saving spatial parcels
#}
## MZ: this is where we are right now
parcels_shp <- st_read("~/Desktop/Academic/BARI/Scripts/GI/Inputs/Parcels_(2024)/Parcels_(2024).shp")

parcels_shp$GIS_ID <- as.numeric(parcels_shp$MAP_PAR_ID)
sum(parcels_shp$GIS_ID %in% MtchedTLID$GIS_ID) #69183 in 2019 - this was because of charact instead of numeric #98930 in 2020 #98695 in 2021 #98857 in Feb 2023 #98644 in Sep 23 #98529 in 2024
length(unique(parcels_shp$GIS_ID)) #98905 in 2019 #98903 in 2020 #98822 in 2021, #98879 in 2022 #98822 in March 2023 #98644 in Sep 23 #98532 in 2024

parcels_shp <- parcels_shp %>%
  dplyr:: select(GIS_ID)
parcels_shp$GIS_ID <- as.numeric(parcels_shp$GIS_ID)

length(unique(MtchedTLID$GIS_ID)) #98903 in 2020 #98902 in 2021 #98822  in March 2023 #98639 in Sep 23 #98529 in 2024
length(unique(MtchedTLID$Land_Parcel_ID)) #97954 in 2020 # 98231 in 2018 #97912 in 2021 #98060 in March, 2023 #97894 in Sep 23 #97766 in 2024

#setdiff(parcels_shp$GIS_ID, MtchedTLID$GIS_ID) #0
parcels_shp$Land_Parcel_ID <- MtchedTLID$Land_Parcel_ID[match(parcels_shp$GIS_ID, MtchedTLID$GIS_ID)]
sum(is.na(parcels_shp$Land_Parcel_ID))/length(parcels_shp$Land_Parcel_ID) #0.001586 not matched in 2019 #0.00279996 in 2020 #0.0025997 in 2021 #0.001001447 in March, 2023 #0.003233499 in Sep 23 #0.003196924478 in 2024
# some checks
length(unique(parcels_shp$GIS_ID))  #98822 in 2021 #98903 in 2022 #98822 in 2023 #98644 in Sep 23 #98532 in 2024
length(unique(parcels_shp$Land_Parcel_ID)) #97911 in 2021 #97954 in 2022 #98060 in 2023 #97895 in Sep 23 #97767 in 2024
#GIS_ID - Land Parcel 98903-97954 == 949 missing
# 2021: 98822 - 97911 = 911 missing

class(parcels_shp)
st_as_sf(parcels_shp)

# spatial
#a <- as(parcels_shp, 'Spatial') # define sp (rgdal) object
#writeOGR(a, dsn = "C:/Users/bariuser2/Google Drive/BARI Research Team Data Library/Geographical Infrastructure/Boston Geographical Infrastructure 2020/Outputs/BostonParcels2020_05082020", layer = "Parcels2020_06082020", driver = "ESRI Shapefile")

st_write(parcels_shp, "~/Desktop/Academic/BARI/Scripts/GI/Outputs/BARIParcels2024_YET.shp", delete_layer = TRUE)

#start_time <- Sys.time()
# mz note run in console below
## see https://github.com/r-spatial/sf/issues/306
#st_write(parcels_shp, "BARIParcels2022.shp",
#        driver = "ESRI Shapefile")
#end_time <- Sys.time()
#end_time - start_time




#}
####### the properties_file

# fixing a couple affending land parcel ids
new_parcels_sp_geo$BG_ID_20[new_parcels_sp_geo$Land_Parcel_ID==201831000] <- 250250406001
new_parcels_sp_geo$BG_ID_20[new_parcels_sp_geo$Land_Parcel_ID==201831001] <- 250250406001

# adding geo variables
PAD_unit$Land_Parcel_ID <- prpty_to_prcl$Land_Parcel_ID[match(PAD_unit$GIS_ID,prpty_to_prcl$GIS_ID)]

PAD_unit$X <- new_parcels_sp_geo$X[match(PAD_unit$Land_Parcel_ID,new_parcels_sp_geo$Land_Parcel_ID)]
PAD_unit$Y <- new_parcels_sp_geo$Y[match(PAD_unit$Land_Parcel_ID,new_parcels_sp_geo$Land_Parcel_ID)]
PAD_unit$TLID <- new_parcels_sp_geo$TLID[match(PAD_unit$Land_Parcel_ID,new_parcels_sp_geo$Land_Parcel_ID)]
PAD_unit$Blk_ID_20 <- new_parcels_sp_geo$Blk_ID_20[match(PAD_unit$Land_Parcel_ID,new_parcels_sp_geo$Land_Parcel_ID)]
PAD_unit$BG_ID_20 <- new_parcels_sp_geo$BG_ID_20[match(PAD_unit$Land_Parcel_ID,new_parcels_sp_geo$Land_Parcel_ID)]
PAD_unit$CT_ID_20 <- new_parcels_sp_geo$CT_ID_20[match(PAD_unit$Land_Parcel_ID,new_parcels_sp_geo$Land_Parcel_ID)]

# add both 10 and 20
PAD_unit$Blk_ID_10 <- new_parcels_sp_geo$Blk_ID_10[match(PAD_unit$Land_Parcel_ID,new_parcels_sp_geo$Land_Parcel_ID)]
PAD_unit$BG_ID_10 <- new_parcels_sp_geo$BG_ID_10[match(PAD_unit$Land_Parcel_ID,new_parcels_sp_geo$Land_Parcel_ID)]
PAD_unit$CT_ID_10 <- new_parcels_sp_geo$CT_ID_10[match(PAD_unit$Land_Parcel_ID,new_parcels_sp_geo$Land_Parcel_ID)]

# reordering variables to match documentation:
PAD_unit <- PAD_unit %>%
  dplyr::select(PID, 
                CM_ID,
                GIS_ID,
                ST_NUM, ST_NAME,
                UNIT_NUM, 
                ZIP_CODE,
                unit_N, 
                unit_N_orig, 
                LU,
                OWN_OCC,
                YR_BUILT,
                YR_REMOD,
                LAND_SF,
                GROSS_AREA,
                NUM_FLOORS,
                X,
                Y,
                Land_Parcel_ID,
                TLID,
                Blk_ID_10,
                BG_ID_10,
                CT_ID_10,
                Blk_ID_20,
                BG_ID_20,
                CT_ID_20
  )

blocks$Blk_ID_10 <- as.numeric(as.character(blocks$Blk_ID_10))

PAD_unit$NSA_Name <- blocks$NSA_NAME[match(PAD_unit$Blk_ID_20,blocks$Blk_ID_10)]
PAD_unit$BRA_PD <- blocks$BRA_PD[match(PAD_unit$Blk_ID_20,blocks$Blk_ID_10)]

# check if we have correct number of unique Blk_ID_10 (5480 for 2018)
# if not run new_parcels_sp_geo$Blk_ID_10 <- format(new_parcels_sp_geo$Blk_ID_10, scientific = F)
#View(table(new_parcels_sp_geo$Blk_ID_10))
length(unique(PAD_unit$Blk_ID_20)) #5480 in 2018 #5479 in 2019 #5479 in 2020 #5478 in 2021 #5156 in March, 2023 #5155 in Sep 23 #5156 in 2024

# corrections: some unit_N are too large; manually changed; PAD_unit$unit_N == 1271 changed to 7 because of 2019 data
#PAD_unit[PAD_unit$unit_N == 1742,]$unit_N <- 500
#PAD_unit[PAD_unit$unit_N == 1271,]$unit_N <- 7 # we do not change this one because in the original PAD data has 1271, it is not inferred unit
#PAD_unit[PAD_unit$unit_N == 1040,]$unit_N <- 500
#PAD_unit[PAD_unit$unit_N == 1000,]$unit_N <- 0

# add 0 in the zip column
PAD_unit$ZIP_CODE <- sprintf("%05d", PAD_unit$ZIP_CODE)

# a few more corrections
PAD_unit$YR_REMOD <- ifelse(PAD_unit$Land_Parcel_ID == "601186000", "1920", PAD_unit$YR_REMOD)
PAD_unit$YR_REMOD <- ifelse(PAD_unit$YR_REMOD == "2075", NA, PAD_unit$YR_REMOD)
PAD_unit$YR_REMOD <- ifelse(PAD_unit$YR_REMOD == "0", NA, PAD_unit$YR_REMOD)
PAD_unit$YR_REMOD <- ifelse(PAD_unit$YR_REMOD == "2", NA, PAD_unit$YR_REMOD)

PAD_unit$YR_REMOD <- as.numeric(PAD_unit$YR_REMOD)

# check the coordinates to be in wgs84 (as per last years; *** do not use wgs 84 for calculations, use a projected projection)
# if not - change proj, run the lines below
PAD_unit$ID_unique <- 1:nrow(PAD_unit)

prop_nonsf <- PAD_unit[is.na(PAD_unit$X),] #1198 in 2023 #1431 in 2022
prop_sf <- PAD_unit[!is.na(PAD_unit$X),] #175893 in 2023 #173573 in 2022
prop_sf <- st_as_sf(prop_sf, coords = c("X", "Y"), crs = 26986)
#st_crs(prop_sf)
#prop_sf2 <- st_transform(prop_sf, 4326)
prop_sf <- cbind(prop_sf, st_coordinates(prop_sf))

PAD_unit$X <- prop_sf$X[match(PAD_unit$ID_unique, prop_sf$ID_unique)]
PAD_unit$Y <- prop_sf$Y[match(PAD_unit$ID_unique, prop_sf$ID_unique)]
PAD_unit$ID_unique <- NULL

# save file
write.csv(PAD_unit, "~/Desktop/Academic/BARI/Scripts/GI/Outputs/properties_geo_2024_YET.csv", row.names = FALSE) #180627, 28 in Sep 23 # 177091, 25 in 2023
#174074 in 2019 #175004 in 2020 # 177091 in 2021 # 172266, 26 in 2022



