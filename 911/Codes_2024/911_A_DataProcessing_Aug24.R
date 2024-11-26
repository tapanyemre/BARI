#### Create full 911 call record-level file (including BARI types and geographical information) ####
# Note: In line ~73, the data is reduced to the following variables:

## AGENCY EVENT ::: ae[,c("eid","ag_id","num_1","priority","rev_num", "dgroup",
##             "sdts","ad_ts","cdts","ds_ts","en_ts","ar_ts","tr_ts","ta_ts","xdts",
##             "tycod","typ_eng","sub_tycod","sub_eng")]

## COMMON EVENT ::: ce[,c("eid","rev_num","ani_num","dow","cdts","rev_dts","udts",
##             "call_sour","clname","clrnum","cstr_add","ccity","cob_911_name","cob_911_phone","cob_911_addr",
##             "estnum","edirpre","efeanme","efeatyp","edirsuf","eapt","earea","emun","zip",
##             "ecompl","eloc_fld1","eloc_fld2","eloc_fld3","eloc_fld4",
##             "xstreet1","xstreet2","x_cord","y_cord",
##             "fea_mslink","hash","loc_com","loc_ver","loi_event","loi_inf","loi_specsit","parse_type")]

############################################################################################################
############################################################################################################
### THIS SECTION NEEDS TO BE EDITED BEFORE RUNNING THE CODE ###
#install.packages("rgdal")
install.packages("sp")
#install.packages("rgeos")
install.packages("here")
install.packages("nngeo")
install.packages("tidyverse")


# load packages -- make sure they're installed
#library(rgdal)
library(sp)
library(sf)
#library(rgeos)
library(plyr)
library(lubridate)
library(dplyr)
library(here)
library(nngeo)
library(units)
options(scipen=15)
options(digits=10)
`%notin%` <- Negate(`%in%`)

# PATHS

# read 911 raw data (i.e. the new data we received from the BPD)
# if the data is not for a full year time, we need to also add the previous data existent for the other part of the year; see previous "A" scripts
# in the following rows we do not consider "previous" data
common_event <- read.csv("~/Desktop/Academic/BARI/911/Data/911/2023_ag_common.csv")
agency_event <- read.csv("~/Desktop/Academic/BARI/911/Data/911/2023_ag_event.csv")
# common_event_22 <- read.csv("~/Desktop/Academic/BARI/911/Data/911/2022_common_event.csv")
# agency_event_22 <- read.csv("~/Desktop/Academic/BARI/911/Data/911/2022_agency_event.csv")
# common_event_m <- rbind(common_event, common_event_22)
# agency_event_m <- rbind(agency_event, agency_event_22)

# read parcels, roads, census location information
#parcels.2018 <- read.csv("C:\\Users\\User\\Google Drive\\BARI Research Team Data Library\\911 Database Working Folder\\2020\\Input\\Parcel_final_2019_13112019.csv", header=T, stringsAsFactors = F)

## due to geo problems we rerun in 02/23/2021 by using the 2018 file
#parcels.2018 <- read.csv("Parcel_final_2018_11132019.csv", header=T, stringsAsFactors = F)

#par <- st_read("C:\\Users\\User\\Google Drive\\BARI Research Team Data Library\\911 Database Working Folder\\2020\\Input\\BostonParcels2019_10282019\\Parcels2019_10282019.shp")
par.2023 <- st_read("~/Downloads/LandParcels.2023/LandParcels.2023.shp")
par_2023 <- st_read("~/Downloads/LandParcels_2023/LandParcels_2023.shp")
par <- st_read("~/Downloads/LandParcels_2023/LandParcels_2023.shp")
#parcels_buffer50m <- st_read("parcels_boundary_buffer50m.shp")
boston_boundary <- st_read("~/Desktop/Academic/BARI/911/Data/City_of_Boston_Boundary_(Water_Included)/City_of_Boston_Boundary_(Water_Included).shp")
buffer_50m <- st_buffer(boston_boundary, dist = 50)

road <- st_read("~/Desktop/Academic/BARI/911/Data/Road/Roads.2023/Roads.2023.shp")
# block
cb <- st_read("~/Desktop/Academic/BARI/911/Data/Blocks/Blocks_Boston_2020_BARI/Blocks_Boston_2020_BARI.shp")

# name for the formatted NEW data dump alone, fill in "***"
date <- as.character("Dec2023")  # today's date, for saving
save_name <- paste("911 Calls From Jan2023-", date, ".csv", sep="")

# file name for the output file of the updated 911 file (i.e. adding the new data to the previous data), starting at January 1st of the most recent incomplete year
file_start_date <- "2023-01-01 00:00:00"
save_name_updated <- paste("911 Calls_Full_Jan2023_", date, ".csv", sep="") # example file name

# type file - may need updating if the code stops in the "TYPES" section
# adding in case types to the data after case type analysis 

# read the 911 types file # I downloaded it from Dataverse from the last update
typ <- read.csv("911/data_DAN/911_Types_2014_to_2022_final.csv")
############### AR: I couldn't find the exact file type; I found one on drive and now I am testing it###########

# location to save new types file if needed
typloc <- "~/Desktop/Academic/BARI/911/Final"

###################### ****   END OF EDITING   ****   ########################################


######################################################
###### SET OPTIONS AND PACKAGES // READ IN DATA ######
######################################################

`%notin%` <- Negate(`%in%`)

### LOAD SHAPEFILES

# merge parcel csv with parcel shp for more information
colnames(par)[1] <- "Land_Parcel_ID"
#par <- merge(par, parcels.2018[,c("Land_Parcel_ID", "TLID", "Blk_ID_10", "BG_ID_10", "CT_ID_10", "NSA_Name", "BRA_PD")],   by = "Land_Parcel_ID", all.x = T)

# limit to just non-NA parcels
# par.save <- par # save a copy
# par <- par[!is.na(par$Land_Parcel_ID),]

### TRANSFORM EVERYTHING TO UTM: especially for finding nearest parcel/road, need to transform to utm (utm zone 19 for Boston)
# specify UTM projection
utmStr <- "+proj=utm +zone=19 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

# transform par, road, and cb to UTM
# first check their initial projections
st_crs(par) #4326
st_crs(road) #9001
st_crs(cb) #4326

par.utm <- st_transform(par, utmStr)
road.utm <- st_transform(road, utmStr)
table(nchar(road.utm$TLID))

### READ IN 911 DATA

# load existing data set from BARI Dataverse:
previous <- typ

# load new data sets: common event and agency event
ce <- common_event
ae <- agency_event

# remove duplicates, CE and AE to produce ce2 and ae2, then replace ce and ae with them
#remove ce dups
ce.dup.sum <- sum(duplicated(ce$eid, fromLast = T) | duplicated(ce$eid, fromLast = F))
ce.dup.eids <- unique(ce$eid[duplicated(ce$eid, fromLast = T) | duplicated(ce$eid, fromLast = F)])

ce.dup <- ce[duplicated(ce$eid, fromLast = T) | duplicated(ce$eid, fromLast = F),]
ce.dup <- ce.dup[!duplicated(ce.dup),] 

ce2 <- rbind.fill(ce[!ce$eid %in% ce.dup.eids,], ce.dup)

nrow(ce) - ce.dup.sum + nrow(ce.dup) == nrow(ce2)
stopifnot(length(unique(ce$eid)) == length(unique(ce2$eid)))

#remove ae dups
ae.dup.sum <- sum(duplicated(ae$eid, fromLast = T) | duplicated(ae$eid, fromLast = F))
ae.dup.eids <- unique(ae$eid[duplicated(ae$eid, fromLast = T) | duplicated(ae$eid, fromLast = F)])

ae.dup <- ae[duplicated(ae$eid, fromLast = T) | duplicated(ae$eid, fromLast = F),]
ae.dup <- ae.dup[!duplicated(ae.dup),] 

ae2 <- rbind.fill(ae[!ae$eid %in% ae.dup.eids,], ae.dup)

nrow(ae) - ae.dup.sum + nrow(ae.dup) == nrow(ae2)
stopifnot(length(unique(ae$eid)) == length(unique(ae2$eid)))

# save a copy, just in case
save.ce <- ce
save.ae <- ae

# replace
ce <- ce2
ae <- ae2

###################
### REDUCE DATA ###
###################

ae <- ae[,c("eid","ag_id","num_1","priority","rev_num", "dgroup",
            "sdts","ad_ts","cdts","ds_ts","en_ts","ar_ts","tr_ts","ta_ts","xdts",
            "tycod","typ_eng","sub_tycod","sub_eng")]
ce <- ce[,c("eid","rev_num","ani_num_old","dow","cdts","rev_dts","udts",
            "call_sour_old","clname_old","clrnum_old","cstr_add_old","ccity_old","cob_911_name","cob_911_phone","cob_911_addr",
            "estnum","edirpre","efeanme","efeatyp","edirsuf","eapt","earea","emun","zip",
            "ecompl","eloc_fld1","eloc_fld2","eloc_fld3","eloc_fld4",
            "xstreet1","xstreet2","x_cord","y_cord",
            "fea_mslink","hash","loc_com","loc_ver","loi_event","loi_inf","loi_specsit","parse_type")]


###################
### FORMAT DATA ###
###################

# Create date and year variable for agency event data - based on sdts
ae$date <- parse_date_time(gsub("ED", "", as.character(ae$sdts)), "ymd HMS") 
ae$year <- year(ae$date)

# format names of repeated variables (i.e. variables both in the common and agency event data sets)
names(ae)[names(ae) == "cdts"] <- "a_cdts"
names(ce)[names(ce) == "cdts"] <- "c_cdts"
names(ae)[names(ae) == "rev_num"] <- "a_rev_num"
names(ce)[names(ce) == "rev_num"] <- "c_rev_num"

# format subtype and type and their text descriptions  
ae$tycod <- toupper(as.character(ae$tycod))
ae$sub_tycod <- toupper(as.character(ae$sub_tycod))
ae$typ_eng <- toupper(as.character(ae$typ_eng))
ae$sub_eng <- toupper(as.character(ae$sub_eng))

# create combined, more specific and finalized TYPE
ae$TYPE <- paste(ae$tycod, ae$sub_tycod, sep="")

########################################################
#### MAKE SURE BOTH DATA SETS SPAN THE SAME PERIOD  ####
########################################################

###### not running the following for the 2020 update ######

## Note: there are duplicates, but will keep both records, using eid as the unique identifier

# # make sure same number of unique eid's in both agency_event and common_event, usually year should be enough
# last.common.eid <- min(c(max(ae$eid), max(ce$eid)))
# first.eid <- max(previous$eid)
# 
# # remove those with dates after the end of the "common event" data set (just have different dates because of when the data was pulled)
# ae <- ae[ae$eid <= last.common.eid & ae$eid > first.eid,] # the problem is from ae$eid > last.common.eid
# ce <- ce[ce$eid <= last.common.eid & ce$eid > first.eid,]
# 
# # ( NOTE: THERE MAY BE MORE EIDS IN AE THAN CE, in March 2017 - Jan 2019, there are 1705 more - so they just won't have geo information -- see e-mails with Dan 2/14-15/19)
# 
# # be sure that the sets of eid's are identical
# stopifnot(length(unique(ae$eid)) == length(unique(ce$eid)))
# stopifnot(identical(sort(unique(ae$eid)), sort(unique(ce$eid))))

length(setdiff(ae$eid, ce$eid)) #1703; the opposite is 0; they were removed (this was before the 2020 update) #in 2020 run value is 0
#ae <- ae[ae$eid %in% ce$eid,]
# in 2020
# ae 1043537 1292296
# ce 1037937 1285441

### Note 2020: we notice two eid from 2018 for the 2019-2020 data
#View(ae[ae$year == 2018,]) #3804281 and 3804280
#we change them to 2019
ae$year <- ifelse(ae$eid == 3804281, 2019, ae$year)
ae$year <- ifelse(ae$eid == 3804280, 2019, ae$year)


#################################################################################
###### CHECK IF THERE ARE NEW TYPES AND MERGE TYPE CATEGORIES WITH DATA #########
#################################################################################
#### check to make sure that types match ####

# needs attention if stopped: are all the types in the new data in the 2016 types file? 
# Note 2020: here it stops because in ae we have 314 TYPE, while in the data 2014-2018 we have 301 unique TYPE
stopifnot(ae$TYPE %in% typ$TYPE)
length(setdiff(ae$TYPE, typ$TYPE))
length(setdiff(typ$TYPE, ae$TYPE))
x1 <- ae %>% group_by(TYPE) %>% dplyr::summarise(n()) # for 2019-2020 there are 322 obs ##2021 - 337 obs
x2 <- typ %>% group_by(TYPE) %>% dplyr::summarise(n()) # for 2014-2020 half: there are 341 obs ##2021 - 353 obs
#x3 <- calls_2018_JanJun %>% group_by(TYPE) %>% dplyr::summarise(n())  #AR: these are already included in the TYPE calc
#x4 <- calls_2018_Jul %>% group_by(TYPE) %>% dplyr::summarise(n()) #AR: these are already included in the TYPE calc
#x5 <- rbind(x3,x4)
#View(setdiff(x1$TYPE, x5$TYPE))
#View(setdiff(x2$TYPE, x1$TYPE))

#View(table(ae$TYPE))
#View(table(typ$TYPE))

#### CREATE NEW TYPE FILE HERE IF NEEDED ####
length(unique(ae$TYPE[!ae$TYPE %in% typ$TYPE])) #6 in 2021 January update same in 2021 full year update # 23 in 2024
new.typ <- unique(ae$TYPE[!ae$TYPE %in% typ$TYPE]) # need to see if these affect ecometrics; do any belong in current ecometrics categories? Also, need to create new "types" file.

# if new types
ae.typ <- data.frame(distinct(ae[ae$TYPE %in% new.typ , c("TYPE", "tycod", "typ_eng", "sub_tycod", "sub_eng")]), 
                     SocDis = 0, PrivateConflict = 0, Violence = 0, Guns = 0, Used = 0)
ae.typ

#### create new file 
typs.new <- rbind.fill(typ, ae.typ)
typs.new <- typs.new[order(typs.new$TYPE),]
head(typs.new)

### Note 2020: We do not find in the code above where we add info for SocDis, Priv Confl, Viol, Guns
### We add info in the type file for now - create new definitions based on the 911 Documentation for 2019

names_types <- c("DISTRBDRUNKS", "DISTRBPANHAN", "IVPERLEWD")
typs.new$SocDis <- ifelse(typs.new$TYPE %in% names_types, 1, 0)

names_types <- c("AB===>>>", "ABIP", "ABRPT", "ARMROBDEFAULT", "EDP2", "FIGHTDEFAULT", "PKNIFEDEFAULT")
typs.new$Violence <- ifelse(typs.new$TYPE %in% names_types, 1, 0)

names_types <- c("LANTENDEFAULT")
typs.new$PrivateConflict <- ifelse(typs.new$TYPE %in% names_types, 1, 0)

names_types <- c("ABDWIP", "PERGUNDEFAULT", "SHOTSDEFAULT", "PSHOTDEFAULT", "FDWEAPGUN")
typs.new$Guns <- ifelse(typs.new$TYPE %in% names_types, 1, 0)


#### NEW 2024 ####
names_types <- unique(ae$TYPE[grepl('^LARC',ae$TYPE)])
typs.new$Larceny <- ifelse(typs.new$TYPE %in% names_types, 1, 0)

names_types <- unique(ae$TYPE[grepl("^BE", ae$TYPE)])
typs.new$Burglary <- ifelse(typs.new$TYPE %in% names_types, 1, 0)

names_types <- unique(ae$TYPE[grepl('^VAN',ae$TYPE)])
typs.new$Vandalism <- ifelse(typs.new$TYPE %in% names_types, 1, 0)

names_types <- c(unique(ae$TYPE[grepl('^ROB',ae$TYPE)]), 'ARMROBDEFAULT')
typs.new$Robbery <- ifelse(typs.new$TYPE %in% names_types, 1, 0)



# put together with rest of 2019 to get accurate 2019 frequencies and last yr/etc.
# the 2020 update starts in January 2019 so we do not use the "previous"
#d911 <- rbind.fill(previous, ae)
#
d911 <- ae
head(d911)

types <- typs.new$TYPE
length(types) == nrow(typs.new)
# typs.new$Frequency_2019 <- NA
typs.new$Frequency_2022 <- NA
typs.new$Frequency_2023 <- NA
typs.new$yr.intro2 <- NA
typs.new$last.yr2 <- NA


for (i in 1:length(types)){
  typs.new$yr.intro2[i] <- ifelse(is.na(typs.new$yr.intro[i]),
                                  min(d911$year[d911$TYPE == types[i]]),
                                  typs.new$yr.intro[i])
  typs.new$last.yr2[i] <- ifelse(types[i] %in% d911$TYPE,
                                 max(d911$year[d911$TYPE == types[i]]),
                                 typs.new$last.yr[i])
  # typs.new$Frequency_2019[i] <- ifelse(types[i] %in% d911$TYPE,
  #                                      length(unique(d911$eid[d911$TYPE == types[i] & d911$year == 2019])),0)
  typs.new$Frequency_2022[i] <- ifelse(types[i] %in% d911$TYPE,
                                      length(unique(d911$eid[d911$TYPE == types[i] & d911$year == 2022])),
                                                                            0)
  typs.new$Frequency_2023[i] <- ifelse(types[i] %in% d911$TYPE,
                                       length(unique(d911$eid[d911$TYPE == types[i] & d911$year == 2023])),
                                       0)
}
plyr::count(typs.new, c("yr.intro", "yr.intro2","last.yr", "last.yr2"))

#it does not work, yr.intro and last.yr do not exist # in 2020 confirm the past note, it does not work # it worked for Jan 2021 Update

# replace yr.intro with yr.intro2 and remove
typs.new$yr.intro <- NULL
typs.new$last.yr <- NULL
typs.new <- typs.new %>% dplyr::rename(last.yr = last.yr2)
typs.new <- typs.new %>% dplyr::rename(yr.intro = yr.intro2)

### Note 2020: the "used" column is not populated; we add 1 if it is one of the ecometrics
typs.new$Used <- ifelse(typs.new$SocDis == 1 | typs.new$PrivateConflict == 1 | typs.new$Violence == 1 | typs.new$Guns == 1 | 
                        typs.new$Larceny == 1 | typs.new$Burglary == 1 | typs.new$Vandalism == 1 | typs.new$Robbery == 1, 1, 0)

# # get only the ones which exist in 2019 or 2020
# typs.new1 <- typs.new[!is.infinite(typs.new$yr.intro),]
# # typs.new1_2019 <- typs.new1[typs.new1$last.yr == 2019,]
# typs.new1_2022 <- typs.new1[typs.new1$last.yr == 2022,]
# typs.new1_2023 <- typs.new1[typs.new1$last.yr == 2023,]


### Note 2020: Adding types from the previous years dataframe typ
# typs_join <- typ
# # typs_join$Frequency_2019 <- typs.new$Frequency_2019[match(typs_join$TYPE, typs.new1$TYPE)]
# typs_join$Frequency_2022 <- typs.new$Frequency_2022[match(typs_join$TYPE, typs.new1$TYPE)]
# typs_join$Frequency_2023 <- typs.new$Frequency_2023[match(typs_join$TYPE, typs.new1$TYPE)]
# 
# # typs_join$last.yr <- ifelse(typs_join$TYPE %in% typs.new1_2019$TYPE, 2019, typs_join$last.yr)
# typs_join$last.yr <- ifelse(typs_join$TYPE %in% typs.new1_2022$TYPE, 2022, typs_join$last.yr)
# typs_join$last.yr <- ifelse(typs_join$TYPE %in% typs.new1_2023$TYPE, 2023, typs_join$last.yr)
# 
# 
# typs_to_add <- typs.new[typs.new$TYPE %notin% typs_join$TYPE,]
# 
# typs_to_add <- typs_to_add %>%
#   mutate(Frequency_2014 = 0, Frequency_2015 = 0, Frequency_2016 = 0, Frequency_2017 = 0, Frequency_2018 = 0) %>%
#   dplyr::select(TYPE, tycod, typ_eng, sub_tycod, sub_eng, SocDis, PrivateConflict, Violence, Guns, Larceny, Burglary, Vandalism, Robbery,
#          Frequency_2015, Frequency_2016, Frequency_2017, Frequency_2018, Frequency_2019, Frequency_2020, Frequency_2021, Frequency_2022, Frequency_2023, yr.intro, last.yr)
#   
# typs_join <- typs_join %>%
#   dplyr::select(TYPE, tycod, typ_eng, sub_tycod, sub_eng, SocDis, PrivateConflict, Violence, Guns, Larceny, Burglary, Vandalism, Robbery,
#          Frequency_2015, Frequency_2016, Frequency_2017, Frequency_2018, Frequency_2019, Frequency_2020, Frequency_2021, Frequency_2022, Frequency_2023, yr.intro, last.yr)
# 
# typs_join <- rbind(typs_join, typs_to_add)
# 
# #sort dataframe by col
# typs_join <- with(typs_join,  typs_join[order(TYPE), ])


cols <- c("Frequency_2015", "Frequency_2016", "Frequency_2017", 
          "Frequency_2018", "Frequency_2019", "Frequency_2020", 
          "Frequency_2021")

typs.new[cols] <- lapply(typs.new[cols], function(x) ifelse(is.na(x), 0, x))


order <- c("TYPE"  ,          "tycod"      ,     "typ_eng"  ,       "sub_tycod"      , "sub_eng",
           "Frequency_2015", "Frequency_2016", "Frequency_2017", 
           "Frequency_2018", "Frequency_2019", "Frequency_2020", 
           "Frequency_2021", "Frequency_2022", "Frequency_2023", "yr.intro", "last.yr",
           "PrivateConflict", "Violence"  ,      "SocDis"  ,        "Guns"  ,          "Larceny"      ,   "Burglary", 
           "Vandalism"  ,     "Robbery"   ,      "Used"  )
typs.new <- typs.new[,order]
setwd(typloc)


write.csv(typs.new, paste("911_Types_Jan2014_to_", date, ".csv", sep = ""), row.names = F)

# check if ECOMETRICS CATEGORIES types are all still used
stopifnot(typ$TYPE[typ$Used==1] %in% ae$TYPE)

### given that there are not any "types" problems, can run the rest of the code smoothly
# note AR: there are new types so the code is not running smoothly above; it needs manual checks; after checks it works!

################################################
###### MERGE TYPE CATEGORIES WITH DATA #########
################################################

# merge agency event and types files so that ecometrics categories are added to the data
ae <- merge(ae, 
            #typ[,c("TYPE", "SocDis", "PrivateConflict", "Violence", "Guns")], # NOTE AR: typ does not include the ecometrics; change to typs.new
            typs.new[,c("TYPE", "SocDis", "PrivateConflict", "Violence", "Guns", "Larceny", "Burglary",  "Vandalism", "Robbery")], # NOTE AR: typ does not include the ecometrics; change to typs.new
            by="TYPE", all.x = T)

### SAVE A COPY
save.ae2 <- ae
save.ce2 <- ce

## colnames
colnames(ae)
colnames(ce)

################################################
######### ADD GEOGRAPHICAL INFORMATION #########
################################################

# format ce geographic data; assign NA to those with =0 coordinates
stopifnot(sum(is.na(ce$x_cord))==0)
stopifnot(sum(is.na(ce$y_cord))==0)

ce$y_cord[ce$y_cord==0] <- NA
ce$x_cord[ce$x_cord==0] <- NA

# limit to geographic information and event id, for those with geo info
ce.togeo <- ce[!is.na(ce$x_cord) & !is.na(ce$y_cord), c("eid","x_cord","y_cord")]
#nrow(ce.togeo) == 1203081 #before 2020 update #984687 in 2020 update

# correct coordinates
ce.togeo$x_fix <- ce.togeo$x_cord / 100
ce.togeo$y_fix <- ce.togeo$y_cord / 100
ce.togeo$x_cord <- NULL
ce.togeo$y_cord <- NULL

# change coordinate system from NAD83 (MA: 2001) [espg: 2249] to lat/long [espg:4326] ### OLD ###
ce.togeo.proj <- st_as_sf(ce.togeo, coords = c("x_fix", "y_fix"), crs = 2249) # transform to spatial data set
class(ce.togeo)
st_crs(ce.togeo.proj) #9003
ce.togeo.proj <- st_transform(ce.togeo.proj, 4326) 

plot(ce.togeo.proj[1])

################################################
######### REMOVE POINTS OUTSIDE A 50M BUFFER AROUND BOSTON CITY BOUNDARY #########
################################################

buffer_50m <- buffer_50m$geometry
buffer_50m <- st_transform(buffer_50m, st_crs(ce.togeo.proj))

# Check which points are within the 50m buffer
within_buffer_logical <- apply(st_within(ce.togeo.proj, buffer_50m, sparse = FALSE), 1, any)

# Filter the points based on the logical vector
ce.togeo.proj1 <- ce.togeo.proj[within_buffer_logical, ]


# overlap the 911 point data with the parcel data (be sure to have the same projection) # time optimized ~3min (from 17 min) ~4.5
st_crs(par)
st_crs(par.2023)
par.2023 <- st_transform(par.2023, crs = 4326)
par.2023 <- st_transform(par, st_crs(ce.togeo.proj1))
par.2023_join <- par.2023[,c("Land_Parcel_ID","TLID" ,"B_ID_10", "BG_ID_1", "CT_ID_1", "B_ID_20", "BG_ID_2","CT_ID_2", "NSA_Nam", "BRA_PD")]
geo.data_par.2023 <- st_join(ce.togeo.proj1, par.2023_join, join = st_nearest_feature, maxdist=100)

par <- st_transform(par, st_crs(ce.togeo.proj1))
names(par)[1] <- "Land_Parcel_ID"
par_join <- par[,c("Land_Parcel_ID","TLID" ,"B_ID_10", "BG_ID_1", "CT_ID_1", "B_ID_20", "BG_ID_2","CT_ID_2", "NSA_Nam", "BRA_PD")]
start_time <- Sys.time()

# not all the data will match to a parcel
#geo.data1 <- st_intersection(ce.togeo.proj1, par_join) # problems with the polygons
geo.data2 <- st_join(ce.togeo.proj1, par_join, join = st_nearest_feature, maxdist=100)
geo.data_within <- st_join(ce.togeo.proj1, par_join, join = st_within)
end_time <- Sys.time()
end_time - start_time

geo.data <- geo.data2
# merge the 911 calls with geo info with the initial ones that did not intersect
geo.data$geometry <- NULL
start_time <- Sys.time()
ce.geo <- merge(ce.togeo.proj1, geo.data, by = "eid", all.x = TRUE) #left_join(ce.togeo.proj1, geo.data, by="eid", all.x=T)
end_time <- Sys.time()
end_time - start_time




# # save this original ce.geo
# ce.geo.save <- ce.geo
# sum(duplicated(ce.geo)) #1321  in the 2020 update #3690 in 2019-2020 full data update ##1863 in 2021 update
# 
# # isolate the data that did match (ce.geo1) and did not match to a parcel (ce.geo2)
# ce.geo1 <- ce.geo[!is.na(ce.geo$Land_Parcel_ID),] # 876609 in the 2019-2020 full data update, #412,640 in 2021
# # remove duplicates
# ce.geo1 <- ce.geo1[!duplicated(ce.geo1),] #872919
# ce.geo2 <- ce.geo[is.na(ce.geo$Land_Parcel_ID),] # 340679 in the 2019-2020 full data update, #146,047 in 2021
# # remove duplicates
# ce.geo2 <- ce.geo2[!duplicated(ce.geo2),] 
# 
# # Create subset of just the unique X/Y coordinate pairs for faster nearest-place calculation
# ce.geo2$x_fix <- ce.togeo$x_fix[match(ce.geo2$eid, ce.togeo$eid)]
# ce.geo2$y_fix <- ce.togeo$y_fix[match(ce.geo2$eid, ce.togeo$eid)]
# 
# 
# #check.dat.uni <- ce.geo2[,c("eid","x_fix", "y_fix")]
# check.dat.uni <- ce.geo2[,c("eid","geometry")]
# #check.dat.uni <- check.dat.uni[!duplicated(check.dat.uni[,c("x_fix", "y_fix")]),]
# check.dat.uni <- check.dat.uni[!duplicated(check.dat.uni[,c("geometry")]),]
# length(unique(check.dat.uni$eid)) #84022 in 2020 update #102209 Jan 2021 (102004 by using parcels 2018)
# 
# # create utm projection of the 911 event points (works best with the nearest-parcel code for everything to be projected in UTM)
# ce.togeo.proj.utm <- st_transform(ce.togeo.proj, utmStr)
# 
# # using utm projection, subset the projection with just the points with unique X/Y 
# check.dat.uni.proj <- ce.togeo.proj.utm[ce.togeo.proj.utm$eid %in% check.dat.uni$eid,]
# nrow(check.dat.uni.proj) == 84022 # this is for 2019-2020 data #it was 86478 #102209 #102004, ##59,487 in 2021


################################################
######### GET NEAREST PARCEL #########
################################################

# NOTE for 2020 update and 2021 January (Alina):
# There are two versions below for R. Both of them are extremely slow (it can take two days for one line to run; Time difference of 1.199417052 days for version 2);
# Thus we add VERSION 3, which is the result from QGIS. It is a lot faster. --> PAY ATTENTION TO THE RESULTS; problems with Land Parcels and others having NA

# The way we made it work:
# We make sure the points and polygons are on epsg 26986 NAD 83 Mass Mainland
check.dat.uni.proj <- st_transform(check.dat.uni.proj, 26986)
par.utm <- st_transform(par.utm, 26986)  
par.utm <- st_make_valid(par.utm)
# we save them as csv file and add them in QGIS, which is the fastest approach found ~30 seconds
st_write(check.dat.uni.proj, "check.dat.uni.proj_02252022.shp")
st_write(par.utm, "par.utm_02252022.shp")
# We use the function "Join attributes by nearest" (!!!! the "Distance to nearest hub (points)" does not have good results )
# Remove n > 1 
# save as csv and bring it back to R
# in R remove distance > 100
# In 2021 January update we report 204 differences between results from R and Qgis for Land Parcel; we continue on the Qgis result

## Conclusion: we only use VERSION 3 ##

#################################### VERSION 1 ######################################

# function creating buffers around each 911 point and trying to find the shortest distance between the parcels intersecting the buffer
# this works in the GI

# points_to_near_poly <- function(points_transf, poly_transf){
#   # transform points and polygons
#   points <- st_as_sf(points_transf)
#   polygons <- st_as_sf(poly_transf)
#   # buffer around points
#   points_buffer <- st_buffer(points, dist = 100)
#   # create new variable for the joined info points and poly
#   mixed_var <- data.frame(matrix(ncol = dim(points)[2]+dim(polygons)[2]-1, nrow = dim(points)[1]))
#   for (i in 1:dim(points)[1]){
#     intersect_buffer_poly <- st_intersection(polygons, points_buffer[i,])
#     shortdist <- set_units(10000000000,cm)
#     if (dim(intersect_buffer_poly)[1] != 0){
#       for (j in 1:dim(intersect_buffer_poly)[1]){
#         dst <- st_distance(points[i,], intersect_buffer_poly[j,])#,byid=TRUE)
#         if (dst < shortdist){
#           shortdist <- dst
#           mixed_var[i,] <- intersect_buffer_poly[j,]
#         }
#       }
#     } else {
#       # change here according to the dataframe size
#       mixed_var[i,2:9] <- points[i,]
#     }
#   }
#   return(mixed_var)
# }
# 
# # calculate using the above function
# # this takes about two days to run
# start1 <- Sys.time()
# a <- points_to_near_poly(check.dat.uni.proj, par.utm)
# end1 <- Sys.time()
# end1-start1
# 
# # the function is losing the headers; add names to header (eid from 911 data + names from par.utm)
# names = c(colnames(par.utm))
# colnames(a) = c(par.utm, "eid")
# 
#################################### VERSION 2 ######################################
# 
# # calculate the nearest neighbor of the 911 call location to parcels polygons
# # this also takes about one-two days to run # 1.871655118 mins for 100 points -> so ~26.2 hours
# 
# start1 <- Sys.time()
# a <- st_join(check.dat.uni.proj, par.utm, join = st_nn, k = 1, maxdist = 100)
# end1 <- Sys.time()
# end1-start1

#################################### VERSION 3 ######################################

landparcel_Qgis <- read.csv("check.dat.uni.proj_LandParcel_02252022.Qgis.csv")
# remove distance > 100 and keep only eid and land parcel
landparcel_Qgis <- landparcel_Qgis %>% 
  filter(distance < 100) %>%
  dplyr::select(eid, Ln_P_ID)


##merge the landparcel_qgis data to the original check.dat.uni.proj dataset now that there are matches
#check.dat.uni.proj <- merge(landparcel_Qgis, check.dat.uni.proj, by = "eid")

##check for na's in the land_parcel_id field
#colSums(is.na(check.dat.uni.proj))

check.dat.uni.proj$Land_Parcel_ID <- landparcel_Qgis$Ln_P_ID[match(check.dat.uni.proj$eid, landparcel_Qgis$eid)]
check.dat.uni.proj <- check.dat.uni.proj[!is.na(check.dat.uni.proj$Land_Parcel_ID),]

################################################
######### GET NEAREST ROAD #########
################################################

# NOTE for 2020 update and 2021 January (Alina):
# we only use VERSION 3: please read the explanations for this above at the Land Parcel side.

# We make sure the points and polygons are on epsg 26986 NAD 83 Mass Mainland
check.dat.uni.proj <- st_as_sf(check.dat.uni.proj)
check.dat.uni.proj <- st_transform(check.dat.uni.proj, 26986)
road.utm <- st_transform(road.utm, 26986)  
road.utm <- st_make_valid(road.utm)
st_write(road.utm, "road.utm_02252022.shp")
st_write(check.dat.uni.proj, "check.dat.uni.proj.wpar_02252022.shp")

#################################### VERSION 2 ######################################
# 
# # calculate the nearest neighbor of the 911 call location to roads
# # this also takes about Time difference of 6.598986403 hours
# 
# start1 <- Sys.time()
# tlid_version2 <- st_join(check.dat.uni.proj, road.utm, join = st_nn, k = 1, maxdist = 100)
# end1 <- Sys.time()
# end1-start1

#check.dat.uni.proj$TLID <- tlid_version2$TLID[match(check.dat.uni.proj$eid, tlid_version2$eid)]

#################################### VERSION 3 ######################################

tlid_Qgis <- read.csv("check.dat.uni.proj_TLID_02252022_Qgis.csv")
# remove distance > 100 and keep only eid and tlid
tlid_Qgis <- tlid_Qgis %>% 
  filter(distance < 100) %>%
  dplyr::select(eid, TLID)

check.dat.uni.proj$TLID <- tlid_Qgis$TLID[match(check.dat.uni.proj$eid, tlid_Qgis$eid)]

#################################### VERSION 4 ######################################

road.utm <- st_transform(road.utm, st_crs(ce.togeo.proj1))
road_join <- road.utm[,c("TLID" , "BG_ID_1", "CT_ID_1", "BG_ID_2","CT_ID_2")]

geo.data_road <- st_join(ce.togeo.proj1, road_join, join = st_nearest_feature, maxdist=100)


################################################
######### GET SEPARATE CENSUS BLOCK, BLOCK GROUP, AND TRACT INFORMATION #########
################################################

# change coordinate system from that of check.dat.uni.proj to census block
cb <- st_transform(cb, 26986)
#ce.togeo.proj.cb <- spTransform(check.dat.uni.proj, proj4string(cb)) 
ce.togeo.proj.cb <- check.dat.uni.proj
ce.togeo.proj.cb <- st_transform(ce.togeo.proj.cb, 26986)

# overlap the 911 data with the census block data
#geo.data.cb <- over(ce.togeo.proj.cb, cb[,c("Blk_ID_10","BG_ID_10","CT_ID_10", "NSA_NAME", "BRA_PD")])
ce.geo.cb <- st_intersection(ce.togeo.proj.cb, cb[,c("Blk_ID_10","BG_ID_10","CT_ID_10", "NSA_NAME", "BRA_PD")]) #101,404 joined from 101,987
# if they do not intersect, then st_intersection will not consider them as output
# try to use st_intersects, it did not work well

# NEW: 02232021 -- we add the values of land parcel and tlid even if there are no block groups
ce.geo.cb$geometry <- NULL
ce.geo.cb1 <- left_join(ce.togeo.proj.cb, ce.geo.cb[,c("eid", "Blk_ID_10","BG_ID_10","CT_ID_10", "NSA_NAME", "BRA_PD")], by = "eid")

# if from the intersection we did not get block group we are trying the nearest neighbor
ce.geo.cb_noBlk <- ce.geo.cb1[is.na(ce.geo.cb1$Blk_ID_10),]
start1 <- Sys.time()
ce.geo.cb_neighbor <- st_join(ce.togeo.proj.cb, ce.geo.cb_noBlk[,c("eid", "Blk_ID_10","BG_ID_10","CT_ID_10", "NSA_NAME", "BRA_PD")], join = st_nn, k = 1, maxdist = 100)
end1 <- Sys.time()
end1-start1



##get blocks for those without blocks - load common events without blocks into qgis - ce.geo.cb_neighbor
st_write(ce.geo.cb_neighbor, "ce.geo.cb_neighbor.shp")
##load the blocks data into qgis - BlocksBostonBARI
# the version above takes long time for spatial join; we join in QGIS using the function "Join attributes by nearest"
blk_Qgis <- read.csv("check.dat.uni.proj_BlkBGCT_02252022_Qgis.csv")
# remove distance > 100 and keep only eid and tlid
blk_Qgis <- rename(blk_Qgis, eid = eid_x)
blk_Qgis <- blk_Qgis %>% 
  filter(distance < 100) %>%
  dplyr::select(eid, Blk_ID_10, BG_ID_10, CT_ID_10, NSA_NAME, BRA_PD)

ce.geo.cb1$Blk_ID_10 <- blk_Qgis$Blk_ID_10[match(ce.geo.cb1$eid, blk_Qgis$eid)]
ce.geo.cb1$BG_ID_10 <- blk_Qgis$BG_ID_10[match(ce.geo.cb1$eid, blk_Qgis$eid)]
ce.geo.cb1$CT_ID_10 <- blk_Qgis$CT_ID_10[match(ce.geo.cb1$eid, blk_Qgis$eid)]
ce.geo.cb1$NSA_NAME <- blk_Qgis$NSA_NAME[match(ce.geo.cb1$eid, blk_Qgis$eid)]
ce.geo.cb1$BRA_PD <- blk_Qgis$BRA_PD[match(ce.geo.cb1$eid, blk_Qgis$eid)]

# we notice 380 joined to different Blk in Qgis compared with R; we visually inspect the map in Qgis and we decide QGis did a better job, the blocks are correct
#View(table(ce.geo.cb1$CT_ID_10))

## ADD TO THE OTHER GEOGRAPHIC DATA

# save the projection version
save.proj <- ce.togeo.proj.cb # this includes the eid, land parcel and tlid
ce.geo.cb1$geometry <- NULL

# add Ln_P_ID and TLID to the unique X,Y and eid data (i.e. to check.dat.uni)
near.data1 <- merge(check.dat.uni, #this contains all the eid to join
                    ce.geo.cb1, # this contains the intersection between eids and blocks and nearest neighbor; data from Qgis, see above
                    by = "eid", all.x = T)

near.data1$geometry <- NULL 
near.data1$x_fix <- ce.geo2$x_fix[match(near.data1$eid, ce.geo2$eid)]
near.data1$y_fix <- ce.geo2$y_fix[match(near.data1$eid, ce.geo2$eid)]

# add Ln_P_ID, etc, to the non-unique data based on X,Y
near.data <- merge(ce.geo2[,c("eid", "x_fix", "y_fix")], near.data1[,-1],
                   by = c("x_fix", "y_fix"), all.x = T)

summary(near.data)

# remove any duplicate rows
near.data <- near.data[!duplicated(near.data),]
stopifnot(nrow(ce.geo2) == nrow(near.data))

# indicate whether it was a spatial join ( = 2 if nearest) for near.data and other data (ce.geo1)
near.data$sj_cat <- 2
ce.geo1$sj_cat <- 1

# put together with other data: (1) those records that didn't need the nearest procedure, (2) those that used spatial join and nearest
ce.g <- rbind.fill(near.data, ce.geo1)

ce.g$x_cord <- NULL
ce.g$y_cord <- NULL
sum(is.na(ce.g$Land_Parcel_ID))

# merge full geographic data with full common event data
ce.geo.full <- merge(ce, ce.g, by="eid", all.x=T) # it takes a few minutes
nrow(ce.geo.full) == nrow(ce) # not true in 2020

### because of all the changes now we have geometry instead of lat long
ce.geo.full_1 <- st_as_sf(ce.geo.full)
coord <- st_coordinates(ce.geo.full_1)
ce.geo.full_1 <- cbind(ce.geo.full_1, coord)

# rename coordinates to lat/long
colnames(ce.geo.full_1)[colnames(ce.geo.full_1)=="X"] <- "long"
colnames(ce.geo.full_1)[colnames(ce.geo.full_1)=="Y"] <- "lat"

ce.geo.full <- ce.geo.full_1
sum(is.na(ce.geo.full$Land_Parcel_ID)) #72.363 now for 2021 - 33,151
sum(is.na(ce.geo.full$long)) #72.297 now for 2021 - 33,118
sum(is.na(ce.geo.full$x_cord)) #65.286 now for 2021 - 32,358


##### alternative way #####

cb_join <- cb[,c( "GEOID20", "BG_ID_20", "CT_ID_20","NSA_NAME", "BPDA_PD")]

# Reproject both datasets to a planar coordinate system (e.g., UTM)
ce.togeo.proj1_utm <- st_transform(ce.togeo.proj1, crs = 32633)  # Replace with appropriate UTM zone
cb_join_utm <- st_transform(cb_join, crs = 32633) 

geo.data_cb <- st_join(ce.togeo.proj1_utm, cb_join_utm, join = st_intersects)


##################################################################
###### SOME POSSIBLE CHECKS ######
##################################################################

# percent unmatched to a Blk that have lat/long information
nrow(ce.geo.full[is.na(ce.geo.full$Blk_ID_10) & !is.na(ce.geo.full$long),])/sum(!is.na(ce.geo.full$long)) #0.0205 in 2020 update
# 0.00007580763976 in 2020 update, multiple runs
##0.00008009255139 in 2021 update

# review some of the data
colnames(ce.geo.full)
summary(ce.geo.full)
tail(ce.geo.full,20)

### possible check: map the data outside of the boundary (there should not be any point outside)

# identify points un-matched with a census block
check.dat <- ce.geo.full[!is.na(ce.geo.full$long) & !is.na(ce.geo.full$lat) & ce.geo.full$sj.grp == 2, ] #0 same for 2021
check.map <- ce.togeo.proj[ce.togeo.proj$eid %in% check.dat$eid,] #0 same for 2021
#head(check.map)

## if 0 before, the following will not run ##

# random sample of points in a few parcel/blks
samp <- check.dat[check.dat$eid %in% sample(check.dat$eid[!is.na(check.dat$BG_ID_10)], 2, replace = FALSE),]

plot(par[!is.na(par$BG_ID_10) & par$BG_ID_10 == samp$BG_ID_10[1],])
plot(par[par$Land_Parcel_ID == samp$Land_Parcel_ID[1],], add = T, col = "pink")
plot(ce.togeo.proj[ce.togeo.proj@data$eid %in% check.dat$eid[check.dat$Land_Parcel_ID == samp$Land_Parcel_ID[1]],], add = T, col = "red")

plot(par[!is.na(par$BG_ID_10) & par$BG_ID_10 == samp$BG_ID_10[2],])
plot(par[par$Land_Parcel_ID == samp$Land_Parcel_ID[2],], add = T, col = "green")
plot(ce.togeo.proj[ce.togeo.proj@data$eid %in% check.dat$eid[check.dat$Land_Parcel_ID == samp$Land_Parcel_ID[2]],], add = T, col = "blue")

# map of unmappable points
sum(!is.na(ce.geo.full$long) & is.na(ce.geo.full$Blk_ID_10)) #92
View(ce.geo.full[!is.na(ce.geo.full$long) & is.na(ce.geo.full$Blk_ID_10),]) 


#plot(cb)
#plot(ce.togeo.proj.cb[ce.togeo.proj.cb$eid %in% ce.geo.full$eid[!is.na(ce.geo.full$long) & is.na(ce.geo.full$Blk_ID_10)],], add = T, col = "red")

##################################################################
###### FINAL STEP: MERGE AGENCY EVENT AND COMMON EVENT DATA ######
##################################################################
ce.geo.full <- ce.geo
stopifnot(sum(colnames(ae) %in% colnames(ce.geo.full)) == 1) # just "eid" overlaps

# merge to create final record-level data set # takes a few minutes
data911 <- merge(ae, ce.geo.full, by="eid", all = T) 
data911$date <- parse_date_time(gsub("ED", "", as.character(data911$sdts)), "ymd HMS") 

# order by date
data911 <- data911[order(data911$date),]

# add indicator that this data is post - change in 2014
data911$pre_sys_change <- 0

# Note: some problems on the NSA name 
data911$NSA_Name <- ifelse(is.na(data911$NSA_Name), data911$NSA_NAME, data911$NSA_Name)
data911$NSA_NAME <- NULL

# save the record-level file for just new data dump
save_loc <- "C:\\Users\\daveh\\Documents\\Research\\BARI\\bari_911_prep\\"
setwd(typloc)
save_name

### Note 2020: if there is a column "geometry" it needs to be removed; it is not well saved in the csv format
### we only save lat lon for the csv format

### Make sure that the columns are in the same order as in the previous years
### NOTE: Column "date" is not present in the previous years (?!), we keep it for the 2020 release 
# data911 <- data911 %>% rename(lon = long, NSA_NAME = NSA_Name)

# data911 <- data911[,c("eid", "date", "ag_id","dgroup","num_1","a_rev_num","c_rev_num", "ani_num_old","tycod","typ_eng",
#                                     "sub_tycod", "sub_eng", "dow", "TYPE", "priority",  "sdts", "pre_sys_change", "year", "ad_ts",
#                                     "a_cdts", "c_cdts", "ds_ts", "en_ts", "ar_ts", "tr_ts", 
#                                     "ta_ts","xdts", "rev_dts",   "udts","call_sour_old", "clname_old", 
#                                     "clrnum_old",    "cstr_add_old",  "ccity_old",     "cob_911_name", "cob_911_phone", "cob_911_addr", 
#                                     "estnum",    "edirpre",   "efeanme",   "efeatyp",   "edirsuf",   "eapt", 
#                                     "eloc_fld1", "eloc_fld2", "eloc_fld3", "eloc_fld4", "ecompl",    "earea", 
#                                     "emun","zip", "loc_com",   "x_cord",    "y_cord",    "xstreet1", 
#                                     "xstreet2",  "fea_mslink", "hash", "parse_type","loc_ver", "loi_event", 
#                                     "loi_inf",   "loi_specsit", "lat", "lon", "sj_cat",    "Land_Parcel_ID", 
#                                     "TLID", "Blk_ID_10", "BG_ID_10",  "CT_ID_10",  "NSA_NAME",  "BRA_PD", "SocDis", "PrivateConflict", "Violence",  "Guns" )]
# 



# Rename old column names to new ones in the data frame
names(data911)[names(data911) == "B_ID_10"] <- "Blk_ID_10"
names(data911)[names(data911) == "BG_ID_1"] <- "BG_ID_10"
names(data911)[names(data911) == "CT_ID_1"] <- "CT_ID_10"
names(data911)[names(data911) == "B_ID_20"] <- "Blk_ID_20"
names(data911)[names(data911) == "BG_ID_2"] <- "BG_ID_20"
names(data911)[names(data911) == "CT_ID_2"] <- "CT_ID_20"
names(data911)[names(data911) == "NSA_Nam"] <- "NSA_NAME"

coordinates <- st_coordinates(st_as_sf(data911) )
data911$x_cord <- coordinates[, 1]  # X values (longitude)
data911$y_cord <- coordinates[, 2]   # Y values (latitude)
data911$geometry <- NULL
data911_2023 <- data911[data911$year == 2023, ]

### RENAME AND TRIM COLUMNS #######
trim <- c(
  "eid", "date", "year", "pre_sys_change", "TYPE", "SocDis", "Guns",
  "PrivateConflict", "Violence", "Larceny", "Burglary", "Vandalism", "Robbery",
  "Land_Parcel_ID", "TLID", "Blk_ID_10", "BG_ID_10", "CT_ID_10", "Blk_ID_20", "BG_ID_20", "CT_ID_20",
  "x_cord", "y_cord"
)
d911_2023 <- data911_2023[,trim]
d911_2023 <- d911_2023[!duplicated(d911_2023),]
### If we want to save all the 911 calls without subset only the ones with geo info  ###
write.csv(d911_2023, save_name, row.names = F)

### If we want to save only the ones inside the boundary of Boston and have minimum one geo characteristic ###
data911_1 <- data911_2023[!is.na(data911_2023$CT_ID_10) | !is.na(data911_2023$BG_ID_10) | !is.na(data911_2023$Blk_ID_10) |
                            !is.na(data911_2023$CT_ID_20) | !is.na(data911_2023$BG_ID_20) | !is.na(data911_2023$Blk_ID_20) |
               !is.na(data911_2023$Land_Parcel_ID) | !is.na(data911_2023$TLID),]
data911_ll <- data911_1[,trim]
data911_ll <- data911_ll[!duplicated(data911_ll),]
write.csv(data911_ll, "911 Calls From Jan2023-Dec2023_with_LatLon.csv", row.names = F)

###################### DID NOT RUN THE FOLLOWING IN THE 2020 UPDATE ################

# create updated file for all 911 data
# previous$date <- parse_date_time(gsub("ED", "", as.character(previous$sdts)), "ymd HMS") 
# stopifnot(sum(!colnames(data911) %in% colnames(previous)) == 0)
# stopifnot(sum(!colnames(previous) %in% colnames(data911)) == 0)
# 
# data911updated <- rbind.fill(previous, data911)
# write.csv(data911updated, save_name_updated, row.names=F)
# 
# data911updated <- data911updated[order(data911updated$date),]

###################### RUN THE FOLLOWING IN THE 2020 UPDATE ################

# because the previous rows were not run we add the following
data911updated <- data911
data911updated <- data911_1 # run this if you only want to save the data with lat lon

# create 6-month increment files, assuming years[1] is a full year, and years[2] is not
years <- unique(data911updated$year)
years

max_date <- max(data911updated$date)
max_date
month_max_name <- lubridate::month(max_date, label = T, abbr = T)

nrow(data911updated[data911updated$year == years[1] & month(data911updated$date) <= 6,]) + 
  nrow(data911updated[data911updated$year == years[1] & month(data911updated$date) > 6,]) +
  nrow(data911updated[data911updated$year == years[2] & month(data911updated$date) <= 6,]) + 
  nrow(data911updated[data911updated$year == years[2] & month(data911updated$date) > 6,]) ==
  #  nrow(data911updated[data911updated$year == years[3] & month(data911updated$date) <= 6,]) ==
  nrow(data911updated)

head(data911updated[data911updated$year == years[1] & month(data911updated$date) <= 6,]) 
head(data911updated[data911updated$year == years[1] & month(data911updated$date) > 6,]) 
head(data911updated[data911updated$year == years[2] & month(data911updated$date) <= 6,]) 
tail(data911updated[data911updated$year == years[2] & month(data911updated$date) > 6,]) 
#tail(data911updated[data911updated$year == years[3] & month(data911updated$date) <= 6,]) 


save_loc <- "C:\\Users\\User\\Google Drive\\BARI Research Team Data Library\\911 Database Working Folder\\2020\\Processed_calls\\"
setwd(save_loc)

for (i in 1:length(years)){
  if(i < length(years)){
    write.csv(data911updated[data911updated$year == years[i] & month(data911updated$date) <= 6,], paste("911 Calls ", years[i], ", Jan - Jun", "_",  date, ".csv", sep = ""), row.names = F)
    write.csv(data911updated[data911updated$year == years[i] & month(data911updated$date) > 6,], paste("911 Calls ", years[i], ", Jul - Dec", "_", date, ".csv", sep = ""), row.names = F)
  }
  else {
    if(month(max_date) <= 6){ 
      write.csv(data911updated[data911updated$year == years[i] & month(data911updated$date) <= 6,], 
                paste("911 Calls ", years[i], ifelse((month(max_date) > 1), ", Jan - ", ", "),
                      month_max_name, "_", date,".csv", sep = ""), 
                row.names = F)  
    }  else {
      write.csv(data911updated[data911updated$year == years[i] & month(data911updated$date) <= 6,], paste("911 Calls ", years[i], ", Jan - Jun", "_", date, ".csv", sep = ""), row.names = F)
      write.csv(data911updated[data911updated$year == years[i] & month(data911updated$date) > 6,], paste("911 Calls ", years[i], ", Jul - ", month_max_name, "_", date,".csv", sep = ""), row.names = F)   
    }}}


###################### DID NOT RUN THE FOLLOWING IN THE January 2021 UPDATE ################


#### ADDITIONAL FIX ####

# load correct jan-mar 2017
existing <- "~/Desktop/Work_BARI/911 Work/2019/911 Update/Data/Processed Calls/911 Calls 2017, Jan - Mar.csv"
previous <- read.csv(existing, head = T, stringsAsFactors = F)

# load updated data 911- mar 2017-Jan 30, 2019
update19 <- read.csv("/Users/alexandraciomek/Desktop/Work_BARI/911 Work/2019/911 Update/Data/Processed Calls/911 Calls From Mar2017-Jan2019_030119.csv", header = T, stringsAsFactors = F)
table(nchar(update19$TLID))
sum(previous$eid %in% update19$eid) == 0

#create updated file for all 911 data
previous$date <- parse_date_time(gsub("ED", "", as.character(previous$sdts)), "ymd HMS") 
stopifnot(sum(!colnames(update19) %in% colnames(previous)) == 0)
stopifnot(sum(!colnames(previous) %in% colnames(update19)) == 0)

apply(previous, 2, class)
apply(update19, 2, class)

data911updated <- rbind.fill(previous, update19)
data911updated <- data911updated[order(data911updated$date),]

#getwd()
write.csv(data911updated, "911 Calls_Full_2017_Jan2019_031319.csv", row.names=F)

write.csv(data911updated[data911updated$year == 2017 & month(data911updated$date) <= 6,], 
          "911 Calls 2017, Jan - Jun_031319.csv", row.names = F)   

###################### In case there are column names problems in the 2020 UPDATE ################

### re upload them and fix the problems ###
#data <- read.csv("C:\\Users\\User\\Google Drive\\BARI Research Team Data Library\\911 Database Working Folder\\Testing\\Output\\911 Calls From Jan2019-Jul2020_08162020.csv", header = T, stringsAsFactors = F, sep=",", row.names=NULL)
#colnames(data)
#names <- colnames(data[2:76])
#colnames(data) <- c(names, "long", "lat", "geom1", "geom2", "pre_sys_change")
#write.csv(data, "C:\\Users\\User\\Google Drive\\BARI Research Team Data Library\\911 Database Working Folder\\Testing\\Output\\911 Calls From Jan2019-Jul2020_11102020.csv", row.names = FALSE)

#data_to_save <- rbind(calls_2019_1, calls_2019_2, calls_2020_1, calls_2020_2) 
#write.csv(data_to_save, "C:\\Users\\User\\Google Drive\\BARI Research Team Data Library\\911 Database Working Folder\\Testing\\Output\\911 Calls From Jan2019-Jul2020_11102020_only50mbufferBoston_NEWjoin.csv", row.names = FALSE)


###### ADJUSTMENTS #####


cbgs_new<-cad_2018 %>%
  group_by(BG_ID_10) %>%
  summarise(larceny=sum(grepl('^LARC',TYPE)),
            burglary=sum(grepl('^BE',TYPE)),
            vandal=sum(grepl('^VAN',TYPE)),
            robbery= sum(grepl('^ROB',TYPE)) + sum(TYPE=='ARMROBDEFAULT'),
            SocDis= sum(SocDis),
            SocDis_new= sum(SocDis) - sum(TYPE=='VANDIP'),
            PrivateConflict= sum(PrivateConflict),
            PrivateConflict_new= sum(PrivateConflict) - sum(TYPE=='VANDRPT') - sum(TYPE=='BEIP'),
            Violence= sum(Violence),
            Violence_new= sum(Violence) - sum(TYPE=='ARMROBDEFAULT'),
            Guns = sum(Guns))
