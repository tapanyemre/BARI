### libraries ###
library(sf)
library(tidyverse)
library(data.table)

### to prevent scientific notation issues ###
options(scipen=100000000)

# set the working directory to the BARI shared drive
BARI <- "~/Desktop/BARI/PA-2023/outputs2/PADLong/"

### Cleaning Functions ###
source("~/Desktop/BARI/PA-2023/scripts /functions/Cleaning_functions_20190326.R")
source("~/Desktop/BARI/PA-2023/scripts /functions/Geocoding_functions_20190326.R")
source("~/Desktop/BARI/PA-2023/scripts /functions/Helper_functions_20190326.R")

### INPUT PATHS ###
# OUTPUT PATHS
pal_path = "PADLong.Record.11012023.csv"
pal_ct_path = "PADLong.CT.11012023.csv"
pal_ct_shp_path = "Tract Shp Long"
pal_ct_shp_name = "PADLong.CT.11012023"

pal_bg_path = "PADLong.CBG.11012023.csv"
pal_bg_shp_path = "BG Shp Long"
pal_bg_shp_name = "PADLong.CBG.11012023"

# READ IN FILES

#palold20 = read.csv("~/Desktop/BARI/PA-2023/base_data/2021/PADLong.Record.082020.csv")
#palold19 = read.csv("~/Desktop/BARI/PA-2023/base_data/2021/PADLong.Record.2019.csv")
palold = read.csv("~/Desktop/BARI/PA-2023/base_data/2021/PAD.Long.2021.csv")
PA <- read.csv("~/Downloads/PADCross.Record.2022.csv")
bgsShp <- st_read("~/Desktop/BARI/PA-2023/base_data/Block Groups 2010 BARI/Census Block Groups.shp")
tractsShp <- st_read("~/Desktop/BARI/PA-2023/base_data/Tracts_Boston_2010_BARI/Tracts_Boston BARI.shp")
cts_20_shp <- tigris::tracts(state = "MA", county = "Suffolk", year = 2022) #%>% dplyr::select(CT_ID_20 = GEOID)
bgs_20_shp <- tigris::block_groups(state = "MA",county = "Suffolk",year = 2022) #%>%  dplyr::select(BG_ID_20 = GEOID)
landParcels <- read.csv("dataverse_GI/LandParcels_2023.csv")


# adding old data 
# doesnt have these "BRA_PD","NSA_NAME"
PA_bckp <- PA # 177091
PA <- PA[,-1]
names(PA)[1] <- "parcel_num"
#names(palold19)[1] <- "parcel_num"
#names(palold18)[1] <- "parcel_num"
names(palold)[1] <- "parcel_num"

#PA <- PADcross
palold$parcel_num <- as.numeric(palold$parcel_num)
palold$parcel_num <- format(palold$parcel_num, scientific = F)

#palold18$parcel_num <- as.numeric(palold18$parcel_num)
#palold18$parcel_num <- format(palold18$parcel_num, scientific = F)

#palold19$parcel_num <- as.numeric(palold19$parcel_num)
#palold19$parcel_num <- format(palold19$parcel_num, scientific = F)

PA$parcel_num <- as.numeric(PA$parcel_num)
PA$parcel_num <- format(PA$parcel_num, scientific = F)





# renaming the new columns - 2021
PAclean <- PA
PAclean <- rename(PAclean, PTYPE=LUC)
PAclean <- rename(PAclean, ST_NAME_SUFF=ST_NAME)
#PAclean <- rename(PAclean, MAIL.CS, LUC) MAIL_CITY, MAIL_STATE
PAclean <- rename(PAclean, AV_LAND=LAND_VALUE)
PAclean <- rename(PAclean, AV_BLDG=BLDG_VALUE)
PAclean <- rename(PAclean, AV_TOTAL=TOTAL_VALUE)
PAclean <- rename(PAclean, YR_REMOD=YR_REMODEL)
PAclean <- rename(PAclean, NUM_FLOORS=RES_FLOOR)
PAclean <- rename(PAclean, R_ROOF_TYP=ROOF_STRUCTURE)
PAclean <- rename(PAclean, R_EXT_FIN=ROOF_COVER)
PAclean <- rename(PAclean, R_BDRMS=BED_RMS)
PAclean <- rename(PAclean, R_FULL_BTH=FULL_BTH)
PAclean <- rename(PAclean, R_HALF_BTH=HLF_BTH)
PAclean <- rename(PAclean, R_BTH_STYLE=BTHRM_STYLE1)
PAclean <- rename(PAclean, R_BTH_STYLE2=BTHRM_STYLE2)
PAclean <- rename(PAclean, R_BTH_STYLE3=BTHRM_STYLE3)
PAclean <- rename(PAclean, R_KITCH=KITCHEN_TYPE)
PAclean <- rename(PAclean, R_KITCH_STYLE=KITCHEN_STYLE1)
PAclean <- rename(PAclean, R_KITCH_STYLE2=KITCHEN_STYLE2)
PAclean <- rename(PAclean, R_KITCH_STYLE3=KITCHEN_STYLE3)
PAclean <- rename(PAclean, R_HEAT_TYP=HEAT_TYPE)
PAclean <- rename(PAclean, R_AC=AC_TYPE)
PAclean <- rename(PAclean, R_FPLACE=FIRE_PLACE)
PAclean <- rename(PAclean, R_EXT_CND=EXT_COND)
PAclean <- rename(PAclean, R_OVRALL_CND=OVERALL_COND)
PAclean <- rename(PAclean, R_INT_CND=INT_COND)
#PAclean <- rename(PAclean, R_INT_FIN, NA)
PAclean <- rename(PAclean, R_VIEW=PROP_VIEW)
PAclean <- rename(PAclean, U_TOTAL_RMS=TT_RMS)
########################PAclean <- rename(PAclean, U_BDRMS=BED_RMS)
########### PAclean <- rename(PAclean, U_KITCH_STYLE=KITCHEN_STYLE1)
#PAclean <- rename(PAclean, U_HEAT_TYP=HEAT_TYPE)
#PAclean <- rename(PAclean, U_FULL_BTH=FULL_BTH)
#PAclean <- rename(PAclean, U_HALF_BTH=HLF_BTH)
PAclean <- rename(PAclean, S_NUM_BLDG=NUM_BLDGS)
PAclean <- rename(PAclean, S_BLDG_STYL=BLDG_TYPE)
PAclean <- rename(PAclean, S_UNIT_RES=RES_UNITS)
PAclean <- rename(PAclean, S_UNIT_COM=COM_UNITS)
#PAclean <- rename(PAclean, S_UNIT_RC, NA)
PAclean <- rename(PAclean, S_EXT_FIN=EXT_FINISHED)
#PAclean <- rename(PAclean, S_EXT_CND=EXT_COND)
#PAclean <- rename(PAclean, U_BASE_FLOOR, NA)
PAclean <- rename(PAclean, U_NUM_PARK=NUM_PARKING)
PAclean <- rename(PAclean, U_CORNER=CORNER_UNIT)
#PAclean <- rename(PAclean, U_ORIENT, NA)
#PAclean <- rename(PAclean, U_BTH_STYLE=BTHRM_STYLE1)
#PAclean <- rename(PAclean, U_BTH_STYLE2=BTHRM_STYLE2)
#PAclean <- rename(PAclean, U_BTH_STYLE3=BTHRM_STYLE3)
#PAclean <- rename(PAclean, U_AC=AC_TYPE)
PA <- PAclean
names(PA)

names(PA)[5] <- "ST_NAME_SUF"
names(PA)[7] <- "ZIPCODE"

PA$parcel_num <- trimws(PA$parcel_num)
PA.toAdd <- PA[!duplicated(PA$parcel_num),c("parcel_num","CM_ID","ST_NUM", "ST_NAME_SUF",
                                            "LU","AV_TOTAL","OWN_OCC","ZIPCODE","X",
                                            "Y","GIS_ID","Land_Parcel_ID","TLID",
                                            "Blk_ID_10","BG_ID_10","CT_ID_10", 
                                            "Blk_ID_20" , "BG_ID_20" , "CT_ID_20")] #

# THE NAMES OF THE FY WILL NEED TO CHANGE #
names(PA.toAdd)<-c("parcel_num","CM_ID","ST_NUM", "ST_NAME_SUF",
                   "FY2022.LU","FY2022.AV","FY2022.RESEX","ZIPCODE","X","Y","GIS_ID",
                   "Land_Parcel_ID","TLID","Blk_ID_10","BG_ID_10","CT_ID_10", 
                   "Blk_ID_20" , "BG_ID_20" , "CT_ID_20") 

# remove old geographic data, no "ST_NAME_SUF"
for (var in c("X","Y","GIS_ID","Land_Parcel_ID","TLID","Blk_ID_10","BG_ID_10","CT_ID_10",
              "BRA_PD","NSA_NAME","Location_ID","CM_ID","ST_NUM", "unit_N", "unit_N_orig",
              "ST_NAME_SUF", "ZIPCODE","ST_NAME","UNIT_NUM")) {
  palold[,var] = NULL
}

palold$parcel_num <- trimws(palold$parcel_num)

table(duplicated(PA.toAdd$parcel_num))
# FALSE 
# 180448 
table(duplicated(palold$parcel_num))
# FALSE   TRUE 
# 179353     39 

#palold <- palold[!duplicated(palold$parcel_num), ]

# merge on all last year data
pal = merge(PA.toAdd,palold,by="parcel_num",all=TRUE) #180176 #183488
pal <- distinct(pal) # 180176 #183527 #183488
sum(is.na(pal$FY2022.LU) & !is.na(pal$FY2021.LU)) #603
# 208 new rows
summary(pal)
names(pal)

### SKIPPED THIS PART AS NO NEED TO MERGE ####
# # add geo data for old TAL, this is to include any changes that might have been made to 
# # the GI for the old TAL rows, their parcel_nums have been matched to GIS_IDs that are in 
# # the land parcels file first we merge onto the IDConnector,then the land parcels
# 
# # landParcels = dplyr::rename(landParcels, TLID = TLID_1)
# names(IDconnector)[1] <- "parcel_num"
# pal = merge(
#   merge(pal,IDconnector[!duplicated(IDconnector$parcel_num),c("parcel_num","GIS_ID",
#                                                               "Land_Parcel_ID")],
#         by="parcel_num",all.x=T),
#   landParcels[,c("X","Y","Land_Parcel_ID","TLID","Blk_ID_10","BG_ID_10","CT_ID_10")],
#   
#   by.x="Land_Parcel_ID.y",by.y = "Land_Parcel_ID",all.x=T)
# # adds in the merged in geographic data, but keeps the original data for those that did 
# #   not need the merged data
# for (var in c("X","Y","GIS_ID","Land_Parcel_ID","TLID","Blk_ID_10","BG_ID_10","CT_ID_10"
# )) {
#   pal[,var] = ifelse(!is.na(pal[,paste(var,".x",sep="")]),
#                      pal[,paste(var,".x",sep="")],
#                      pal[,paste(var,".y",sep="")])
#   pal[,paste(var,".x",sep="")] = NULL
#   pal[,paste(var,".y",sep="")] = NULL
# }
# 
# # clean up
# PA$parcel_num[which(PA$parcel_num == "NA")] <- NA
# pal_bckup <- pal # 179617

#pal <- pal_bckup
#--------------------------------#
#       Adding Extra Vars        #
#--------------------------------#

# Modify Assessed Value Variables #
# THIS WILL NEED TO CHANGE BASED ON PREVIOUS YEAR #
lastyear=2022

# calculates the change in valuation year to year 
# Where assessed value is $0, set it to be NA
for (year in c(2000:lastyear)) {
  #year = 2022
  av = paste("FY",paste(year,".AV",sep=""),sep="")
  pal[,av] = ifelse(pal[,av] >0 & !is.na(pal[,av]),pal[,av],NA)
  if (!is.na(lastyear)) {
    diff = paste("DiffAV",year,sep="")
    avly = paste("FY",paste(lastyear,".AV",sep=""),sep="")
    pal[,diff] = pal[,av]-pal[,avly]
    perc = paste("PercChangeAV",year,sep="")
    pal[,perc] = pal[,diff]/pal[,avly]
  }
  lastyear = year
}


# Calculate change during growth, crash, and recovery periods
#  -- Calculate median value in Boston for each year
apply(pal[,paste(paste("FY",c(2000:lastyear),sep=""),".AV",sep="")], 2,median, na.rm=TRUE)            
# Results:
# min = 2000
# max = 2007
# 2nd min = 2011
# Therefore, 
# growth years = 2000 - 2007
# crash years = 2007 - 2011
# recovery years = 2011 - 2022

# Step 3 -- Create variables for value difference in each time period
# THIS WILL NEED TO CHANGE BASED ON PREVIOUS YEAR # 

pal$GrowthDiffAV <- pal$FY2007.AV-pal$FY2000.AV
pal$GrowthPercChangeAV <- (pal$FY2007.AV-pal$FY2000.AV)/pal$FY2000.AV
pal$CrashDiffAV <- pal$FY2011.AV-pal$FY2007.AV
pal$CrashPercChangeAV <- (pal$FY2011.AV-pal$FY2007.AV)/pal$FY2007.AV
pal$RecoveryDiffAV <- pal$FY2022.AV-pal$FY2011.AV
pal$RecoveryPercChangeAV <- (pal$FY2022.AV-pal$FY2011.AV)/pal$FY2011.AV

# ----------------------------------------------------------------
# Broader Land Use Categories ####

# Group land use codes into 4 simplified categories####

#Res= R1, R2, R3, R4, RL, A, CD, CP
#Comm= CC, C, CL, CM, RC
#Ind= I, AH
#Exem= E, EA
# no "ST_NAME_SUF",
variables_ordered = c("parcel_num","CM_ID","ST_NUM", "ST_NAME_SUF",
                      "ZIPCODE")
for (year in c(2000:lastyear)) {
  #year = 2020
  fourcat = paste(paste("LU",year,sep=""),"FourCat",sep="")
  lu = paste("FY",paste(year,".LU",sep=""),sep="")
  pal[,fourcat] = ifelse(!is.na(match(pal[,lu],c("R1","R2","R3","R4","RL","A","CD",
                                                 "CP"))),"Res",
                         ifelse(!is.na(match(pal[,lu],c("CC","C","CL","CM","RC"))),
                                "Comm",
                                ifelse(!is.na(match(pal[,lu],c("I","AH"))),"Ind",
                                       ifelse(!is.na(match(pal[,lu],c("E","EA"))),"Exem",
                                              NA))))
  av = paste("FY",paste(year,".AV",sep=""),sep="")
  resex = paste("FY",paste(year,".RESEX",sep=""),sep="")
  if (year != 2000) {
    diffav = paste("DiffAV",year,sep="")
    percChange = paste("PercChangeAV",year,sep="")
    variables_ordered = c(variables_ordered,lu,av,resex,diffav,percChange,fourcat)
  }
  else {
    variables_ordered = c(variables_ordered,lu,av,resex,fourcat)
    
  }
}
variables_ordered = c(variables_ordered,
                      "GrowthDiffAV","GrowthPercChangeAV","CrashDiffAV",
                      "CrashPercChangeAV","RecoveryDiffAV","RecoveryPercChangeAV","X","Y",
                      "GIS_ID","Land_Parcel_ID","TLID","Blk_ID_10","BG_ID_10","CT_ID_10",
                      "Blk_ID_20", "BG_ID_20", "CT_ID_20"
)

#ordering variables
setdiff(names(pal),variables_ordered)
setdiff(variables_ordered, names(pal))
#drops <- c("unit_N.x","unit_N_orig.x")
#pal <- pal[ , !(names(pal) %in% drops)]
pal = pal[,variables_ordered] # 

pal <- distinct(pal) # 179617

TLID <- read.csv("~/Desktop/BARI/GI-2023/drive/Parcel_final_10032023_YET.csv")
TLID$TLID <- format(TLID$TLID, scientific = F)
# include as much TLIDs as possible
pal$TLID <- landParcels$TLID[match(pal$Land_Parcel_ID,landParcels$Land_Parcel_ID)]

names(pal)[1] <- "PID"
# fix formattings
pal$CT_ID_10 <- as.numeric(pal$CT_ID_10)
pal$CT_ID_10 <- format(pal$CT_ID_10, scientific = F)
pal$BG_ID_10 <- as.numeric(pal$BG_ID_10)
pal$BG_ID_10 <- format(pal$BG_ID_10, scientific = F)
pal$Blk_ID_10 <- as.numeric(pal$Blk_ID_10)
pal$Blk_ID_10 <- format(pal$Blk_ID_10, scientific = F)
pal$CT_ID_20 <- as.numeric(pal$CT_ID_20)
pal$CT_ID_20 <- format(pal$CT_ID_20, scientific = F)
pal$BG_ID_20 <- as.numeric(pal$BG_ID_20)
pal$BG_ID_20 <- format(pal$BG_ID_20, scientific = F)
pal$Blk_ID_20 <- as.numeric(pal$Blk_ID_20)
pal$Blk_ID_20 <- format(pal$Blk_ID_20, scientific = F)
pal$TLID <- as.numeric(pal$TLID)
pal$TLID <- format(pal$TLID, scientific = F)
pal$GIS_ID <- as.numeric(pal$GIS_ID)
pal$GIS_ID <- format(pal$GIS_ID, scientific = F)
pal$Land_Parcel_ID <- as.numeric(pal$Land_Parcel_ID)
pal$Land_Parcel_ID <- format(pal$Land_Parcel_ID, scientific = F)
pal$CM_ID <- as.numeric(pal$CM_ID)
pal$CM_ID <- format(pal$CM_ID, scientific = F)

pal$Land_Parcel_ID <- trimws(pal$Land_Parcel_ID)
pal$Blk_ID_10 <- trimws(pal$Blk_ID_10)
pal$BG_ID_10 <- trimws(pal$BG_ID_10)
pal$CT_ID_10 <- trimws(pal$CT_ID_10)
pal$Blk_ID_20 <- trimws(pal$Blk_ID_20)
pal$BG_ID_20 <- trimws(pal$BG_ID_20)
pal$CT_ID_20 <- trimws(pal$CT_ID_20)
pal$GIS_ID <- trimws(pal$GIS_ID)
pal$TLID <- trimws(pal$TLID)
pal$CM_ID <- trimws(pal$CM_ID)

pal$Land_Parcel_ID[which(pal$Land_Parcel_ID == "NA")] <- NA
pal$Blk_ID_10[which(pal$Blk_ID_10 == "NA")] <- NA
pal$BG_ID_10[which(pal$BG_ID_10 == "NA")] <- NA
pal$CT_ID_10[which(pal$CT_ID_10 == "NA")] <- NA
pal$Blk_ID_20[which(pal$Blk_ID_20 == "NA")] <- NA
pal$BG_ID_20[which(pal$BG_ID_20 == "NA")] <- NA
pal$CT_ID_20[which(pal$CT_ID_20 == "NA")] <- NA
pal$GIS_ID[which(pal$GIS_ID == "NA")] <- NA
pal$TLID[which(pal$TLID == "NA")] <- NA
pal$CM_ID[which(pal$CM_ID == "NA")] <- NA

# taking care of some NAs
pal$GIS_ID[is.na(pal$GIS_ID) & !is.na(pal$Land_Parcel_ID)] <- landParcels$GIS_ID[!is.na(landParcels$GIS_ID)][match(pal$Land_Parcel_ID[is.na(pal$GIS_ID) & !is.na(pal$Land_Parcel_ID)], landParcels$Land_Parcel_ID)] # 

pal$X[is.na(pal$X) & !is.na(pal$Land_Parcel_ID)] <- landParcels$X[!is.na(landParcels$X)][match(pal$Land_Parcel_ID[is.na(pal$X) & !is.na(pal$Land_Parcel_ID)], landParcels$Land_Parcel_ID)] # 

pal$Y[is.na(pal$Y) & !is.na(pal$Land_Parcel_ID)] <- landParcels$Y[!is.na(landParcels$Y)][match(pal$Land_Parcel_ID[is.na(pal$Y) & !is.na(pal$Land_Parcel_ID)], landParcels$Land_Parcel_ID)] # 

pal$CT_ID_10[is.na(pal$CT_ID_10) & !is.na(pal$Land_Parcel_ID)] <- landParcels$CT_ID_10[!is.na(landParcels$CT_ID_10)][match(pal$Land_Parcel_ID[is.na(pal$CT_ID_10) & !is.na(pal$Land_Parcel_ID)], landParcels$Land_Parcel_ID)] # 

pal$BG_ID_10[is.na(pal$BG_ID_10) & !is.na(pal$Land_Parcel_ID)] <- landParcels$BG_ID_10[!is.na(landParcels$BG_ID_10)][match(pal$Land_Parcel_ID[is.na(pal$BG_ID_10) & !is.na(pal$Land_Parcel_ID)], landParcels$Land_Parcel_ID)] # 

pal$Blk_ID_10[is.na(pal$Blk_ID_10) & !is.na(pal$Land_Parcel_ID)] <- landParcels$Blk_ID_10[!is.na(landParcels$Blk_ID_10)][match(pal$Land_Parcel_ID[is.na(pal$Blk_ID_10) & !is.na(pal$Land_Parcel_ID)], landParcels$Land_Parcel_ID)] # 

pal$CT_ID_20[is.na(pal$CT_ID_20) & !is.na(pal$Land_Parcel_ID)] <- landParcels$CT_ID_20[!is.na(landParcels$CT_ID_20)][match(pal$Land_Parcel_ID[is.na(pal$CT_ID_20) & !is.na(pal$Land_Parcel_ID)], landParcels$Land_Parcel_ID)] # 

pal$BG_ID_20[is.na(pal$BG_ID_20) & !is.na(pal$Land_Parcel_ID)] <- landParcels$BG_ID_20[!is.na(landParcels$BG_ID_20)][match(pal$Land_Parcel_ID[is.na(pal$BG_ID_20) & !is.na(pal$Land_Parcel_ID)], landParcels$Land_Parcel_ID)] # 

pal$Blk_ID_20[is.na(pal$Blk_ID_20) & !is.na(pal$Land_Parcel_ID)] <- landParcels$Blk_ID_20[!is.na(landParcels$Blk_ID_20)][match(pal$Land_Parcel_ID[is.na(pal$Blk_ID_20) & !is.na(pal$Land_Parcel_ID)], landParcels$Land_Parcel_ID)] # 

pal$TLID[is.na(pal$TLID) & !is.na(pal$Land_Parcel_ID)] <- landParcels$TLID[!is.na(landParcels$TLID)][match(pal$Land_Parcel_ID[is.na(pal$TLID) & !is.na(pal$Land_Parcel_ID)], landParcels$Land_Parcel_ID)] # 

# formatting check
pal$PID <- format(pal$PID, scientific = F)
pal$GIS_ID <- format(pal$GIS_ID, scientific = F)
pal$Blk_ID_10 <- format(pal$Blk_ID_10, scientific = F)
pal$BG_ID_10 <- format(pal$BG_ID_10, scientific = F)
pal$CT_ID_10 <- format(pal$CT_ID_10, scientific = F)
pal$Blk_ID_20 <- format(pal$Blk_ID_20, scientific = F)
pal$BG_ID_20 <- format(pal$BG_ID_20, scientific = F)
pal$CT_ID_20 <- format(pal$CT_ID_20, scientific = F)
pal$TLID <- format(pal$TLID, scientific = F)
pal$CM_ID <- format(pal$CM_ID, scientific = F)
pal$Land_Parcel_ID <- format(pal$Land_Parcel_ID, scientific = F)

pal$Blk_ID_10 <- trimws(pal$Blk_ID_10)
pal$CT_ID_10 <- trimws(pal$CT_ID_10)
pal$BG_ID_10 <- trimws(pal$BG_ID_10)
pal$Blk_ID_20 <- trimws(pal$Blk_ID_20)
pal$CT_ID_20 <- trimws(pal$CT_ID_20)
pal$BG_ID_20 <- trimws(pal$BG_ID_20)
pal$PID <- trimws(pal$PID)
pal$GIS_ID <- trimws(pal$GIS_ID)
pal$TLID <- trimws(pal$TLID)
pal$CM_ID <- trimws(pal$CM_ID)
pal$Land_Parcel_ID <- trimws(pal$Land_Parcel_ID)

pal$Land_Parcel_ID[which(pal$Land_Parcel_ID == "NA")] <- NA
pal$Blk_ID_10[which(pal$Blk_ID_10 == "NA")] <- NA
pal$CT_ID_10[which(pal$CT_ID_10 == "NA")] <- NA
pal$BG_ID_10[which(pal$BG_ID_10 == "NA")] <- NA
pal$Blk_ID_20[which(pal$Blk_ID_20 == "NA")] <- NA
pal$CT_ID_20[which(pal$CT_ID_20 == "NA")] <- NA
pal$BG_ID_20[which(pal$BG_ID_20 == "NA")] <- NA
pal$TLID[which(pal$TLID == "NA")] <- NA
pal$CM_ID[which(pal$CM_ID == "NA")] <- NA
pal$GIS_ID[which(pal$GIS_ID == "NA")] <- NA

# sanity checks
view(table(pal$CT_ID_10))
view(table(pal$BG_ID_10))
view(table(pal$Blk_ID_10))
view(table(pal$CT_ID_20))
view(table(pal$BG_ID_20))
view(table(pal$Blk_ID_20))
view(table(pal$TLID))
view(table(pal$PID))
view(table(pal$GIS_ID))
view(table(pal$Land_Parcel_ID))
view(table(pal$CM_ID))
pal[rowSums(is.na(pal)) != ncol(pal), ]

# check NAs
pal %>%
  summarise_all(~ sum(is.na(.)))

apply(pal,2,max, na.rm = TRUE)
names(pal)
# number of NAs for each row
max(rowSums(is.na((pal))))

# filling in some NAs
pal$X[is.na(pal$X) & !is.na(pal$Land_Parcel_ID)] <- landParcels$X[!is.na(landParcels$X)][match(pal$Land_Parcel_ID[is.na(pal$X) & !is.na(pal$Land_Parcel_ID)], landParcels$Land_Parcel_ID[!is.na(landParcels$X)])]

pal$Y[is.na(pal$Y) & !is.na(pal$Land_Parcel_ID)] <- landParcels$Y[!is.na(landParcels$Y)][match(pal$Land_Parcel_ID[is.na(pal$Y) & !is.na(pal$Land_Parcel_ID)], landParcels$Land_Parcel_ID[!is.na(landParcels$Y)])]

pal$CT_ID_10[is.na(pal$CT_ID_10) & !is.na(pal$Land_Parcel_ID)] <- landParcels$CT_ID_10[!is.na(landParcels$CT_ID_10)][match(pal$Land_Parcel_ID[is.na(pal$CT_ID_10) & !is.na(pal$Land_Parcel_ID)], landParcels$Land_Parcel_ID[!is.na(landParcels$CT_ID_10)])]

pal$BG_ID_10[is.na(pal$BG_ID_10) & !is.na(pal$Land_Parcel_ID)] <- landParcels$BG_ID_10[!is.na(landParcels$BG_ID_10)][match(pal$Land_Parcel_ID[is.na(pal$BG_ID_10) & !is.na(pal$Land_Parcel_ID)], landParcels$Land_Parcel_ID[!is.na(landParcels$BG_ID_10)])]

pal$Blk_ID_10[is.na(pal$Blk_ID_10) & !is.na(pal$Land_Parcel_ID)] <- landParcels$Blk_ID_10[!is.na(landParcels$Blk_ID_10)][match(pal$Land_Parcel_ID[is.na(pal$Blk_ID_10) & !is.na(pal$Land_Parcel_ID)], landParcels$Land_Parcel_ID[!is.na(landParcels$Blk_ID_10)])]

pal$CT_ID_20[is.na(pal$CT_ID_20) & !is.na(pal$Land_Parcel_ID)] <- landParcels$CT_ID_20[!is.na(landParcels$CT_ID_20)][match(pal$Land_Parcel_ID[is.na(pal$CT_ID_20) & !is.na(pal$Land_Parcel_ID)], landParcels$Land_Parcel_ID[!is.na(landParcels$CT_ID_20)])]

pal$BG_ID_20[is.na(pal$BG_ID_20) & !is.na(pal$Land_Parcel_ID)] <- landParcels$BG_ID_20[!is.na(landParcels$BG_ID_20)][match(pal$Land_Parcel_ID[is.na(pal$BG_ID_20) & !is.na(pal$Land_Parcel_ID)], landParcels$Land_Parcel_ID[!is.na(landParcels$BG_ID_20)])]

pal$Blk_ID_20[is.na(pal$Blk_ID_20) & !is.na(pal$Land_Parcel_ID)] <- landParcels$Blk_ID_20[!is.na(landParcels$Blk_ID_20)][match(pal$Land_Parcel_ID[is.na(pal$Blk_ID_20) & !is.na(pal$Land_Parcel_ID)], landParcels$Land_Parcel_ID[!is.na(landParcels$Blk_ID_20)])]

pal$TLID[is.na(pal$TLID) & !is.na(pal$Land_Parcel_ID)] <- landParcels$TLID[!is.na(landParcels$TLID)][match(pal$Land_Parcel_ID[is.na(pal$TLID) & !is.na(pal$Land_Parcel_ID)], landParcels$Land_Parcel_ID[!is.na(landParcels$TLID)])]


pal$ZIPCODE[is.na(pal$ZIPCODE)] <- palold$ZIPCODE[!is.na(palold$ZIPCODE)][match(pal$PID[is.na(pal$ZIPCODE)], palold$PID[!is.na(palold$ZIPCODE)])]

pal$ST_NUM[ is.na(pal$ST_NUM)] <- palold$ST_NUM[!is.na(palold$ST_NUM)][match(pal$PID[ is.na(pal$ST_NUM)], palold$PID[!is.na(palold$ST_NUM)])]

pal$ST_NAME_SUF[ is.na(pal$ST_NAME_SUF)] <- palold$ST_NAME_SUF[!is.na(palold$ST_NAME_SUF)][match(pal$PID[ is.na(pal$ST_NAME_SUF)], palold$PID[!is.na(palold$ST_NAME_SUF)])]

pal$CM_ID[ is.na(pal$CM_ID)] <- palold$CM_ID[!is.na(palold$CM_ID)][match(pal$PID[ is.na(pal$CM_ID)], palold$PID[!is.na(palold$CM_ID)])]

# final checks on long

view(rowSums(is.na((pal))))

order(rowSums(is.na(pal)),decreasing=T)

max(rowSums(is.na(pal)))

sum(PA$AV_TOTAL == 0, na.rm = T)
sum(pal$FY2022.AV == 0, na.rm = T)

pal %>%  
  summarise(across(everything(), ~ sum(is.na(.))))

nrow(pal) #179611 #183527 #183488

view(table(pal$CT_ID_10))
view(table(pal$BG_ID_10))
view(table(pal$Blk_ID_10))
view(table(pal$GIS_ID))
view(table(pal$Land_Parcel_ID))
view(table(pal$TLID))

view(table(pal$X))

names(pal)

max(pal$PercChangeAV2021, na.rm = T)
max(pal$PercChangeAV2020, na.rm = T)

write.csv(pal, paste0(BARI, pal_path), row.names=F)





#--------------------------------#
#       Aggregations             #
#--------------------------------#

lastyear = 2022

#make new condo vars for aggregation
for (year in c(2001:lastyear)) {
  nc = paste("NewCondo",year,sep="")
  prevyear = year -1
  thisyearLU = paste(paste("FY",year,sep=""),".LU",sep="")
  prevyearLU = paste(paste("FY",prevyear,sep=""),".LU",sep="")
  thisyearmatch = match(pal[,thisyearLU],c("CD","CP","CM"))
  prevyearmatch = match(pal[,prevyearLU],c("CD","CP","CM"))
  pal[,nc] = ifelse(!is.na(thisyearmatch) & is.na(prevyearmatch),1,
                    ifelse(!is.na(prevyearmatch) & is.na(thisyearmatch), -1, 0))
}


pal.CT <- pal %>%
  filter(!is.na(CT_ID_10)) %>%
  group_by(CT_ID_10) %>%
  summarise(DiffAV2001 = sum(DiffAV2001, na.rm = T), DiffAV2002 = sum(DiffAV2002, na.rm = T), 
            DiffAV2003 = sum(DiffAV2003, na.rm = T), DiffAV2004 = sum(DiffAV2004, na.rm = T), 
            DiffAV2005 = sum(DiffAV2005, na.rm = T), DiffAV2006 = sum(DiffAV2006, na.rm = T), 
            DiffAV2007 = sum(DiffAV2007, na.rm = T), DiffAV2008 = sum(DiffAV2008, na.rm = T), 
            DiffAV2009 = sum(DiffAV2009, na.rm = T), DiffAV2010 = sum(DiffAV2010, na.rm = T), 
            DiffAV2011 = sum(DiffAV2011, na.rm = T), DiffAV2012 = sum(DiffAV2012, na.rm = T), 
            DiffAV2013 = sum(DiffAV2013, na.rm = T), DiffAV2014 = sum(DiffAV2014, na.rm = T), 
            DiffAV2015 = sum(DiffAV2015, na.rm = T), DiffAV2016 = sum(DiffAV2016, na.rm = T), 
            DiffAV2017 = sum(DiffAV2017, na.rm = T), DiffAV2018 = sum(DiffAV2018, na.rm = T), 
            DiffAV2019 = sum(DiffAV2019, na.rm = T), DiffAV2020 = sum(DiffAV2020, na.rm = T), 
            DiffAV2021 = sum(DiffAV2021, na.rm = T), DiffAV2022 = sum(DiffAV2022, na.rm = T), 
            NewCondo2001 = sum(NewCondo2001, na.rm = T), NewCondo2002 = sum(NewCondo2002, na.rm = T), NewCondo2003 = sum(NewCondo2003, na.rm = T), 
            NewCondo2004 = sum(NewCondo2004, na.rm = T), NewCondo2005 = sum(NewCondo2005, na.rm = T), NewCondo2006 = sum(NewCondo2006, na.rm = T), 
            NewCondo2007 = sum(NewCondo2007, na.rm = T), NewCondo2008 = sum(NewCondo2008, na.rm = T), NewCondo2009 = sum(NewCondo2009, na.rm = T), 
            NewCondo2010 = sum(NewCondo2010, na.rm = T), NewCondo2011 = sum(NewCondo2011, na.rm = T), NewCondo2012 = sum(NewCondo2012, na.rm = T), 
            NewCondo2013 = sum(NewCondo2013, na.rm = T), NewCondo2014 = sum(NewCondo2014, na.rm = T), NewCondo2015 = sum(NewCondo2015, na.rm = T), 
            NewCondo2016 = sum(NewCondo2016, na.rm = T), NewCondo2017 = sum(NewCondo2017, na.rm = T), NewCondo2018 = sum(NewCondo2018), 
            NewCondo2019 = sum(NewCondo2019), NewCondo2020 = sum(NewCondo2020, na.rm = T), NewCondo2021 = sum(NewCondo2021, na.rm = T),
            NewCondo2022 = sum(NewCondo2022, na.rm = T))

names(pal.CT)[1:(length(c(2001:lastyear))+1)] <- c("CT_ID_10",
                                                   "SumDiffAV2001","SumDiffAV2002",
                                                   "SumDiffAV2003","SumDiffAV2004",
                                                   "SumDiffAV2005","SumDiffAV2006",
                                                   "SumDiffAV2007","SumDiffAV2008",
                                                   "SumDiffAV2009","SumDiffAV2010",
                                                   "SumDiffAV2011","SumDiffAV2012",
                                                   "SumDiffAV2013","SumDiffAV2014",
                                                   "SumDiffAV2015","SumDiffAV2016",
                                                   "SumDiffAV2017","SumDiffAV2018",
                                                   "SumDiffAV2019","SumDiffAV2020",
                                                   "SumDiffAV2021","SumDiffAV2022")


Median_PercentValueChangeYear_CT_Yearly <- pal %>%
  filter(!is.na(CT_ID_10)) %>%
  group_by(CT_ID_10) %>%
  summarise(PercChangeAV2001 = median(PercChangeAV2001, na.rm = T), PercChangeAV2002 = median(PercChangeAV2002, na.rm = T), PercChangeAV2003 = median(PercChangeAV2003, na.rm = T), 
            PercChangeAV2004 = median(PercChangeAV2004, na.rm = T), PercChangeAV2005 = median(PercChangeAV2005, na.rm = T), PercChangeAV2006 = median(PercChangeAV2006, na.rm = T), 
            PercChangeAV2007 = median(PercChangeAV2007, na.rm = T), PercChangeAV2008 = median(PercChangeAV2008, na.rm = T), PercChangeAV2009 = median(PercChangeAV2009, na.rm = T), 
            PercChangeAV2010 = median(PercChangeAV2010, na.rm = T), PercChangeAV2011 = median(PercChangeAV2011, na.rm = T), PercChangeAV2012 = median(PercChangeAV2012, na.rm = T), 
            PercChangeAV2013 = median(PercChangeAV2013, na.rm = T), PercChangeAV2014 = median(PercChangeAV2014, na.rm = T), PercChangeAV2015 = median(PercChangeAV2015, na.rm = T), 
            PercChangeAV2016 = median(PercChangeAV2016, na.rm = T), PercChangeAV2017 = median(PercChangeAV2017, na.rm = T), PercChangeAV2018 = median(PercChangeAV2018, na.rm = T), 
            PercChangeAV2019 = median(PercChangeAV2019, na.rm = T), PercChangeAV2020 = median(PercChangeAV2020, na.rm = T), PercChangeAV2021 = median(PercChangeAV2021, na.rm = T), 
            PercChangeAV2022 = median(PercChangeAV2022, na.rm = T),
            GrowthPercChangeAV = median(GrowthPercChangeAV, na.rm = T), CrashPercChangeAV = median(CrashPercChangeAV, na.rm = T), RecoveryPercChangeAV = median(RecoveryPercChangeAV, na.rm = T))

pal.CT <- merge(pal.CT,Median_PercentValueChangeYear_CT_Yearly,by = "CT_ID_10")

#keeping only certain vars 
pal.CT = pal.CT[,c(
  "CT_ID_10",
  "SumDiffAV2001","PercChangeAV2001","NewCondo2001" ,
  "SumDiffAV2002","PercChangeAV2002","NewCondo2002" ,
  "SumDiffAV2003","PercChangeAV2003","NewCondo2003" ,
  "SumDiffAV2004","PercChangeAV2004","NewCondo2004" ,
  "SumDiffAV2005","PercChangeAV2005","NewCondo2005" ,
  "SumDiffAV2006","PercChangeAV2006","NewCondo2006" ,
  "SumDiffAV2007","PercChangeAV2007","NewCondo2007" ,
  "SumDiffAV2008","PercChangeAV2008","NewCondo2008" ,
  "SumDiffAV2009","PercChangeAV2009","NewCondo2009" ,
  "SumDiffAV2010","PercChangeAV2010","NewCondo2010" ,
  "SumDiffAV2011","PercChangeAV2011","NewCondo2011" ,
  "SumDiffAV2012","PercChangeAV2012","NewCondo2012" ,
  "SumDiffAV2013","PercChangeAV2013","NewCondo2013" ,
  "SumDiffAV2014","PercChangeAV2014","NewCondo2014" ,
  "SumDiffAV2015","PercChangeAV2015","NewCondo2015" ,
  "SumDiffAV2016","PercChangeAV2016","NewCondo2016" ,
  "SumDiffAV2017","PercChangeAV2017","NewCondo2017" ,
  "SumDiffAV2018","PercChangeAV2018","NewCondo2018" ,
  "SumDiffAV2019","PercChangeAV2019","NewCondo2019" ,
  "SumDiffAV2020","PercChangeAV2020","NewCondo2020" ,
  "SumDiffAV2021","PercChangeAV2021","NewCondo2021" ,
  "SumDiffAV2022","PercChangeAV2022","NewCondo2022" ,
  "GrowthPercChangeAV","CrashPercChangeAV", "RecoveryPercChangeAV")]

write.csv(pal.CT,paste0(BARI, pal_ct_path) , row.names=F)

#names(tractsShp)[4] <- "CT_ID_10"
# merge onto ct shp file
pal.CT.shp = merge(tractsShp,pal.CT,by="CT_ID_10",all.x=T)

# save shp file
st_write(pal.CT.shp,paste0(BARI, pal_ct_shp_path) ,pal_ct_shp_name,driver="ESRI Shapefile",
         overwrite_layer=TRUE)

#st_write(pal.CT.shp,paste0(BARI, pal_ct_shp_path) ,pal_ct_shp_name,driver="ESRI Shapefile", overwrite_layer=TRUE)

#aggregate to BG

pal.BG <- pal %>%
  filter(!is.na(BG_ID_10)) %>%
  group_by(BG_ID_10) %>%
  summarise(DiffAV2001 = sum(DiffAV2001, na.rm = T), DiffAV2002 = sum(DiffAV2002, na.rm = T), DiffAV2003 = sum(DiffAV2003, na.rm = T), DiffAV2004 = sum(DiffAV2004, na.rm = T), 
            DiffAV2005 = sum(DiffAV2005, na.rm = T), DiffAV2006 = sum(DiffAV2006, na.rm = T), DiffAV2007 = sum(DiffAV2007, na.rm = T), DiffAV2008 = sum(DiffAV2008, na.rm = T), 
            DiffAV2009 = sum(DiffAV2009, na.rm = T), DiffAV2010 = sum(DiffAV2010, na.rm = T), DiffAV2011 = sum(DiffAV2011, na.rm = T), DiffAV2012 = sum(DiffAV2012, na.rm = T), 
            DiffAV2013 = sum(DiffAV2013, na.rm = T), DiffAV2014 = sum(DiffAV2014, na.rm = T), DiffAV2015 = sum(DiffAV2015, na.rm = T), DiffAV2016 = sum(DiffAV2016, na.rm = T), 
            DiffAV2017 = sum(DiffAV2017, na.rm = T), DiffAV2018 = sum(DiffAV2018, na.rm = T), DiffAV2019 = sum(DiffAV2019, na.rm = T), DiffAV2020 = sum(DiffAV2020, na.rm = T), 
            DiffAV2021 = sum(DiffAV2021, na.rm = T), DiffAV2022 = sum(DiffAV2022, na.rm = T), 
            NewCondo2001 = sum(NewCondo2001, na.rm = T), NewCondo2002 = sum(NewCondo2002, na.rm = T), NewCondo2003 = sum(NewCondo2003, na.rm = T), NewCondo2004 = sum(NewCondo2004, na.rm = T), 
            NewCondo2005 = sum(NewCondo2005, na.rm = T), NewCondo2006 = sum(NewCondo2006, na.rm = T), NewCondo2007 = sum(NewCondo2007, na.rm = T), NewCondo2008 = sum(NewCondo2008, na.rm = T), 
            NewCondo2009 = sum(NewCondo2009, na.rm = T), NewCondo2010 = sum(NewCondo2010, na.rm = T), NewCondo2011 = sum(NewCondo2011, na.rm = T), NewCondo2012 = sum(NewCondo2012, na.rm = T), 
            NewCondo2013 = sum(NewCondo2013, na.rm = T), NewCondo2014 = sum(NewCondo2014, na.rm = T), NewCondo2015 = sum(NewCondo2015, na.rm = T), NewCondo2016 = sum(NewCondo2016, na.rm = T), 
            NewCondo2017 = sum(NewCondo2017, na.rm = T), NewCondo2018 = sum(NewCondo2018, na.rm = T), NewCondo2019 = sum(NewCondo2019, na.rm = T), NewCondo2020 = sum(NewCondo2020, na.rm = T), 
            NewCondo2021 = sum(NewCondo2021, na.rm = T),  NewCondo2022 = sum(NewCondo2022, na.rm = T))


names(pal.BG)[1:(length(c(2001:lastyear))+1)]  <- c("BG_ID_10",
                                                    "SumDiffAV2001","SumDiffAV2002",
                                                    "SumDiffAV2003","SumDiffAV2004",
                                                    "SumDiffAV2005","SumDiffAV2006",
                                                    "SumDiffAV2007","SumDiffAV2008",
                                                    "SumDiffAV2009","SumDiffAV2010",
                                                    "SumDiffAV2011","SumDiffAV2012",
                                                    "SumDiffAV2013","SumDiffAV2014",
                                                    "SumDiffAV2015","SumDiffAV2016",
                                                    "SumDiffAV2017","SumDiffAV2018",
                                                    "SumDiffAV2019","SumDiffAV2020",
                                                    "SumDiffAV2021", "SumDiffAV2022")

Median_PercentValueChangeYear_BG_Yearly <- pal %>%
  filter(!is.na(BG_ID_10)) %>%
  group_by(BG_ID_10) %>%
  summarise(PercChangeAV2001 = median(PercChangeAV2001, na.rm = T), PercChangeAV2002 = median(PercChangeAV2002, na.rm = T), PercChangeAV2003 = median(PercChangeAV2003, na.rm = T), 
            PercChangeAV2004 = median(PercChangeAV2004, na.rm = T), PercChangeAV2005 = median(PercChangeAV2005, na.rm = T), PercChangeAV2006 = median(PercChangeAV2006, na.rm = T), 
            PercChangeAV2007 = median(PercChangeAV2007, na.rm = T), PercChangeAV2008 = median(PercChangeAV2008, na.rm = T), PercChangeAV2009 = median(PercChangeAV2009, na.rm = T), 
            PercChangeAV2010 = median(PercChangeAV2010, na.rm = T), PercChangeAV2011 = median(PercChangeAV2011, na.rm = T), PercChangeAV2012 = median(PercChangeAV2012, na.rm = T), 
            PercChangeAV2013 = median(PercChangeAV2013, na.rm = T), PercChangeAV2014 = median(PercChangeAV2014, na.rm = T), PercChangeAV2015 = median(PercChangeAV2015, na.rm = T), 
            PercChangeAV2016 = median(PercChangeAV2016, na.rm = T), PercChangeAV2017 = median(PercChangeAV2017, na.rm = T), PercChangeAV2018 = median(PercChangeAV2018, na.rm = T), 
            PercChangeAV2019 = median(PercChangeAV2019, na.rm = T), PercChangeAV2020 = median(PercChangeAV2020, na.rm = T), PercChangeAV2021 = median(PercChangeAV2021, na.rm = T), 
            PercChangeAV2022 = median(PercChangeAV2022, na.rm = T),
            GrowthPercChangeAV = median(GrowthPercChangeAV, na.rm = T), CrashPercChangeAV = median(CrashPercChangeAV, na.rm = T), RecoveryPercChangeAV = median(RecoveryPercChangeAV, na.rm = T))


pal.BG <- merge(pal.BG,Median_PercentValueChangeYear_BG_Yearly,by = "BG_ID_10")

pal.BG = pal.BG[,c(
  "BG_ID_10",
  "SumDiffAV2001","PercChangeAV2001","NewCondo2001" ,
  "SumDiffAV2002","PercChangeAV2002","NewCondo2002" ,
  "SumDiffAV2003","PercChangeAV2003","NewCondo2003" ,
  "SumDiffAV2004","PercChangeAV2004","NewCondo2004" ,
  "SumDiffAV2005","PercChangeAV2005","NewCondo2005" ,
  "SumDiffAV2006","PercChangeAV2006","NewCondo2006" ,
  "SumDiffAV2007","PercChangeAV2007","NewCondo2007" ,
  "SumDiffAV2008","PercChangeAV2008","NewCondo2008" ,
  "SumDiffAV2009","PercChangeAV2009","NewCondo2009" ,
  "SumDiffAV2010","PercChangeAV2010","NewCondo2010" ,
  "SumDiffAV2011","PercChangeAV2011","NewCondo2011" ,
  "SumDiffAV2012","PercChangeAV2012","NewCondo2012" ,
  "SumDiffAV2013","PercChangeAV2013","NewCondo2013" ,
  "SumDiffAV2014","PercChangeAV2014","NewCondo2014" ,
  "SumDiffAV2015","PercChangeAV2015","NewCondo2015" ,
  "SumDiffAV2016","PercChangeAV2016","NewCondo2016" ,
  "SumDiffAV2017","PercChangeAV2017","NewCondo2017" ,
  "SumDiffAV2018","PercChangeAV2018","NewCondo2018" ,
  "SumDiffAV2019","PercChangeAV2019","NewCondo2019" ,
  "SumDiffAV2020","PercChangeAV2020","NewCondo2020" ,
  "SumDiffAV2021","PercChangeAV2021","NewCondo2021" ,
  "SumDiffAV2022","PercChangeAV2022","NewCondo2022" ,
  "GrowthPercChangeAV","CrashPercChangeAV" , "RecoveryPercChangeAV" )]

write.csv(pal.BG, paste0(BARI, pal_bg_path) , row.names=F)

# merge to bg shpfile
#names(bgsShp)[5] <- "BG_ID_10"
pal.BG.shp = merge(bgsShp,pal.BG,by="BG_ID_10",all.x=T)

# write bg shpfile
st_write(pal.BG.shp,paste0(BARI, pal_bg_shp_path), pal_bg_shp_name,driver="ESRI Shapefile",
         overwrite_layer=TRUE)



