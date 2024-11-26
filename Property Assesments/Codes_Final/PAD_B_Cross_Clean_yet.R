
### STEP 0  ####
# List of required packages
packages <- c("sp", "sf", "tidyverse")

# Install and load packages
lapply(packages, function(pkg) {
  if (!require(pkg, character.only = TRUE)) install.packages(pkg, dependencies = TRUE)
  library(pkg, character.only = TRUE, quietly = TRUE)
})

### to prevent scientific notation issues ###
options(scipen=100000000)

# set the working directory to the BARI shared drive
BARI <- "~/BARI/Property Assesments/Outputs"

input_dir <- "~/Desktop/Academic/BARI/Scripts/Property Assesments/Inputs"

# Loading functions
cleaning_functions_path <- file.path(input_dir, "Cleaning_functions_20190326.R")
geocoding_functions_path <- file.path(input_dir, "Geocoding_functions_20190326.R")
helper_functions_path <- file.path(input_dir, "Helper_functions_20190326.R")
source(cleaning_functions_path)
source(geocoding_functions_path)
source(helper_functions_path)


### load neccesary inputs ###
PADunit <- read.csv("~/Desktop/Academic/BARI/Scripts/Property Assesments/Outputs/PAD.Record.wUnit.csv")
oldpad <- read.csv("~/Desktop/Academic/BARI/Scripts/Property Assesments/Inputs/PADCross.Record.2022.csv")
cts_10 <- tigris::tracts(state = "MA", county = "Suffolk", year = 2019) %>% dplyr::select(CT_ID_10 = GEOID)
cts_20 <- tigris::tracts(state = "MA", county = "Suffolk", year = 2023) %>% dplyr::select(CT_ID_20 = GEOID)
bgs_10 <- tigris::block_groups(state = "MA",county = "Suffolk",year = 2019) %>%  dplyr::select(BG_ID_10 = GEOID)
bgs_20 <- tigris::block_groups(state = "MA",county = "Suffolk",year = 2023) %>%  dplyr::select(BG_ID_20 = GEOID)
properties <- read.csv("~/Desktop/Academic/BARI/Scripts/GI/Outputs/properties_geo_postsanitychecked_2024.csv")
TLID <- read.csv("Scripts/GI/Outputs/Parcel_final_postsanitychecked_2024.csv") #98146 #98150 in 2023 #97894 in Sep 23
# OUTPUT PATHS

#records
PArecord_final_path = "PADCross.Record.2023.csv"
PArecord_ct_2010 = "PADCross.CT10.2024.csv"
PArecord_ct_2020 = "PADCross.CT20.2024.csv"
PArecord_ctshp_2010 = "PADCross.CT10.2024"
PArecord_ctshp_2020 = "PADCross.CT20.2024"

PArecord_bg_2010 = "PADCross.CBG10.2024.csv"
PArecord_bg_2020 = "PADCross.CBG20.2024.csv"
PArecord_bgshp_2010 = "PADCross.CBG10.2024"
PArecord_bgshp_2020 = "PADCross.CBG20.2024"



### STEP 1  ####

# fix IDs
PAclean = standardizeGeoNames(PADunit)

names(properties)[1] <- "parcel_num"

#add on geo data from GI
# properties2018 "NSA_NAME","BRA_PD" doesnt have these columns
PAclean = merge(PAclean, properties[,c("parcel_num",c("X","Y","Land_Parcel_ID","TLID","Blk_ID_10", "BG_ID_10","CT_ID_10", 
                                                      "Blk_ID_20",  "BG_ID_20", "CT_ID_20"))], by= "parcel_num", all.x=T) # 177483

PAclean <- distinct(PAclean) # 177091 #107591 in 2023 #180626 in Sep 23 #182242 in 2024

#rename columns
PA <- PAclean <- dplyr::rename(
  PAclean,
  PTYPE = LUC,
  ST_NAME_SUFF = ST_NAME,
  AV_LAND = LAND_VALUE,
  AV_BLDG = BLDG_VALUE,
  AV_TOTAL = TOTAL_VALUE,
  YR_REMOD = YR_REMODEL,
  NUM_FLOORS = RES_FLOOR,
  R_ROOF_TYP = ROOF_STRUCTURE,
  R_EXT_FIN = ROOF_COVER,
  R_BDRMS = BED_RMS,
  R_FULL_BTH = FULL_BTH,
  R_HALF_BTH = HLF_BTH,
  R_BTH_STYLE = BTHRM_STYLE1,
  R_BTH_STYLE2 = BTHRM_STYLE2,
  R_BTH_STYLE3 = BTHRM_STYLE3,
  R_KITCH = KITCHEN_TYPE,
  R_KITCH_STYLE = KITCHEN_STYLE1,
  R_KITCH_STYLE2 = KITCHEN_STYLE2,
  R_KITCH_STYLE3 = KITCHEN_STYLE3,
  R_HEAT_TYP = HEAT_TYPE,
  R_AC = AC_TYPE,
  R_EXT_CND = EXT_COND,
  R_OVRALL_CND = OVERALL_COND,
  R_INT_CND = INT_COND,
  R_VIEW = PROP_VIEW,
  U_TOTAL_RMS = TT_RMS,
  S_NUM_BLDG = NUM_BLDGS,
  S_BLDG_STYL = BLDG_TYPE,
  S_UNIT_RES = RES_UNITS,
  S_UNIT_COM = COM_UNITS,
  U_NUM_PARK = NUM_PARKING,
  U_CORNER = CORNER_UNIT
)



# Columns to format and trim
cols_to_process <- c("parcel_num", "GIS_ID", "Blk_ID_10", "BG_ID_10", "CT_ID_10",
                     "Blk_ID_20", "BG_ID_20", "CT_ID_20", "TLID", "Land_Parcel_ID")

# Format and trim whitespace for specified columns
PA <- PA %>%
  mutate(across(all_of(cols_to_process), ~ format(as.numeric(.), scientific = FALSE))) %>%
  mutate(across(all_of(cols_to_process), ~ trimws(.))) 

# Replace "NA" strings with actual NA values
PA <- PA %>%
  mutate(across(all_of(cols_to_process), ~ ifelse(. == "NA", NA, .)))


#-------------------------------------------#
#       Create new vars                     #
#-------------------------------------------#

#PA = PAclean
# doesn't exist anymore
# PA$LU[PA$LU == 'XX'] <- "E"

## Allocating Residential Heat Type Energy Efficiency Score
PA$HEAT_SCORE <- NA
PA$HEAT_SCORE <- ifelse(PA$R_HEAT_TYP == 'S - Space Heat', 0, PA$HEAT_SCORE)
PA$HEAT_SCORE <- ifelse(PA$R_HEAT_TYP == 'W - Ht Water/Steam', 1, PA$HEAT_SCORE)
PA$HEAT_SCORE <- ifelse(PA$R_HEAT_TYP == 'P - Heat Pump', 2, PA$HEAT_SCORE)
PA$HEAT_SCORE <- ifelse(PA$R_HEAT_TYP == 'F - Forced Hot Air', 3, PA$HEAT_SCORE)
PA$HEAT_SCORE <- ifelse(PA$R_HEAT_TYP == 'E - Electric', 4, PA$HEAT_SCORE) 

## Allocating age to Residential buildings
### none of these are present in the data 2022 update
PA$YR_BUILT[PA$YR_BUILT == '' | PA$YR_BUILT == 0] <- NA
PA$YR_REMOD[PA$YR_REMOD == '' | PA$YR_REMOD == 0] <- NA

PA$YR_REMOD[!is.na(PA$YR_REMOD) & PA$YR_REMOD==995]=1995 ### this happened one time only 
###TYPOS in 2023 data
PA$YR_BUILT[which(PA$YR_BUILT==20198)] <- NA
PA$YR_REMOD[which(PA$YR_REMOD==2121)] <- NA
PA$YR_REMOD[which(PA$YR_REMOD==2921)] <- NA
PA$YR_REMOD[which(PA$YR_REMOD==20220)] <- NA


PA$BLDG_AGE <- ifelse(is.na(PA$YR_REMOD), (2023 - PA$YR_BUILT), (2023 - PA$YR_REMOD))
PA$BLDG_AGE[PA$BLDG_AGE <=0] <- NA

##Allocating Building Age Score

PA$AGE_SCORE <- NA
PA$AGE_SCORE <- ifelse(PA$BLDG_AGE < 50,4,PA$AGE_SCORE)
PA$AGE_SCORE <- ifelse(PA$BLDG_AGE >= 50,3,PA$AGE_SCORE)
PA$AGE_SCORE <- ifelse(PA$BLDG_AGE >= 100,2,PA$AGE_SCORE)
PA$AGE_SCORE <- ifelse(PA$BLDG_AGE >= 150,1,PA$AGE_SCORE)
PA$AGE_SCORE <- ifelse(PA$BLDG_AGE >= 200,0,PA$AGE_SCORE)

## Allocating Residential Air Conditioner Energy Efficiency Score
PA$COOL_SCORE <- NA
PA$COOL_SCORE <- ifelse(PA$R_AC == 'C - Central AC', 1, PA$COOL_SCORE)
PA$COOL_SCORE <- ifelse(PA$R_AC == 'D - Ductless AC', 2, PA$COOL_SCORE)
PA$COOL_SCORE <- ifelse(PA$R_AC == 'N - None', 3, PA$COOL_SCORE)

## Aggregate Energy Efficiency score at building level
PA$EE_SCORE <- PA$AGE_SCORE+0.75*PA$HEAT_SCORE+0.75*PA$COOL_SCORE
PA = PA
#PA[!(PA$LU =='R1'| PA$LU == 'R2'| PA$LU == 'R3'), 
#   c("HEAT_SCORE","COOL_SCORE","EE_SCORE")]=c(NA,NA,NA)

#BLDG PER SF missing: AH, CC, CL, CM, CP, RL (many E)
#TOTAL PER SF missing: AH, CC, CL, CM, CP, RL (many E)
#HEAT, COOL, EE missing: all but R1-3
#AGE missing: AH, CC, CL, CP, RL (many E)

PA$AV_BLDG <- as.numeric(gsub("[\\$,]", "", PA$AV_BLDG))
PA$GROSS_AREA <- as.numeric(gsub("[\\$,]", "", PA$GROSS_AREA))
PA$AV_LAND <- as.numeric(gsub("[\\$,]", "", PA$AV_LAND))
PA$AV_TOTAL <- as.numeric(gsub("[\\$,]", "", PA$AV_TOTAL))
PA$LAND_SF <- as.numeric(gsub("[\\$,]", "", PA$LAND_SF))

#And "AV_BLDG_PER_SF" will give us the assessed value per square foot of a building:
PA <- transform(PA, AV_BLDG_PER_SF = ifelse((AV_BLDG != 0 & GROSS_AREA != 0), 
                                            AV_BLDG / GROSS_AREA, NA))
PA <- transform(PA, AV_LAND_PER_SF =  ifelse((AV_LAND != 0 & LAND_SF != 0), 
                                             AV_LAND / LAND_SF,NA))


simplify_LU <- function(LU) {
  if (LU %in% c("R1", "R2", "R3", "R4", "RL", "A")) {
    return("RESIDENTIAL")
  } else if (LU %in% c("CM", "CP")) {
    return("CONDO")
  } else if (LU == "CD") {
    return("CONDO_UNIT")
  } else if (LU == "RC") {
    return("MIX_RC")
  } else if (LU %in% c("CC", "C", "CL")) {
    return("COMMERCIAL")
  } else if (LU == "AH") {
    return("AGRICULTURAL")
  } else if (LU == "I") {
    return("INDUSTRIAL")
  } else if (LU == "E") {
    return("TAX_EXEMPT")
  } else if (LU == "EA") {
    return("TAX_EXEMPT_BRA")
  } else {
    return(NA)
  }
}

#Create a new column by applying the simplifyLU function
PA <- transform(PA, SIMPLIFIED_LU = sapply(LU, simplify_LU))

varnames = c("parcel_num","CM_ID","GIS_ID","ST_NUM" ,"ST_NAME_SUFF","UNIT_NUM",
             "ZIP_CODE","PTYPE","LU","OWN_OCC","OWNER","MAIL_ADDRESSEE", "MAIL_STREET_ADDRESS","MAIL_CITY", "MAIL_STATE", "MAIL_ZIP_CODE",
             "AV_LAND","AV_BLDG","AV_TOTAL","GROSS_TAX","LAND_SF",
             "YR_BUILT","YR_REMOD","GROSS_AREA","NUM_FLOORS","STRUCTURE_CLASS",
             "R_ROOF_TYP","R_EXT_FIN","R_BDRMS","R_FULL_BTH",
             "R_HALF_BTH","R_BTH_STYLE","R_BTH_STYLE2","R_BTH_STYLE3","R_KITCH",
             "R_KITCH_STYLE","R_KITCH_STYLE2","R_KITCH_STYLE3","R_HEAT_TYP","R_AC",
             "FIREPLACES","R_EXT_CND","R_OVRALL_CND","R_INT_CND","R_VIEW",    
             "S_NUM_BLDG","S_BLDG_STYL","S_UNIT_RES","S_UNIT_COM","R_EXT_FIN",
             "U_NUM_PARK","U_CORNER","LIVING_AREA","AV_BLDG_PER_SF",
             "AV_LAND_PER_SF", "SIMPLIFIED_LU","COOL_SCORE", "AGE_SCORE", "HEAT_SCORE", "EE_SCORE",
             "BLDG_AGE","X","Y","CITY","LU_DESC","CD_FLOOR","INT_WALL","KITCHENS","U_TOTAL_RMS","BDRM_COND","HEAT_SYSTEM" ,
             "Land_Parcel_ID","TLID","Blk_ID_10","BG_ID_10","CT_ID_10", "Blk_ID_20", "BG_ID_20", "CT_ID_20", "unit_N", "unit_N_orig")

# no "PlUMBING"
########### Some column names have changed in 2023
setdiff(names(PA),varnames)
setdiff(varnames, names(PA))
PA = PA[,varnames]

names(PA)[1] <- "PID" # 177091 #107591 in 2023 #180626 in Sep 23


#TLID$TLID <- format(TLID$TLID, scientific = F)
PA$TLID <- TLID$TLID[match(PA$Land_Parcel_ID,TLID$Land_Parcel_ID)]

nrow(PA)
summary(PA) #177091 #107591 in 2023
# check NAs
sapply(PA, function(x) sum(is.na(x)))


# convert back to new column names
# Define old and new column names as vectors
old_names <- c("PTYPE", "ST_NAME_SUFF", "AV_LAND", "AV_BLDG", "AV_TOTAL", 
               "YR_REMOD", "NUM_FLOORS", "R_ROOF_TYP", "R_EXT_FIN", "R_BDRMS", 
               "R_FULL_BTH", "R_HALF_BTH", "R_BTH_STYLE", "R_BTH_STYLE2", "R_BTH_STYLE3",
               "R_KITCH", "R_KITCH_STYLE", "R_KITCH_STYLE2", "R_KITCH_STYLE3", 
               "R_HEAT_TYP", "R_AC", "FIREPLACES", "R_EXT_CND", "R_OVRALL_CND",
               "R_INT_CND", "R_VIEW", "U_TOTAL_RMS", "S_NUM_BLDG", "S_BLDG_STYL", 
               "S_UNIT_RES", "S_UNIT_COM", "R_EXT_FIN.1", "U_NUM_PARK", "U_CORNER", 
               "KITCHENS", "ZIP_CODE")

new_names <- c("LUC", "ST_NAME", "LAND_VALUE", "BLDG_VALUE", "TOTAL_VALUE",
               "YR_REMODEL", "RES_FLOOR", "ROOF_STRUCTURE", "ROOF_COVER", "BED_RMS",
               "FULL_BTH", "HLF_BTH", "BTHRM_STYLE1", "BTHRM_STYLE2", "BTHRM_STYLE3",
               "KITCHEN_TYPE", "KITCHEN_STYLE1", "KITCHEN_STYLE2", "KITCHEN_STYLE3", 
               "HEAT_TYPE", "AC_TYPE", "FIRE_PLACE", "EXT_COND", "OVERALL_COND",
               "INT_COND", "PROP_VIEW", "TT_RMS", "NUM_BLDGS", "BLDG_TYPE", 
               "RES_UNITS", "COM_UNITS", "EXT_FINISHED", "NUM_PARKING", "CORNER_UNIT",
               "KITCHEN", "ZIPCODE")

# Rename columns using `rename_with`
PADcross <- PA %>%
  rename_with(~ new_names[match(.x, old_names)], .cols = all_of(old_names))



##########

setdiff(names(oldpad), names(PADcross))
setdiff(names(PADcross), names(oldpad))
PADcross <- dplyr::rename(PADcross, MAIL_ADDRESS = MAIL_STREET_ADDRESS)
PADcross <- dplyr::rename(PADcross, MAIL_ZIPCODE=MAIL_ZIP_CODE)
PADcross <- dplyr::rename(PADcross, HEAT_FUEL=HEAT_SYSTEM)
PADcross <- PADcross[,names(oldpad)]
write.csv(PADcross,PArecord_final_path,row.names=F)

#### AGGREGATE ####

PA <- PADcross
#HEAT, COOL, EE missing: all but R1-3
#BLDG/LAND PER SF missing: AH, CC, CL, CM, CP, RL (many E)
#AGE missing: AH, CC, CL, CP, RL (many E)
#HEAT/COOL - only R1-3 (but syntax is in same form as below so everything is clear)
ee_res = PA$LU == "R1" | PA$LU == "R2" | PA$LU == "R3"
PA$EE_SCORE.res[ee_res]=PA$EE_SCORE[ee_res] #this is null??? 2023
# deal with outliers
summary(PA$AV_BLDG_PER_SF)
summary(PA$AV_LAND_PER_SF)
summary(PA$AV_BLDG_PER_SF.nonres) #0
summary(PA$AV_BLDG_PER_SF.res) #0
summary(PA$AV_LAND_PER_SF.nonres) #0
summary(PA$AV_LAND_PER_SF.res) #0
summary(PA$AV_BLDG)

# let's go with dropping the top .4 percent
PA <- PA[PA$AV_BLDG_PER_SF < quantile(PA$AV_BLDG_PER_SF, 0.996, na.rm = T) | is.na(PA$AV_BLDG_PER_SF),]
#VALUATIONS PER SF - separate for residential and non-res, for land usages which values 
#   consistently exist
#valuation_res = (PA$LU == "R1" | PA$LU == "R2" | PA$LU == "R3" | PA$LU == "R4" 
#                | PA$LU == "A" | PA$LU == "CD")

#PA$AV_LAND_PER_SF.res[valuation_res] = PA$AV_LAND_PER_SF[valuation_res]
#PA$AV_BLDG_PER_SF.res[valuation_res] =  PA$AV_BLDG_PER_SF[ valuation_res]


# new way
PA$AV_LAND_PER_SF.res <- ifelse((PA$LU == "R1" | PA$LU == "R2" | PA$LU == "R3" | PA$LU == "R4" | PA$LU == "A" | PA$LU == "CD"), PA$AV_LAND_PER_SF, NA)
PA$AV_BLDG_PER_SF.res <- ifelse((PA$LU == "R1" | PA$LU == "R2" | PA$LU == "R3" | PA$LU == "R4" | PA$LU == "A" | PA$LU == "CD"), PA$AV_BLDG_PER_SF, NA)


#valuation_nonres = (PA$LU == "C" | PA$LU == "E" | PA$LU == "EA" | PA$LU == "I" 
#               | PA$LU == "RC" )

#PA$AV_LAND_PER_SF.nonres[valuation_nonres] = PA$AV_LAND_PER_SF[valuation_nonres]
#PA$AV_BLDG_PER_SF.nonres[valuation_nonres] =  PA$AV_BLDG_PER_SF[ valuation_nonres]

PA$AV_LAND_PER_SF.nonres <- ifelse((PA$LU == "C" | PA$LU == "E" | PA$LU == "EA" | PA$LU == "I" 
                                    | PA$LU == "RC" ), PA$AV_LAND_PER_SF, NA)
PA$AV_BLDG_PER_SF.nonres <- ifelse((PA$LU == "C" | PA$LU == "E" | PA$LU == "EA" | PA$LU == "I" 
                                    | PA$LU == "RC" ), PA$AV_BLDG_PER_SF, NA)

#AGE  - separate for residential and non-res, for land usages which values consistently 
#   exist
PA$YR_BUILT_REMOD = ifelse(is.na(PA$YR_REMODEL),PA$YR_BUILT,PA$YR_REMODEL)
PA$DEC_BUILT_REMOD = floor(PA$YR_BUILT_REMOD/10)*10

#age_res = (PA$LU == "R1" | PA$LU == "R2" | PA$LU == "R3" | PA$LU == "R4" | PA$LU == "A" 
#          | PA$LU == "CD" | PA$LU == "CM")

#age_res = (PA$LU == "R1" | PA$LU == "R2" | PA$LU == "R3" | PA$LU == "R4" | PA$LU == "A" 
#           | PA$LU == "CD")

PA$YR_BUILT_REMOD.res <- ifelse((PA$LU == "R1" | PA$LU == "R2" | PA$LU == "R3" | PA$LU == "R4" | PA$LU == "A" 
                                 | PA$LU == "CD"), PA$YR_BUILT_REMOD, NA)

PA$DEC_BUILT_REMOD.res <- ifelse((PA$LU == "R1" | PA$LU == "R2" | PA$LU == "R3" | PA$LU == "R4" | PA$LU == "A" 
                                  | PA$LU == "CD"), PA$DEC_BUILT_REMOD, NA)

#PA$YR_BUILT_REMOD.res[age_res] = PA$YR_BUILT_REMOD[age_res]
#PA$DEC_BUILT_REMOD.res[age_res] = PA$DEC_BUILT_REMOD[age_res]

#age_nonres = (PA$LU == "C" | PA$LU == "E" | PA$LU == "EA" | PA$LU == "I" | PA$LU == "RC" )

PA$YR_BUILT_REMOD.nonres <- ifelse((PA$LU == "C" | PA$LU == "E" | PA$LU == "EA" | PA$LU == "I" | PA$LU == "RC" ), PA$YR_BUILT_REMOD, NA)

PA$DEC_BUILT_REMOD.nonres <- ifelse((PA$LU == "C" | PA$LU == "E" | PA$LU == "EA" | PA$LU == "I" | PA$LU == "RC" ), PA$DEC_BUILT_REMOD, NA)


#PA$YR_BUILT_REMOD.nonres[age_nonres] = PA$YR_BUILT_REMOD[age_nonres]
#PA$DEC_BUILT_REMOD.nonres[age_nonres] = PA$DEC_BUILT_REMOD[age_nonres]

# handling NAs - NOPE DONT DO THIS
#PA$EE_SCORE.res <- ifelse(is.na(PA$EE_SCORE.res), 0 , PA$EE_SCORE.res)
#PA$AV_LAND_PER_SF.res <- ifelse(is.na(PA$AV_LAND_PER_SF.res), 0, PA$AV_LAND_PER_SF.res)
#PA$AV_BLDG_PER_SF.res <- ifelse(is.na(PA$AV_BLDG_PER_SF.res), 0, PA$AV_BLDG_PER_SF.res)
#PA$AV_LAND_PER_SF.nonres <- ifelse(is.na(PA$AV_LAND_PER_SF.nonres), 0, PA$AV_LAND_PER_SF.nonres)
#PA$AV_BLDG_PER_SF.nonres <- ifelse(is.na(PA$AV_BLDG_PER_SF.nonres), 0, PA$AV_BLDG_PER_SF.nonres)
#PA$YR_BUILT_REMOD.res <- ifelse(is.na(PA$YR_BUILT_REMOD.res), 0, PA$YR_BUILT_REMOD.res)

# PA$DEC_BUILT_REMOD.res <- ifelse(is.na(PA$DEC_BUILT_REMOD.res), 0, PA$DEC_BUILT_REMOD.res)
# PA$DEC_BUILT_REMOD.nonres <- ifelse(is.na(PA$DEC_BUILT_REMOD.nonres), 0, PA$DEC_BUILT_REMOD.nonres)

#define custom Mode function
Mode <- function(x) {                     # Create mode function 
  unique_x <- unique(x)
  tabulate_x <- tabulate(match(x, unique_x))
  unique_x[tabulate_x == max(tabulate_x)]
}
# # create mode function
# Mode <- function(x) {
#   ux <- unique(x)
#   ux[which.max(tabulate(match(x, ux)))]
# }

### Aggregate by census tract 10 ####
PA.agg.CT.mean = aggregate(cbind(YR_BUILT_REMOD.res,YR_BUILT_REMOD.nonres)~CT_ID_10,
                           data=PA,FUN=mean,na.action=na.pass, na.rm=T)

PA.agg.CT.mean$YR_BUILT_REMOD.res <- round(PA.agg.CT.mean$YR_BUILT_REMOD.res)
PA.agg.CT.mean$YR_BUILT_REMOD.nonres <- round(PA.agg.CT.mean$YR_BUILT_REMOD.nonres)
PA.agg.CT.mean$YR_BUILT_REMOD.res[is.nan(PA.agg.CT.mean$YR_BUILT_REMOD.res)] <- NA
PA.agg.CT.mean$YR_BUILT_REMOD.nonres[is.nan(PA.agg.CT.mean$YR_BUILT_REMOD.nonres)] <- NA


PA.agg.CT.median = aggregate(cbind(EE_SCORE.res, 
                                   AV_LAND_PER_SF.res,AV_LAND_PER_SF.nonres,
                                   AV_BLDG_PER_SF.res,AV_BLDG_PER_SF.nonres)~CT_ID_10,
                             data=PA,FUN=median,na.action=na.pass, na.rm=T)

#PA.agg.CT.mode = aggregate(cbind(DEC_BUILT_REMOD.res,DEC_BUILT_REMOD.nonres)~CT_ID_10,
#                           data=PA,FUN = Mode,na.action=na.pass)


PA.agg.CT.mode.res = aggregate(DEC_BUILT_REMOD.res~CT_ID_10,
                               data=na.omit(PA[c("DEC_BUILT_REMOD.res", "CT_ID_10")]),FUN = Mode)
PA.agg.CT.mode.nonres = aggregate(DEC_BUILT_REMOD.nonres~CT_ID_10,
                                  data=na.omit(PA[c("DEC_BUILT_REMOD.nonres", "CT_ID_10")]),FUN = Mode)

PA.agg.CT.mode <- merge(PA.agg.CT.mode.res, PA.agg.CT.mode.nonres, by="CT_ID_10", all = T)
PA.agg.CT.mode$DEC_BUILT_REMOD.res[PA.agg.CT.mode$DEC_BUILT_REMOD.res == "NULL"] <- NA
PA.agg.CT.mode$DEC_BUILT_REMOD.res <- sapply(PA.agg.CT.mode$DEC_BUILT_REMOD.res, function(x) x[1])
PA.agg.CT.mode$DEC_BUILT_REMOD.nonres <- sapply(PA.agg.CT.mode$DEC_BUILT_REMOD.nonres, function(x) x[1])



PA.agg.CT.mean = merge(PA.agg.CT.median,PA.agg.CT.mean,by="CT_ID_10",all=T)
PA.agg.CT = merge(PA.agg.CT.mean,PA.agg.CT.mode,by="CT_ID_10",all=T)


######## go to script C to obtain proptracts_effects ########
tract.unique <- read.csv("~/Desktop/Academic/BARI/Scripts/Property Assesments/Outputs/proptracts_effects_2024_10.csv")

### Aggregate by census tract 10 ####
tract.unique$CT_ID_10 <- as.numeric(tract.unique$CT_ID_10)
PA.agg.CT$CT_ID_10 <-as.numeric(PA.agg.CT$CT_ID_10)
PA.agg.CT <- left_join(PA.agg.CT, tract.unique[,c("CT_ID_10", "random")], by = "CT_ID_10")

names(PA.agg.CT)[11] <- "nbhdval" #random

PA.agg.CT %>%  # 
  summarise_all(list(~sum(is.na(.))))

sapply(PA.agg.CT, function(x) sum(is.nan(x)))

# write census tract aggregation
write.csv(PA.agg.CT, "PADCross.CT.10.csv",row.names=F)

# rename the tract aggregates so the names are short enough for the shape file
PA.agg.CT.rename = dplyr::rename(PA.agg.CT, EESR = EE_SCORE.res, ALPSFR = AV_LAND_PER_SF.res, 
                          ALPSFN = AV_LAND_PER_SF.nonres,ABPSFR = AV_BLDG_PER_SF.res, 
                          ABPSFN = AV_BLDG_PER_SF.nonres, YBRR = YR_BUILT_REMOD.res, 
                          YBRN = YR_BUILT_REMOD.nonres, DBRR = DEC_BUILT_REMOD.res, 
                          DBRN = DEC_BUILT_REMOD.nonres, 
                          nbhdval = nbhdval 
)

PA.agg.CT.shp = merge(PA.agg.CT.rename, cts_10, by="CT_ID_10",all.x=T, duplicateGeoms = TRUE)

PA.agg.CT.shp %>%  # 
  summarise_all(list(~sum(is.na(.))))
PA.agg.CT.shp <- st_as_sf(
  PA.agg.CT.shp,
  crs = 4326
)
# Fix any invalid geometries, if present
PA.agg.CT.shp <- st_make_valid(PA.agg.CT.shp)
# Convert CT_ID_10 to character to prevent truncation
PA.agg.CT.shp$CT_ID_10 <- as.character(PA.agg.CT.shp$CT_ID_10)

# Write the shapefile with the adjusted field
st_write(PA.agg.CT.shp, "PADCross.CT.10", driver = "ESRI Shapefile", overwrite_layer = TRUE)


### Aggregate by census tract 20 ####
sum(is.na(PA$CT_ID_20))
PA.agg.CT.mean = aggregate(cbind(YR_BUILT_REMOD.res,YR_BUILT_REMOD.nonres)~CT_ID_20,
                           data=PA,FUN=mean,na.action=na.pass, na.rm=T)
PA.agg.CT.mean$YR_BUILT_REMOD.res <- round(PA.agg.CT.mean$YR_BUILT_REMOD.res)
PA.agg.CT.mean$YR_BUILT_REMOD.nonres <- round(PA.agg.CT.mean$YR_BUILT_REMOD.nonres)
PA.agg.CT.mean$YR_BUILT_REMOD.res[is.nan(PA.agg.CT.mean$YR_BUILT_REMOD.res)] <- NA
PA.agg.CT.mean$YR_BUILT_REMOD.nonres[is.nan(PA.agg.CT.mean$YR_BUILT_REMOD.nonres)] <- NA


PA.agg.CT.median = aggregate(cbind(EE_SCORE.res, 
                                   AV_LAND_PER_SF.res,AV_LAND_PER_SF.nonres,
                                   AV_BLDG_PER_SF.res,AV_BLDG_PER_SF.nonres)~CT_ID_20,
                             data=PA,FUN=median,na.action=na.pass, na.rm=T)

PA.agg.CT.mode.res = aggregate(DEC_BUILT_REMOD.res~CT_ID_20,
                               data=na.omit(PA[c("DEC_BUILT_REMOD.res", "CT_ID_20")]),FUN = Mode)
PA.agg.CT.mode.nonres = aggregate(DEC_BUILT_REMOD.nonres~CT_ID_20,
                                  data=na.omit(PA[c("DEC_BUILT_REMOD.nonres", "CT_ID_20")]),FUN = Mode)

PA.agg.CT.mode <- merge(PA.agg.CT.mode.res, PA.agg.CT.mode.nonres, by="CT_ID_20", all = T)
PA.agg.CT.mode$DEC_BUILT_REMOD.res[PA.agg.CT.mode$DEC_BUILT_REMOD.res == "NULL"] <- NA
PA.agg.CT.mode$DEC_BUILT_REMOD.res <- sapply(PA.agg.CT.mode$DEC_BUILT_REMOD.res, function(x) x[1])
PA.agg.CT.mode$DEC_BUILT_REMOD.nonres <- sapply(PA.agg.CT.mode$DEC_BUILT_REMOD.nonres, function(x) x[1])

summary(PA$DEC_BUILT_REMOD.nonres)
table(PA$DEC_BUILT_REMOD,PA$LU)
table(is.na(PA$DEC_BUILT_REMOD),PA$LU)

PA.agg.CT.mean = merge(PA.agg.CT.median,PA.agg.CT.mean,by="CT_ID_20",all=T)
PA.agg.CT = merge(PA.agg.CT.mean,PA.agg.CT.mode,by="CT_ID_20",all=T)

tract.unique <- read.csv("~/Desktop/Academic/BARI/Scripts/Property Assesments/Outputs/proptracts_effects_2024_20.csv")


tract.unique$CT_ID_20 <-as.numeric(tract.unique$CT_ID_20)
PA.agg.CT$CT_ID_20 <-as.numeric(PA.agg.CT$CT_ID_20)
PA.agg.CT <- left_join(PA.agg.CT, tract.unique[,c("CT_ID_20", "random")], by = "CT_ID_20")

names(PA.agg.CT)[11] <- "nbhdval" #random

PA.agg.CT %>%  # 
  summarise_all(list(~sum(is.na(.))))


# write census tract aggregation
write.csv(PA.agg.CT, "PADCross.CT.20.csv",row.names=F)


# rename the tract aggregates so the names are short enough for the shape file
PA.agg.CT.rename = dplyr::rename(PA.agg.CT, EESR = EE_SCORE.res, ALPSFR = AV_LAND_PER_SF.res, 
                          ALPSFN = AV_LAND_PER_SF.nonres,ABPSFR = AV_BLDG_PER_SF.res, 
                          ABPSFN = AV_BLDG_PER_SF.nonres, YBRR = YR_BUILT_REMOD.res, 
                          YBRN = YR_BUILT_REMOD.nonres, DBRR = DEC_BUILT_REMOD.res, 
                          DBRN = DEC_BUILT_REMOD.nonres, 
                          nbhdval = nbhdval 
)

# merge to tract shapefile 

PA.agg.CT.shp = merge(PA.agg.CT.rename, cts_20, by="CT_ID_20",all.x=T, duplicateGeoms = TRUE)

PA.agg.CT.shp %>%  # 
  summarise_all(list(~sum(is.na(.))))

PA.agg.CT.shp[st_is_empty(PA.agg.CT.shp$geometry),"geometry"]

PA.agg.CT.shp <- st_set_crs(PA.agg.CT.shp, 4326)


# Fix any invalid geometries, if present
PA.agg.CT.shp <- st_make_valid(PA.agg.CT.shp)
# Convert CT_ID_10 to character to prevent truncation
PA.agg.CT.shp$CT_ID_20 <- as.character(PA.agg.CT.shp$CT_ID_20)


st_write(PA.agg.CT.shp, "PADCross.CT.20", driver="ESRI Shapefile", 
         overwrite_layer = T)



#### aggregate be census block group 10 ####
PA.CBG <- PA[!(PA$BG_ID_10 %in% c(250259901010, 250259811002)), ]


PA.agg.CBG.mean = aggregate(cbind(YR_BUILT_REMOD.res,YR_BUILT_REMOD.nonres)~BG_ID_10,
                            data=PA.CBG,FUN=mean,na.action=na.pass, na.rm=T)
PA.agg.CBG.mean$YR_BUILT_REMOD.res <- round(PA.agg.CBG.mean$YR_BUILT_REMOD.res)
PA.agg.CBG.mean$YR_BUILT_REMOD.nonres <- round(PA.agg.CBG.mean$YR_BUILT_REMOD.nonres)
PA.agg.CBG.mean$YR_BUILT_REMOD.res[is.nan(PA.agg.CBG.mean$YR_BUILT_REMOD.res)] <- NA
PA.agg.CBG.mean$YR_BUILT_REMOD.nonres[is.nan(PA.agg.CBG.mean$YR_BUILT_REMOD.nonres)] <- NA


PA.agg.CBG.median = aggregate(cbind(EE_SCORE.res, AV_LAND_PER_SF.res,AV_LAND_PER_SF.nonres,
                                    AV_BLDG_PER_SF.res,AV_BLDG_PER_SF.nonres)~BG_ID_10,
                              data=PA.CBG,FUN=median,na.action=na.pass, na.rm=T)

#PA.agg.CBG.mode = aggregate(cbind(DEC_BUILT_REMOD.res,DEC_BUILT_REMOD.nonres)~BG_ID_10,
#                            data=PA.CBG,FUN=Mode,na.action=na.pass)

PA.agg.CBG.mode.res = aggregate(DEC_BUILT_REMOD.res~BG_ID_10,
                                data=na.omit(PA[c("DEC_BUILT_REMOD.res", "BG_ID_10")]),FUN = Mode)
PA.agg.CBG.mode.nonres = aggregate(DEC_BUILT_REMOD.nonres~BG_ID_10,
                                   data=na.omit(PA[c("DEC_BUILT_REMOD.nonres", "BG_ID_10")]),FUN = Mode)

PA.agg.CBG.mode <- merge(PA.agg.CBG.mode.res, PA.agg.CBG.mode.nonres, by="BG_ID_10", all = T)
PA.agg.CBG.mode$DEC_BUILT_REMOD.res[PA.agg.CBG.mode$DEC_BUILT_REMOD.res == "NULL"] <- NA
PA.agg.CBG.mode$DEC_BUILT_REMOD.res <- sapply(PA.agg.CBG.mode$DEC_BUILT_REMOD.res, function(x) x[1])
PA.agg.CBG.mode$DEC_BUILT_REMOD.nonres <- sapply(PA.agg.CBG.mode$DEC_BUILT_REMOD.nonres, function(x) x[1])


PA.agg.CBG.mean = merge(PA.agg.CBG.median,PA.agg.CBG.mean,by="BG_ID_10",all=T)
PA.agg.CBG = merge(PA.agg.CBG.mean,PA.agg.CBG.mode,by="BG_ID_10",all=T) #559

PA.agg.CBG %>%  # replace to your needs
  summarise_all(list(~sum(is.na(.))))


#check the number of NaNs, Nulls and NAs
sapply(PA.agg.CBG, function(x) sum(is.nan(x)))
sapply(PA.agg.CBG, function(x) sum(is.null(x)))
sapply(PA.agg.CBG, function(x) sum(is.na(x)))

write.csv(PA.agg.CBG, "PADCross.CBG.10.csv", row.names = F)


# rename so that variable names are not cut off in shape file
PA.agg.CBG.rename = dplyr::rename(PA.agg.CBG, EESR = EE_SCORE.res, 
                           ALPSFR = AV_LAND_PER_SF.res, 
                           ALPSFN = AV_LAND_PER_SF.nonres, ABPSFR = AV_BLDG_PER_SF.res, 
                           ABPSFN = AV_BLDG_PER_SF.nonres, YBRR = YR_BUILT_REMOD.res, 
                           YBRN = YR_BUILT_REMOD.nonres, DBRR = DEC_BUILT_REMOD.res, 
                           DBRN = DEC_BUILT_REMOD.nonres)

PA.agg.CBG.shp = merge(PA.agg.CBG.rename, bgs_10, by="BG_ID_10",all.x=T)

PA.agg.CBG.shp[st_is_empty(PA.agg.CBG.shp$geometry),"geometry"]

st_write(PA.agg.CBG.shp, "PADCross.CBG.10", driver="ESRI Shapefile", 
         overwrite_layer = T)


#### aggregate be census block group 20 ####
PA.CBG <- PA[!(PA$BG_ID_20 %in% c(250259901010, 250259811002)), ]

PA.CBG <- PA
PA.agg.CBG.mean = aggregate(cbind(YR_BUILT_REMOD.res,YR_BUILT_REMOD.nonres)~BG_ID_20,
                            data=PA.CBG,FUN=mean,na.action=na.pass, na.rm=T)
PA.agg.CBG.mean$YR_BUILT_REMOD.res <- round(PA.agg.CBG.mean$YR_BUILT_REMOD.res)
PA.agg.CBG.mean$YR_BUILT_REMOD.nonres <- round(PA.agg.CBG.mean$YR_BUILT_REMOD.nonres)
PA.agg.CBG.mean$YR_BUILT_REMOD.res[is.nan(PA.agg.CBG.mean$YR_BUILT_REMOD.res)] <- NA
PA.agg.CBG.mean$YR_BUILT_REMOD.nonres[is.nan(PA.agg.CBG.mean$YR_BUILT_REMOD.nonres)] <- NA



PA.agg.CBG.median = aggregate(cbind(EE_SCORE.res, AV_LAND_PER_SF.res,AV_LAND_PER_SF.nonres,
                                    AV_BLDG_PER_SF.res,AV_BLDG_PER_SF.nonres)~BG_ID_20,
                              data=PA.CBG,FUN=median,na.action=na.pass, na.rm=T)

#PA.agg.CBG.mode = aggregate(cbind(DEC_BUILT_REMOD.res,DEC_BUILT_REMOD.nonres)~BG_ID_20,
#                            data=PA.CBG,FUN=Mode,na.action=na.pass)

PA.agg.CBG.mode.res = aggregate(DEC_BUILT_REMOD.res~BG_ID_20,
                                data=na.omit(PA[c("DEC_BUILT_REMOD.res", "BG_ID_20")]),FUN = Mode)
PA.agg.CBG.mode.nonres = aggregate(DEC_BUILT_REMOD.nonres~BG_ID_20,
                                   data=na.omit(PA[c("DEC_BUILT_REMOD.nonres", "BG_ID_20")]),FUN = Mode)

PA.agg.CBG.mode <- merge(PA.agg.CBG.mode.res, PA.agg.CBG.mode.nonres, by="BG_ID_20", all = T)
PA.agg.CBG.mode$DEC_BUILT_REMOD.res[PA.agg.CBG.mode$DEC_BUILT_REMOD.res == "NULL"] <- NA
PA.agg.CBG.mode$DEC_BUILT_REMOD.res <- sapply(PA.agg.CBG.mode$DEC_BUILT_REMOD.res, function(x) x[1])
PA.agg.CBG.mode$DEC_BUILT_REMOD.nonres <- sapply(PA.agg.CBG.mode$DEC_BUILT_REMOD.nonres, function(x) x[1])



PA.agg.CBG.mean = merge(PA.agg.CBG.median,PA.agg.CBG.mean,by="BG_ID_20",all=T)
PA.agg.CBG = merge(PA.agg.CBG.mean,PA.agg.CBG.mode,by="BG_ID_20",all=T) #559

PA.agg.CBG %>%  # replace to your needs
  summarise_all(list(~sum(is.na(.))))



#check the number of NaNs, Nulls and NAs
sapply(PA.agg.CBG, function(x) sum(is.nan(x)))
sapply(PA.agg.CBG, function(x) sum(is.null(x)))
sapply(PA.agg.CBG, function(x) sum(is.na(x)))



# write cbg file
write.csv(PA.agg.CBG,"PADCross.CBG.20.csv",row.names=F)


# rename so that variable names are not cut off in shape file
PA.agg.CBG.rename = dplyr::rename(PA.agg.CBG, EESR = EE_SCORE.res, 
                           ALPSFR = AV_LAND_PER_SF.res, 
                           ALPSFN = AV_LAND_PER_SF.nonres, ABPSFR = AV_BLDG_PER_SF.res, 
                           ABPSFN = AV_BLDG_PER_SF.nonres, YBRR = YR_BUILT_REMOD.res, 
                           YBRN = YR_BUILT_REMOD.nonres, DBRR = DEC_BUILT_REMOD.res, 
                           DBRN = DEC_BUILT_REMOD.nonres)

PA.agg.CBG.shp = merge(PA.agg.CBG.rename, bgs_20, by="BG_ID_20",all.x=T)

PA.agg.CBG.shp[st_is_empty(PA.agg.CBG.shp$geometry),"geometry"]

# write cbg shapefile
st_write(PA.agg.CBG.shp, "PADCross.CBG.20", driver="ESRI Shapefile", 
         overwrite_layer = T)

