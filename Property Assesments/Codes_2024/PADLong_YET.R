### libraries ###
library(sf)
library(tidyverse)
library(data.table)

### to prevent scientific notation issues ###
options(scipen=100000000)

# set the working directory to the BARI shared drive
BARI <- "~/Desktop/BARI/PA-2023/outputs/PADLong/"

### Cleaning Functions ###
source("~/Desktop/BARI/PA-2023/scripts /functions/Cleaning_functions_20190326.R")
source("~/Desktop/BARI/PA-2023/scripts /functions/Geocoding_functions_20190326.R")
source("~/Desktop/BARI/PA-2023/scripts /functions/Helper_functions_20190326.R")

### INPUT PATHS ###
# OUTPUT PATHS
pal_path = "PADLong.Record.100423.csv"
pal_ct_path = "PADLong.CT.100423.csv"
pal_ct_shp_path = "Tract Shp Long"
pal_ct_shp_name = "PADLong.CT.100423"

pal_bg_path = "PADLong.CBG.100423.csv"
pal_bg_shp_path = "BG Shp Long"
pal_bg_shp_name = "PADLong.CBG.100423"

# READ IN FILES

#palold = read.csv("/Users/saina/Downloads/PADLong.Record.082020.csv")
palold20 = read.csv("~/Desktop/BARI/PA-2023/base_data/2021/PADLong.Record.082020.csv")
palold19 = read.csv("~/Desktop/BARI/PA-2023/base_data/2021/PADLong.Record.2019.csv")
palold = read.csv("~/Desktop/BARI/PA-2023/base_data/2021/PAD.Long.2021.csv")
PA <- read.csv("~/Desktop/BARI/PA-2023/outputs/PADCross/PADCross.Record.DRFT1.09212023.csv")
bgsShp <- st_read("~/Desktop/BARI/PA-2023/base_data/Block Groups 2010 BARI/Census Block Groups.shp")
ctsShp <- st_read("~/Desktop/BARI/PA-2023/base_data/Tracts_Boston_2010_BARI/Tracts_Boston BARI.shp")
#IDconnector <- read.csv("/Users/saina/Downloads/IDConnector2020.csv") #390321
landParcels <- read.csv("~/Desktop/BARI/GI-2023/drive/Parcel_final_10032023_YET_postsanitychecks.csv")

# adding old data 
# doesnt have these "BRA_PD","NSA_NAME"
PA_bckp <- PA # 177091 #180626 in Sep 23

names(PA)[1] <- "parcel_num"
names(palold20)[1] <- "parcel_num"
names(palold19)[1] <- "parcel_num"
names(palold)[1] <- "parcel_num"

#PA <- PADcross
palold$parcel_num <- as.numeric(palold$parcel_num)
palold$parcel_num <- format(palold$parcel_num, scientific = F)

palold19$parcel_num <- as.numeric(palold19$parcel_num)
palold19$parcel_num <- format(palold19$parcel_num, scientific = F)

palold20$parcel_num <- as.numeric(palold20$parcel_num)
palold20$parcel_num <- format(palold20$parcel_num, scientific = F)

PA$parcel_num <- as.numeric(PA$parcel_num)
PA$parcel_num <- format(PA$parcel_num, scientific = F)

# combining all old data
# just for this once
parcep_1920 <- setdiff(palold19$Land_Parcel_ID, palold20$Land_Parcel_ID)
palold19 <- palold19[palold19$Land_Parcel_ID %in% parcep_1920,] #220 #678
# << this has to be added to 2019

parcep_2021 <- setdiff(palold20$Land_Parcel_ID, palold$Land_Parcel_ID)
palold20 <- palold20[palold20$Land_Parcel_ID %in% parcep_2021,] #678 #822
# << this has to be added to 2020

# let's combine the two
palold19$Blk_ID_10 <- as.character(palold19$Blk_ID_10)
palold20$Blk_ID_10 <- as.character(palold20$Blk_ID_10)
palold19$BG_ID_10 <- as.character(palold19$BG_ID_10)
palold20$BG_ID_10 <- as.character(palold20$BG_ID_10)
palold19$CT_ID_10 <- as.character(palold19$CT_ID_10)
palold20$CT_ID_10 <- as.character(palold20$CT_ID_10)

palold20_bind <- palold20[,!names(palold20) %in% diff2019]

leftovers <- rbind(palold19, palold20_bind)
leftovers <- dplyr::distinct(leftovers) #891 #1500
leftovers <- leftovers[1:884,] # 884
leftovers <- leftovers[c(-846, -779),] #882
# << this now gets added to 2020 and then 2021

# add to 2020
leftovers$TLID <- as.character(leftovers$TLID)
#176988
parcep_21 <- setdiff(leftovers$Land_Parcel_ID, palold$Land_Parcel_ID) #177 #394
leftovers <- leftovers[leftovers$Land_Parcel_ID %in% parcep_21,] #772 #1196

palold_bind <- palold[,!names(palold) %in% setdiff(colnames(palold), colnames(leftovers))]

palold <- rbind(palold_bind, leftovers) #177760
palold <- palold[,c(-6,-7)]
palold <- palold[,c(1:144)]

# get rid of contradicting values of the same attributes
wait1 <- palold %>%
  group_by(parcel_num) %>%
  select(contains(".LU")) %>%
  unite(key_FY.LU, FY2000.LU:FY2020.LU) %>%
  select(parcel_num, key_FY.LU)

wait1 <- distinct(wait1) #177723

wait2 <- palold %>%
  group_by(parcel_num) %>%
  select(contains("LU2")) %>%
  unite(key_FourCat, LU2000FourCat:LU2020FourCat) %>%
  select(parcel_num, key_FourCat)

wait2 <- distinct(wait2) #[1] 177711

palold$key_FY.LU <- wait1$key_FY.LU[match(palold$parcel_num, wait1$parcel_num)]
palold$key_FourCat <- wait2$key_FourCat[match(palold$parcel_num, wait2$parcel_num)]

wait3 <- palold %>%
  group_by(parcel_num, key_FY.LU) %>%
  select_if(is.numeric) %>%
  select(!c( X, Y, GIS_ID, Land_Parcel_ID, CM_ID, ZIPCODE)) %>%
  mutate_at(vars(FY2000.AV:RecoveryPercChangeAV),list(a = ~ mean(., na.rm = TRUE, na.action=na.pass))) %>%
  select(parcel_num, key_FY.LU,contains("_a"))

wait3 <- distinct(wait3)
wait3 <- wait3 %>% mutate_all(~ifelse(is.nan(.), NA, .)) #176923
names(wait3)

palold_wait3 <- left_join(palold, wait3[,-2], by = "parcel_num")
palold_wait3 <- palold_wait3 %>% 
  mutate(across(where(is.character), str_trim))

# although the property characteristics do not matter
palold_wait3$ST_NUM <- str_remove_all(palold_wait3$ST_NUM, " ")
palold_wait3$ST_NUM <- ifelse(palold_wait3$ST_NUM == "", NA, palold_wait3$ST_NUM)

palold_wait3 <- distinct(palold_wait3) #177760
names(palold_wait3)
palold_wait3_backup <- palold_wait3
palold_wait3 <- palold_wait3 %>%
  select(c(1:6), contains(".LU"), contains(".RESEX"), c(145:211))

colnames(palold_wait3) <- sub("_a", "", colnames(palold_wait3))
#
palold_wait3 <- distinct(palold_wait3) #177750

for (var in c("X","Y","GIS_ID","Land_Parcel_ID","TLID","Blk_ID_10","BG_ID_10","CT_ID_10",
              "BRA_PD","NSA_NAME","Location_ID","CM_ID","ST_NUM", "unit_N", "unit_N_orig",
              "ST_NAME_SUF", "ZIPCODE","ST_NAME","UNIT_NUM")) {
  palold_wait3[,var] = NULL
}

palold_wait3 <- distinct(palold_wait3) #177746


palold_wait3$FY2000.LU <- ifelse(palold_wait3$FY2000.LU == "", NA, palold_wait3$FY2000.LU)
palold_wait3$FY2001.LU <- ifelse(palold_wait3$FY2001.LU == "", NA, palold_wait3$FY2001.LU)
palold_wait3$FY2002.LU <- ifelse(palold_wait3$FY2002.LU == "", NA, palold_wait3$FY2002.LU)
palold_wait3$FY2003.LU <- ifelse(palold_wait3$FY2003.LU == "", NA, palold_wait3$FY2003.LU)
palold_wait3$FY2004.LU <- ifelse(palold_wait3$FY2004.LU == "", NA, palold_wait3$FY2004.LU)
palold_wait3$FY2005.LU <- ifelse(palold_wait3$FY2005.LU == "", NA, palold_wait3$FY2005.LU)
palold_wait3$FY2006.LU <- ifelse(palold_wait3$FY2006.LU == "", NA, palold_wait3$FY2006.LU)
palold_wait3$FY2007.LU <- ifelse(palold_wait3$FY2007.LU == "", NA, palold_wait3$FY2007.LU)
palold_wait3$FY2008.LU <- ifelse(palold_wait3$FY2008.LU == "", NA, palold_wait3$FY2008.LU)
palold_wait3$FY2009.LU <- ifelse(palold_wait3$FY2009.LU == "", NA, palold_wait3$FY2009.LU)
palold_wait3$FY2010.LU <- ifelse(palold_wait3$FY2010.LU == "", NA, palold_wait3$FY2010.LU)
palold_wait3$FY2011.LU <- ifelse(palold_wait3$FY2011.LU == "", NA, palold_wait3$FY2011.LU)
palold_wait3$FY2012.LU <- ifelse(palold_wait3$FY2012.LU == "", NA, palold_wait3$FY2012.LU)
palold_wait3$FY2013.LU <- ifelse(palold_wait3$FY2013.LU == "", NA, palold_wait3$FY2013.LU)
palold_wait3$FY2014.LU <- ifelse(palold_wait3$FY2014.LU == "", NA, palold_wait3$FY2014.LU)
palold_wait3$FY2015.LU <- ifelse(palold_wait3$FY2015.LU == "", NA, palold_wait3$FY2015.LU)
palold_wait3$FY2016.LU <- ifelse(palold_wait3$FY2016.LU == "", NA, palold_wait3$FY2016.LU)
palold_wait3$FY2017.LU <- ifelse(palold_wait3$FY2017.LU == "", NA, palold_wait3$FY2017.LU)
palold_wait3$FY2018.LU <- ifelse(palold_wait3$FY2018.LU == "", NA, palold_wait3$FY2018.LU)
palold_wait3$FY2019.LU <- ifelse(palold_wait3$FY2019.LU == "", NA, palold_wait3$FY2019.LU)
palold_wait3$FY2020.LU <- ifelse(palold_wait3$FY2020.LU == "", NA, palold_wait3$FY2020.LU)

wait <- palold_wait3 %>% 
  select(1:22)

wait <- wait[sapply(1:nrow(wait), function(i) any(is.na(wait[i, seq(2, ncol(wait), 2)]))),]
wait <- distinct(wait) #45152

wait$FY2000.LU[is.na(wait$FY2000.LU)] <- palold_wait3$FY2000.LU[!is.na(palold_wait3$FY2000.LU)][match(wait$parcel_num[is.na(wait$FY2000.LU)], palold_wait3$parcel_num[!is.na(palold_wait3$FY2000.LU)])]

wait$FY2001.LU[is.na(wait$FY2001.LU)] <- palold_wait3$FY2001.LU[!is.na(palold_wait3$FY2001.LU)][match(wait$parcel_num[is.na(wait$FY2001.LU)], palold_wait3$parcel_num[!is.na(palold_wait3$FY2001.LU)])]

wait$FY2002.LU[is.na(wait$FY2002.LU)] <- palold_wait3$FY2002.LU[!is.na(palold_wait3$FY2002.LU)][match(wait$parcel_num[is.na(wait$FY2002.LU)], palold_wait3$parcel_num[!is.na(palold_wait3$FY2002.LU)])]

wait$FY2003.LU[is.na(wait$FY2003.LU)] <- palold_wait3$FY2003.LU[!is.na(palold_wait3$FY2003.LU)][match(wait$parcel_num[is.na(wait$FY2003.LU)], palold_wait3$parcel_num[!is.na(palold_wait3$FY2003.LU)])]

wait$FY2004.LU[is.na(wait$FY2004.LU)] <- palold_wait3$FY2004.LU[!is.na(palold_wait3$FY2004.LU)][match(wait$parcel_num[is.na(wait$FY2004.LU)], palold_wait3$parcel_num[!is.na(palold_wait3$FY2004.LU)])]

wait$FY2005.LU[is.na(wait$FY2005.LU)] <- palold_wait3$FY2005.LU[!is.na(palold_wait3$FY2005.LU)][match(wait$parcel_num[is.na(wait$FY2005.LU)], palold_wait3$parcel_num[!is.na(palold_wait3$FY2005.LU)])]

wait$FY2006.LU[is.na(wait$FY2006.LU)] <- palold_wait3$FY2006.LU[!is.na(palold_wait3$FY2006.LU)][match(wait$parcel_num[is.na(wait$FY2006.LU)], palold_wait3$parcel_num[!is.na(palold_wait3$FY2006.LU)])]

wait$FY2007.LU[is.na(wait$FY2007.LU)] <- palold_wait3$FY2007.LU[!is.na(palold_wait3$FY2007.LU)][match(wait$parcel_num[is.na(wait$FY2007.LU)], palold_wait3$parcel_num[!is.na(palold_wait3$FY2007.LU)])]

wait$FY2008.LU[is.na(wait$FY2008.LU)] <- palold_wait3$FY2008.LU[!is.na(palold_wait3$FY2008.LU)][match(wait$parcel_num[is.na(wait$FY2008.LU)], palold_wait3$parcel_num[!is.na(palold_wait3$FY2008.LU)])]

wait$FY2009.LU[is.na(wait$FY2009.LU)] <- palold_wait3$FY2009.LU[!is.na(palold_wait3$FY2009.LU)][match(wait$parcel_num[is.na(wait$FY2009.LU)], palold_wait3$parcel_num[!is.na(palold_wait3$FY2009.LU)])]

wait$FY2010.LU[is.na(wait$FY2010.LU)] <- palold_wait3$FY2010.LU[!is.na(palold_wait3$FY2010.LU)][match(wait$parcel_num[is.na(wait$FY2010.LU)], palold_wait3$parcel_num[!is.na(palold_wait3$FY2010.LU)])]

wait$FY2011.LU[is.na(wait$FY2011.LU)] <- palold_wait3$FY2011.LU[!is.na(palold_wait3$FY2011.LU)][match(wait$parcel_num[is.na(wait$FY2011.LU)], palold_wait3$parcel_num[!is.na(palold_wait3$FY2011.LU)])]

wait$FY2012.LU[is.na(wait$FY2012.LU)] <- palold_wait3$FY2012.LU[!is.na(palold_wait3$FY2012.LU)][match(wait$parcel_num[is.na(wait$FY2012.LU)], palold_wait3$parcel_num[!is.na(palold_wait3$FY2012.LU)])]

wait$FY2013.LU[is.na(wait$FY2013.LU)] <- palold_wait3$FY2013.LU[!is.na(palold_wait3$FY2013.LU)][match(wait$parcel_num[is.na(wait$FY2013.LU)], palold_wait3$parcel_num[!is.na(palold_wait3$FY2013.LU)])]

wait$FY2014.LU[is.na(wait$FY2014.LU)] <- palold_wait3$FY2014.LU[!is.na(palold_wait3$FY2014.LU)][match(wait$parcel_num[is.na(wait$FY2014.LU)], palold_wait3$parcel_num[!is.na(palold_wait3$FY2014.LU)])]

wait$FY2015.LU[is.na(wait$FY2015.LU)] <- palold_wait3$FY2015.LU[!is.na(palold_wait3$FY2015.LU)][match(wait$parcel_num[is.na(wait$FY2015.LU)], palold_wait3$parcel_num[!is.na(palold_wait3$FY2015.LU)])]

wait$FY2016.LU[is.na(wait$FY2016.LU)] <- palold_wait3$FY2016.LU[!is.na(palold_wait3$FY2016.LU)][match(wait$parcel_num[is.na(wait$FY2016.LU)], palold_wait3$parcel_num[!is.na(palold_wait3$FY2016.LU)])]

wait$FY2017.LU[is.na(wait$FY2017.LU)] <- palold_wait3$FY2017.LU[!is.na(palold_wait3$FY2017.LU)][match(wait$parcel_num[is.na(wait$FY2017.LU)], palold_wait3$parcel_num[!is.na(palold_wait3$FY2017.LU)])]

wait$FY2018.LU[is.na(wait$FY2018.LU)] <- palold_wait3$FY2018.LU[!is.na(palold_wait3$FY2018.LU)][match(wait$parcel_num[is.na(wait$FY2018.LU)], palold_wait3$parcel_num[!is.na(palold_wait3$FY2018.LU)])]

wait$FY2019.LU[is.na(wait$FY2019.LU)] <- palold_wait3$FY2019.LU[!is.na(palold_wait3$FY2019.LU)][match(wait$parcel_num[is.na(wait$FY2019.LU)], palold_wait3$parcel_num[!is.na(palold_wait3$FY2019.LU)])]

wait$FY2020.LU[is.na(wait$FY2020.LU)] <- palold_wait3$FY2020.LU[!is.na(palold_wait3$FY2020.LU)][match(wait$parcel_num[is.na(wait$FY2020.LU)], palold_wait3$parcel_num[!is.na(palold_wait3$FY2020.LU)])]

## now fill in NAs
palold_wait3$FY2020.LU[is.na(palold_wait3$FY2020.LU)] <- wait$FY2020.LU[!is.na(wait$FY2020.LU)][match(palold_wait3$parcel_num[is.na(palold_wait3$FY2020.LU)], wait$parcel_num[!is.na(wait$FY2020.LU)])]

palold_wait3$FY2019.LU[is.na(palold_wait3$FY2019.LU)] <- wait$FY2019.LU[!is.na(wait$FY2019.LU)][match(palold_wait3$parcel_num[is.na(palold_wait3$FY2019.LU)], wait$parcel_num[!is.na(wait$FY2019.LU)])]

palold_wait3$FY2018.LU[is.na(palold_wait3$FY2018.LU)] <- wait$FY2018.LU[!is.na(wait$FY2018.LU)][match(palold_wait3$parcel_num[is.na(palold_wait3$FY2018.LU)], wait$parcel_num[!is.na(wait$FY2018.LU)])]

palold_wait3$FY2017.LU[is.na(palold_wait3$FY2017.LU)] <- wait$FY2017.LU[!is.na(wait$FY2017.LU)][match(palold_wait3$parcel_num[is.na(palold_wait3$FY2017.LU)], wait$parcel_num[!is.na(wait$FY2017.LU)])]

palold_wait3$FY2016.LU[is.na(palold_wait3$FY2016.LU)] <- wait$FY2016.LU[!is.na(wait$FY2016.LU)][match(palold_wait3$parcel_num[is.na(palold_wait3$FY2016.LU)], wait$parcel_num[!is.na(wait$FY2016.LU)])]

palold_wait3$FY2015.LU[is.na(palold_wait3$FY2015.LU)] <- wait$FY2015.LU[!is.na(wait$FY2015.LU)][match(palold_wait3$parcel_num[is.na(palold_wait3$FY2015.LU)], wait$parcel_num[!is.na(wait$FY2015.LU)])]

palold_wait3$FY2014.LU[is.na(palold_wait3$FY2014.LU)] <- wait$FY2014.LU[!is.na(wait$FY2014.LU)][match(palold_wait3$parcel_num[is.na(palold_wait3$FY2014.LU)], wait$parcel_num[!is.na(wait$FY2014.LU)])]

palold_wait3$FY2013.LU[is.na(palold_wait3$FY2013.LU)] <- wait$FY2013.LU[!is.na(wait$FY2013.LU)][match(palold_wait3$parcel_num[is.na(palold_wait3$FY2013.LU)], wait$parcel_num[!is.na(wait$FY2013.LU)])]

palold_wait3$FY2012.LU[is.na(palold_wait3$FY2012.LU)] <- wait$FY2012.LU[!is.na(wait$FY2012.LU)][match(palold_wait3$parcel_num[is.na(palold_wait3$FY2012.LU)], wait$parcel_num[!is.na(wait$FY2012.LU)])]

palold_wait3$FY2011.LU[is.na(palold_wait3$FY2011.LU)] <- wait$FY2011.LU[!is.na(wait$FY2011.LU)][match(palold_wait3$parcel_num[is.na(palold_wait3$FY2011.LU)], wait$parcel_num[!is.na(wait$FY2011.LU)])]

palold_wait3$FY2010.LU[is.na(palold_wait3$FY2010.LU)] <- wait$FY2010.LU[!is.na(wait$FY2010.LU)][match(palold_wait3$parcel_num[is.na(palold_wait3$FY2010.LU)], wait$parcel_num[!is.na(wait$FY2010.LU)])]

palold_wait3$FY2009.LU[is.na(palold_wait3$FY2009.LU)] <- wait$FY2009.LU[!is.na(wait$FY2009.LU)][match(palold_wait3$parcel_num[is.na(palold_wait3$FY2009.LU)], wait$parcel_num[!is.na(wait$FY2009.LU)])]

palold_wait3$FY2008.LU[is.na(palold_wait3$FY2008.LU)] <- wait$FY2008.LU[!is.na(wait$FY2008.LU)][match(palold_wait3$parcel_num[is.na(palold_wait3$FY2008.LU)], wait$parcel_num[!is.na(wait$FY2008.LU)])]

palold_wait3$FY2007.LU[is.na(palold_wait3$FY2007.LU)] <- wait$FY2007.LU[!is.na(wait$FY2007.LU)][match(palold_wait3$parcel_num[is.na(palold_wait3$FY2007.LU)], wait$parcel_num[!is.na(wait$FY2007.LU)])]

palold_wait3$FY2006.LU[is.na(palold_wait3$FY2006.LU)] <- wait$FY2006.LU[!is.na(wait$FY2006.LU)][match(palold_wait3$parcel_num[is.na(palold_wait3$FY2006.LU)], wait$parcel_num[!is.na(wait$FY2006.LU)])]

palold_wait3$FY2005.LU[is.na(palold_wait3$FY2005.LU)] <- wait$FY2005.LU[!is.na(wait$FY2005.LU)][match(palold_wait3$parcel_num[is.na(palold_wait3$FY2005.LU)], wait$parcel_num[!is.na(wait$FY2005.LU)])]

palold_wait3$FY2004.LU[is.na(palold_wait3$FY2004.LU)] <- wait$FY2004.LU[!is.na(wait$FY2004.LU)][match(palold_wait3$parcel_num[is.na(palold_wait3$FY2004.LU)], wait$parcel_num[!is.na(wait$FY2004.LU)])]

palold_wait3$FY2003.LU[is.na(palold_wait3$FY2003.LU)] <- wait$FY2003.LU[!is.na(wait$FY2003.LU)][match(palold_wait3$parcel_num[is.na(palold_wait3$FY2003.LU)], wait$parcel_num[!is.na(wait$FY2003.LU)])]

palold_wait3$FY2002.LU[is.na(palold_wait3$FY2002.LU)] <- wait$FY2002.LU[!is.na(wait$FY2002.LU)][match(palold_wait3$parcel_num[is.na(palold_wait3$FY2002.LU)], wait$parcel_num[!is.na(wait$FY2002.LU)])]

palold_wait3$FY2001.LU[is.na(palold_wait3$FY2001.LU)] <- wait$FY2001.LU[!is.na(wait$FY2001.LU)][match(palold_wait3$parcel_num[is.na(palold_wait3$FY2001.LU)], wait$parcel_num[!is.na(wait$FY2001.LU)])]

palold_wait3$FY2000.LU[is.na(palold_wait3$FY2000.LU)] <- wait$FY2000.LU[!is.na(wait$FY2000.LU)][match(palold_wait3$parcel_num[is.na(palold_wait3$FY2000.LU)], wait$parcel_num[!is.na(wait$FY2000.LU)])]

palold_wait3 <- distinct(palold_wait3) #177746

# we do have extra records for condo parcels

# check which columns of the same parcel nums have contradicting values
#wait <- palold_wait3[palold_wait3$parcel_num == "1100000000",]

#m <- combn(nrow(wait),2)
#result <- sapply(wait,function(C) {z=C[m];z[c(TRUE,FALSE)]==z[c(FALSE,TRUE)]})

# 177746
names(palold_wait3)
max(palold_wait3$PercChangeAV2019, na.rm = T)

#write.csv(palold_wait3, "/Users/Shared/Files From d.localized/School/Semester 6/BARI PAD update/PAD/palold_edited.csv", row.names = F)
sum(palold_wait3$parcel_num == 1302000000)




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




# now it's ready to use for 2021

names(PA)[5] <- "ST_NAME_SUF"

PA$parcel_num <- trimws(PA$parcel_num)

PA.toAdd <- PA[!duplicated(PA$parcel_num),c("parcel_num","CM_ID","ST_NUM", "ST_NAME_SUF",
                                            "LU","AV_TOTAL","OWN_OCC","ZIPCODE","X",
                                            "Y","GIS_ID","Land_Parcel_ID","TLID",
                                            "Blk_ID_10","BG_ID_10","CT_ID_10")] #,

# THE NAMES OF THE FY WILL NEED TO CHANGE #
names(PA.toAdd)<-c("parcel_num","CM_ID","ST_NUM", "ST_NAME_SUF",
                   "FY2021.LU","FY2021.AV","FY2021.RESEX","ZIPCODE","X","Y","GIS_ID",
                   "Land_Parcel_ID","TLID","Blk_ID_10","BG_ID_10","CT_ID_10") 
palold <- palold_wait3
# remove old geographic data, no "ST_NAME_SUF"
for (var in c("X","Y","GIS_ID","Land_Parcel_ID","TLID","Blk_ID_10","BG_ID_10","CT_ID_10",
              "BRA_PD","NSA_NAME","Location_ID","CM_ID","ST_NUM", "unit_N", "unit_N_orig",
              "ST_NAME_SUF", "ZIPCODE","ST_NAME","UNIT_NUM")) {
  palold[,var] = NULL
}

palold$parcel_num <- trimws(palold$parcel_num)

# merge on all last year data
pal = merge(PA.toAdd,palold,by="parcel_num",all=TRUE) #180176
pal <- distinct(pal) # 180176
sum(is.na(pal$FY2021.LU) & !is.na(pal$FY2020.LU))
# 208 new rows
summary(pal)
names(pal)

# add geo data for old TAL, this is to include any changes that might have been made to 
# the GI for the old TAL rows, their parcel_nums have been matched to GIS_IDs that are in 
# the land parcels file first we merge onto the IDConnector,then the land parcels

# landParcels = dplyr::rename(landParcels, TLID = TLID_1)
names(IDconnector)[1] <- "parcel_num"
pal = merge(
  merge(pal,IDconnector[!duplicated(IDconnector$parcel_num),c("parcel_num","GIS_ID",
                                                              "Land_Parcel_ID")],
        by="parcel_num",all.x=T),
  landParcels[,c("X","Y","Land_Parcel_ID","TLID","Blk_ID_10","BG_ID_10","CT_ID_10")],
  
  by.x="Land_Parcel_ID.y",by.y = "Land_Parcel_ID",all.x=T)
# adds in the merged in geographic data, but keeps the original data for those that did 
#   not need the merged data
for (var in c("X","Y","GIS_ID","Land_Parcel_ID","TLID","Blk_ID_10","BG_ID_10","CT_ID_10"
)) {
  pal[,var] = ifelse(!is.na(pal[,paste(var,".x",sep="")]),
                     pal[,paste(var,".x",sep="")],
                     pal[,paste(var,".y",sep="")])
  pal[,paste(var,".x",sep="")] = NULL
  pal[,paste(var,".y",sep="")] = NULL
}

#pal <- pal[,-138]

pal_bckup <- pal # 179617
#pal <- pal_bckup
#--------------------------------#
#       Adding Extra Vars        #
#--------------------------------#
PA$parcel_num[which(PA$parcel_num == "NA")] <- NA





A <- pal$parcel_num
names(pal)
pal <- pal %>% select(-matches("PercChangeAV"))
pal <- pal %>% select(-matches("DiffAV"))


pal$DiffAV2001 <- ifelse(is.na(pal$FY2001.AV) | pal$FY2001.AV == 0, NA, ifelse(!is.na(pal$FY2000.AV) & pal$FY2000.AV != 0, (pal$FY2001.AV- pal$FY2000.AV), NA))
pal$PercChangeAV2001 <- ifelse(!is.na(pal$DiffAV2001), pal$DiffAV2001/pal$FY2000.AV, NA)

pal$DiffAV2002 <- ifelse(is.na(pal$FY2002.AV) | pal$FY2002.AV == 0, NA, ifelse(!is.na(pal$FY2001.AV) & pal$FY2001.AV != 0, (pal$FY2002.AV- pal$FY2001.AV), NA))
pal$PercChangeAV2002 <- ifelse(!is.na(pal$DiffAV2002), pal$DiffAV2002/pal$FY2001.AV, NA)

pal$DiffAV2003 <- ifelse(is.na(pal$FY2003.AV) | pal$FY2003.AV == 0, NA, ifelse(!is.na(pal$FY2002.AV) & pal$FY2002.AV != 0, (pal$FY2003.AV- pal$FY2002.AV), NA))
pal$PercChangeAV2003 <- ifelse(!is.na(pal$DiffAV2003), pal$DiffAV2003/pal$FY2002.AV, NA)

pal$DiffAV2004 <- ifelse(is.na(pal$FY2004.AV) | pal$FY2004.AV == 0, NA, ifelse(!is.na(pal$FY2003.AV) & pal$FY2003.AV != 0, (pal$FY2004.AV- pal$FY2003.AV), NA))
pal$PercChangeAV2004 <- ifelse(!is.na(pal$DiffAV2004), pal$DiffAV2004/pal$FY2003.AV, NA)

pal$DiffAV2005 <- ifelse(is.na(pal$FY2005.AV) | pal$FY2005.AV == 0, NA, ifelse(!is.na(pal$FY2004.AV) & pal$FY2004.AV != 0, (pal$FY2005.AV- pal$FY2004.AV), NA))
pal$PercChangeAV2005 <- ifelse(!is.na(pal$DiffAV2005), pal$DiffAV2005/pal$FY2004.AV, NA)

pal$DiffAV2006 <- ifelse(is.na(pal$FY2006.AV) | pal$FY2006.AV == 0, NA, ifelse(!is.na(pal$FY2005.AV) & pal$FY2005.AV != 0, (pal$FY2006.AV- pal$FY2005.AV), NA))
pal$PercChangeAV2006 <- ifelse(!is.na(pal$DiffAV2006), pal$DiffAV2006/pal$FY2005.AV, NA)

pal$DiffAV2007 <- ifelse(is.na(pal$FY2007.AV) | pal$FY2007.AV == 0, NA, ifelse(!is.na(pal$FY2006.AV) & pal$FY2006.AV != 0, (pal$FY2007.AV- pal$FY2006.AV), NA))
pal$PercChangeAV2007 <- ifelse(!is.na(pal$DiffAV2007), pal$DiffAV2007/pal$FY2006.AV, NA)

pal$DiffAV2008 <- ifelse(is.na(pal$FY2008.AV) | pal$FY2008.AV == 0, NA, ifelse(!is.na(pal$FY2007.AV) & pal$FY2007.AV != 0, (pal$FY2008.AV- pal$FY2007.AV), NA))
pal$PercChangeAV2008 <- ifelse(!is.na(pal$DiffAV2008), pal$DiffAV2008/pal$FY2007.AV, NA)

pal$DiffAV2009 <- ifelse(is.na(pal$FY2009.AV) | pal$FY2009.AV == 0, NA, ifelse(!is.na(pal$FY2008.AV) & pal$FY2008.AV != 0, (pal$FY2009.AV- pal$FY2008.AV), NA))
pal$PercChangeAV2009 <- ifelse(!is.na(pal$DiffAV2009), pal$DiffAV2009/pal$FY2008.AV, NA)

pal$DiffAV2010 <- ifelse(is.na(pal$FY2010.AV) | pal$FY2010.AV == 0, NA, ifelse(!is.na(pal$FY2009.AV) & pal$FY2009.AV != 0, (pal$FY2010.AV- pal$FY2009.AV), NA))
pal$PercChangeAV2010 <- ifelse(!is.na(pal$DiffAV2010), pal$DiffAV2010/pal$FY2009.AV, NA)

pal$DiffAV2011 <- ifelse(is.na(pal$FY2011.AV) | pal$FY2011.AV == 0, NA, ifelse(!is.na(pal$FY2010.AV) & pal$FY2010.AV != 0, (pal$FY2011.AV- pal$FY2010.AV), NA))
pal$PercChangeAV2011 <- ifelse(!is.na(pal$DiffAV2011), pal$DiffAV2011/pal$FY2010.AV, NA)

pal$DiffAV2012 <- ifelse(is.na(pal$FY2012.AV) | pal$FY2012.AV == 0, NA, ifelse(!is.na(pal$FY2011.AV) & pal$FY2011.AV != 0, (pal$FY2012.AV- pal$FY2011.AV), NA))
pal$PercChangeAV2012 <- ifelse(!is.na(pal$DiffAV2012), pal$DiffAV2012/pal$FY2011.AV, NA)

pal$DiffAV2013 <- ifelse(is.na(pal$FY2013.AV) | pal$FY2013.AV == 0, NA, ifelse(!is.na(pal$FY2012.AV) & pal$FY2012.AV != 0, (pal$FY2013.AV- pal$FY2012.AV), NA))
pal$PercChangeAV2013 <- ifelse(!is.na(pal$DiffAV2013), pal$DiffAV2013/pal$FY2012.AV, NA)

pal$DiffAV2014 <- ifelse(is.na(pal$FY2014.AV) | pal$FY2014.AV == 0, NA, ifelse(!is.na(pal$FY2013.AV) & pal$FY2013.AV != 0, (pal$FY2014.AV- pal$FY2013.AV), NA))
pal$PercChangeAV2014 <- ifelse(!is.na(pal$DiffAV2014), pal$DiffAV2014/pal$FY2013.AV, NA)

pal$DiffAV2015 <- ifelse(is.na(pal$FY2015.AV) | pal$FY2015.AV == 0, NA, ifelse(!is.na(pal$FY2014.AV) & pal$FY2014.AV != 0, (pal$FY2015.AV- pal$FY2014.AV), NA))
pal$PercChangeAV2015 <- ifelse(!is.na(pal$DiffAV2015), pal$DiffAV2015/pal$FY2014.AV, NA)

pal$DiffAV2016 <- ifelse(is.na(pal$FY2016.AV) | pal$FY2016.AV == 0, NA, ifelse(!is.na(pal$FY2015.AV) & pal$FY2015.AV != 0, (pal$FY2016.AV- pal$FY2015.AV), NA))
pal$PercChangeAV2016 <- ifelse(!is.na(pal$DiffAV2016), pal$DiffAV2016/pal$FY2015.AV, NA)

pal$DiffAV2017 <- ifelse(is.na(pal$FY2017.AV) | pal$FY2017.AV == 0, NA, ifelse(!is.na(pal$FY2016.AV) & pal$FY2016.AV != 0, (pal$FY2017.AV- pal$FY2016.AV), NA))
pal$PercChangeAV2017 <- ifelse(!is.na(pal$DiffAV2017), pal$DiffAV2017/pal$FY2016.AV, NA)

pal$DiffAV2018 <- ifelse(is.na(pal$FY2018.AV) | pal$FY2018.AV == 0, NA, ifelse(!is.na(pal$FY2017.AV) & pal$FY2017.AV != 0, (pal$FY2018.AV- pal$FY2017.AV), NA))
pal$PercChangeAV2018 <- ifelse(!is.na(pal$DiffAV2018), pal$DiffAV2018/pal$FY2017.AV, NA)

pal$DiffAV2019 <- ifelse(is.na(pal$FY2019.AV) | pal$FY2019.AV == 0, NA, ifelse(!is.na(pal$FY2018.AV) & pal$FY2018.AV != 0, (pal$FY2019.AV- pal$FY2018.AV), NA))
pal$PercChangeAV2019 <- ifelse(!is.na(pal$DiffAV2019), pal$DiffAV2019/pal$FY2018.AV, NA)

pal$DiffAV2020 <- ifelse(is.na(pal$FY2020.AV) | pal$FY2020.AV == 0, NA, ifelse(!is.na(pal$FY2019.AV) & pal$FY2019.AV != 0, (pal$FY2020.AV- pal$FY2019.AV), NA))
pal$PercChangeAV2020 <- ifelse(!is.na(pal$DiffAV2020), pal$DiffAV2020/pal$FY2019.AV, NA)

pal$DiffAV2021 <- ifelse(is.na(pal$FY2021.AV) | pal$FY2021.AV == 0, NA, ifelse(!is.na(pal$FY2020.AV) & pal$FY2020.AV != 0, (pal$FY2021.AV- pal$FY2020.AV), NA))
pal$PercChangeAV2021 <- ifelse(!is.na(pal$DiffAV2021), pal$DiffAV2021/pal$FY2020.AV, NA)

#A <- str_sort(names(pal), numeric = TRUE)
names(pal)
max(pal$PercChangeAV2021, na.rm = T)




#2005170000
# Modify Assessed Value Variables #
# THIS WILL NEED TO CHANGE BASED ON PREVIOUS YEAR #
lastyear=2021

# calculates the change in valuation year to year 
# Where assessed value is $0, set it to be NA
for (year in c(2000:lastyear)) {
  year = 2021
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

#names(pal)
max(pal$PercChangeAV2019, na.rm = T)
max(pal$PercChangeAV2020, na.rm = T)
#max(pal$PercChangeAV2021[!(pal$PercChangeAV2021 %in% c(959,343))], na.rm = T)



lastyear=2021
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
# recovery years = 2011 - 2017

# Step 3 -- Create variables for value difference in each time period
# THIS WILL NEED TO CHANGE BASED ON PREVIOUS YEAR # 

pal$GrowthDiffAV <- pal$FY2007.AV-pal$FY2000.AV
pal$GrowthPercChangeAV <- (pal$FY2007.AV-pal$FY2000.AV)/pal$FY2000.AV
pal$CrashDiffAV <- pal$FY2011.AV-pal$FY2007.AV
pal$CrashPercChangeAV <- (pal$FY2011.AV-pal$FY2007.AV)/pal$FY2007.AV
pal$RecoveryDiffAV <- pal$FY2017.AV-pal$FY2011.AV
pal$RecoveryPercChangeAV <- (pal$FY2017.AV-pal$FY2011.AV)/pal$FY2011.AV


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
                      "GIS_ID","Land_Parcel_ID","TLID","Blk_ID_10","BG_ID_10","CT_ID_10"
)

#ordering variables
setdiff(names(pal),variables_ordered)
setdiff(variables_ordered, names(pal))
#drops <- c("unit_N.x","unit_N_orig.x")
#pal <- pal[ , !(names(pal) %in% drops)]
pal = pal[,variables_ordered] # 

pal <- distinct(pal) # 179617

backUp2 <- pal
#TLID <- read.csv("D:\\Google Drive\\BARI Research Team Data Library\\Geographical Infrastructure\\Boston Geographical Infrastructure 2020\\Outputs\\Parcel_final_2020_08182020.csv")
#TLID$TLID <- format(TLID$TLID, scientific = F)

pal$TLID <- landParcels$TLID[match(pal$Land_Parcel_ID,landParcels$Land_Parcel_ID)]
names(pal)[1] <- "PID"

# drops <- "unit_N" 
# pal <- pal[ , !(names(pal) %in% drops)]

#pal$CT_ID_10[is.na(pal$CT_ID_10) & !is.na(pal$Land_Parcel_ID)] <- landParcels$CT_ID_10[match(pal$Land_Parcel_ID[is.na(pal$CT_ID_10) & !is.na(pal$Land_Parcel_ID)], landParcels$Land_Parcel_ID)] # this does not cover anything

#pal$CT_ID_10[is.na(pal$CT_ID_10) & !is.na(pal$Land_Parcel_ID)] <- leftovers$CT_ID_10[match(pal$Land_Parcel_ID[is.na(pal$CT_ID_10) & !is.na(pal$Land_Parcel_ID)], leftovers$Land_Parcel_ID)] # this does not cover anything either
pal$CT_ID_10 <- as.numeric(pal$CT_ID_10)
pal$CT_ID_10 <- format(pal$CT_ID_10, scientific = F)
pal$BG_ID_10 <- as.numeric(pal$BG_ID_10)
pal$BG_ID_10 <- format(pal$BG_ID_10, scientific = F)
pal$Blk_ID_10 <- as.numeric(pal$Blk_ID_10)
pal$Blk_ID_10 <- format(pal$Blk_ID_10, scientific = F)
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
pal$GIS_ID <- trimws(pal$GIS_ID)
pal$TLID <- trimws(pal$TLID)
pal$CM_ID <- trimws(pal$CM_ID)

pal$Land_Parcel_ID[which(pal$Land_Parcel_ID == "NA")] <- NA
pal$Blk_ID_10[which(pal$Blk_ID_10 == "NA")] <- NA
pal$BG_ID_10[which(pal$BG_ID_10 == "NA")] <- NA
pal$CT_ID_10[which(pal$CT_ID_10 == "NA")] <- NA
pal$GIS_ID[which(pal$GIS_ID == "NA")] <- NA
pal$TLID[which(pal$TLID == "NA")] <- NA
pal$CM_ID[which(pal$CM_ID == "NA")] <- NA

prop21 <- read.csv("/Users/saina/Downloads/properties_geo_2021_9302021_postsanitychecks (2).csv")

pal$GIS_ID[is.na(pal$GIS_ID) & !is.na(pal$Land_Parcel_ID)] <- prop21$GIS_ID[!is.na(prop21$GIS_ID)][match(pal$Land_Parcel_ID[is.na(pal$GIS_ID) & !is.na(pal$Land_Parcel_ID)], prop21$Land_Parcel_ID)] # 

pal$X[is.na(pal$X) & !is.na(pal$Land_Parcel_ID)] <- prop21$X[!is.na(prop21$X)][match(pal$Land_Parcel_ID[is.na(pal$X) & !is.na(pal$Land_Parcel_ID)], prop21$Land_Parcel_ID)] # 

pal$Y[is.na(pal$Y) & !is.na(pal$Land_Parcel_ID)] <- prop21$Y[!is.na(prop21$Y)][match(pal$Land_Parcel_ID[is.na(pal$Y) & !is.na(pal$Land_Parcel_ID)], prop21$Land_Parcel_ID)] # 

pal$CT_ID_10[is.na(pal$CT_ID_10) & !is.na(pal$Land_Parcel_ID)] <- prop21$CT_ID_10[!is.na(prop21$CT_ID_10)][match(pal$Land_Parcel_ID[is.na(pal$CT_ID_10) & !is.na(pal$Land_Parcel_ID)], prop21$Land_Parcel_ID)] # 

pal$BG_ID_10[is.na(pal$BG_ID_10) & !is.na(pal$Land_Parcel_ID)] <- prop21$BG_ID_10[!is.na(prop21$BG_ID_10)][match(pal$Land_Parcel_ID[is.na(pal$BG_ID_10) & !is.na(pal$Land_Parcel_ID)], prop21$Land_Parcel_ID)] # 

pal$Blk_ID_10[is.na(pal$Blk_ID_10) & !is.na(pal$Land_Parcel_ID)] <- prop21$Blk_ID_10[!is.na(prop21$Blk_ID_10)][match(pal$Land_Parcel_ID[is.na(pal$Blk_ID_10) & !is.na(pal$Land_Parcel_ID)], prop21$Land_Parcel_ID)] # 

pal$TLID[is.na(pal$TLID) & !is.na(pal$Land_Parcel_ID)] <- prop21$TLID[!is.na(prop21$TLID)][match(pal$Land_Parcel_ID[is.na(pal$TLID) & !is.na(pal$Land_Parcel_ID)], prop21$Land_Parcel_ID)] # 

PA$Blk_ID_10 <- as.numeric(PA$Blk_ID_10)
PA$Blk_ID_10 <- format(PA$Blk_ID_10, scientific = F)

PA$Land_Parcel_ID <- as.numeric(PA$Land_Parcel_ID)
PA$Land_Parcel_ID <- format(PA$Land_Parcel_ID, scientific = F)

PA$Land_Parcel_ID <- trimws(PA$Land_Parcel_ID)
PA$Blk_ID_10 <- trimws(PA$Blk_ID_10)

PA$Land_Parcel_ID[which(PA$Land_Parcel_ID == "NA")] <- NA
PA$Blk_ID_10[which(PA$Blk_ID_10 == "NA")] <- NA

palold = read.csv("/Users/saina/Downloads/PADLong.Record.082020.csv")
palold20 = read.csv("/Users/saina/Downloads/PADLong.Record.2019.csv")
palold19 = read.csv("/Users/saina/Downloads/PADLong.Record.2018.csv")

names(palold)[1] <- "parcel_num"
names(palold20)[1] <- "parcel_num"

# load palold again
pal$GIS_ID[is.na(pal$GIS_ID) & !is.na(pal$PID)] <- palold$GIS_ID[!is.na(palold$GIS_ID)][match(pal$PID[is.na(pal$GIS_ID) & !is.na(pal$PID)], palold$parcel_num)] # 

pal$CT_ID_10[is.na(pal$CT_ID_10) & !is.na(pal$PID)] <- palold$CT_ID_10[!is.na(palold$CT_ID_10)][match(pal$PID[is.na(pal$CT_ID_10) & !is.na(pal$PID)], palold$parcel_num)] # 

pal$BG_ID_10[is.na(pal$BG_ID_10) & !is.na(pal$PID)] <- palold$BG_ID_10[!is.na(palold$BG_ID_10)][match(pal$PID[is.na(pal$BG_ID_10) & !is.na(pal$PID)], palold$parcel_num)] # 

pal$Blk_ID_10[is.na(pal$Blk_ID_10) & !is.na(pal$PID)] <- palold$Blk_ID_10[!is.na(palold$Blk_ID_10)][match(pal$PID[is.na(pal$Blk_ID_10) & !is.na(pal$PID)], palold$parcel_num)] # 
pal$X[is.na(pal$X) & !is.na(pal$PID)] <- palold$X[!is.na(palold$X)][match(pal$PID[is.na(pal$X) & !is.na(pal$PID)], palold$parcel_num)] 

pal$Y[is.na(pal$Y) & !is.na(pal$PID)] <- palold$Y[!is.na(palold$Y)][match(pal$PID[is.na(pal$Y) & !is.na(pal$PID)], palold$parcel_num)] 

pal$TLID[is.na(pal$TLID) & !is.na(pal$PID)] <- palold$TLID[!is.na(palold$TLID)][match(pal$PID[is.na(pal$TLID) & !is.na(pal$PID)], palold$parcel_num)] 

pal$Land_Parcel_ID[is.na(pal$Land_Parcel_ID) & !is.na(pal$PID)] <- palold$Land_Parcel_ID[!is.na(palold$Land_Parcel_ID)][match(pal$PID[is.na(pal$Land_Parcel_ID) & !is.na(pal$PID)], palold$parcel_num)] 

pal$Land_Parcel_ID[is.na(pal$Land_Parcel_ID) & !is.na(pal$PID)] <- palold20$Land_Parcel_ID[!is.na(palold20$Land_Parcel_ID)][match(pal$PID[is.na(pal$Land_Parcel_ID) & !is.na(pal$PID)], palold20$PID)] 

pal$GIS_ID[is.na(pal$GIS_ID) & !is.na(pal$PID)] <- palold20$GIS_ID[!is.na(palold20$GIS_ID)][match(pal$PID[is.na(pal$GIS_ID) & !is.na(pal$PID)], palold20$PID)] 

pal$CT_ID_10[is.na(pal$CT_ID_10) & !is.na(pal$PID)] <- palold20$CT_ID_10[!is.na(palold20$CT_ID_10)][match(pal$PID[is.na(pal$CT_ID_10) & !is.na(pal$PID)], palold20$PID)] 

pal$BG_ID_10[is.na(pal$BG_ID_10) & !is.na(pal$PID)] <- palold20$BG_ID_10[!is.na(palold20$BG_ID_10)][match(pal$PID[is.na(pal$BG_ID_10) & !is.na(pal$PID)], palold20$PID)] 

pal$Blk_ID_10[is.na(pal$Blk_ID_10) & !is.na(pal$PID)] <- palold20$Blk_ID_10[!is.na(palold20$Blk_ID_10)][match(pal$PID[is.na(pal$Blk_ID_10) & !is.na(pal$PID)], palold20$PID)] 

pal$CM_ID[is.na(pal$CM_ID) & !is.na(pal$PID)] <- palold20$CM_ID[!is.na(palold20$CM_ID)][match(pal$PID[is.na(pal$CM_ID) & !is.na(pal$PID)], palold20$PID)] 

pal$ST_NUM[is.na(pal$ST_NUM) & !is.na(pal$PID)] <- palold20$ST_NUM[!is.na(palold20$ST_NUM)][match(pal$PID[is.na(pal$ST_NUM) & !is.na(pal$PID)], palold20$PID)] 

pal$ST_NAME_SUF[is.na(pal$ST_NAME_SUF) & !is.na(pal$PID)] <- palold20$ST_NAME_SUF[!is.na(palold20$ST_NAME_SUF)][match(pal$PID[is.na(pal$ST_NAME_SUF) & !is.na(pal$PID)], palold20$PID)] 

pal$ZIPCODE[is.na(pal$ZIPCODE) & !is.na(pal$PID)] <- palold20$ZIPCODE[!is.na(palold20$ZIPCODE)][match(pal$PID[is.na(pal$ZIPCODE) & !is.na(pal$PID)], palold20$PID)] 

pal$TLID[is.na(pal$TLID) & !is.na(pal$PID)] <- palold20$TLID[!is.na(palold20$TLID)][match(pal$PID[is.na(pal$TLID) & !is.na(pal$PID)], palold20$PID)] 

pal$X[is.na(pal$X) & !is.na(pal$PID)] <- palold20$X[!is.na(palold20$X)][match(pal$PID[is.na(pal$X) & !is.na(pal$PID)], palold20$PID)] 

pal$Y[is.na(pal$Y) & !is.na(pal$PID)] <- palold20$Y[!is.na(palold20$Y)][match(pal$PID[is.na(pal$Y) & !is.na(pal$PID)], palold20$PID)] 

pal$PID <- format(pal$PID, scientific = F)
pal$GIS_ID <- format(pal$GIS_ID, scientific = F)
pal$Blk_ID_10 <- format(pal$Blk_ID_10, scientific = F)
pal$BG_ID_10 <- format(pal$BG_ID_10, scientific = F)
pal$CT_ID_10 <- format(pal$CT_ID_10, scientific = F)
pal$TLID <- format(pal$TLID, scientific = F)
pal$CM_ID <- format(pal$CM_ID, scientific = F)
pal$Land_Parcel_ID <- format(pal$Land_Parcel_ID, scientific = F)

pal$Blk_ID_10 <- trimws(pal$Blk_ID_10)
pal$CT_ID_10 <- trimws(pal$CT_ID_10)
pal$BG_ID_10 <- trimws(pal$BG_ID_10)
pal$PID <- trimws(pal$PID)
pal$GIS_ID <- trimws(pal$GIS_ID)
pal$TLID <- trimws(pal$TLID)
pal$CM_ID <- trimws(pal$CM_ID)
pal$Land_Parcel_ID <- trimws(pal$Land_Parcel_ID)

pal$Land_Parcel_ID[which(pal$Land_Parcel_ID == "NA")] <- NA
pal$Blk_ID_10[which(pal$Blk_ID_10 == "NA")] <- NA
pal$CT_ID_10[which(pal$CT_ID_10 == "NA")] <- NA
pal$Blk_ID_10[which(pal$Blk_ID_10 == "NA")] <- NA
pal$BG_ID_10[which(pal$BG_ID_10 == "NA")] <- NA
pal$TLID[which(pal$TLID == "NA")] <- NA
pal$CM_ID[which(pal$CM_ID == "NA")] <- NA
pal$GIS_ID[which(pal$GIS_ID == "NA")] <- NA


view(table(pal$CT_ID_10))
view(table(pal$BG_ID_10))
view(table(pal$Blk_ID_10))
view(table(pal$TLID))
view(table(pal$PID))
view(table(pal$GIS_ID))
view(table(pal$Land_Parcel_ID))
view(table(pal$CM_ID))
pal[rowSums(is.na(pal)) != ncol(pal), ]

## don't do that a min

pal <- pal[pal$PID != 401461020,]
pal <- pal[pal$PID != 1200354020,]
pal <- pal[pal$PID != 800900021,]
pal <- pal[pal$PID != 1400193002,]
pal <- pal[pal$PID != 500815229,]
pal <- pal[pal$PID != "NA",]

pal_add <- pal[pal$PID %in% c(401461020, 1200354020, 800900021, 1400193002, 500815229),]


pal %>%  #
  summarise_all(funs(sum(is.na(.))))

apply(pal,2,max, na.rm = TRUE)
names(pal)
max(rowSums(is.na((pal))))

view(pal[rowSums(is.na(pal)) == 144,])
A <- pal$PID[rowSums(is.na(pal)) == 144]
A <- palold[palold$parcel_num %in% A,c(1,139:146)]

A$CT_ID_10 <- trimws(A$CT_ID_10)
A$CT_ID_10[which(A$CT_ID_10 == "NA")] <- NA
A$BG_ID_10 <- trimws(A$BG_ID_10)
A$BG_ID_10[which(A$BG_ID_10 == "NA")] <- NA
A$Blk_ID_10 <- trimws(A$Blk_ID_10)
A$Blk_ID_10[which(A$Blk_ID_10 == "NA")] <- NA
A$TLID <- trimws(A$TLID)
A$TLID[which(A$TLID == "NA")] <- NA
A$Land_Parcel_ID <- trimws(A$Land_Parcel_ID)
A$Land_Parcel_ID[which(A$Land_Parcel_ID == "NA")] <- NA
A$GIS_ID <- trimws(A$GIS_ID)
A$GIS_ID[which(A$GIS_ID == "NA")] <- NA

A$X <- trimws(A$X)
A$X[which(A$X == "NA")] <- NA

A$Y <- trimws(A$Y)
A$Y[which(A$Y == "NA")] <- NA

pal$CT_ID_10[rowSums(is.na(pal)) == 144 & is.na(pal$CT_ID_10)] <- A$CT_ID_10[match(pal$PID[rowSums(is.na(pal)) == 144 & is.na(pal$CT_ID_10)], A$PID)]
pal$BG_ID_10[rowSums(is.na(pal)) == 144 & is.na(pal$BG_ID_10)] <- A$BG_ID_10[match(pal$PID[rowSums(is.na(pal)) == 144 & is.na(pal$BG_ID_10)], A$PID)]
pal$Blk_ID_10[rowSums(is.na(pal)) == 144 & is.na(pal$Blk_ID_10)] <- A$Blk_ID_10[match(pal$PID[rowSums(is.na(pal)) == 144 & is.na(pal$Blk_ID_10)], A$PID)]
pal$TLID[rowSums(is.na(pal)) == 144 & is.na(pal$TLID)] <- A$TLID[match(pal$PID[rowSums(is.na(pal)) == 144 & is.na(pal$TLID)], A$PID)]
pal$Land_Parcel_ID[rowSums(is.na(pal)) == 144 & is.na(pal$Land_Parcel_ID)] <- A$Land_Parcel_ID[match(pal$PID[rowSums(is.na(pal)) == 144 & is.na(pal$Land_Parcel_ID)], A$PID)]
pal$X[rowSums(is.na(pal)) == 145 & is.na(pal$X)] <- A$X[match(pal$PID[rowSums(is.na(pal)) == 144 & is.na(pal$X)], A$PID)]
pal$Y[rowSums(is.na(pal)) == 144 & is.na(pal$Y)] <- A$Y[match(pal$PID[rowSums(is.na(pal)) == 144 & is.na(pal$Y)], A$PID)]


pal$PID <- format(pal$PID, scientific = F)
pal$GIS_ID <- format(pal$GIS_ID, scientific = F)
pal$Blk_ID_10 <- format(pal$Blk_ID_10, scientific = F)
pal$BG_ID_10 <- format(pal$BG_ID_10, scientific = F)
pal$CT_ID_10 <- format(pal$CT_ID_10, scientific = F)
pal$TLID <- format(pal$TLID, scientific = F)
pal$CM_ID <- format(pal$CM_ID, scientific = F)
pal$Land_Parcel_ID <- format(pal$Land_Parcel_ID, scientific = F)

pal$Blk_ID_10 <- trimws(pal$Blk_ID_10)
pal$CT_ID_10 <- trimws(pal$CT_ID_10)
pal$BG_ID_10 <- trimws(pal$BG_ID_10)
pal$PID <- trimws(pal$PID)
pal$GIS_ID <- trimws(pal$GIS_ID)
pal$TLID <- trimws(pal$TLID)
pal$CM_ID <- trimws(pal$CM_ID)
pal$Land_Parcel_ID <- trimws(pal$Land_Parcel_ID)

pal$Land_Parcel_ID[which(pal$Land_Parcel_ID == "NA")] <- NA
pal$Blk_ID_10[which(pal$Blk_ID_10 == "NA")] <- NA
pal$CT_ID_10[which(pal$CT_ID_10 == "NA")] <- NA
pal$Blk_ID_10[which(pal$Blk_ID_10 == "NA")] <- NA
pal$BG_ID_10[which(pal$BG_ID_10 == "NA")] <- NA
pal$TLID[which(pal$TLID == "NA")] <- NA
pal$CM_ID[which(pal$CM_ID == "NA")] <- NA
pal$GIS_ID[which(pal$GIS_ID == "NA")] <- NA

palold$CT_ID_10 <- trimws(palold$CT_ID_10)
palold$CT_ID_10[which(palold$CT_ID_10 == "NA")] <- NA
palold$BG_ID_10 <- trimws(palold$BG_ID_10)
palold$BG_ID_10[which(palold$BG_ID_10 == "NA")] <- NA
palold$Blk_ID_10 <- trimws(palold$Blk_ID_10)
palold$Blk_ID_10[which(palold$Blk_ID_10 == "NA")] <- NA
palold$TLID <- trimws(palold$TLID)
palold$TLID[which(palold$TLID == "NA")] <- NA
palold$Land_Parcel_ID <- trimws(palold$Land_Parcel_ID)
palold$Land_Parcel_ID[which(palold$Land_Parcel_ID == "NA")] <- NA
palold$GIS_ID <- trimws(palold$GIS_ID)
palold$GIS_ID[which(palold$GIS_ID == "NA")] <- NA

palold20$CT_ID_10 <- trimws(palold20$CT_ID_10)
palold20$CT_ID_10[which(palold20$CT_ID_10 == "NA")] <- NA
palold20$BG_ID_10 <- trimws(palold20$BG_ID_10)
palold20$BG_ID_10[which(palold20$BG_ID_10 == "NA")] <- NA
palold20$Blk_ID_10 <- trimws(palold20$Blk_ID_10)
palold20$Blk_ID_10[which(palold20$Blk_ID_10 == "NA")] <- NA
palold20$TLID <- trimws(palold20$TLID)
palold20$TLID[which(palold20$TLID == "NA")] <- NA
palold20$Land_Parcel_ID <- trimws(palold20$Land_Parcel_ID)
palold20$Land_Parcel_ID[which(palold20$Land_Parcel_ID == "NA")] <- NA
palold20$GIS_ID <- trimws(palold20$GIS_ID)
palold20$GIS_ID[which(palold20$GIS_ID == "NA")] <- NA

palbackup3 <- pal
#pal <- palbackup3

pal$CT_ID_10[is.na(pal$CT_ID_10)] <- palold$CT_ID_10[!is.na(palold$CT_ID_10)][match(pal$PID[is.na(pal$CT_ID_10)], palold$PID[!is.na(palold$CT_ID_10)])]

pal$BG_ID_10[ is.na(pal$BG_ID_10)] <- palold$BG_ID_10[!is.na(palold$BG_ID_10)][match(pal$PID[ is.na(pal$BG_ID_10)], palold$PID[!is.na(palold$BG_ID_10)])]

pal$Blk_ID_10[ is.na(pal$Blk_ID_10)] <- palold$Blk_ID_10[!is.na(palold$Blk_ID_10)][match(pal$PID[ is.na(pal$Blk_ID_10)], palold$PID[!is.na(palold$Blk_ID_10)])]

pal$TLID[ is.na(pal$TLID)] <- palold$TLID[!is.na(palold$TLID)][match(pal$PID[ is.na(pal$TLID)], palold$PID[!is.na(palold$TLID)])]

pal$GIS_ID[ is.na(pal$GIS_ID)] <- palold$GIS_ID[!is.na(palold$GIS_ID)][match(pal$PID[is.na(pal$GIS_ID)], palold$PID[!is.na(palold$GIS_ID)])]

pal$X[ is.na(pal$X)] <- palold$X[!is.na(palold$X)][match(pal$PID[ is.na(pal$X)], palold$PID[!is.na(palold$X)])]

pal$Y[ is.na(pal$Y)] <- palold$Y[!is.na(palold$Y)][match(pal$PID[ is.na(pal$Y)], palold$PID[!is.na(palold$Y)])]

pal$Land_Parcel_ID[ is.na(pal$Land_Parcel_ID) & !is.na(pal$PID)] <- palold$Land_Parcel_ID[!is.na(palold$Land_Parcel_ID)][match(pal$PID[is.na(pal$Land_Parcel_ID)], palold$PID[!is.na(palold$Land_Parcel_ID)])]


# landParcels
A <- pal$Land_Parcel_ID[pal$Land_Parcel_ID %in% landParcels$Land_Parcel_ID & is.na(pal$X)]
(landParcels$X[landParcels$Land_Parcel_ID %in% A])

pal$X[is.na(pal$X) & !is.na(pal$Land_Parcel_ID)] <- landParcels$X[!is.na(landParcels$X)][match(pal$Land_Parcel_ID[is.na(pal$X) & !is.na(pal$Land_Parcel_ID)], landParcels$Land_Parcel_ID[!is.na(landParcels$X)])]

pal$Y[is.na(pal$Y) & !is.na(pal$Land_Parcel_ID)] <- landParcels$Y[!is.na(landParcels$Y)][match(pal$Land_Parcel_ID[is.na(pal$Y) & !is.na(pal$Land_Parcel_ID)], landParcels$Land_Parcel_ID[!is.na(landParcels$Y)])]

pal$CT_ID_10[is.na(pal$CT_ID_10) & !is.na(pal$Land_Parcel_ID)] <- landParcels$CT_ID_10[!is.na(landParcels$CT_ID_10)][match(pal$Land_Parcel_ID[is.na(pal$CT_ID_10) & !is.na(pal$Land_Parcel_ID)], landParcels$Land_Parcel_ID[!is.na(landParcels$CT_ID_10)])]

pal$BG_ID_10[is.na(pal$BG_ID_10) & !is.na(pal$Land_Parcel_ID)] <- landParcels$BG_ID_10[!is.na(landParcels$BG_ID_10)][match(pal$Land_Parcel_ID[is.na(pal$BG_ID_10) & !is.na(pal$Land_Parcel_ID)], landParcels$Land_Parcel_ID[!is.na(landParcels$BG_ID_10)])]

pal$Blk_ID_10[is.na(pal$Blk_ID_10) & !is.na(pal$Land_Parcel_ID)] <- landParcels$Blk_ID_10[!is.na(landParcels$Blk_ID_10)][match(pal$Land_Parcel_ID[is.na(pal$Blk_ID_10) & !is.na(pal$Land_Parcel_ID)], landParcels$Land_Parcel_ID[!is.na(landParcels$Blk_ID_10)])]

pal$TLID[is.na(pal$TLID) & !is.na(pal$Land_Parcel_ID)] <- landParcels$TLID[!is.na(landParcels$TLID)][match(pal$Land_Parcel_ID[is.na(pal$TLID) & !is.na(pal$Land_Parcel_ID)], landParcels$Land_Parcel_ID[!is.na(landParcels$TLID)])]

#### 2019
sum(is.na(pal$GIS_ID) & !is.na(pal$FY2019.AV))
A <- pal$PID[is.na(pal$GIS_ID) & !is.na(pal$FY2019.AV)]
view(palold20[palold20$PID %in% A,])

pal$X[is.na(pal$X) & !is.na(pal$PID)] <- palold20$X[!is.na(palold20$X)][match(pal$PID[is.na(pal$X) & !is.na(pal$PID)], palold20$PID[!is.na(palold20$X)])]

pal$Y[is.na(pal$Y) & !is.na(pal$PID)] <- palold20$Y[!is.na(palold20$Y)][match(pal$PID[is.na(pal$Y) & !is.na(pal$PID)], palold20$PID[!is.na(palold20$Y)])]

pal$CT_ID_10[is.na(pal$CT_ID_10) & !is.na(pal$PID)] <- palold20$CT_ID_10[!is.na(palold20$CT_ID_10)][match(pal$PID[is.na(pal$CT_ID_10) & !is.na(pal$PID)], palold20$PID[!is.na(palold20$CT_ID_10)])]

pal$BG_ID_10[is.na(pal$BG_ID_10) & !is.na(pal$PID)] <- palold20$BG_ID_10[!is.na(palold20$BG_ID_10)][match(pal$PID[is.na(pal$BG_ID_10) & !is.na(pal$PID)], palold20$PID[!is.na(palold20$BG_ID_10)])]

pal$Blk_ID_10[is.na(pal$Blk_ID_10) & !is.na(pal$PID)] <- palold20$Blk_ID_10[!is.na(palold20$Blk_ID_10)][match(pal$PID[is.na(pal$Blk_ID_10) & !is.na(pal$PID)], palold20$PID[!is.na(palold20$Blk_ID_10)])]

pal$TLID[is.na(pal$TLID) & !is.na(pal$PID)] <- palold20$TLID[!is.na(palold20$TLID)][match(pal$PID[is.na(pal$TLID) & !is.na(pal$PID)], palold20$PID[!is.na(palold20$TLID)])]

pal$GIS_ID[is.na(pal$GIS_ID) & !is.na(pal$PID)] <- palold20$GIS_ID[!is.na(palold20$GIS_ID)][match(pal$PID[is.na(pal$GIS_ID) & !is.na(pal$PID)], palold20$PID[!is.na(palold20$GIS_ID)])]

pal$Land_Parcel_ID[is.na(pal$Land_Parcel_ID) & !is.na(pal$PID)] <- palold20$Land_Parcel_ID[!is.na(palold20$Land_Parcel_ID)][match(pal$PID[is.na(pal$Land_Parcel_ID) & !is.na(pal$PID)], palold20$PID[!is.na(palold20$Land_Parcel_ID)])]

#### 2018
sum(is.na(pal$X) & !is.na(pal$FY2018.AV))
A <- pal$PID[is.na(pal$CT_ID_10) & !is.na(pal$FY2018.AV)]
view(palold19[palold19$parcel_num %in% A,])

names(palold19)[1] <- "PID"

pal$X[is.na(pal$X) & !is.na(pal$PID)] <- palold19$X[!is.na(palold19$X)][match(pal$PID[is.na(pal$X) & !is.na(pal$PID)], palold19$PID[!is.na(palold19$X)])]

pal$Y[is.na(pal$Y) & !is.na(pal$PID)] <- palold19$Y[!is.na(palold19$Y)][match(pal$PID[is.na(pal$Y) & !is.na(pal$PID)], palold19$PID[!is.na(palold19$Y)])]

pal$CT_ID_10[is.na(pal$CT_ID_10) & !is.na(pal$PID)] <- palold19$CT_ID_10[!is.na(palold19$CT_ID_10)][match(pal$PID[is.na(pal$CT_ID_10) & !is.na(pal$PID)], palold19$PID[!is.na(palold19$CT_ID_10)])]

pal$BG_ID_10[is.na(pal$BG_ID_10) & !is.na(pal$PID)] <- palold19$BG_ID_10[!is.na(palold19$BG_ID_10)][match(pal$PID[is.na(pal$BG_ID_10) & !is.na(pal$PID)], palold19$PID[!is.na(palold19$BG_ID_10)])]

pal$Blk_ID_10[is.na(pal$Blk_ID_10) & !is.na(pal$PID)] <- palold19$Blk_ID_10[!is.na(palold19$Blk_ID_10)][match(pal$PID[is.na(pal$Blk_ID_10) & !is.na(pal$PID)], palold19$PID[!is.na(palold19$Blk_ID_10)])]

pal$TLID[is.na(pal$TLID) & !is.na(pal$PID)] <- palold19$TLID[!is.na(palold19$TLID)][match(pal$PID[is.na(pal$TLID) & !is.na(pal$PID)], palold19$PID[!is.na(palold19$TLID)])]

pal$GIS_ID[is.na(pal$GIS_ID) & !is.na(pal$PID)] <- palold19$GIS_ID[!is.na(palold19$GIS_ID)][match(pal$PID[is.na(pal$GIS_ID) & !is.na(pal$PID)], palold19$PID[!is.na(palold19$GIS_ID)])]

pal$Land_Parcel_ID[is.na(pal$Land_Parcel_ID) & !is.na(pal$PID)] <- palold19$Land_Parcel_ID[!is.na(palold19$Land_Parcel_ID)][match(pal$PID[is.na(pal$Land_Parcel_ID) & !is.na(pal$PID)], palold19$PID[!is.na(palold19$Land_Parcel_ID)])]


#### 2020
sum(is.na(pal$ZIPCODE) & !is.na(pal$FY2020.AV))
A <- pal$PID[is.na(pal$ZIPCODE) & !is.na(pal$FY2020.AV)]
names(palold)[1] <- "PID"
view(palold[palold$PID %in% A,])


pal$ZIPCODE[is.na(pal$ZIPCODE)] <- palold$ZIPCODE[!is.na(palold$ZIPCODE)][match(pal$PID[is.na(pal$ZIPCODE)], palold$PID[!is.na(palold$ZIPCODE)])]

pal$ST_NUM[ is.na(pal$ST_NUM)] <- palold$ST_NUM[!is.na(palold$ST_NUM)][match(pal$PID[ is.na(pal$ST_NUM)], palold$PID[!is.na(palold$ST_NUM)])]

pal$ST_NAME_SUF[ is.na(pal$ST_NAME_SUF)] <- palold$ST_NAME_SUF[!is.na(palold$ST_NAME_SUF)][match(pal$PID[ is.na(pal$ST_NAME_SUF)], palold$PID[!is.na(palold$ST_NAME_SUF)])]

pal$CM_ID[ is.na(pal$CM_ID)] <- palold$CM_ID[!is.na(palold$CM_ID)][match(pal$PID[ is.na(pal$CM_ID)], palold$PID[!is.na(palold$CM_ID)])]




# final checks on long
view(rowSums(is.na((pal))))

order(rowSums(is.na(pal)),decreasing=T)

max(rowSums(is.na(pal)))

view(pal[rowSums(is.na(pal)) == 140,])
A <- pal$PID[rowSums(is.na(pal)) == 140]
view(PA[PA$PID %in% A,])

sum(PA$AV_TOTAL == 0, na.rm = T)
sum(pal$FY2021.AV == 0, na.rm = T)

sum(is.na(pal$FY2021.AV))
A <- pal$PID[is.na(pal$FY2021.AV)]
view(PA[PA$PID %in% A,])

pal %>%  
  summarise_all(funs(sum(is.na(.))))
nrow(pal) #179611
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


long <- read.csv("/Users/Shared/Files From d.localized/School/Semester 6/BARI PAD update/PAD/PADLong.Record.FULL.06212022.csv")



write.csv(pal, "/Users/Shared/Files From d.localized/School/Semester 6/BARI PAD update/PAD/PADLong.Record.NoOutFromPADCross.06212022.csv", row.names = F)


write.csv(pal, "/Users/Shared/Files From d.localized/School/Semester 6/BARI PAD update/PAD/PADLong.Record.FULL.06212022.csv", row.names = F)


#pal_out <- read.csv("/Users/Shared/Files From d.localized/School/Semester 6/BARI PAD update/PAD/PADLong.Record.NoOutFromPADCross.06212022.csv")

#pal_out_full <- read.csv("/Users/Shared/Files From d.localized/School/Semester 6/BARI PAD update/PAD/PADLong.Record.FULL.06212022.csv")

#write.csv(pal, "/Users/Shared/Files From d.localized/School/Semester 6/BARI PAD update/PAD/PADLong.Record.06122022.csv", row.names = F)

#write.csv(pal, paste0(BARI, pal_path), row.names=F)
pal <- PADLong_1 #178908
pal_noOUT <- pal #179611
####################
# pal <- read.csv("D:/Google Drive/BARI Research Team Data Library/Property Assessment Data/Outputs/PADLong.Record.2020.csv")

lastyear = 2021
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


## Aggregate sum change in assessed value over each year by census tract
# pal.CT <- aggregate(cbind(as.numeric(DiffAV2001),as.numeric(DiffAV2002),
#                           as.numeric(DiffAV2003),as.numeric(DiffAV2004),
#                           as.numeric(DiffAV2005),as.numeric(DiffAV2006),
#                           as.numeric(DiffAV2007),as.numeric(DiffAV2008),
#                           as.numeric(DiffAV2009),as.numeric(DiffAV2010),
#                           as.numeric(DiffAV2011),as.numeric(DiffAV2012),
#                           as.numeric(DiffAV2013),as.numeric(DiffAV2014),
#                           as.numeric(DiffAV2015),as.numeric(DiffAV2016),
#                           as.numeric(DiffAV2017),as.numeric(DiffAV2018),
#                           NewCondo2001, NewCondo2002, NewCondo2003, NewCondo2004, 
#                           NewCondo2005, NewCondo2006, NewCondo2007, NewCondo2008, 
#                           NewCondo2009, NewCondo2010, NewCondo2011, NewCondo2012, 
#                           NewCondo2013, NewCondo2014, NewCondo2015, NewCondo2016, 
#                           NewCondo2017, NewCondo2018) ~ CT_ID_10, pal, sum, na.rm = TRUE)

pal.CT <- pal %>%
  filter(!is.na(CT_ID_10)) %>%
  group_by(CT_ID_10) %>%
  summarise(DiffAV2001 = sum(DiffAV2001, na.rm = T), DiffAV2002 = sum(DiffAV2002, na.rm = T), DiffAV2003 = sum(DiffAV2003, na.rm = T), DiffAV2004 = sum(DiffAV2004, na.rm = T), DiffAV2005 = sum(DiffAV2005, na.rm = T), DiffAV2006 = sum(DiffAV2006, na.rm = T), DiffAV2007 = sum(DiffAV2007, na.rm = T), DiffAV2008 = sum(DiffAV2008, na.rm = T), DiffAV2009 = sum(DiffAV2009, na.rm = T), DiffAV2010 = sum(DiffAV2010, na.rm = T), DiffAV2011 = sum(DiffAV2011, na.rm = T), DiffAV2012 = sum(DiffAV2012, na.rm = T), DiffAV2013 = sum(DiffAV2013, na.rm = T), DiffAV2014 = sum(DiffAV2014, na.rm = T), DiffAV2015 = sum(DiffAV2015, na.rm = T), DiffAV2016 = sum(DiffAV2016, na.rm = T), DiffAV2017 = sum(DiffAV2017, na.rm = T), DiffAV2018 = sum(DiffAV2018, na.rm = T), DiffAV2019 = sum(DiffAV2019, na.rm = T), DiffAV2020 = sum(DiffAV2020, na.rm = T), DiffAV2021 = sum(DiffAV2021, na.rm = T), NewCondo2001 = sum(NewCondo2001, na.rm = T), NewCondo2002 = sum(NewCondo2002, na.rm = T), NewCondo2003 = sum(NewCondo2003, na.rm = T), NewCondo2004 = sum(NewCondo2004, na.rm = T), NewCondo2005 = sum(NewCondo2005, na.rm = T), NewCondo2006 = sum(NewCondo2006, na.rm = T), NewCondo2007 = sum(NewCondo2007, na.rm = T), NewCondo2008 = sum(NewCondo2008, na.rm = T), NewCondo2009 = sum(NewCondo2009, na.rm = T), NewCondo2010 = sum(NewCondo2010, na.rm = T), NewCondo2011 = sum(NewCondo2011, na.rm = T), NewCondo2012 = sum(NewCondo2012, na.rm = T), NewCondo2013 = sum(NewCondo2013, na.rm = T), NewCondo2014 = sum(NewCondo2014, na.rm = T), NewCondo2015 = sum(NewCondo2015, na.rm = T), NewCondo2016 = sum(NewCondo2016, na.rm = T), NewCondo2017 = sum(NewCondo2017, na.rm = T), NewCondo2018 = sum(NewCondo2018), NewCondo2019 = sum(NewCondo2019), NewCondo2020 = sum(NewCondo2020, na.rm = T), NewCondo2021 = sum(NewCondo2021, na.rm = T))

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
                                                   "SumDiffAV2021")


## Aggregate median percentage change in assessed value over each year by census tract
# Median_PercentValueChangeYear_CT_Yearly <- aggregate(cbind(PercChangeAV2001,
#                                                            PercChangeAV2002,
#                                                            PercChangeAV2003,
#                                                            PercChangeAV2004,
#                                                            PercChangeAV2005,
#                                                            PercChangeAV2006,
#                                                            PercChangeAV2007,
#                                                            PercChangeAV2008,
#                                                            PercChangeAV2009,
#                                                            PercChangeAV2010,
#                                                            PercChangeAV2011,
#                                                            PercChangeAV2012,
#                                                            PercChangeAV2013,
#                                                            PercChangeAV2014,
#                                                            PercChangeAV2015,
#                                                            PercChangeAV2016,
#                                                            PercChangeAV2017,
#                                                            PercChangeAV2018,
#                                                            GrowthPercChangeAV,
#                                                            CrashPercChangeAV,
#                                                            RecoveryPercChangeAV) 
#                                                      ~ CT_ID_10, pal, median, na.rm=TRUE)

Median_PercentValueChangeYear_CT_Yearly <- pal %>%
  filter(!is.na(CT_ID_10)) %>%
  group_by(CT_ID_10) %>%
  summarise(PercChangeAV2001 = median(PercChangeAV2001, na.rm = T), PercChangeAV2002 = median(PercChangeAV2002, na.rm = T), PercChangeAV2003 = median(PercChangeAV2003, na.rm = T), PercChangeAV2004 = median(PercChangeAV2004, na.rm = T), PercChangeAV2005 = median(PercChangeAV2005, na.rm = T), PercChangeAV2006 = median(PercChangeAV2006, na.rm = T), PercChangeAV2007 = median(PercChangeAV2007, na.rm = T), PercChangeAV2008 = median(PercChangeAV2008, na.rm = T), PercChangeAV2009 = median(PercChangeAV2009, na.rm = T), PercChangeAV2010 = median(PercChangeAV2010, na.rm = T), PercChangeAV2011 = median(PercChangeAV2011, na.rm = T), PercChangeAV2012 = median(PercChangeAV2012, na.rm = T), PercChangeAV2013 = median(PercChangeAV2013, na.rm = T), PercChangeAV2014 = median(PercChangeAV2014, na.rm = T), PercChangeAV2015 = median(PercChangeAV2015, na.rm = T), PercChangeAV2016 = median(PercChangeAV2016, na.rm = T), PercChangeAV2017 = median(PercChangeAV2017, na.rm = T), PercChangeAV2018 = median(PercChangeAV2018, na.rm = T), PercChangeAV2019 = median(PercChangeAV2019, na.rm = T), PercChangeAV2020 = median(PercChangeAV2020, na.rm = T), PercChangeAV2021 = median(PercChangeAV2021, na.rm = T), GrowthPercChangeAV = median(GrowthPercChangeAV, na.rm = T), CrashPercChangeAV = median(CrashPercChangeAV, na.rm = T), RecoveryPercChangeAV = median(RecoveryPercChangeAV, na.rm = T))

pal.CT <- merge(pal.CT,Median_PercentValueChangeYear_CT_Yearly,by = "CT_ID_10")


#keeping only certain vars (so... didn't need to make condo vars? )
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
  "GrowthPercChangeAV","CrashPercChangeAV" , "RecoveryPercChangeAV" )]

#pal.CT <- pal.CT[-1,]
# write csv
#write.csv(pal.CT,paste0(BARI, pal_ct_path) , row.names=F)
write.csv(pal.CT, "/Users/Shared/Files From d.localized/School/Semester 6/BARI PAD update/PAD/PADLong.CT.07072022.csv",
          row.names=F)

#names(tractsShp)[4] <- "CT_ID_10"
# merge onto ct shp file
pal.CT.shp = merge(tractsShp,pal.CT,by="CT_ID_10",all.x=T)

# save shp file
st_write(pal.CT.shp,paste0(BARI, pal_ct_shp_path) ,pal_ct_shp_name,driver="ESRI Shapefile",
         overwrite_layer=TRUE)

st_write(pal.CT.shp,"/Users/Shared/Files From d.localized/School/Semester 6/BARI PAD update/PAD/Tract Shp Long",pal_ct_shp_name,driver="ESRI Shapefile",
         overwrite_layer=TRUE)


#writeOGR(pal.CT.shp,paste0(BARI, pal_ct_shp_path) ,pal_ct_shp_name,driver="ESRI Shapefile",
# overwrite_layer=TRUE)


#aggregate to BG

# pal.BG <- aggregate(cbind(as.numeric(DiffAV2001),as.numeric(DiffAV2002),
#                           as.numeric(DiffAV2003),as.numeric(DiffAV2004),
#                           as.numeric(DiffAV2005),as.numeric(DiffAV2006),
#                           as.numeric(DiffAV2007),as.numeric(DiffAV2008),
#                           as.numeric(DiffAV2009),as.numeric(DiffAV2010),
#                           as.numeric(DiffAV2011),as.numeric(DiffAV2012),
#                           as.numeric(DiffAV2013),as.numeric(DiffAV2014),
#                           as.numeric(DiffAV2015),as.numeric(DiffAV2016),
#                           as.numeric(DiffAV2017),as.numeric(DiffAV2018),
#                           NewCondo2001, NewCondo2002, NewCondo2003, NewCondo2004, 
#                           NewCondo2005, NewCondo2006, NewCondo2007, NewCondo2008, 
#                           NewCondo2009, NewCondo2010, NewCondo2011, NewCondo2012, 
#                           NewCondo2013, NewCondo2014, NewCondo2015, NewCondo2016,
#                           NewCondo2017, NewCondo2018) ~ BG_ID_10, pal, sum, na.rm = TRUE)


pal.BG <- pal %>%
  filter(!is.na(BG_ID_10)) %>%
  group_by(BG_ID_10) %>%
  summarise(DiffAV2001 = sum(DiffAV2001, na.rm = T), DiffAV2002 = sum(DiffAV2002, na.rm = T), DiffAV2003 = sum(DiffAV2003, na.rm = T), DiffAV2004 = sum(DiffAV2004, na.rm = T), DiffAV2005 = sum(DiffAV2005, na.rm = T), DiffAV2006 = sum(DiffAV2006, na.rm = T), DiffAV2007 = sum(DiffAV2007, na.rm = T), DiffAV2008 = sum(DiffAV2008, na.rm = T), DiffAV2009 = sum(DiffAV2009, na.rm = T), DiffAV2010 = sum(DiffAV2010, na.rm = T), DiffAV2011 = sum(DiffAV2011, na.rm = T), DiffAV2012 = sum(DiffAV2012, na.rm = T), DiffAV2013 = sum(DiffAV2013, na.rm = T), DiffAV2014 = sum(DiffAV2014, na.rm = T), DiffAV2015 = sum(DiffAV2015, na.rm = T), DiffAV2016 = sum(DiffAV2016, na.rm = T), DiffAV2017 = sum(DiffAV2017, na.rm = T), DiffAV2018 = sum(DiffAV2018, na.rm = T), DiffAV2019 = sum(DiffAV2019, na.rm = T), DiffAV2020 = sum(DiffAV2020, na.rm = T), DiffAV2021 = sum(DiffAV2021, na.rm = T), NewCondo2001 = sum(NewCondo2001, na.rm = T), NewCondo2002 = sum(NewCondo2002, na.rm = T), NewCondo2003 = sum(NewCondo2003, na.rm = T), NewCondo2004 = sum(NewCondo2004, na.rm = T), NewCondo2005 = sum(NewCondo2005, na.rm = T), NewCondo2006 = sum(NewCondo2006, na.rm = T), NewCondo2007 = sum(NewCondo2007, na.rm = T), NewCondo2008 = sum(NewCondo2008, na.rm = T), NewCondo2009 = sum(NewCondo2009, na.rm = T), NewCondo2010 = sum(NewCondo2010, na.rm = T), NewCondo2011 = sum(NewCondo2011, na.rm = T), NewCondo2012 = sum(NewCondo2012, na.rm = T), NewCondo2013 = sum(NewCondo2013, na.rm = T), NewCondo2014 = sum(NewCondo2014, na.rm = T), NewCondo2015 = sum(NewCondo2015, na.rm = T), NewCondo2016 = sum(NewCondo2016, na.rm = T), NewCondo2017 = sum(NewCondo2017, na.rm = T), NewCondo2018 = sum(NewCondo2018, na.rm = T), NewCondo2019 = sum(NewCondo2019, na.rm = T), NewCondo2020 = sum(NewCondo2020, na.rm = T), NewCondo2021 = sum(NewCondo2021, na.rm = T))


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
                                                    "SumDiffAV2021")


# Median_PercentValueChangeYear_BG_Yearly <- aggregate(cbind(PercChangeAV2001,
#                                                            PercChangeAV2002,
#                                                            PercChangeAV2003,
#                                                            PercChangeAV2004,
#                                                            PercChangeAV2005,
#                                                            PercChangeAV2006,
#                                                            PercChangeAV2007,
#                                                            PercChangeAV2008,
#                                                            PercChangeAV2009,
#                                                            PercChangeAV2010,
#                                                            PercChangeAV2011,
#                                                            PercChangeAV2012,
#                                                            PercChangeAV2013,
#                                                            PercChangeAV2014,
#                                                            PercChangeAV2015,
#                                                            PercChangeAV2016,
#                                                            PercChangeAV2017,
#                                                            PercChangeAV2018,
#                                                            GrowthPercChangeAV,
#                                                            CrashPercChangeAV, 
#                                                            RecoveryPercChangeAV)
#                                                      ~ BG_ID_10, pal, median, na.rm=TRUE)

Median_PercentValueChangeYear_BG_Yearly <- pal %>%
  filter(!is.na(BG_ID_10)) %>%
  group_by(BG_ID_10) %>%
  summarise(PercChangeAV2001 = median(PercChangeAV2001, na.rm = T), PercChangeAV2002 = median(PercChangeAV2002, na.rm = T), PercChangeAV2003 = median(PercChangeAV2003, na.rm = T), PercChangeAV2004 = median(PercChangeAV2004, na.rm = T), PercChangeAV2005 = median(PercChangeAV2005, na.rm = T), PercChangeAV2006 = median(PercChangeAV2006, na.rm = T), PercChangeAV2007 = median(PercChangeAV2007, na.rm = T), PercChangeAV2008 = median(PercChangeAV2008, na.rm = T), PercChangeAV2009 = median(PercChangeAV2009, na.rm = T), PercChangeAV2010 = median(PercChangeAV2010, na.rm = T), PercChangeAV2011 = median(PercChangeAV2011, na.rm = T), PercChangeAV2012 = median(PercChangeAV2012, na.rm = T), PercChangeAV2013 = median(PercChangeAV2013, na.rm = T), PercChangeAV2014 = median(PercChangeAV2014, na.rm = T), PercChangeAV2015 = median(PercChangeAV2015, na.rm = T), PercChangeAV2016 = median(PercChangeAV2016, na.rm = T), PercChangeAV2017 = median(PercChangeAV2017, na.rm = T), PercChangeAV2018 = median(PercChangeAV2018, na.rm = T), PercChangeAV2019 = median(PercChangeAV2019, na.rm = T), PercChangeAV2020 = median(PercChangeAV2020, na.rm = T), PercChangeAV2021 = median(PercChangeAV2021, na.rm = T), GrowthPercChangeAV = median(GrowthPercChangeAV, na.rm = T), CrashPercChangeAV = median(CrashPercChangeAV, na.rm = T), RecoveryPercChangeAV = median(RecoveryPercChangeAV, na.rm = T))


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
  "GrowthPercChangeAV","CrashPercChangeAV" , "RecoveryPercChangeAV" )]
#pal.BG <- pal.BG[-1,]
# write bg csv
write.csv(pal.BG, paste0(BARI, pal_bg_path) , row.names=F)
write.csv(pal.BG, "/Users/Shared/Files From d.localized/School/Semester 6/BARI PAD update/PAD/PADLong.BG.07072022.csv" , row.names=F)

# merge to bg shpfile
#names(bgsShp)[5] <- "BG_ID_10"
pal.BG.shp = merge(bgsShp,pal.BG,by="BG_ID_10",all.x=T)

# write bg shpfile
st_write(pal.BG.shp,"/Users/Shared/Files From d.localized/School/Semester 6/BARI PAD update/PAD/Tract Shp Long", pal_bg_shp_name,driver="ESRI Shapefile",
         overwrite_layer=TRUE)


#writeOGR(pal.BG.shp,paste0(BARI, pal_bg_shp_path), pal_bg_shp_name,driver="ESRI Shapefile",
#  overwrite_layer=TRUE)


bg <- read.csv( "/Users/Shared/Files From d.localized/School/Semester 6/BARI PAD update/PAD/PADLong.BG.07072022.csv")
bg_old <- read.csv( "/Users/Shared/Files From d.localized/School/Semester 6/BARI PAD update/PAD/PADLong.BG.06212022.csv")




# checks
#Land use counts between the cross and long data still don't match perfectly. There are more parcels in most land use #classifications, implying that land use is being carried over from previous years from parcels that are no longer treated as #existing by the City.

PA_PA <- read.csv("/Users/saina/Downloads/PADCross.Record.07132022.csv")

PA_PA
PADLong <- read.csv("/Users/Shared/Files From d.localized/School/Semester 6/BARI PAD update/PAD/PADLong.Record.FULL.06212022.csv")
length(unique(PADLong$PID)) #179348
#PADLong <- pal_out
view(table(PADLong$FY2021.LU))
view(table(PA_PA$LU))
PADLong21 <- PADLong[!is.na(PADLong$FY2021.LU),] #177711
length(setdiff(PADLong21$PID, PA_PA$PID)) # 0

PADLong21 <- PADLong[!is.na(PADLong$FY2021.LU) & PADLong$FY2021.LU == "CL",] #1503
length(setdiff(PADLong21$PID, PA_PA$PID)) # 0
view(PADLong21)
view(PA_PA[PA_PA$LU == "CL",])
PA_CL <- PA_PA[PA_PA$LU == "CL",] #1500

# this is because of the duplicates
## this is all of the 2021 records that have NAs for resex 2019 and 2020
PADLong21_problem <- PADLong[!is.na(PADLong$FY2021.LU) & is.na(PADLong$FY2019.RESEX) & is.na(PADLong$FY2020.RESEX),]
PID_problem <- unique(PADLong21_problem$PID) #3202
sum(PADLong$PID %in% PID_problem) #3968
PADLong_problem <- PADLong[PADLong$PID %in% PID_problem,]
A <- PADLong_problem %>%
  group_by(PID) %>%
  tally() %>%
  filter( n > 1)
A <- unique(A$PID) # 766

PADLong21_clean <- PADLong[!(PADLong$PID %in% A),]

PADLong21_problem <- PADLong[(PADLong$PID %in% A),] #1532

A <- unique(PADLong21_problem$PID) #766
#A <- head(A)

dt_dt <- setNames(data.frame(matrix(ncol = 149, nrow = 0)), names(PADLong21_problem))

for (i in 1:length(A)){
  #i = 1
  dt <- NULL
  dt <- PADLong[PADLong$PID == A[i],]
  dt <- dt %>%
    group_by(vars(c(-FY2020.RESEX, -FY2019.RESEX))) %>% 
    filter(FY2020.RESEX == max(FY2020.RESEX, na.rm = T),
           FY2019.RESEX == max(FY2019.RESEX, na.rm = T)) %>% 
    ungroup()
  dt <- dt[,-150]
  nrow(dt)
  dt_dt[i,] <- dt
  nrow(dt_dt)
}

sum(dt_dt$PID %in% PADLong21_problem$PID)


# stack the corrected data to the clean data
PADLong_1 <- rbind(PADLong21_clean, dt_dt)
nrow(PADLong_1) #179405
view(table(PADLong_1$FY2021.LU))
view(table(PA_PA$LU))

# the problem still exists for R1 - R3 - CM
## R1
test <- PADLong_1[!is.na(PADLong_1$FY2021.LU) & PADLong_1$FY2021.LU == "R1",]
A <- test %>%
  group_by(PID) %>%
  tally() %>%
  filter(n > 1)

A <- unique(A$PID) #2

names(PADLong_1)
nrow(PADLong_1)

PADLong21_clean <- PADLong_1[!(PADLong_1$PID %in% A),]

PADLong21_problem <- PADLong_1[(PADLong_1$PID %in% A),] #11

dt_dt <- setNames(data.frame(matrix(ncol = 149, nrow = 0)), names(PADLong21_problem))


for (i in 1:length(A)){
  cols <- setdiff(names(PADLong_1), c("FY2000.RESEX", "FY2001.RESEX", "FY2002.RESEX", "FY2003.RESEX", "FY2004.RESEX",
                                      "FY2005.RESEX", "FY2006.RESEX", "FY2007.RESEX", "FY2008.RESEX", "FY2009.RESEX",
                                      "FY2010.RESEX", "FY2011.RESEX", "FY2012.RESEX", "FY2013.RESEX", "FY2014.RESEX"))
  dt <- NULL
  dt <- PADLong21_problem[PADLong21_problem$PID == A[i],]
  dt <- dt %>%
    group_by_at(cols) %>% 
    filter(FY2000.RESEX == max(FY2000.RESEX, na.rm = T),
           FY2001.RESEX == max(FY2001.RESEX, na.rm = T),
           FY2002.RESEX == max(FY2002.RESEX, na.rm = T),
           FY2003.RESEX == max(FY2003.RESEX, na.rm = T),
           FY2004.RESEX == max(FY2004.RESEX, na.rm = T),
           FY2005.RESEX == max(FY2005.RESEX, na.rm = T),
           FY2006.RESEX == max(FY2006.RESEX, na.rm = T),
           FY2007.RESEX == max(FY2007.RESEX, na.rm = T),
           FY2008.RESEX == max(FY2008.RESEX, na.rm = T),
           FY2009.RESEX == max(FY2009.RESEX, na.rm = T),
           FY2010.RESEX == max(FY2010.RESEX, na.rm = T),
           FY2011.RESEX == max(FY2011.RESEX, na.rm = T),
           FY2012.RESEX == max(FY2012.RESEX, na.rm = T),
           FY2013.RESEX == max(FY2013.RESEX, na.rm = T),
           FY2014.RESEX == max(FY2014.RESEX, na.rm = T)) %>% 
    ungroup()
  #dt <- dt[,-150]
  dt <- distinct(dt)
  nrow(dt)
  dt_dt <- rbind(dt_dt,dt)
  nrow(dt_dt)
}

PADLong_1 <- rbind(PADLong21_clean, dt_dt)
PADLong_1 <- distinct(PADLong_1) #179398

# R3
test <- PADLong_1[!is.na(PADLong_1$FY2021.LU) & PADLong_1$FY2021.LU == "R3",]
A <- test %>%
  group_by(PID) %>%
  tally() %>%
  filter(n > 1)

A <- unique(A$PID)

PADLong21_clean <- PADLong_1[!(PADLong_1$PID %in% A),] #179396

PADLong21_problem <- PADLong_1[(PADLong_1$PID %in% A),] #2

dt_dt <- setNames(data.frame(matrix(ncol = 149, nrow = 0)), names(PADLong21_problem))


for (i in 1:length(A)){
  cols <- setdiff(names(PADLong21_clean), c("FY2000.RESEX", "FY2001.RESEX", "FY2002.RESEX", "FY2003.RESEX", "FY2004.RESEX",
                                            "FY2005.RESEX", "FY2006.RESEX", "FY2007.RESEX", "FY2008.RESEX", "FY2009.RESEX",
                                            "FY2010.RESEX", "FY2011.RESEX", "FY2012.RESEX", "FY2013.RESEX", "FY2014.RESEX",
                                            "FY2015.RESEX", "FY2016.RESEX", "FY2017.RESEX", "FY2018.RESEX", "FY2019.RESEX"))
  dt <- NULL
  dt <- PADLong21_problem[PADLong21_problem$PID == A[i],]
  dt <- dt %>%
    group_by_at(cols) %>% 
    filter(FY2000.RESEX == max(FY2000.RESEX, na.rm = T),
           FY2001.RESEX == max(FY2001.RESEX, na.rm = T),
           FY2002.RESEX == max(FY2002.RESEX, na.rm = T),
           FY2003.RESEX == max(FY2003.RESEX, na.rm = T),
           FY2004.RESEX == max(FY2004.RESEX, na.rm = T),
           FY2005.RESEX == max(FY2005.RESEX, na.rm = T),
           FY2006.RESEX == max(FY2006.RESEX, na.rm = T),
           FY2007.RESEX == max(FY2007.RESEX, na.rm = T),
           FY2008.RESEX == max(FY2008.RESEX, na.rm = T),
           FY2009.RESEX == max(FY2009.RESEX, na.rm = T),
           FY2010.RESEX == max(FY2010.RESEX, na.rm = T),
           FY2011.RESEX == max(FY2011.RESEX, na.rm = T),
           FY2012.RESEX == max(FY2012.RESEX, na.rm = T),
           FY2013.RESEX == max(FY2013.RESEX, na.rm = T),
           FY2014.RESEX == max(FY2014.RESEX, na.rm = T)) %>% 
    ungroup()
  #dt <- dt[,-150]
  dt <- distinct(dt)
  nrow(dt)
  dt_dt <- rbind(dt_dt,dt)
  nrow(dt_dt)
}

PADLong_1 <- rbind(PADLong21_clean, dt_dt)
PADLong_1 <- distinct(PADLong_1) #179397

sum(PADLong_1$PID == 1302000000)
####

# CM
PADLong_1_bck <- PADLong_1
test <- PADLong_1[!is.na(PADLong_1$FY2021.LU) & PADLong_1$FY2021.LU == "CM",]
A <- test %>%
  group_by(PID) %>%
  tally() %>%
  filter(n > 1)

A <- unique(A$PID)
# removing 1302000000
A <- c(203000000,306000000,501000000,502000000, 603000000 , 702000000, 1202000000)
PADLong21_clean <- PADLong_1[!(PADLong_1$PID %in% A),] #179369 - 179372

PADLong21_problem <- PADLong_1[(PADLong_1$PID %in% A),] #28 - 25

dt_dt <- setNames(data.frame(matrix(ncol = 149, nrow = 0)), names(PADLong21_problem))


for (i in 1:length(A)){
  cols <- setdiff(names(PADLong21_clean), c("FY2000.RESEX", "FY2001.RESEX", "FY2002.RESEX", "FY2003.RESEX", "FY2004.RESEX",
                                            "FY2005.RESEX", "FY2006.RESEX", "FY2007.RESEX", "FY2008.RESEX", "FY2009.RESEX",
                                            "FY2010.RESEX", "FY2011.RESEX", "FY2012.RESEX", "FY2013.RESEX", "FY2014.RESEX",
                                            "FY2015.RESEX", "FY2016.RESEX", "FY2017.RESEX", "FY2018.RESEX", "FY2019.RESEX"))
  dt <- NULL
  dt <- PADLong21_problem[PADLong21_problem$PID == A[i],]
  dt <- dt %>%
    group_by_at(cols) %>% 
    filter(FY2000.RESEX == max(FY2000.RESEX, na.rm = T),
           FY2001.RESEX == max(FY2001.RESEX, na.rm = T),
           FY2002.RESEX == max(FY2002.RESEX, na.rm = T),
           FY2003.RESEX == max(FY2003.RESEX, na.rm = T),
           FY2004.RESEX == max(FY2004.RESEX, na.rm = T),
           FY2005.RESEX == max(FY2005.RESEX, na.rm = T),
           FY2006.RESEX == max(FY2006.RESEX, na.rm = T),
           FY2007.RESEX == max(FY2007.RESEX, na.rm = T),
           FY2008.RESEX == max(FY2008.RESEX, na.rm = T),
           FY2009.RESEX == max(FY2009.RESEX, na.rm = T),
           FY2010.RESEX == max(FY2010.RESEX, na.rm = T),
           FY2011.RESEX == max(FY2011.RESEX, na.rm = T),
           FY2012.RESEX == max(FY2012.RESEX, na.rm = T),
           FY2013.RESEX == max(FY2013.RESEX, na.rm = T),
           FY2014.RESEX == max(FY2014.RESEX, na.rm = T)) %>% 
    ungroup()
  #dt <- dt[,-150]
  dt <- distinct(dt)
  nrow(dt)
  dt_dt <- rbind(dt_dt,dt)
  nrow(dt_dt)
}

PADLong_1 <- rbind(PADLong21_clean, dt_dt)
PADLong_1 <- distinct(PADLong_1) #179384, 179387


###########
PADLong_1
length(unique(PADLong_1$PID)) #179348
Paraw20
length(setdiff(Paraw20$PID, PADLong_1$PID))

# merge pal_add
PADLong_1_add <- rbind(PADLong_1, pal_add) #179392
length(unique(PADLong_1_add$PID))


nrow(PADLong) - nrow(PADLong_1_add)
ncol(PADLong) - ncol(PADLong_1_add)
names(PADLong_1_add) == names(PADLong)
view(table(PADLong_1$GIS_ID))

write.csv(PADLong_1, "/Users/Shared/Files From d.localized/School/Semester 6/BARI PAD update/PAD/PAD.Long.07072022.csv", row.names = F)

write.csv(PADLong_1, "/Users/Shared/Files From d.localized/School/Semester 6/BARI PAD update/PAD/PAD.Long.NoOut.07072022.csv", row.names = F)

write.csv(PADLong_1_add, "/Users/Shared/Files From d.localized/School/Semester 6/BARI PAD update/PAD/PAD.Long.07272022.csv", row.names = F)
view(head(PADlongnew, 100))
view(head(PADLong_1_add, 100))







view(table(PADLong_1$FY2021.LU))
view(table(PA_PA$LU))

sum(setdiff(PA_PA$PID, PADLong_1$PID[!is.na(PADLong_1$FY2021.LU)]))
sum(setdiff(PADLong_1$PID[!is.na(PADLong_1$FY2021.LU)], PA_PA$PID))

PID_PA <- as.data.frame(table(PA_PA$PID))
PID_PA <- PA_PA %>%
  group_by(PID) %>%
  tally()

PID_long <- PADLong_1[!is.na(PADLong_1$FY2021.LU),] %>%
  group_by(PID) %>%
  tally()

A <- PA_PA[PA_PA$PID == 1700511000,]
A <- distinct(A)
# still R1,3 and CM has problems

test <- PADLong_1[!is.na(PADLong_1$FY2021.LU) & PADLong_1$FY2021.LU == "CM",]
AA <- test$PID
AA <- unique(AA)
length(AA)
length(setdiff(test$PID, PA_PA$PID))
length(setdiff(PA_PA$PID[PA_PA$LU == "CM"], test$PID)) 
A <- PA_PA[PA_PA$LU == "CM",]
#length(A)
#A <- unique(A) #10019

A <- test %>%
  group_by(PID) %>%
  tally() %>%
  filter(n > 1)

A <- unique(A$PID) #5
view(PA_PA[PA_PA$PID %in% AA,])



dt <- clean_address(long$ST_NAME_SUF)
dt <- as.data.frame(dt)
names(dt)

# joining in suffix
names(long)[4] <- "ST_NAME"
long$ST_NAME_SUF <- palold$ST_NAME_SUF[match(long$PID, palold$PID)]
long$ST_NAME_SUF[is.na(long$ST_NAME_SUF)] <- palold20$ST_NAME_SUF[match(long$PID[is.na(long$ST_NAME_SUF)], palold20$PID)]
long$ST_NAME_SUF[is.na(long$ST_NAME_SUF)] <- palold19$ST_NAME_SUF[match(long$PID[is.na(long$ST_NAME_SUF)], palold19$PID)]
names(long)
long <- long[,c(1:4,150,5:149)]
write.csv(long, "/Users/Shared/Files From d.localized/School/Semester 6/BARI PAD update/PAD/PAD.Long.08022022.csv", row.names = F)


