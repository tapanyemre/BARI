# FILE TO CREATE 911 ECOMETRICS AUTOMATICALLY, NEED ONLY EDIT LINES 10 - 67 #
### THIS IS FOR INTERNAL ECOMETRICS, meaning that all types are included (therefore including DVIP (domestic violence) and FDWEAPGUN (found gun))

############################################################################################################
############################################################################################################
###### ************      THIS SECTION NEEDS TO BE EDITED BEFORE RUNNING THE CODE       ************* #######
############################################################################################################

options(scipen=999)
`%notin%` <- Negate(`%in%`)
library(plyr)
library(reshape2)
library(lubridate)
library(tictoc)
library(tidyverse)
library(sf)

## read in full 911 call data to be processed (i.e. file produced using "save_name_updated" from 911_A_Data Processing code) # the one with lat lon
data <- read.csv("911 Calls From Jan2021-Dec2021_with_LatLon.csv", header = T, stringsAsFactors = F)

# NOTE 2020: Removed "YouthHealth", "MajorMed" because they do not exist in the new data; also, removed NP_Dist, NT_dist
data <- data[,c("eid", "date", "year", "TYPE", "SocDis", "Guns", "PrivateConflict", "Violence",
                "Land_Parcel_ID", "TLID", "Blk_ID_10", "BG_ID_10", "CT_ID_10", "sj_cat",
                "pre_sys_change")]

# check that formatting is correct (no rounding issues)
min(data$Blk_ID_10, na.rm = T)
min(data$BG_ID_10, na.rm = T)
min(data$CT_ID_10, na.rm = T)

## create a copy just in case, so that you don't need to re-load if problems come up
save.data <- data

## set time period for which to calculate ecometrics
start <- "2021-01-01 00:00:00"
end <- "2021-12-31 23:59:40"

## set vector of geographical levels using their variable names
geo <- c("CT_ID_10", "BG_ID_10", "Blk_ID_10") # typically some selection of: CT_ID_10, BG_ID_10, Blk_ID_10
time <- c("year", "month") # typically some selection of : year, month, week

## format date
data$date <- parse_date_time(data$date, "ymd HMS")

## create time period variables, i.e. for month of year or week of year (un-comment what is needed)
data$month <- paste(month(data$date), data$year, sep = "_")
data$week <- paste(week(data$date), data$year, sep = "_")

## read in population data in one data set per census geography and year, typically use ACS 5-year for its midpoint, i.e. ACS0913 for 2011
## be sure geographic variable name (i.e. CT_ID_10) matches names in geo and population variable is called "TotalPop"
## NOTE: any census data from the ACS section of the MA Census Indicators Dataverse will already comply with this formatting

## fill in the ordered list of years for which ecometrics should be calculated; example below
time.periods <- sort(unique(data$year))[1:2]

## in the same order as the above list, read in population files for census geographies of interest
## NOTE 2020: We only consider two years
##2021 is a single year, no need to create these annual time periods but just to check
##create the time periods to ensure only 2021 is included...a final check on the dates
pop.ct <- read.csv("ACS_1418_TRACT.csv", header=T)
pop.cbg <- read.csv("ACS_1418_BLKGRP.csv", header =T)

pop.ct <- list()
pop.ct[[1]] <- read.csv("ACS_1418_TRACT.csv", header=T)
pop.ct[[2]] <- read.csv("ACS_1418_TRACT.csv", header=T)

pop.cbg <- list()
pop.cbg[[1]] <- read.csv("ACS_1418_BLKGRP.csv", header=T)
pop.cbg[[2]] <- read.csv("ACS_1418_BLKGRP.csv", header=T)

## NOTE: ecometrics are counts, not rates, for blocks so just fill in data.temp set with 1000's for Total Pop so that things even out to counts
pop.blk <- list()

blk.pop <- read.csv("Blocks_Boston_2010_BARI CSV.csv", header = T)
colnames(blk.pop)[5] <- "Blk_ID_10"
blk.pop$TotalPop <- 1000
blk.pop <- blk.pop[,5:29]
blk.pop <- blk.pop[,c(1, 25, 2:23) ]
for (i in 1:length(time.periods)){
pop.blk[[i]] <- blk.pop
}

## be sure that this is in the same order as "geo" in line 22 (above)
geo_data <- list()
geo_data[[1]] <- pop.ct
geo_data[[2]] <- pop.cbg
geo_data[[3]] <- pop.blk

## set the directory in which you want to save the ecometrics files
#setwd("C:\\Users\\User\\Google Drive\\BARI Research Team Data Library\\911 Database Working Folder\\2020\\Ecometrics\\Intermediate_files_2019-2020_Ext_and_Int\\")
#if using R Project - no need to set wd

## create a matrix of the names under which to save the files - internal, external, and public health # order down the rows first...
date <- "02282022"  # today's date

eco_save_name_int <-  matrix(c(paste("Int_Ecometrics_CT_Year_", date, ".csv", sep = ""), paste("Int_Ecometrics_CT_Month_", date, ".csv", sep = ""),
                           paste("Int_Ecometrics_BG_Year_", date, ".csv", sep = ""), paste("Int_Ecometrics_BG_Month_", date, ".csv", sep = ""),
                           paste("Int_Ecometrics_Blk_Year_", date, ".csv", sep = ""), paste("Int_Ecometrics_Blk_Month_", date, ".csv", sep = "")), 
                         nrow = length(time), ncol = length(geo))
eco_save_name_int # double_check that rows are changing time and columns are changing geography

eco_save_name_ext <-  matrix(c(paste("Ext_Ecometrics_CT_Year_", date, ".csv", sep = ""), paste("Ext_Ecometrics_CT_Month_", date, ".csv", sep = ""),
                           paste("Ext_Ecometrics_BG_Year_", date, ".csv", sep = ""), paste("Ext_Ecometrics_BG_Month_", date, ".csv", sep = ""),
                           paste("Ext_Ecometrics_Blk_Year_", date, ".csv", sep = ""), paste("Ext_Ecometrics_Blk_Month_", date, ".csv", sep = "")), 
                         nrow = length(time), ncol = length(geo))
eco_save_name_ext # double_check that rows are changing time and columns are changing geography

final_eco_save_ext <-  matrix(c("911 Ecometrics CT Longitudinal, Yearly, External.csv", "911 Ecometrics CT Longitudinal, Monthly, External.csv",
                                "911 Ecometrics CBG Longitudinal, Yearly, External.csv", "911 Ecometrics CBG Longitudinal, Monthly, External.csv",
                                "911 Ecometrics BLK Longitudinal, Yearly, External.csv", "911 Ecometrics BLK Longitudinal, Monthly, External.csv"), 
                              nrow = length(time), ncol = length(geo))
final_eco_save_ext # double_check that rows are changing time and columns are changing geography

final_eco_save_int <-  matrix(c(paste("911 Ecometrics CT Longitudinal, Yearly, Internal_", date, ".csv", sep = ""), paste("911 Ecometrics CT Longitudinal, Monthly, Internal_", date, ".csv", sep = ""),
                                paste("911 Ecometrics BG Longitudinal, Yearly, Internal_", date, ".csv", sep = ""), paste("911 Ecometrics BG Longitudinal, Monthly, Internal_", date, ".csv", sep = ""),
                                paste("911 Ecometrics Blk Longitudinal, Yearly, Internal_", date, ".csv", sep = ""), paste("911 Ecometrics Blk Longitudinal, Monthly, Internal_", date, ".csv", sep = "")), 
                              nrow = length(time), ncol = length(geo))
final_eco_save_int # double_check that rows are changing time and columns are changing geography


#### read old ecometrics from dataverse #### 
######as of 2021 - internal and external versions of these historical files can be found
######in the "ecometrics" main folder within 2020 in the 911 Working Folder
###Labeled INT/EXT  _Ecometrics_Blk/BG/CT_Month_ 02232021.csv

###### INTERNAL # 911 Calls_Unpublished_June2018
ecoint_Blk_month_old <- read.csv("Int_Ecometrics_Blk_Month_ 02232021 .csv", header = T, stringsAsFactors = F, sep=",", row.names=NULL)
ecoint_Blk_year_old <- read.csv("Int_Ecometrics_Blk_Year_ 02232021 .csv", header = T, stringsAsFactors = F, sep=",", row.names=NULL)

ecoint_BG_month_old <- read.csv("Int_Ecometrics_BG_Month_ 02232021 .csv", header = T, stringsAsFactors = F, sep=",", row.names=NULL)
ecoint_BG_year_old <- read.csv("Int_Ecometrics_BG_Year_ 02232021 .csv", header = T, stringsAsFactors = F, sep=",", row.names=NULL)

ecoint_CT_month_old <- read.csv("Int_Ecometrics_CT_Month_ 02232021 .csv", header = T, stringsAsFactors = F, sep=",", row.names=NULL)
ecoint_CT_year_old <- read.csv("Int_Ecometrics_CT_Year_ 02232021 .csv", header = T, stringsAsFactors = F, sep=",", row.names=NULL)

###### EXTERNAL # Boston 911 Calls_Published_httpsdoi.org10.7910DVNXTEJRE
##2021 -loaded in the previous years
ecoext_Blk_month_old <- read.csv("Ext_Ecometrics_Blk_Month_ 02232021 .csv", header = T, stringsAsFactors = F, sep=",", row.names=NULL)
ecoext_Blk_year_old <- read.csv("Ext_Ecometrics_Blk_Year_ 02232021 .csv", header = T, stringsAsFactors = F, sep=",", row.names=NULL)

ecoext_BG_month_old <- read.csv("Ext_Ecometrics_BG_Month_ 02232021 .csv", header = T, stringsAsFactors = F, sep=",", row.names=NULL)
ecoext_BG_year_old <- read.csv("Ext_Ecometrics_BG_Year_ 02232021 .csv", header = T, stringsAsFactors = F, sep=",", row.names=NULL)

ecoext_CT_month_old <- read.csv("Ext_Ecometrics_CT_Month_ 02232021 .csv", header = T, stringsAsFactors = F, sep=",", row.names=NULL)
ecoext_CT_year_old <- read.csv("Ext_Ecometrics_CT_Year_ 02232021 .csv", header = T, stringsAsFactors = F, sep=",", row.names=NULL)

### save only the column of Blk, BG, CT
# blk_ext <- ecoext_Blk_month_old %>% select(Blk_ID_10)
# bg_ext <- ecoext_BG_month_old %>% select(BG_ID_10)
# ct_ext <- ecoext_CT_month_old %>% select(CT_ID_10)
### we keep only the int ones
blk_int <- ecoint_Blk_month_old %>% select(Blk_ID_10)
bg_int <- ecoint_BG_month_old %>% select(BG_ID_10)
ct_int <- ecoint_CT_month_old %>% select(CT_ID_10)

###### some checks #
# blk <- read.csv("C:\\Users\\User\\Google Drive\\BARI Research Team Data Library\\911 Database Working Folder\\Testing\\Input\\Blocks_Boston_2010_BARI CSV.csv")#7288
# cbg <- read.csv("C:\\Users\\User\\Google Drive\\BARI Research Team Data Library\\911 Database Working Folder\\Testing\\Input\\Blocks_Groups_Boston_2010_BARI CSV.csv") #558
# ct <- read.csv("C:\\Users\\User\\Google Drive\\BARI Research Team Data Library\\911 Database Working Folder\\Testing\\Input\\Tracts_Boston_2010_BARI CSV.csv") #178
# 
# pop.ct_1 <- read.csv("C:\\Users\\User\\Google Drive\\BARI Research Team Data Library\\911 Database Working Folder\\Testing\\Input\\ACS_1418_TRACT.csv", header=T)
# pop.cbg_1 <- read.csv("C:\\Users\\User\\Google Drive\\BARI Research Team Data Library\\911 Database Working Folder\\Testing\\Input\\ACS_1418_BLKGRP.csv", header=T)
# 
# cbg <- left_join(cbg, pop.cbg_1, by = "BG_ID_10") # 9 have 0, plus 25 unde 500
# ct <- left_join(ct, pop.ct_1, by = "CT_ID_10") #6 have 0, plus 5 are under 500

###############   *********  END OF EDITING   **********     ####################################
#################################################################################################


###################################################################
################# SET OPTIONS //  FORMAT DATA ##################### 
###################################################################

# reduce data to the time period of interest
data <- data[data$date >= start & data$date <= end, ]

#################################################################
###################### CHANGES FOR CAT ##########################
################################################################

# create a category variable
data$CAT <- rep(NA, nrow(data))
data$CAT <- ifelse(data$SocDis==1, "SocDis", data$CAT)
data$CAT <- ifelse(data$PrivateConflict==1, "PrivateConflict", data$CAT)
data$CAT <- ifelse(data$Violence==1, "Violence", data$CAT)
data$CAT <- ifelse(data$Guns==1, "Guns", data$CAT)

# reduce to just those with geography and with reasonable NP and NT Distances
# no need to run the following because we saved only the geo one in script A

#data <- data[!is.na(data$CT_ID_10) | !is.na(data$BG_ID_10) | !is.na(data$Blk_ID_10) |
#               !is.na(data$Land_Parcel_ID) | !is.na(data$TLID),]

# we do not have NP_dist anymore, so we do not run the following anymore
#table(data$CAT[((!is.na(data$NP_Dist) & data$NP_Dist > 50) | (!is.na(data$NT_Dist) & data$NT_Dist > 50)) & !is.na(data$Blk_ID_10)], data$year[((!is.na(data$NP_Dist) & data$NP_Dist > 50) | (!is.na(data$NT_Dist) & data$NT_Dist > 50)) & !is.na(data$Blk_ID_10)])
#data <- data[is.na(data$NP_Dist) | (!is.na(data$NP_Dist) & data$NP_Dist < 50),]
#data <- data[is.na(data$NT_Dist) | (!is.na(data$NT_Dist) & data$NT_Dist < 50),]

# save a copy of the data with categories, just in case
save.data2 <- data

# reduce data to records of a pertinent category
data <- data[!is.na(data$CAT),]

###################################################################
######## CREATE ALL ECOMETRICS FILES AND SAVE THEM ################ 
###################################################################

#### INTERNAL ####
## NOTE 2020: removed mjormed and youth, and the NP_Dist and NT_dist

data <- data[,c("eid", "year", "SocDis", "Guns", "PrivateConflict", "Violence",
                "Land_Parcel_ID", "TLID", "Blk_ID_10", "BG_ID_10", "CT_ID_10", "CAT", "month", "week", "TYPE")]

data.int <- data[!duplicated(data[,c("CAT", "eid", "Land_Parcel_ID","TLID","Blk_ID_10", "BG_ID_10", "CT_ID_10")]),]
sum(duplicated(data.int[,c("eid", "CAT")])) #83 #94 #108 in Jan 2021 #16 in Feb 2021, #3 in Feb 2022

# create ECOMETRICS files

start1 <- Sys.time()
for (j in 1:length(time)) {
  for (i in 1:length(geo))  {
    
    # checks to make sure data is in correct format
    stopifnot(length(geo_data)==length(geo))
    stopifnot(length(pop.ct) == length(time.periods))
    stopifnot(length(pop.cbg) == length(time.periods))
    stopifnot(length(pop.blk) == length(time.periods))
    
    #############################################################################################
    # Format working data
    ############################################################################################# 
    
    # reduce data to only necessary rows: need to be in one of the pertinent categories and have an associated census tract
    data.temp <- data.int[!is.na(data.int[,colnames(data.int)==geo[i]]),]
    
    # variables/data
    data.temp$period <- data.temp[,colnames(data.temp) == time[j]]
    data.temp$geo.var <- data.temp[,colnames(data.temp) == geo[i]]

    #############################################################################################
    # Calculate frequencies by geography and time period
    #############################################################################################
    
    # get frequencies by CAT and format data.temp
    cat.freq <- ddply(data.temp, .(period, CAT, geo.var), summarise, freq = length(eid))
    
    # create makings of data set that covers entire time period at desired level (i.e. year, month, week)
    start_ymd <- date(start)
    end_ymd <- date(end)
    
    t <- seq(from = start_ymd,  to = end_ymd, by = time[j])
    
    a <- if(time[j]== "month"){ unique(paste(month(t), year(t), sep="_")) } else {if(time[j]== "week"){ unique(paste(week(t), year(t), sep="_")) }  else {if(time[j] == "year"){unique(data.temp$year)}}}
    #a <- if(time[j]== "month"){ unique(paste(year(t), month(t), sep="_")) } else {if(time[j]== "week"){ unique(paste(year(t), week(t), sep="_")) }  else {if(time[j] == "year"){unique(data.temp$year)}}}
    d <- unique(data.temp$CAT)
    e <- expand.grid(a, d, unique(data.temp$geo.var))

    cat.freq <- merge(cat.freq, e, by.x = c("period","CAT", "geo.var"), 
                      by.y = c("Var1", "Var2", "Var3"), 
                      all.x = T, all.y = T)
    
    # assign any "missing" values for a census geography freq=0
    cat.freq$freq[is.na(cat.freq$freq)] <- 0
    
    # reformat
    # January 2021: maybe also here add fun.aggregate = sum ??
    cat.freq.2 <- reshape2::dcast(cat.freq,... ~ CAT+period)
    
    # convert the data to long form to make the calculations easy
    cat.freq.3 <- melt(cat.freq.2, id.vars = "geo.var")
    names(cat.freq.3)[names(cat.freq.3) == "value"] <- "freq"
    
    # create a list of the variable names
    vars <- as.character(unique(cat.freq.3$variable))
    
    #############################################################################################
    # Create beginnings of final data set to use to calculate ecometrics
    #############################################################################################
    
    # limit the population data sets to just CT_ID_10 and population
    datalist <- list()
    
    for (k in 1:length(time.periods)){
      temp <- geo_data[[i]][[k]]
      temp <- subset(temp, temp[,colnames(temp) == geo[i]] %in% unique(cat.freq.3$geo.var))
      temp <- temp[, colnames(temp) %in% c(geo[i], "TotalPop")]
      datalist[[k]] <- merge(temp, cat.freq.3[cat.freq.3$variable %in% vars[grep(time.periods[k], vars)],], by.x = geo[i], by.y = "geo.var", all.y = T)
    }
    
    # combine all data sets 
    cat_counts <- do.call(rbind, datalist)
    
    # Create rates
    names(cat_counts)[names(cat_counts)=="variable"] <- "CAT_TIME"
    cat_counts <- ddply(cat_counts, .(CAT_TIME), transform, 
                        Rate = ((freq / TotalPop) * 1000))

    # Remove Pop=0 and Pop <500 CBGs and Tracts
    cat_counts <- cat_counts[cat_counts$TotalPop != 0 & cat_counts$TotalPop > 500,]
    cat_counts$TotalPop <- NULL # have to remove to make the final census geography-level data set since populations vary over time
    
    # reshape wide format (in two steps)
    cat_counts_long <- melt(cat_counts, id.vars = c(geo[i], "CAT_TIME"))
    # in Jan 2021 we added fun.aggregate = sum
    cat_counts_wide <- dcast(cat_counts_long, ... ~ variable + CAT_TIME, fun.aggregate = sum)
    
    # rename columns
    colnames(cat_counts_wide) <- gsub("freq","Freq", colnames(cat_counts_wide))
    
    #############################################################################################
    # finalize ecometrics
    #############################################################################################
    
    # create a just-ecometrics file (without frequencies)
    ecometrics <- cat_counts_wide[,!grepl("Freq",colnames(cat_counts_wide))]
    
    colnames(ecometrics) <- gsub("Rate_", "", colnames(ecometrics))
    
    ecometrics[is.na(ecometrics)] <- 0
    
    write.csv(ecometrics, eco_save_name_int[j, i], row.names=F)
  }
}
end1 <- Sys.time()
end1-start1

# Using freq as value column: use value.var to override.
# Using freq as value column: use value.var to override.
# Using freq as value column: use value.var to override.
# Using freq as value column: use value.var to override.
# Using freq as value column: use value.var to override.
# Using freq as value column: use value.var to override.

###################################################################
######## CREATE EXTERNAL ECOMETRICS FILES AND SAVE THEM ################ 
###################################################################
#### EXTERNAL ####

# create external data
data.ext <- data[(data$PrivateConflict == 1 | data$Guns == 1) & !data$TYPE %in% c("DVIP", "FDWEAPGUN") ,]
data.ext <- data.ext[!duplicated(data.ext[,c("CAT", "eid", "Land_Parcel_ID","TLID","Blk_ID_10", "BG_ID_10", "CT_ID_10")]), ]
sum(duplicated(data.ext[,c("eid", "CAT")])) #21 #23 #9

# create ecometrics files
start1 <- Sys.time()

for (j in 1:length(time)) {
  for (i in 1:length(geo))  {
    
    # checks to make sure data is in correct format
    stopifnot(length(geo_data)==length(geo))
    stopifnot(length(pop.ct) == length(time.periods))
    stopifnot(length(pop.cbg) == length(time.periods))
    stopifnot(length(pop.blk) == length(time.periods))
    
    #############################################################################################
    # Format working data
    ############################################################################################# 
    
    # reduce data to only necessary rows: need to be in one of the pertinent categories and have an associated census tract
    data.temp <- data.ext[!is.na(data.ext[,colnames(data.ext)==geo[i]]),]
    
    # variables/data.temp
    data.temp$period <- data.temp[,colnames(data.temp) == time[j]]
    data.temp$geo.var <- data.temp[,colnames(data.temp) == geo[i]]
    
    #############################################################################################
    # Calculate frequencies by geography and time period
    #############################################################################################
    
    # get frequencies by CAT and format data
    cat.freq <- ddply(data.temp, .(period, CAT, geo.var), summarise, freq = length(unique(eid)))
    
    # create makings of data set that covers entire time period at desired level (i.e. year, month, week)
    start_ymd <- date(start)
    end_ymd <- date(end)
    
    t <- seq(from = start_ymd,  to = end_ymd, by = time[j])
    
    a <- if(time[j]== "month"){ unique(paste(month(t), year(t), sep="_")) } else {if(time[j]== "week"){ unique(paste(week(t), year(t), sep="_")) }  else {if(time[j] == "year"){unique(data.temp$year)}}}
    #a <- if(time[j]== "month"){ unique(paste(year(t), month(t), sep="_")) } else {if(time[j]== "week"){ unique(paste(year(t), week(t), sep="_")) }  else {if(time[j] == "year"){unique(data.temp$year)}}}
    d <- unique(data.temp$CAT)
    e <- expand.grid(a, d, unique(data.temp$geo.var))
    #e <- expand.grid(d, a, unique(data.temp$geo.var))
    
    cat.freq <- merge(cat.freq, e, by.x = c("period","CAT", "geo.var"), 
                      by.y = c("Var1", "Var2", "Var3"), 
                      all.x = T, all.y = T)
    
    # assign any "missing" values for a census geography freq=0
    cat.freq$freq[is.na(cat.freq$freq)] <- 0
    
    # reformat
    cat.freq.2 <- dcast(cat.freq,... ~ CAT+period)
    head(cat.freq)
    
    # convert the data to long form to make the calculations easy
    cat.freq.3 <- melt(cat.freq.2, id.vars = "geo.var")
    names(cat.freq.3)[names(cat.freq.3) == "value"] <- "freq"
    
    # create a list of the variable names
    vars <- as.character(unique(cat.freq.3$variable))
    
    #############################################################################################
    # Create beginnings of final data set to use to calculate ecometrics
    #############################################################################################
    
    # limit the population data sets to just CT_ID_10 and population
    datalist <- list()
    
    for (k in 1:length(time.periods)){
      temp <- geo_data[[i]][[k]]
      temp <- subset(temp, temp[,colnames(temp) == geo[i]] %in% unique(cat.freq.3$geo.var))
      temp <- temp[, colnames(temp) %in% c(geo[i], "TotalPop")]
      datalist[[k]] <- merge(temp, cat.freq.3[cat.freq.3$variable %in% vars[grep(time.periods[k], vars)],], by.x = geo[i], by.y = "geo.var", all.y = T)
    }
    
    # combine all data sets 
    cat_counts <- do.call(rbind, datalist)
    
    # Create rates
    names(cat_counts)[names(cat_counts)=="variable"] <- "CAT_TIME"
    cat_counts <- ddply(cat_counts, .(CAT_TIME), transform, 
                        Rate = ((freq / TotalPop) * 1000))
    
    # Remove Pop=0 and Pop <500 CBGs and Tracts
    cat_counts <- cat_counts[cat_counts$TotalPop != 0 & cat_counts$TotalPop > 500,]
    cat_counts$TotalPop <- NULL # have to remove to make the final census geography-level data set since populations vary over time
    
    # reshape wide format (in two steps)
    cat_counts_long <- melt(cat_counts, id.vars = c(geo[i], "CAT_TIME"))
    #changed in January 2021, added fun.aggregate = sum
    cat_counts_wide <- dcast(cat_counts_long, ... ~ variable + CAT_TIME, fun.aggregate = sum)
    
    # rename columns
    colnames(cat_counts_wide) <- gsub("freq","Freq", colnames(cat_counts_wide))
    
    #############################################################################################
    # finalize ecometrics
    #############################################################################################
    
    # create a just-ecometrics file (without frequencies)
    ecometrics <- cat_counts_wide[,!grepl("Freq",colnames(cat_counts_wide))]
    
    colnames(ecometrics) <- gsub("Rate_", "", colnames(ecometrics))
    
    ecometrics[is.na(ecometrics)] <- 0
    
    write.csv(ecometrics, eco_save_name_ext[j, i], row.names=F)
  }
}

end1 <- Sys.time()
end1-start1

############################################################
##### Append the Soc Dis and Violence from INT to EXT (because only Guns and Priv Confl are considered when calculating ext ecometrics) #####
############################################################

##### ***NEEDS EDITING:*** #####

#### read the previously saved files if not in memory ####
setwd("C:\\Users\\User\\Google Drive\\BARI Research Team Data Library\\911 Database Working Folder\\2020\\Ecometrics\\Intermediate_files_2019-2020_Ext_and_Int\\")

ecoext_Blk_month_new <- read.csv("Ext_Ecometrics_Blk_Month_02282022.csv", header = T, stringsAsFactors = F, sep=",", row.names=NULL)
ecoext_Blk_year_new <- read.csv("Ext_Ecometrics_Blk_Year_02282022.csv", header = T, stringsAsFactors = F, sep=",", row.names=NULL)
ecoext_Blk_month_new$Blk_ID_10 <- as.numeric(ecoext_Blk_month_new$Blk_ID_10)

ecoext_BG_month_new <- read.csv("Ext_Ecometrics_BG_Month_02282022.csv", header = T, stringsAsFactors = F, sep=",", row.names=NULL)
ecoext_BG_year_new <- read.csv("Ext_Ecometrics_BG_Year_02282022.csv", header = T, stringsAsFactors = F, sep=",", row.names=NULL)

ecoext_CT_month_new <- read.csv("Ext_Ecometrics_CT_Month_02282022.csv", header = T, stringsAsFactors = F, sep=",", row.names=NULL)
ecoext_CT_year_new <- read.csv("Ext_Ecometrics_CT_Year_02282022.csv", header = T, stringsAsFactors = F, sep=",", row.names=NULL)

ecoint_Blk_month_new <- read.csv("Int_Ecometrics_Blk_Month_02282022.csv", header = T, stringsAsFactors = F, sep=",", row.names=NULL)
ecoint_Blk_year_new <- read.csv("Int_Ecometrics_Blk_Year_02282022.csv", header = T, stringsAsFactors = F, sep=",", row.names=NULL)

ecoint_BG_month_new <- read.csv("Int_Ecometrics_BG_Month_02282022.csv", header = T, stringsAsFactors = F, sep=",", row.names=NULL)
ecoint_BG_year_new <- read.csv("Int_Ecometrics_BG_Year_02282022.csv", header = T, stringsAsFactors = F, sep=",", row.names=NULL)

ecoint_CT_month_new <- read.csv("Int_Ecometrics_CT_Month_02282022.csv", header = T, stringsAsFactors = F, sep=",",row.names=NULL)
ecoint_CT_year_new <- read.csv("Int_Ecometrics_CT_Year_02282022.csv", header = T, stringsAsFactors = F, sep=",", row.names=NULL)

#### problems with 0 to fix in the Blk level ####

ecoext_Blk_month_new$NA. <- NULL
ecoext_Blk_month_new <- ecoext_Blk_month_new[ecoext_Blk_month_new$Blk_ID_10 != 0 & !is.na(ecoext_Blk_month_new$Blk_ID_10),]

ecoext_Blk_year_new$NA. <- NULL
ecoext_Blk_year_new <- ecoext_Blk_year_new[ecoext_Blk_year_new$Blk_ID_10 != 0 & !is.na(ecoext_Blk_year_new$Blk_ID_10),]

ecoint_Blk_month_new$NA. <- NULL
ecoint_Blk_month_new <- ecoint_Blk_month_new[ecoint_Blk_month_new$Blk_ID_10 != 0 & !is.na(ecoint_Blk_month_new$Blk_ID_10),]

ecoint_Blk_year_new$NA. <- NULL
ecoint_Blk_year_new <- ecoint_Blk_year_new[ecoint_Blk_year_new$Blk_ID_10 != 0 & !is.na(ecoint_Blk_year_new$Blk_ID_10),]

#### add the Soc Dis and violence from INT to EXT ####

ecoext_Blk_month_new <- left_join(ecoint_Blk_month_new %>%
                                    select(Blk_ID_10, starts_with("SocDis"), starts_with("Violence")), ecoext_Blk_month_new, by = "Blk_ID_10")
ecoext_Blk_month_new[is.na(ecoext_Blk_month_new)] <- 0

ecoext_Blk_year_new <- left_join(ecoint_Blk_year_new %>%
                                   select(Blk_ID_10, starts_with("SocDis"), starts_with("Violence")), ecoext_Blk_year_new, by = "Blk_ID_10")
ecoext_Blk_year_new[is.na(ecoext_Blk_year_new)] <- 0

ecoext_BG_month_new <- left_join(ecoint_BG_month_new %>%
                                   select(BG_ID_10, starts_with("SocDis"), starts_with("Violence")), ecoext_BG_month_new, by = "BG_ID_10")
ecoext_BG_month_new[is.na(ecoext_BG_month_new)] <- 0

ecoext_BG_year_new <- left_join(ecoint_BG_year_new %>%
                                  select(BG_ID_10, starts_with("SocDis"), starts_with("Violence")), ecoext_BG_year_new, by = "BG_ID_10")
ecoext_BG_year_new[is.na(ecoext_BG_year_new)] <- 0

ecoext_CT_month_new <- left_join(ecoint_CT_month_new %>%
                                   select(CT_ID_10, starts_with("SocDis"), starts_with("Violence")), ecoext_CT_month_new, by = "CT_ID_10")
ecoext_CT_month_new[is.na(ecoext_CT_month_new)] <- 0

ecoext_CT_year_new <- left_join(ecoint_CT_year_new %>%
                                  select(CT_ID_10, starts_with("SocDis"), starts_with("Violence")), ecoext_CT_year_new, by = "CT_ID_10")
ecoext_CT_year_new[is.na(ecoext_CT_year_new)] <- 0


############################################################
##### PUT TOGETHER OLD INTERNAL WITH NEW INTERNAL #####
############################################################

## Blk
ecoint_Blk_month_join <- left_join(ecoint_Blk_month_old, ecoint_Blk_month_new, by = "Blk_ID_10")
ecoint_Blk_year_join <- left_join(ecoint_Blk_year_old, ecoint_Blk_year_new, by = "Blk_ID_10")
ecoint_Blk_year_join[is.na(ecoint_Blk_year_join)] <- 0

## CBG
ecoint_BG_month_join <- left_join(ecoint_BG_month_old, ecoint_BG_month_new, by = "BG_ID_10")
ecoint_BG_year_join <- left_join(ecoint_BG_year_old, ecoint_BG_year_new, by = "BG_ID_10")
ecoint_BG_year_join[is.na(ecoint_BG_year_join)] <- 0

## CT 
ecoint_CT_month_join <- left_join(ecoint_CT_month_old, ecoint_CT_month_new, by = "CT_ID_10")
ecoint_CT_year_join <- left_join(ecoint_CT_year_old, ecoint_CT_year_new, by = "CT_ID_10")
ecoint_CT_year_join[is.na(ecoint_CT_year_join)] <- 0

############################################################
##### RENAME COLUMNS FOR OLD INTERNAL WITH NEW INTERNAL #####
############################################################

##### ***NEEDS EDITING:*** #####
##### For future updates: there is no need to rename everything from the past. #####
##### We had to rename and join columns because it was not part of the processing before #####
##### Please change the values accordingly. #####

## create a function to rename columns to "Name_Year_Month" ##
clean_cols <- function(col_name) {
  map_chr(.x = col_name, .f = ~{
    col_split <- str_split(.x, "_")[[1]]  
    paste(col_split[1], col_split[3], col_split[2], sep = "_")  
  })
}

ecoint_Blk_month_join_1 <- rename_with(ecoint_Blk_month_join, .cols = -contains("ID"), .fn = clean_cols)
ecoint_BG_month_join_1 <- rename_with(ecoint_BG_month_join, .cols = -contains("ID"), .fn = clean_cols)
ecoint_CT_month_join_1 <- rename_with(ecoint_CT_month_join, .cols = -contains("ID"), .fn = clean_cols)

## for all NA's add 0's ##
ecoint_Blk_month_join_1[is.na(ecoint_Blk_month_join_1)] <- 0
ecoint_BG_month_join_1[is.na(ecoint_BG_month_join_1)] <- 0
ecoint_CT_month_join_1[is.na(ecoint_CT_month_join_1)] <- 0

## order columns ##
ecoint_Blk_month_join_1 <- ecoint_Blk_month_join_1[ ,order(names(ecoint_Blk_month_join_1))]
ecoint_BG_month_join_1 <- ecoint_BG_month_join_1[ ,order(names(ecoint_BG_month_join_1))]
ecoint_CT_month_join_1 <- ecoint_CT_month_join_1[ ,order(names(ecoint_CT_month_join_1))]
ecoint_Blk_year_join <- ecoint_Blk_year_join[ ,order(names(ecoint_Blk_year_join))]
ecoint_BG_year_join <- ecoint_BG_year_join[ ,order(names(ecoint_BG_year_join))]
ecoint_CT_year_join <- ecoint_CT_year_join[ ,order(names(ecoint_CT_year_join))]

#### save the combined historical + new internal files ####
setwd("C:\\Users\\User\\Google Drive\\BARI Research Team Data Library\\911 Database Working Folder\\2020\\Ecometrics\\")

write.csv(ecoint_Blk_month_join_1, c(paste("Int_Ecometrics_Blk_Month_",date, ".csv")), row.names = FALSE)
write.csv(ecoint_Blk_year_join, c(paste("Int_Ecometrics_Blk_Year_",date, ".csv")), row.names = FALSE)

write.csv(ecoint_BG_month_join_1, c(paste("Int_Ecometrics_BG_Month_",date, ".csv")), row.names = FALSE)
write.csv(ecoint_BG_year_join, c(paste("Int_Ecometrics_BG_Year_",date, ".csv")), row.names = FALSE)

write.csv(ecoint_CT_month_join_1, c(paste("Int_Ecometrics_CT_Month_",date, ".csv")), row.names = FALSE)
write.csv(ecoint_CT_year_join, c(paste("Int_Ecometrics_CT_Year_",date, ".csv")), row.names = FALSE)

############################################################
##### CHANGE TO FORMAT LONG (many rows, less columns) FOR OLD INTERNAL WITH NEW INTERNAL #####
############################################################

ecoint_Blk_month_join_tidy <- pivot_longer(ecoint_Blk_month_join_1, cols = -Blk_ID_10, 
                         names_to = c("ecometric", "year", "month"),
                         names_sep = "_",
                         values_to = "value")

ecoint_BG_month_join_tidy <- pivot_longer(ecoint_BG_month_join_1, cols = -BG_ID_10, 
                                           names_to = c("ecometric", "year", "month"),
                                           names_sep = "_",
                                           values_to = "value")

ecoint_CT_month_join_tidy <- pivot_longer(ecoint_CT_month_join_1, cols = -CT_ID_10, 
                                           names_to = c("ecometric", "year", "month"),
                                           names_sep = "_",
                                           values_to = "value")


ecoint_Blk_year_join_tidy <- pivot_longer(ecoint_Blk_year_join, cols = -Blk_ID_10, 
                                           names_to = c("ecometric", "year"),
                                           names_sep = "_",
                                           values_to = "value")

ecoint_BG_year_join_tidy <- pivot_longer(ecoint_BG_year_join, cols = -BG_ID_10, 
                                          names_to = c("ecometric", "year"),
                                          names_sep = "_",
                                          values_to = "value")

ecoint_CT_year_join_tidy <- pivot_longer(ecoint_CT_year_join, cols = -CT_ID_10, 
                                          names_to = c("ecometric", "year"),
                                          names_sep = "_",
                                          values_to = "value")

ecoint_Blk_month_join_tidy <- ecoint_Blk_month_join_tidy[with(ecoint_Blk_month_join_tidy, order(ecometric, year, month)), ]
ecoint_BG_month_join_tidy <- ecoint_BG_month_join_tidy[with(ecoint_BG_month_join_tidy, order(ecometric, year, month)), ]
ecoint_CT_month_join_tidy <- ecoint_CT_month_join_tidy[with(ecoint_CT_month_join_tidy, order(ecometric, year, month)), ]
ecoint_Blk_year_join_tidy <- ecoint_Blk_year_join_tidy[with(ecoint_Blk_year_join_tidy, order(ecometric, year)), ]
ecoint_BG_year_join_tidy <- ecoint_BG_year_join_tidy[with(ecoint_BG_year_join_tidy, order(ecometric, year)), ]
ecoint_CT_year_join_tidy <- ecoint_CT_year_join_tidy[with(ecoint_CT_year_join_tidy, order(ecometric, year)), ]

write.csv(ecoint_Blk_month_join_tidy, c(paste("Int_Ecometrics_Blk_Month_",date, "_long", ".csv")), row.names = FALSE)
write.csv(ecoint_Blk_year_join_tidy, c(paste("Int_Ecometrics_Blk_Year_",date, "_long", ".csv")), row.names = FALSE)

write.csv(ecoint_BG_month_join_tidy, c(paste("Int_Ecometrics_BG_Month_",date, "_long", ".csv")), row.names = FALSE)
write.csv(ecoint_BG_year_join_tidy, c(paste("Int_Ecometrics_BG_Year_",date, "_long", ".csv")), row.names = FALSE)

write.csv(ecoint_CT_month_join_tidy, c(paste("Int_Ecometrics_CT_Month_",date, "_long", ".csv")), row.names = FALSE)
write.csv(ecoint_CT_year_join_tidy, c(paste("Int_Ecometrics_CT_Year_",date, "_long", ".csv")), row.names = FALSE)

############################################################
##### PUT TOGETHER OLD EXTERNAL WITH NEW EXTERNAL #####
############################################################

## Blk
ecoext_Blk_month_join <- left_join(ecoext_Blk_month_old, ecoext_Blk_month_new, by = "Blk_ID_10")
ecoext_Blk_year_join <- left_join(ecoext_Blk_year_old, ecoext_Blk_year_new, by = "Blk_ID_10")
ecoext_Blk_year_join[is.na(ecoext_Blk_year_join)] <- 0
## CBG
ecoext_BG_month_join <- left_join(ecoext_BG_month_old, ecoext_BG_month_new, by = "BG_ID_10")
ecoext_BG_year_join <- left_join(ecoext_BG_year_old, ecoext_BG_year_new, by = "BG_ID_10")
ecoext_BG_year_join[is.na(ecoext_BG_year_join)] <- 0
## CT 
ecoext_CT_month_join <- left_join(ecoext_CT_month_old, ecoext_CT_month_new, by = "CT_ID_10")
ecoext_CT_year_join <- left_join(ecoext_CT_year_old, ecoext_CT_year_new, by = "CT_ID_10")
ecoext_CT_year_join[is.na(ecoext_CT_year_join)] <- 0

############################################################
##### RENAME COLUMNS FOR OLD EXTERNAL WITH NEW EXTERNAL #####
############################################################

##### ***NEEDS EDITING:*** #####
##### For future updates: there is no need to rename everything from the past. #####
##### We had to rename and join columns because it was not part of the processing before #####
##### Please change the values accordingly. #####

######### FIRST REMOVE youthhealth and medical for >2014 (they were kept in the past release)

toremove <- c(
  "MajorMed_1_2015", "MajorMed_2_2015", "MajorMed_3_2015", "MajorMed_4_2015", "MajorMed_5_2015", "MajorMed_6_2015", "MajorMed_7_2015", "MajorMed_8_2015", "MajorMed_9_2015", "MajorMed_10_2015", "MajorMed_11_2015", "MajorMed_12_2015",
  "MajorMed_1_2016", "MajorMed_2_2016", "MajorMed_3_2016", "MajorMed_4_2016", "MajorMed_5_2016", "MajorMed_6_2016", "MajorMed_7_2016", "MajorMed_8_2016", "MajorMed_9_2016", "MajorMed_10_2016", "MajorMed_11_2016", "MajorMed_12_2016",
  "MajorMed_1_2017", "MajorMed_2_2017", "MajorMed_3_2017", "MajorMed_4_2017", "MajorMed_5_2017", "MajorMed_6_2017", "MajorMed_7_2017", "MajorMed_8_2017", "MajorMed_9_2017", "MajorMed_10_2017", "MajorMed_11_2017", "MajorMed_12_2017",
  "MajorMed_1_2018", "MajorMed_2_2018", "MajorMed_3_2018", "MajorMed_4_2018", "MajorMed_5_2018", "MajorMed_6_2018", "MajorMed_7_2018", "MajorMed_8_2018", "MajorMed_9_2018", "MajorMed_10_2018", "MajorMed_11_2018", "MajorMed_12_2018",
  "YouthHealth_1_2015", "YouthHealth_2_2015", "YouthHealth_3_2015", "YouthHealth_4_2015", "YouthHealth_5_2015", "YouthHealth_6_2015", "YouthHealth_7_2015", "YouthHealth_8_2015", "YouthHealth_9_2015", "YouthHealth_10_2015", "YouthHealth_11_2015", "YouthHealth_12_2015",
  "YouthHealth_1_2016", "YouthHealth_2_2016", "YouthHealth_3_2016", "YouthHealth_4_2016", "YouthHealth_5_2016", "YouthHealth_6_2016", "YouthHealth_7_2016", "YouthHealth_8_2016", "YouthHealth_9_2016", "YouthHealth_10_2016", "YouthHealth_11_2016", "YouthHealth_12_2016",
  "YouthHealth_1_2017", "YouthHealth_2_2017", "YouthHealth_3_2017", "YouthHealth_4_2017", "YouthHealth_5_2017", "YouthHealth_6_2017", "YouthHealth_7_2017", "YouthHealth_8_2017", "YouthHealth_9_2017", "YouthHealth_10_2017", "YouthHealth_11_2017", "YouthHealth_12_2017",
  "YouthHealth_1_2018", "YouthHealth_2_2018", "YouthHealth_3_2018", "YouthHealth_4_2018", "YouthHealth_5_2018", "YouthHealth_6_2018", "YouthHealth_7_2018", "YouthHealth_8_2018", "YouthHealth_9_2018", "YouthHealth_10_2018", "YouthHealth_11_2018", "YouthHealth_12_2018"
)

ecoext_Blk_month_join <- ecoext_Blk_month_join %>% select(-all_of(toremove))
ecoext_BG_month_join <- ecoext_BG_month_join %>% select(-all_of(toremove))
ecoext_CT_month_join <- ecoext_CT_month_join %>% select(-all_of(toremove))


## use the function clean_cols from internal to rename columns to "Name_Year_Month" ##
ecoext_Blk_month_join_1 <- rename_with(ecoext_Blk_month_join, .cols = -contains("ID"), .fn = clean_cols)
ecoext_BG_month_join_1 <- rename_with(ecoext_BG_month_join, .cols = -contains("ID"), .fn = clean_cols)
ecoext_CT_month_join_1 <- rename_with(ecoext_CT_month_join, .cols = -contains("ID"), .fn = clean_cols)

## for all NA's add 0's ##
ecoext_Blk_month_join_1[is.na(ecoext_Blk_month_join_1)] <- 0
ecoext_BG_month_join_1[is.na(ecoext_BG_month_join_1)] <- 0
ecoext_CT_month_join_1[is.na(ecoext_CT_month_join_1)] <- 0

## order columns ##
ecoext_Blk_month_join_1 <- ecoext_Blk_month_join_1[ ,order(names(ecoext_Blk_month_join_1))]
ecoext_BG_month_join_1 <- ecoext_BG_month_join_1[ ,order(names(ecoext_BG_month_join_1))]
ecoext_CT_month_join_1 <- ecoext_CT_month_join_1[ ,order(names(ecoext_CT_month_join_1))]
ecoext_Blk_year_join <- ecoext_Blk_year_join[ ,order(names(ecoext_Blk_year_join))]
ecoext_BG_year_join <- ecoext_BG_year_join[ ,order(names(ecoext_BG_year_join))]
ecoext_CT_year_join <- ecoext_CT_year_join[ ,order(names(ecoext_CT_year_join))]

#### save the combined historical + new external files ####
#setwd("C:\\Users\\User\\Google Drive\\BARI Research Team Data Library\\911 Database Working Folder\\2020\\Ecometrics\\")

write.csv(ecoext_Blk_month_join_1, c(paste("Ext_Ecometrics_Blk_Month_", date, ".csv")), row.names = FALSE)
write.csv(ecoext_Blk_year_join, c(paste("Ext_Ecometrics_Blk_Year_", date, ".csv")), row.names = FALSE)

write.csv(ecoext_BG_month_join_1, c(paste("Ext_Ecometrics_BG_Month_", date, ".csv")), row.names = FALSE)
write.csv(ecoext_BG_year_join, c(paste("Ext_Ecometrics_BG_Year_", date, ".csv")), row.names = FALSE)

write.csv(ecoext_CT_month_join_1, c(paste("Ext_Ecometrics_CT_Month_", date, ".csv")), row.names = FALSE)
write.csv(ecoext_CT_year_join, c(paste("Ext_Ecometrics_CT_Year_", date, ".csv")), row.names = FALSE)

############################################################
##### CHANGE TO FORMAT LONG (many rows, less columns) FOR OLD EXTERNAL WITH NEW EXTERNAL #####
############################################################

ecoext_Blk_month_join_tidy <- pivot_longer(ecoext_Blk_month_join_1, cols = -Blk_ID_10, 
                                           names_to = c("ecometric", "year", "month"),
                                           names_sep = "_",
                                           values_to = "value")

ecoext_BG_month_join_tidy <- pivot_longer(ecoext_BG_month_join_1, cols = -BG_ID_10, 
                                          names_to = c("ecometric", "year", "month"),
                                          names_sep = "_",
                                          values_to = "value")

ecoext_CT_month_join_tidy <- pivot_longer(ecoext_CT_month_join_1, cols = -CT_ID_10, 
                                          names_to = c("ecometric", "year", "month"),
                                          names_sep = "_",
                                          values_to = "value")


ecoext_Blk_year_join_tidy <- pivot_longer(ecoext_Blk_year_join, cols = -Blk_ID_10, 
                                          names_to = c("ecometric", "year"),
                                          names_sep = "_",
                                          values_to = "value")

ecoext_BG_year_join_tidy <- pivot_longer(ecoext_BG_year_join, cols = -BG_ID_10, 
                                         names_to = c("ecometric", "year"),
                                         names_sep = "_",
                                         values_to = "value")

ecoext_CT_year_join_tidy <- pivot_longer(ecoext_CT_year_join, cols = -CT_ID_10, 
                                         names_to = c("ecometric", "year"),
                                         names_sep = "_",
                                         values_to = "value")

ecoext_Blk_month_join_tidy <- ecoext_Blk_month_join_tidy[with(ecoext_Blk_month_join_tidy, order(ecometric, year, month)), ]
ecoext_BG_month_join_tidy <- ecoext_BG_month_join_tidy[with(ecoext_BG_month_join_tidy, order(ecometric, year, month)), ]
ecoext_CT_month_join_tidy <- ecoext_CT_month_join_tidy[with(ecoext_CT_month_join_tidy, order(ecometric, year, month)), ]
ecoext_Blk_year_join_tidy <- ecoext_Blk_year_join_tidy[with(ecoext_Blk_year_join_tidy, order(ecometric, year)), ]
ecoext_BG_year_join_tidy <- ecoext_BG_year_join_tidy[with(ecoext_BG_year_join_tidy, order(ecometric, year)), ]
ecoext_CT_year_join_tidy <- ecoext_CT_year_join_tidy[with(ecoext_CT_year_join_tidy, order(ecometric, year)), ]

write.csv(ecoext_Blk_month_join_tidy, c(paste("Ext_Ecometrics_Blk_Month_",date, "_long", ".csv")), row.names = FALSE)
write.csv(ecoext_Blk_year_join_tidy, c(paste("Ext_Ecometrics_Blk_Year_",date, "_long", ".csv")), row.names = FALSE)

write.csv(ecoext_BG_month_join_tidy, c(paste("Ext_Ecometrics_BG_Month_",date, "_long", ".csv")), row.names = FALSE)
write.csv(ecoext_BG_year_join_tidy, c(paste("Ext_Ecometrics_BG_Year_",date, "_long", ".csv")), row.names = FALSE)

write.csv(ecoext_CT_month_join_tidy, c(paste("Ext_Ecometrics_CT_Month_",date, "_long", ".csv")), row.names = FALSE)
write.csv(ecoext_CT_year_join_tidy, c(paste("Ext_Ecometrics_CT_Year_",date, "_long", ".csv")), row.names = FALSE)


############################################################
##### CALCULATE FREQUENCIES #####
############################################################

#### NOTE AR: major med and youth health do not exist! so we removed the code for them
#### NOTE AR: NP_Dist does not exist either
#### NOTE AR: remove mutate part; filter year == 2019 and year == 2020 or year > 2018

setwd("C:\\Users\\User\\Google Drive\\BARI Research Team Data Library\\911 Database Working Folder\\2020\\Frequencies")

freq.data <- save.data2 %>%
  filter(year > 2018) %>%
  filter(!is.na(CAT))

freq.lp <- freq.data %>%
  filter(!is.na(Land_Parcel_ID)) %>%
  plyr::count(c("CAT", "year", "Land_Parcel_ID")) %>%
  reshape2::dcast(Land_Parcel_ID ~ CAT+ year) %>%
  replace(is.na(.), 0)

freq.tl <- freq.data %>%
  filter(!is.na(TLID)) %>%
  plyr::count(c("CAT", "year", "TLID")) %>%
  reshape2::dcast(TLID ~ CAT+ year) %>%
  replace(is.na(.), 0)

ct.data <- save.data2 %>%
  filter(!is.na(CT_ID_10)) %>%
  distinct(eid, TYPE, .keep_all = T) 

ct.count <- plyr::count(ct.data, c("TYPE"))
nrow(ct.count[ct.count$freq < 25,])
rare.types <- ct.count$TYPE[ct.count$freq < 25] # 115 # 87 in 2020 #92 in 2020 ##79 in 2021

ct.freq <- ct.data %>%
  filter(!TYPE %in% rare.types) %>%
  plyr::count(c("CT_ID_10", "TYPE", "year")) %>%
  dcast(CT_ID_10 ~ TYPE + year) %>%
  replace(is.na(.), 0)
dim(ct.freq)
dim(freq.lp)
dim(freq.tl)
head(ct.freq)

### read parcels and roads in order to add the ones with 0 to the frequency tables ###
parcels.2018 <- read.csv("C:\\Users\\User\\Google Drive\\BARI Research Team Data Library\\911 Database Working Folder\\2020\\Input\\Parcel_final_2018_11132019.csv", header=T, stringsAsFactors = F)
road <- st_read("C:\\Users\\User\\Google Drive\\BARI Research Team Data Library\\911 Database Working Folder\\2020\\Input\\Streets\\roads_fin_clust_2019_11142019.shp")

land_parcels <- parcels.2018 %>% select(Land_Parcel_ID)
tlid <- road %>% select(TLID)
tlid$geometry <- NULL

freq.lp <- left_join(land_parcels, freq.lp, by = "Land_Parcel_ID")
freq.lp[is.na(freq.lp)] <- 0

freq.tl <- left_join(tlid, freq.tl, by = "TLID")
freq.tl[is.na(freq.tl)] <- 0

### keep only the CT for which we have ecometrics ###
ct.freq <- ct.freq %>% filter(CT_ID_10 %in% ct_int$CT_ID_10)

write.csv(freq.lp, c(paste("911 Ecometric Type Frequency by Land Parcel and Year.csv", date, ".csv")), row.names = F)
write.csv(freq.tl, c(paste("911 Ecometric Type Frequency by Street Segment and Year.csv", date, ".csv")), row.names = F)
write.csv(ct.freq, c(paste("911 Call Type Frequency by CT and Year.csv", date, ".csv")), row.names = F)

############################################################
##### ADD PREVIOUS YEARS FREQUENCIES #####
############################################################

### if not in the global environment, read the following ###
#freq.lp <- read.csv("C:\\Users\\User\\Google Drive\\BARI Research Team Data Library\\911 Database Working Folder\\Testing\\Frequencies\\911 Ecometric Type Frequency by Land Parcel and Year.csv")
#freq.tl <- read.csv("C:\\Users\\User\\Google Drive\\BARI Research Team Data Library\\911 Database Working Folder\\Testing\\Frequencies\\911 Ecometric Type Frequency by Street Segment and Year.csv")
#ct.freq <- read.csv("C:\\Users\\User\\Google Drive\\BARI Research Team Data Library\\911 Database Working Folder\\Testing\\Frequencies\\911 Call Type Frequency by CT and Year.csv")

#911 Call Type Frequency by CT and Year - all years 02232021 
ct.freq_old <- read.csv("911 Call Type Frequency by CT and Year - all years 02232021 .csv")

ct.freq_join <- left_join(ct.freq_old, ct.freq, by = "CT_ID_10")
ct.freq_join <- ct.freq_join %>% filter(CT_ID_10 %in% ct_int$CT_ID_10)
ct_toadd <- ct.freq_join %>% select(CT_ID_10)


ct.freq_join <- ct.freq_join[2:3305] # second and last column - NEED TO UPDATE YEARLY as columns are added
ct.freq_join <- ct.freq_join[,order(colnames(ct.freq_join))]
ct.freq_join <- cbind(ct_toadd, ct.freq_join)
ct.freq_join[is.na(ct.freq_join)] <- 0

write.csv(ct.freq_join, c(paste("911 Call Type Frequency by CT and Year - all years", date, ".csv")), row.names = FALSE)

############################################################
##### Run checks on freq to make sure the values are correct #####
############################################################

# Problems occurred with 
checks_freq_1 <- ct.freq_join %>% select(CT_ID_10, DISTRBDRUNKS_2017, DISTRBDRUNKS_2018, DISTRBDRUNKS_2019, DISTRBDRUNKS_2020, DISTRBDRUNKS_2021)
View(checks_freq_1)

checks_freq_2 <- ct.freq_join %>% select(CT_ID_10, ABIP_2017, ABIP_2018, ABIP_2019, ABIP_2020, ABIP_2021)
View(checks_freq_2)


############################################################
##### CORRELATIONS #####
############################################################

setwd( "C:\\Users\\User\\Google Drive\\BARI Research Team Data Library\\911 Database Working Folder\\2020\\Ecometrics\\")

# ct911 <- read.csv("Ecometrics_CT_Year_External_031519.csv", header = T, stringsAsFactors = F) %>%
#   replace(is.na(.), 0)
# bg911 <- read.csv("Ecometrics_BG_Year_External_031519.csv", header = T, stringsAsFactors = F)%>%
#   replace(is.na(.), 0)
# blk911 <- read.csv("Ecometrics_Blk_Year_External_031519.csv", header = T, stringsAsFactors = F)

ct911 <- read.csv("Ext_Ecometrics_CT_Year_02282022.csv", header = T, stringsAsFactors = F) %>%
  replace(is.na(.), 0)
#ct911 <- ecoext_CT_year_new1
bg911 <- read.csv("Ext_Ecometrics_BG_Year_02282022.csv", header = T, stringsAsFactors = F)%>%
  replace(is.na(.), 0)
#bg911 <- ecoext_BG_year_new1
blk911 <- read.csv("Ext_Ecometrics_Blk_Year_02282022.csv", header = T, stringsAsFactors = F)
#blk911 <- ecoext_Blk_year_new1

summary(ct911)

#### CORRELATIONS BETWEEN ECOMETRICS AND ECONOMIC ACS ####

# identify RES == 1 tracts and reduce ct911 to those tracts
ct10 <- read.csv("Tracts_Boston_2010_BARI CSV.csv", header = T, stringsAsFactors = F)

head(ct10)
summary(ct10)
ct911.save <- ct911
ct911 <- ct911[!is.na(ct911$CT_ID_10) & ct911$CT_ID_10 %in% ct10$CT_ID_10[ct10$Res == 1],]

### run only the first part, not sure why we need the second one considering that pop.ct[] is not changing for [1] and [2]

pct <- pop.ct[[1]]
pct <- pct[pct$CT_ID_10 %in% ct911$CT_ID_10,]
pct <- pct[order(pct$CT_ID_10),]
sum(pct$CT_ID_10 != ct911$CT_ID_10) #0

A <- pct[,c("CT_ID_10", "MedHouseIncome", "PubAssist", "FamPovPer", "UnempRate", "VacantUnitPer", "HomeOwnPer", "MedGrossRent", "MedHomeVal")]
#B <- ct911[,c("CT_ID_10", colnames(ct911)[grepl("2010", colnames(ct911))])]
B <- ct911

A[is.na(A)] <- 0

#cor.tab1 <- matrix(NA, nrow = ncol(A)-1, ncol = ncol(B)-1)
cor.tab1 <- cor(A[,-1], B[,-1])

for (i in 2:ncol(A)){
  for (j in 2:ncol(B)){
    cor.tab1[i-1,j-1] <- paste(round(cor.test(A[,i], B[,j], use = "pairwise.complete.obs")$estimate, 3),
                              (if(cor.test(A[,i], B[,j], use = "pairwise.complete.obs")$p.value < 0.001) {"***"}
                               else {if(cor.test(A[,i], B[,j], use = "pairwise.complete.obs")$p.value >= 0.001 &
                                        cor.test(A[,i], B[,j], use = "pairwise.complete.obs")$p.value < 0.01) {"**"}
                                 else {if(cor.test(A[,i], B[,j], use = "pairwise.complete.obs")$p.value >= 0.01 &
                                          cor.test(A[,i], B[,j], use = "pairwise.complete.obs")$p.value < 0.05) {"*"}
                                   else {if(cor.test(A[,i], B[,j], use = "pairwise.complete.obs")$p.value >= 0.05) {""}}}}), sep="")
  }
}

cor.tab1 <- as.data.frame(cor.tab1)
colnames(cor.tab1) <- colnames(B)[2:ncol(B)]
rownames(cor.tab1) <- colnames(A)[2:ncol(A)]

####### No run in January 2021 ####


# pct <- pop.ct[[2]]
# pct <- pct[pct$CT_ID_10 %in% ct911$CT_ID_10,]
# pct <- pct[order(pct$CT_ID_10),]
# sum(pct$CT_ID_10 != ct911$CT_ID_10)
# 
# A <- pct[,c("CT_ID_10", "MedHouseIncome", "PubAssist", "FamPovPer", "UnempRate", "VacantUnitPer", "HomeOwnPer", "MedGrossRent", "MedHomeVal")]
# #B <- ct911[,c("CT_ID_10", colnames(ct911)[grepl("11", colnames(ct911))])]
# B <- ct911
# 
# #cor.tab2 <- matrix(NA, nrow = ncol(A)-1, ncol = ncol(B)-1)
# cor.tab2 <- cor(A[,-1], B[,-1])
# 
# for (i in 2:ncol(A)){
#   for (j in 2:ncol(B)){
#     cor.tab2[i-1,j-1] <- paste(round(cor.test(A[,i], B[,j], use = "pairwise.complete.obs")$estimate, 3),
#                                (if(cor.test(A[,i], B[,j], use = "pairwise.complete.obs")$p.value < 0.001) {"***"}
#                                 else {if(cor.test(A[,i], B[,j], use = "pairwise.complete.obs")$p.value >= 0.001 &
#                                          cor.test(A[,i], B[,j], use = "pairwise.complete.obs")$p.value < 0.01) {"**"}
#                                   else {if(cor.test(A[,i], B[,j], use = "pairwise.complete.obs")$p.value >= 0.01 &
#                                            cor.test(A[,i], B[,j], use = "pairwise.complete.obs")$p.value < 0.05) {"*"}
#                                     else {if(cor.test(A[,i], B[,j], use = "pairwise.complete.obs")$p.value >= 0.05) {""}}}}), sep="")
#   }
# }
# 
# cor.tab2 <- as.data.frame(cor.tab2)
# colnames(cor.tab2) <- colnames(B)[2:ncol(B)]
# rownames(cor.tab2) <- colnames(A)[2:ncol(A)]

# put everything together #
#cor <- cbind(cor.tab1, cor.tab2) #because we have just two years 
cor <- cor.tab1
cor <- cor[, sort(colnames(cor))]
head(cor)

## change the path and name ##
write.csv(cor, "C:\\Users\\User\\Google Drive\\BARI Research Team Data Library\\911 Database Working Folder\\2020\\Correlations\\ACS_Ecometrics_RES_Tract_Correlations_2019-2020_02232021.csv", row.names = FALSE)


#### CORRELATIONS BETWEEN ECOMETRICS ####

A <- ct911
B <- ct911

#cor.tab <- matrix(NA, nrow = ncol(A)-1, ncol = ncol(B)-1)
cor.tab <- cor(A[,-1], B[,-1])

for (i in 2:ncol(A)){
  for (j in 2:ncol(B)){
    cor.tab[i-1,j-1] <- paste(round(cor.test(A[,i], B[,j], use = "pairwise.complete.obs")$estimate, 3),
                              (if(cor.test(A[,i], B[,j], use = "pairwise.complete.obs")$p.value < 0.001) {"***"}
                               else {if(cor.test(A[,i], B[,j], use = "pairwise.complete.obs")$p.value >= 0.001 &
                                        cor.test(A[,i], B[,j], use = "pairwise.complete.obs")$p.value < 0.01) {"**"}
                                 else {if(cor.test(A[,i], B[,j], use = "pairwise.complete.obs")$p.value >= 0.01 &
                                          cor.test(A[,i], B[,j], use = "pairwise.complete.obs")$p.value < 0.05) {"*"}
                                   else {if(cor.test(A[,i], B[,j], use = "pairwise.complete.obs")$p.value >= 0.05) {""}}}}), sep="")
  }
}

cor.tab <- as.data.frame(cor.tab)
colnames(cor.tab) <- colnames(B)[2:ncol(B)]
rownames(cor.tab) <- colnames(A)[2:ncol(A)]
head(cor.tab)

write.csv(cor.tab, "C:\\Users\\User\\Google Drive\\BARI Research Team Data Library\\911 Database Working Folder\\2020\\Correlations\\Ecometrics_Tract_Correlations_2019-2020_02232021.csv", row.names = FALSE)

############################################################
##### CHECK THE COLUMNS ORDER #####
##### Run this ONLY if necessary!!! #####
############################################################

setwd("C:\\Users\\User\\Google Drive\\BARI Research Team Data Library\\911 Database Working Folder\\2020\\Ecometrics\\")

### EXTERNAL ###

ecoext_CT_year <- read.csv("Ext_Ecometrics_CT_Year_ 02232021 .csv")
ecoext_BG_year <- read.csv("Ext_Ecometrics_BG_Year_ 02232021 .csv")
ecoext_Blk_year <- read.csv("Ext_Ecometrics_Blk_Year_ 02232021 .csv")

ecoext_CT_year_1 <- ecoext_CT_year %>%
  select(CT_ID_10, Guns_2010, Guns_2011, Guns_2012, Guns_2013, Guns_2014, Guns_2015, Guns_2016, Guns_2017, Guns_2018, Guns_2019, Guns_2020,
         PrivateConflict_2010, PrivateConflict_2011, PrivateConflict_2012, PrivateConflict_2013, PrivateConflict_2014, PrivateConflict_2015, PrivateConflict_2016, PrivateConflict_2017, PrivateConflict_2018, PrivateConflict_2019, PrivateConflict_2020,
         SocDis_2010, SocDis_2011, SocDis_2012, SocDis_2013, SocDis_2014, SocDis_2015, SocDis_2016, SocDis_2017, SocDis_2018, SocDis_2019, SocDis_2020,
         Violence_2010, Violence_2011, Violence_2012, Violence_2013, Violence_2014, Violence_2015, Violence_2016, Violence_2017, Violence_2018, Violence_2019, Violence_2020,
         MajorMed_2010, MajorMed_2011, MajorMed_2012, MajorMed_2013, MajorMed_2014, 
         YouthHealth_2010, YouthHealth_2011, YouthHealth_2012, YouthHealth_2013, YouthHealth_2014)

ecoext_BG_year_1 <- ecoext_BG_year %>%
  select(BG_ID_10, Guns_2010, Guns_2011, Guns_2012, Guns_2013, Guns_2014, Guns_2015, Guns_2016, Guns_2017, Guns_2018, Guns_2019, Guns_2020,
         PrivateConflict_2010, PrivateConflict_2011, PrivateConflict_2012, PrivateConflict_2013, PrivateConflict_2014, PrivateConflict_2015, PrivateConflict_2016, PrivateConflict_2017, PrivateConflict_2018, PrivateConflict_2019, PrivateConflict_2020,
         SocDis_2010, SocDis_2011, SocDis_2012, SocDis_2013, SocDis_2014, SocDis_2015, SocDis_2016, SocDis_2017, SocDis_2018, SocDis_2019, SocDis_2020,
         Violence_2010, Violence_2011, Violence_2012, Violence_2013, Violence_2014, Violence_2015, Violence_2016, Violence_2017, Violence_2018, Violence_2019, Violence_2020,
         MajorMed_2010, MajorMed_2011, MajorMed_2012, MajorMed_2013, MajorMed_2014, 
         YouthHealth_2010, YouthHealth_2011, YouthHealth_2012, YouthHealth_2013, YouthHealth_2014)

ecoext_Blk_year_1 <- ecoext_Blk_year %>%
  select(Blk_ID_10, Guns_2010, Guns_2011, Guns_2012, Guns_2013, Guns_2014, Guns_2015, Guns_2016, Guns_2017, Guns_2018, Guns_2019, Guns_2020,
         PrivateConflict_2010, PrivateConflict_2011, PrivateConflict_2012, PrivateConflict_2013, PrivateConflict_2014, PrivateConflict_2015, PrivateConflict_2016, PrivateConflict_2017, PrivateConflict_2018, PrivateConflict_2019, PrivateConflict_2020,
         SocDis_2010, SocDis_2011, SocDis_2012, SocDis_2013, SocDis_2014, SocDis_2015, SocDis_2016, SocDis_2017, SocDis_2018, SocDis_2019, SocDis_2020,
         Violence_2010, Violence_2011, Violence_2012, Violence_2013, Violence_2014, Violence_2015, Violence_2016, Violence_2017, Violence_2018, Violence_2019, Violence_2020,
         MajorMed_2010, MajorMed_2011, MajorMed_2012, MajorMed_2013, MajorMed_2014, 
         YouthHealth_2010, YouthHealth_2011, YouthHealth_2012, YouthHealth_2013, YouthHealth_2014)

date <- "02232021"

write.csv(ecoext_Blk_year_1, c(paste("Ext_Ecometrics_Blk_Year_", date, ".csv")), row.names = FALSE)
write.csv(ecoext_BG_year_1, c(paste("Ext_Ecometrics_BG_Year_", date, ".csv")), row.names = FALSE)
write.csv(ecoext_CT_year_1, c(paste("Ext_Ecometrics_CT_Year_", date, ".csv")), row.names = FALSE)

### INTERNAL ###

ecoint_CT_year <- read.csv("Int_Ecometrics_CT_Year_ 02232021 .csv")
ecoint_BG_year <- read.csv("Int_Ecometrics_BG_Year_ 02232021 .csv")
ecoint_Blk_year <- read.csv("Int_Ecometrics_Blk_Year_ 02232021 .csv")

ecoint_CT_year_1 <- ecoint_CT_year %>%
  select(CT_ID_10, Guns_2010, Guns_2011, Guns_2012, Guns_2013, Guns_2014, Guns_2015, Guns_2016, Guns_2019, Guns_2020,
         PrivateConflict_2010, PrivateConflict_2011, PrivateConflict_2012, PrivateConflict_2013, PrivateConflict_2014, PrivateConflict_2015, PrivateConflict_2016, PrivateConflict_2019, PrivateConflict_2020,
         SocDis_2010, SocDis_2011, SocDis_2012, SocDis_2013, SocDis_2014, SocDis_2015, SocDis_2016, SocDis_2019, SocDis_2020,
         Violence_2010, Violence_2011, Violence_2012, Violence_2013, Violence_2014, Violence_2015, Violence_2016, Violence_2019, Violence_2020,
         MajorMed_2010, MajorMed_2011, MajorMed_2012, MajorMed_2013, MajorMed_2014, 
         YouthHealth_2010, YouthHealth_2011, YouthHealth_2012, YouthHealth_2013, YouthHealth_2014)

ecoint_BG_year_1 <- ecoint_BG_year %>%
  select(BG_ID_10, Guns_2010, Guns_2011, Guns_2012, Guns_2013, Guns_2014, Guns_2015, Guns_2016, Guns_2019, Guns_2020,
         PrivateConflict_2010, PrivateConflict_2011, PrivateConflict_2012, PrivateConflict_2013, PrivateConflict_2014, PrivateConflict_2015, PrivateConflict_2016, PrivateConflict_2019, PrivateConflict_2020,
         SocDis_2010, SocDis_2011, SocDis_2012, SocDis_2013, SocDis_2014, SocDis_2015, SocDis_2016, SocDis_2019, SocDis_2020,
         Violence_2010, Violence_2011, Violence_2012, Violence_2013, Violence_2014, Violence_2015, Violence_2016, Violence_2019, Violence_2020,
         MajorMed_2010, MajorMed_2011, MajorMed_2012, MajorMed_2013, MajorMed_2014, 
         YouthHealth_2010, YouthHealth_2011, YouthHealth_2012, YouthHealth_2013, YouthHealth_2014)

ecoint_Blk_year_1 <- ecoint_Blk_year %>%
  select(Blk_ID_10, Guns_2010, Guns_2011, Guns_2012, Guns_2013, Guns_2014, Guns_2015, Guns_2016, Guns_2019, Guns_2020,
         PrivateConflict_2010, PrivateConflict_2011, PrivateConflict_2012, PrivateConflict_2013, PrivateConflict_2014, PrivateConflict_2015, PrivateConflict_2016, PrivateConflict_2019, PrivateConflict_2020,
         SocDis_2010, SocDis_2011, SocDis_2012, SocDis_2013, SocDis_2014, SocDis_2015, SocDis_2016, SocDis_2019, SocDis_2020,
         Violence_2010, Violence_2011, Violence_2012, Violence_2013, Violence_2014, Violence_2015, Violence_2016, Violence_2019, Violence_2020,
         MajorMed_2010, MajorMed_2011, MajorMed_2012, MajorMed_2013, MajorMed_2014, 
         YouthHealth_2010, YouthHealth_2011, YouthHealth_2012, YouthHealth_2013, YouthHealth_2014)

#date <- "02022021"

write.csv(ecoint_Blk_year_1, c(paste("Int_Ecometrics_Blk_Year_", date, ".csv")), row.names = FALSE)
write.csv(ecoint_BG_year_1, c(paste("Int_Ecometrics_BG_Year_", date, ".csv")), row.names = FALSE)
write.csv(ecoint_CT_year_1, c(paste("Int_Ecometrics_CT_Year_", date, ".csv")), row.names = FALSE)


