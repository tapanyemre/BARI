
#title: "Properties getting TLID from TIGER data"
#output: html_document
# date: "2023-07-19"


#Description- From Parcels to Streets
#- This script is assigning street information (from tiger data) to parcels.
#- In this script, we are changing PAD data and tiger csv with the road from last year.

# 0. Preliminary Stuff

# Set Environment}
## If you use the drive file format, you can use the code below. If not, put your path here.
#GDrivepath <- "~/Google Drive/BARI Research Team Data Library/"
#setwd(paste0(GDrivepath,"Geographical Infrastructure/Boston Geographical Infrastructure 2019/Scripts"))


# Load Libraries}
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
library(tigris)
options(tigris_class = "sf",tigris_use_cache = T)
library(units)



#Read BARI data}
# Parcel Data - from the city of Boston 
#parcels21_shp <- st_read("~/Desktop/BARI/GI-2023/new datasets/Parcel Datasets/Parcels_2021/Parcels_2021.shp")
#parcels22_shp <- st_read("~/Downloads/BARIParcels2022.shp")
parcels23_shp <- st_read("~/Desktop/BARI/GI-2023/new datasets/Parcel Datasets/Parcels_2023/parcels_23.shp")

# PAD unit from Property assessment data for 2021 # this PAD script is run by Saina
# propbari22 <- read_csv("~/Desktop/BARI/new datasets/Property Assesments/fy2022pa-4.csv")
# PAD unit 2019, if needed for any checks
# propbari18 <- read_csv("PAD.Record.wUnit.07242019.csv")
propassraw <- read.csv("PA-2023/outputs_Feb24/PAD.Record.wUnit.2022.csv")

# read street data #we are using the csv created for the GI2019 release
#tiger22 <- read.csv("Streets_2019_Prefinal_190731.csv")
tiger22 <- read.csv("~/Desktop/BARI/GI-2023/new datasets/Street Data/roads_fin_clust_2022_12132022.csv")

# read shp street data and connect geometry with csv (((UPDATE YEARS HERE< TAKE them from drive)))
tiger22_shp <- st_read("~/Downloads/Roads.2022/Roads.2022.shp")
tiger22$geometry <- tiger22_shp$geometry[match(tiger22$TLID, tiger22_shp$TLID)]


## Updates on datasets
#- 2021 shp is the lastest as of 02182023, Toshi


# propbari18  <- propbari18 %>% rename(GIS_ID = parcel_num)


# 1. Data Manipulation


# Cleaning up the properties for centroids
#?propassraw$PID <- as.integer(propassraw$PID)   #gives NAs???                                          
propass23 <- propassraw[!duplicated(propassraw$GIS_ID),] #98639 in Aug 2023 #98873, as of 2022
#98873, # 98907 in 2019 #99777 in 2020, #98943 in 2021

#remove NAs
parcels23_shp <- parcels23_shp[!is.na(as.numeric(as.character(parcels23_shp$MAP_PAR_ID))),]

# Finding the centroid and merging it back unto the parcel shapefile
#P21shp_centroid <- st_centroid(parcels21_shp %>% st_transform(26986)) #st_centroid assumes attributes are constant over geometries of x
P23shp_centroid <- st_centroid(parcels23_shp %>%  st_transform(26986))
#??? give an error ????? solved it by using 2023 data

# Merging the property data with the parcels shapefile and 
# converting them to spatial objects (needed to be re-projected)
P23shp_centroid$GIS_ID <- as.numeric(as.character(P23shp_centroid$MAP_PAR_ID))
#????? no column like GIS_ID - create a new column and rename it as GIS_ID -- changed it to MAP_PAR_ID

#?propass23$GIS_ID <- as.numeric(propass23$GIS_ID) #they are identical
P23sf <- left_join(P23shp_centroid, propass23, by=c("GIS_ID"="GIS_ID"))
a <- left_join(propass23, P23shp_centroid, by=c("GIS_ID"="GIS_ID"))
# a is # 98644, 82 variable in Aug 23
# a is # 98908, 82 Variable in March 2023

#store NAs
P23sf_NAs <- P23sf[is.na(P23sf$ST_NAME),]
#eliminate NAs
P23sf <- P23sf[!is.na(P23sf$ST_NAME),]
#remove duplicates
P23sf <- P23sf[!duplicated(P23sf$PID),]
#98639 after cleaning in Sep 23
#### Street analysis ####

## we remove every suf part because it does not exist in pad 2021 ##

#P23sf$ST_NAME_SUF <- gsub("XT","EXD",P23sf$ST_NAME_SUF)
#P23sf$ST_NAME_SUF2 <- P23sf$ST_NAME_SUF
#P23sf$ST_NAME_SUF2[is.na(P23sf$ST_NAME_SUF2)] <- ""
#P23sf$FULLNAME <- str_c(P23sf$ST_NAME, P23sf$ST_NAME_SUF2, sep = " ") #no ST_NAME_SUF2 column anymore in 2023
P23sf$FULLNAME <- P23sf$ST_NAME
#P23sf$FULLNAME <- str_trim(P23sf$FULLNAME) # trim whitespace
P23sf$ST_NAME <- toupper(P23sf$ST_NAME)
P23sf$FULLNAME <- P23sf$ST_NAME

P23sf$FULLNAME <- recode(P23sf$FULLNAME, "WM T MORRISSEY BL" = "WILLIAM T MORRISSEY BLVD" )
P23sf$FULLNAME <- recode(P23sf$FULLNAME, "JAMES ONEILL ST" = "JAMES O'NEIL ST" )
P23sf$FULLNAME <- recode(P23sf$FULLNAME, "DRYDOCK AV" = "DRY DOCK AV" )
P23sf$FULLNAME <- recode(P23sf$FULLNAME, "GEN JOZEF PILSUDSKI WAY" = "PILSUDSKI WAY")
P23sf$FULLNAME <- recode(P23sf$FULLNAME, "REV R M COSTELLO PL" = "REV ROBERT M COSTELLO PL")
P23sf$FULLNAME <- recode(P23sf$FULLNAME, "MSGR REYNOLDS WAY" = "MONSIGNOR REYNOLDS WAY")
P23sf$FULLNAME <- recode(P23sf$FULLNAME, "WOODDALE AV" = "WOODALE AV")
P23sf$FULLNAME <- recode(P23sf$FULLNAME, "SURREYHILL LA" = "SURREY HILL LA" )
P23sf$FULLNAME <- recode(P23sf$FULLNAME, "RINGGOLD ST" = "RINGOLD ST" )
P23sf$FULLNAME <- recode(P23sf$FULLNAME, "O DONNELL TE" = "ODONNELL TE" )
P23sf$FULLNAME <- recode(P23sf$FULLNAME, "GEN WM H DEVINE WY" = "DEVINE WY" )
P23sf$FULLNAME <- recode(P23sf$FULLNAME, "HAZLETON ST" = "HAZELTON ST")
P23sf$FULLNAME <- recode(P23sf$FULLNAME, "MORRISON ST" = "SELWYN ST") # per BostonMap
P23sf$FULLNAME <- recode(P23sf$FULLNAME, "MALCOLM X BL" = "NEW DUDLEY ST") # per BostonMap
P23sf$FULLNAME <- recode(P23sf$FULLNAME, "LANE PK" = "LN PARK") # per BostonMap
P23sf$FULLNAME <- recode(P23sf$FULLNAME, "ALWIN ST" = "ALWYN ST") # per BostonMap
P23sf$FULLNAME <- recode(P23sf$FULLNAME, "SILVIA CT" = "SYLVIA CT") # per BostonMap
P23sf$FULLNAME <- recode(P23sf$FULLNAME, "JESHURUN RD" = "JESHURUN ST") # per BostonMap
P23sf$FULLNAME <- recode(P23sf$FULLNAME, "GARDEN COURT ST" = "GARDEN CT") # per BostonMap
P23sf$FULLNAME <- recode(P23sf$FULLNAME, "WAYMOUNT ST" = "PARKER HILL AV") # per BostonMap
P23sf$FULLNAME <- recode(P23sf$FULLNAME, "POND VIEW AV" = "PONDVIEW AV") # per BostonMap
P23sf$FULLNAME <- recode(P23sf$FULLNAME, "ABBY RD" = "ABBEY RD") # per BostonMap
P23sf$FULLNAME <- recode(P23sf$FULLNAME, "ALTACREST RD" = "ALTA CREST RD") # per BostonMap
P23sf$FULLNAME <- recode(P23sf$FULLNAME, "TERAGRAM ST" = "TEREGRAM ST") # per BostonMap
P23sf$FULLNAME <- recode(P23sf$FULLNAME, "JOANNE TE" = "JO ANNE TE") # per BostonMap
P23sf$FULLNAME <- recode(P23sf$FULLNAME, "DELANY CI" = "DELANEY CT") # per BostonMap
P23sf$FULLNAME <- recode(P23sf$FULLNAME, "BONELL TE" = "BONNEL TE") # per BostonMap
P23sf$FULLNAME <- recode(P23sf$FULLNAME, "AVENUE LOUIS PASTEUR" = "AVE LOUIS PASTEUR") # per BostonMap
P23sf$FULLNAME <- recode(P23sf$FULLNAME, "ST A" = "A ST") # per BostonMap
P23sf$FULLNAME <- recode(P23sf$FULLNAME, "ANTHONY R VALENTI WY" = "TRAVERSE ST") # per BostonMap TLID matching
P23sf$FULLNAME <- recode(P23sf$FULLNAME, "ADRIAN ST" = "ADRAIN ST") # per BostonMap
P23sf$FULLNAME <- recode(P23sf$FULLNAME, "UFFORD ST" = "DYER ST") # per BostonMap, this is closest
P23sf$FULLNAME <- recode(P23sf$FULLNAME, "ST PAULS AV" = "ST PAUL'S AV") # per BostonMap
P23sf$FULLNAME <- recode(P23sf$FULLNAME, "ORGAN PARK ST" = "ORGAN PARK") # per BostonMap
P23sf$FULLNAME <- recode(P23sf$FULLNAME, "O DONNELL SQ" = "ODONNELL SQ") # per BostonMap
P23sf$FULLNAME <- recode(P23sf$FULLNAME, "WM CARD OCONNELL WY" = "WILLIAM CARDINAL O'CONNELL WAY") # per BostonMap
P23sf$FULLNAME <- recode(P23sf$FULLNAME, "MARYANNA RD" = "MARY ANNA RD") # per BostonMap
P23sf$FULLNAME <- recode(P23sf$FULLNAME, "LEFEVRE ST" = "LE FEVRE ST") # per BostonMap
P23sf$FULLNAME <- recode(P23sf$FULLNAME, "CENTER PZ" = "CAMBRIDGE ST") # per BostonMap
P23sf$FULLNAME <- recode(P23sf$FULLNAME, "YEOMAN PL" = "YEOMAN ST") # per google
P23sf$FULLNAME <- recode(P23sf$FULLNAME, "THEODORE A GLYNN WY" = "THEODORE GLYNN WY") # per bostonmap
P23sf$FULLNAME <- recode(P23sf$FULLNAME, "RIVERBANK PL" = "RIVERBANK PL PVT WAY") # per bostonmap
P23sf$FULLNAME <- recode(P23sf$FULLNAME, "REV RICHARD A BURKE" = "REV RICHARD A BURKE ST") # per bostonmap
P23sf$FULLNAME <- recode(P23sf$FULLNAME, "JEANNE DR" = "BEECHWOOD ST") # per bostonmap
P23sf$FULLNAME <- recode(P23sf$FULLNAME, "GEN JOZEF PILSUDSKI WY" = "PILSUDSKI WY") # per bostonmap
P23sf$FULLNAME <- recode(P23sf$FULLNAME, "FIDELIS WA" = "FIDELIS WAY") # per BostonMap
P23sf$FULLNAME <- recode(P23sf$FULLNAME, "DR MARY M BEATTY CI" = "MARY MOORE BEATTY CIR") # per BostonMap
P23sf$FULLNAME <- recode(P23sf$FULLNAME, "CORNAUBA EXD" = "CORNAUBA ST EXD") # per BostonMap
P23sf$FULLNAME <- recode(P23sf$FULLNAME, "AVENUE DE LAFAYETTE" = "AVE DE LAFAYETTE") # per BostonMap
P23sf$FULLNAME <- recode(P23sf$FULLNAME, "WILLIAM E DOUCETTE SQ" = "WM E DOUCETTE SQ") # per BostonMap
P23sf$FULLNAME <- gsub(" AV$"," AVE",P23sf$FULLNAME)
P23sf$FULLNAME <- gsub(" AV "," AVE ",P23sf$FULLNAME)
P23sf$FULLNAME <- gsub(" HW$"," HWY",P23sf$FULLNAME)
P23sf$FULLNAME <- gsub(" WY$"," WAY",P23sf$FULLNAME)
P23sf$FULLNAME <- gsub(" BL$"," BLVD",P23sf$FULLNAME)
P23sf$FULLNAME <- gsub(" CI$"," CIR",P23sf$FULLNAME)
P23sf$FULLNAME <- gsub(" CR$"," CRES",P23sf$FULLNAME)
P23sf$FULLNAME <- gsub(" TE$"," TER",P23sf$FULLNAME)
P23sf$FULLNAME <- gsub(" PZ$"," PLZ",P23sf$FULLNAME)
P23sf$FULLNAME <- gsub(" PA$"," PATH",P23sf$FULLNAME)
P23sf$FULLNAME <- gsub(" WH$"," WHARF",P23sf$FULLNAME)
P23sf$FULLNAME <- gsub(" LA$"," LN",P23sf$FULLNAME)
P23sf$FULLNAME <- gsub(" RO$"," ROW",P23sf$FULLNAME)
P23sf$FULLNAME <- gsub("FIRST","1ST",P23sf$FULLNAME) # making changes to parcel address names to match census:
P23sf$FULLNAME <- gsub("SECOND","2ND",P23sf$FULLNAME) 
P23sf$FULLNAME <- gsub("THIRD","3RD",P23sf$FULLNAME)
P23sf$FULLNAME <- gsub("FOURTH","4TH",P23sf$FULLNAME) 
P23sf$FULLNAME <- gsub("FIFTH","5TH",P23sf$FULLNAME) 
P23sf$FULLNAME <- gsub("SIXTH","6TH",P23sf$FULLNAME) 
P23sf$FULLNAME <- gsub("SEVENTH","7TH",P23sf$FULLNAME)
P23sf$FULLNAME <- gsub("EIGHTH","8TH",P23sf$FULLNAME) 
P23sf$FULLNAME <- gsub("NINTH","9TH",P23sf$FULLNAME)
# P23sf$FULLNAME <- gsub("TENTH","10TH",P23sf$FULLNAME)

P23sf$FULLNAME <- str_trim(P23sf$FULLNAME)
P23sf$FULLNAME <- gsub(' +',' ', P23sf$FULLNAME)

# Saina's portion
P23sf$FULLNAME <- str_replace(P23sf$FULLNAME, "PW$", "PKWY")
P23sf$FULLNAME <- str_replace(P23sf$FULLNAME, "PK$", "PARK")
P23sf$FULLNAME <- str_replace(P23sf$FULLNAME, "CC$", "CIRCUIT")
# P23sf$FULLNAME <- str_replace(P23sf$FULLNAME, "CT$", "CIRCUIT")
P23sf$FULLNAME <- str_replace(P23sf$FULLNAME, "WM C KELLY SQ", "CENTRAL SQ")
P23sf$FULLNAME <- str_replace(P23sf$FULLNAME, "DELOS ST", "Grove Terrace")
P23sf$FULLNAME <- str_replace(P23sf$FULLNAME, "SEA VIEW AVE", "SEAVIEW AVE")
P23sf$FULLNAME <- str_replace(P23sf$FULLNAME, "MSGR P J LYDON WAY", "MSGR PATRICK J LYDON WAY")
P23sf$FULLNAME <- gsub("^MT ","MOUNT ",P23sf$FULLNAME)

# P23sf$FULLNAME <- str_replace(P23sf$FULLNAME, "MOUNT", "MT")
P23sf$FULLNAME <- str_replace(P23sf$FULLNAME, "DRAYTON ST", "DRAYTON AVE")
P23sf$FULLNAME <- str_replace(P23sf$FULLNAME, "ARBORWAY ST", "ARBORWAY")
P23sf$FULLNAME <- str_replace(P23sf$FULLNAME, "CASTLE ROCK ST", "CASTLEROCK ST")
P23sf$FULLNAME <- str_replace(P23sf$FULLNAME, "NAVARRE PL", "NAVARRE ST")
P23sf$FULLNAME <- str_replace(P23sf$FULLNAME, "WM F MCCLELLAN HWY", "WILLIAM F MCCLELLAN HWY")
P23sf$FULLNAME <- str_replace(P23sf$FULLNAME, "ELM LAWN", "ELM LAWN ST")
P23sf$FULLNAME <- str_replace(P23sf$FULLNAME, "GROTTO GLEN AVE", "GROTTO GLEN RD")
P23sf$FULLNAME <- str_replace(P23sf$FULLNAME, "ROSEBERY", "ROSEBERRY")
P23sf$FULLNAME <- str_replace(P23sf$FULLNAME, "JAMAICAWAY ST", "JAMAICAWAY")
P23sf$FULLNAME <- str_replace(P23sf$FULLNAME, "MARTIN LUTHER KING JR BLVD", "MARTIN LUTHER KING BLVD")
P23sf$FULLNAME <- str_replace(P23sf$FULLNAME, "ROCK AVE", "ROCK AVE PRIVATE WAY")
P23sf$FULLNAME <- str_replace(P23sf$FULLNAME, "CHARLESGATE EAST", "CHARLESGATE E")
P23sf$FULLNAME <- str_replace(P23sf$FULLNAME, "KEYES STREET PL", "KEYES PL")
P23sf$FULLNAME <- str_replace(P23sf$FULLNAME, "PIERSON ST", "PEIRSON ST")
P23sf$FULLNAME <- str_replace(P23sf$FULLNAME, "FENWAY ST", "FENWAY")
P23sf$FULLNAME <- str_replace(P23sf$FULLNAME, "TRUMAN PW", "TRUMAN HWY")
P23sf$FULLNAME <- str_replace(P23sf$FULLNAME, "TALBOT PL", "TALBOT AVE")
P23sf$FULLNAME <- str_replace(P23sf$FULLNAME, "ORCHARD AVE", "ORCHARDHILL RD")
## round two
P23sf$FULLNAME <- str_replace(P23sf$FULLNAME, "NEWBURY ST", "NEWBURY")
P23sf$FULLNAME <- str_replace(P23sf$FULLNAME, "NEWBURY  ST", "NEWBURY")
tiger22$FULLNAM <- str_replace(tiger22$FULLNAM, "NEWBURY ST", "NEWBURY")
P23sf[90105,]$FULLNAME
# 98779 what is this number and code 

# Emma's portion
# P23sf$FULLNAME  <- gsub("CHARLES STREET SOUTH","CHARLES ST S", P23sf$FULLNAME)
P23sf$FULLNAME <- str_replace(P23sf$FULLNAME, "CHARLES STREET SOUTH", "CHARLES ST S")
P23sf$FULLNAME <- str_replace(P23sf$FULLNAME, "OAK STREET WEST","OAK ST W")
tiger22[13579,]$FULLNAM
P23sf[40364,]$FULLNAME

P23sf$FULLNAME <- str_replace(P23sf$FULLNAME, "SIXTEENTH ST","16TH ST")
P23sf$FULLNAME <- str_replace(P23sf$FULLNAME, "TE$"," TER")
P23sf$FULLNAME <- str_replace(P23sf$FULLNAME, "FR FRANCIS GILDAY ST","FATHER FRANCIS GILDAY ST")
# P23sf$FULLNAME <- str_replace(P23sf$FULLNAME, "CEMETERY LN","CEMETERY LA")
# P23sf$FULLNAME <- str_replace(P23sf$FULLNAME, "CHISHOLM LN","CHISHOLM LA")
P23sf$FULLNAME <- str_replace(P23sf$FULLNAME, " WEST"," W")
P23sf$FULLNAME <- str_replace(P23sf$FULLNAME, "MSGR ","MONSIGNOR ")

# P23sf$FULLNAME  <- gsub(" PK"," PARK", P23sf$FULLNAME)
# P23sf$FULLNAME  <- gsub(" WY"," WAY", P23sf$FULLNAME)
# P23sf$FULLNAME  <- gsub(" CI"," CIR", P23sf$FULLNAME)
# P23sf$FULLNAME  <- gsub(" AV"," AVE", P23sf$FULLNAME)
# P23sf$FULLNAME  <- gsub(" TE"," TER", P23sf$FULLNAME)
# P23sf$FULLNAME  <- gsub("FR ","FATHER ", P23sf$FULLNAME)
# P23sf$FULLNAME  <- gsub(" LA"," LN", P23sf$FULLNAME)
# P23sf$FULLNAME  <- gsub(" WH"," WHARF", P23sf$FULLNAME)
# P23sf$FULLNAME  <- gsub(" WEST"," W", P23sf$FULLNAME)
# P23sf$FULLNAME  <- gsub("MSGR ","MONSIGNOR ", P23sf$FULLNAME)

# Railey's portion
# P23sf$FULLNAME<- str_replace(P23sf$FULLNAME, "PK$", "PARK")
P23sf$FULLNAME<- str_replace(P23sf$FULLNAME, "CHICKAMAUGA", "CHICAMAUGA")
P23sf$FULLNAME<- str_replace(P23sf$FULLNAME, "LAGRANGE", "LA GRANGE")
P23sf$FULLNAME<- str_replace(P23sf$FULLNAME, "MORTON VILLAGE", "MORTON VILLIAGE")
P23sf$FULLNAME<- str_replace(P23sf$FULLNAME, "CEDAR GROVE", "CEDAR GROVE ST")
P23sf$FULLNAME<- str_replace(P23sf$FULLNAME, "N BENNET CT", "N BENNETT CT")
P23sf$FULLNAME<- str_replace(P23sf$FULLNAME, "VAN BRUNT", "VAN BRUNT ST")
P23sf$FULLNAME<- str_replace(P23sf$FULLNAME, "WALES PL", "WALES ST")
P23sf$FULLNAME<- str_replace(P23sf$FULLNAME, "UPHAM CT", "UPHAMS CT")
P23sf$FULLNAME<- str_replace(P23sf$FULLNAME, "PARK VALE", "PARKVALE")
P23sf$FULLNAME<- str_replace(P23sf$FULLNAME, "THIRTEENTH", "13th")
P23sf$FULLNAME<- str_replace(P23sf$FULLNAME, "POWER HOUSE", "POWER HOUSE ST")
P23sf$FULLNAME<- str_replace(P23sf$FULLNAME, "SOLDIERS FIELD RD EXD", "SOLDIERS FIELD RD")
P23sf$FULLNAME<- str_replace(P23sf$FULLNAME, "STREET A", "A ST")
P23sf$FULLNAME<- str_replace(P23sf$FULLNAME, "O CONNELL", "OCONNELL")
P23sf$FULLNAME<- str_replace(P23sf$FULLNAME, "PORT NORFOLK", "PORT NORFOLK ST")
P23sf$FULLNAME<- str_replace(P23sf$FULLNAME, "TAFTHILL", "TAFT HILL")
P23sf$FULLNAME<- str_replace(P23sf$FULLNAME, "E BOUNDARY RD", "ENNEKING PKWY")
P23sf$FULLNAME<- str_replace(P23sf$FULLNAME, "PIE AL", "PIE ALY")
P23sf$FULLNAME<- str_replace(P23sf$FULLNAME, "MATIGNON", "MATIGON")
# Courts/very small streets that are being grouped with the larger streets they are connected to 
P23sf$FULLNAME<- str_replace(P23sf$FULLNAME, "RUGGLES CT", "RUGGLES ST")
P23sf$FULLNAME<- str_replace(P23sf$FULLNAME, "JUNIPER TER", "JUNIPER ST")
P23sf$FULLNAME<- str_replace(P23sf$FULLNAME, "MEYER CT", "MEYER ST")
P23sf$FULLNAME<- str_replace(P23sf$FULLNAME, "DUDLEY PL", "DUDLEY ST")
P23sf$FULLNAME<- str_replace(P23sf$FULLNAME, "NEAL CT", "SHORT ST")
P23sf$FULLNAME<- str_replace(P23sf$FULLNAME, "LOWLAND PL", "EVERETT ST")
P23sf$FULLNAME<- str_replace(P23sf$FULLNAME, "MONMOUTH SQ", "MONMOUTH ST")
P23sf$FULLNAME<- str_replace(P23sf$FULLNAME, "FOSTER CT", "FOSTER ST")

#Zakhire <- P23sf
#P23sf <- Zakhire
# Mehrnaz's portion

# P23sf$FULLNAME <- gsub("BOARD AL", "BOARD ST", P23sf$FULLNAME)
P23sf$FULLNAME <- gsub("DEDHAM LINE", "DEDHAM ST", P23sf$FULLNAME) ## done
# P23sf$FULLNAME <- gsub("WARREN PK", "WARREN PARK", P23sf$FULLNAME)
P23sf$FULLNAME <- gsub("HAYDN ST", "HAYDEN ST", P23sf$FULLNAME) ## done
P23sf$FULLNAME <- gsub("NORTHDALE ST", "NORTHDALE RD", P23sf$FULLNAME) ## done

P23sf$FULLNAME <- gsub("E BROADWAY ST", "E BROADWAY", P23sf$FULLNAME) ## done
P23sf$FULLNAME <- gsub("BROADWAY ST", "BROADWAY", P23sf$FULLNAME) ## done
P23sf$FULLNAME <- gsub("W BROADWAY ST", "W BROADWAY", P23sf$FULLNAME) ## done

P23sf$FULLNAME <- gsub("RAILROAD RD", "RAILROAD ST", P23sf$FULLNAME) ## done
P23sf$FULLNAME <- gsub("ADAMS$", "ADAMS ST", P23sf$FULLNAME)  ## done
P23sf$FULLNAME <- gsub("MOTHER JULIA RD", "MOTHER JULIAN RD", P23sf$FULLNAME)  ## done
P23sf$FULLNAME <- gsub("CHARLES RIVER DAM", "CHARLES RIVER DAM RD",P23sf$FULLNAME)  ## done & save
P23sf$FULLNAME <- gsub("SNOW$", "SNOW ST", P23sf$FULLNAME) ## done
P23sf$FULLNAME <- gsub("CHARLESGATE ST", "CHARLESGATE", P23sf$FULLNAME) ## done
P23sf$FULLNAME <- gsub("BELMONT PL", "BELMONT ST", P23sf$FULLNAME) ## done
P23sf$FULLNAME <- gsub("LOTHROP PL", "LOTHROP ST", P23sf$FULLNAME) ## done
P23sf$FULLNAME <- gsub("LEAMINGTON ST", "LEAMINGTON RD", P23sf$FULLNAME)## done & save
P23sf$FULLNAME <- gsub("NEW PICKERTS WHARF", "CENTRAL WHARF", P23sf$FULLNAME) ## done
P23sf$FULLNAME <- gsub("BOWDOIN SQ", "BOWDOIN ST", P23sf$FULLNAME) ## done
P23sf$FULLNAME <- gsub("COMMERCIAL WHARF EAST", "COMMERCIAL WHARF", P23sf$FULLNAME)  ## done
P23sf$FULLNAME <- gsub("GROVE PL", "GROVE ST", P23sf$FULLNAME)  ## done
P23sf$FULLNAME <- gsub("LEVERETT ST", "LEVERETT AVE", P23sf$FULLNAME)  ## done
P23sf$FULLNAME <- gsub("HOWE AVE", "HOWE ST", P23sf$FULLNAME)  ## done
P23sf$FULLNAME <- gsub("BRADEEN STREET FW", "BRADEEN ST", P23sf$FULLNAME)  ## done
P23sf$FULLNAME <- gsub("BAKERS AL", "BAKERS ALY", P23sf$FULLNAME)  ## done
P23sf$FULLNAME <- gsub("BATTERY WHARF ST", "BATTERY WHARF", P23sf$FULLNAME)  ## done
P23sf$FULLNAME <- gsub("PINEFIELD LN", "PINEFIELD RD", P23sf$FULLNAME) ## done
P23sf$FULLNAME <- gsub("OAKLAND PARK", "OAKLAND PL", P23sf$FULLNAME)  ## done
P23sf$FULLNAME <- gsub("READVILLE TROTTING PARK", "READVILLE ST", P23sf$FULLNAME) ## done & save
P23sf$FULLNAME <- gsub("^1ST ST", "E 1ST ST", P23sf$FULLNAME) ## done
# P23sf$FULLNAME <- gsub("HESTIA PK", "HESTIA PARK", P23sf$FULLNAME)
P23sf$FULLNAME <- gsub("^PARLEY VALE ST", "PARLEY VALE", P23sf$FULLNAME) ## done
P23sf$FULLNAME <- gsub("^PARLEY VALE AVE", "PARLEY VALE", P23sf$FULLNAME) ## done
P23sf$FULLNAME <- gsub("CUMMINGS HWY", "CUMMINGS ST", P23sf$FULLNAME) ## done
P23sf$FULLNAME <- gsub("MASSPORT BYPASS RD", "MASSPORT HAUL RD", P23sf$FULLNAME) ## done
P23sf$FULLNAME <- gsub("FID KENNEDY DR", "FID KENNEDY AVE", P23sf$FULLNAME) ## done
P23sf$FULLNAME <- gsub("DRAPER RD", "DRAPER ST", P23sf$FULLNAME) ## done
P23sf$FULLNAME <- gsub("GARDEN COURT CT", "GARDEN CT ST", P23sf$FULLNAME) ## done
P23sf$FULLNAME <- gsub("ALPINE PL", "ALPINE ST", P23sf$FULLNAME)## done
P23sf$FULLNAME <- gsub("HAMILTON TER", "HAMILTON PL", P23sf$FULLNAME) ## done
P23sf$FULLNAME <- gsub("NEW DORCHESTER AVE BRIDGE", "DORCHESTER AVE", P23sf$FULLNAME) ## done & save
P23sf$FULLNAME <- gsub("DEDHAM PARISH RD", "PARISH ST", P23sf$FULLNAME) ## done
P23sf$FULLNAME <- gsub("ELLERY CT", "ELLERY ST", P23sf$FULLNAME) ## done
P23sf$FULLNAME <- gsub("COLUMBUS SQ", "COLUMBUS AVE", P23sf$FULLNAME)  ## done

# P23sf$FULLNAME <- gsub("COLUMBIA CIR", "COLUMBIA RD", P23sf$FULLNAME)
P23sf$FULLNAME <- gsub("^7TH ST", "E 7TH ST", P23sf$FULLNAME) ## done
# P23sf$FULLNAME <- gsub("RUNDEL PK", "RUNDEL PARK", P23sf$FULLNAME)
P23sf$FULLNAME <- gsub("HICKORY AVE", "HICKORY LN", P23sf$FULLNAME) ## done
P23sf$FULLNAME <- gsub("WOODLAND AVE", "WOODLAND RD", P23sf$FULLNAME)  ## done
P23sf$FULLNAME <- gsub("NEW NORTHERN AVE", "NORTHERN AVE", P23sf$FULLNAME)  ## done
P23sf$FULLNAME <- gsub("HOYT PL", "HOYT ST", P23sf$FULLNAME) ## done
P23sf$FULLNAME <- gsub("DEDHAM BRANCH", "DEDHAM ST", P23sf$FULLNAME) ## done
P23sf$FULLNAME <- gsub("LAMARTINE CT", "LAMARTINE ST", P23sf$FULLNAME)## done & save

# The offending TLID in 2019 is 85701050
# View(proP22[proP22$TLID==85701050 & !is.na(proP22$TLID),])
#unique(proP22$ST_NAME[proP22$TLID==85701050 & !is.na(proP22$TLID)]) #length == 401
#View(tiger22[grepl('MC',tiger22$FULLNAME),])
# added at end of subs: 
P23sf$FULLNAME <- gsub("^MT ","MOUNT ",P23sf$FULLNAME)
# added after import of tiger22 in order to create consistency of ST/SAINT, which is tricky. All properties, though, are ST:
tiger22$FULLNAM <- toupper(as.character(tiger22$FULLNAM)) # this was needed
tiger22$FULLNAM <- gsub("^SAINT ","ST ",tiger22$FULLNAM)
# Those two fixes get us down to 382.

length(unique(P23sf$FULLNAME[!P23sf$FULLNAME %in% tiger22$FULLNAM])) 
#211 in Sep, 23
#217???? in August, 2023
#4060 in March,2023
#4087 in 2020 
#4056 in 2021 
#4061 in 2022 

sum(is.na(P23sf$FULLNAME))
#[1] 0
length(which(tiger22$FULLNAM ==  "0"))
#4658
#eliminate NAs as "0"s
tiger22 <- tiger22[which(!tiger22$FULLNAM ==  "0"),]
# 1.1. 

#correct the geometry variables
P23sf$Y <- st_coordinates(st_transform(P23sf, 4326))[,1]
P23sf$X <- st_coordinates(st_transform(P23sf, 4326))[,2]
P23sf <- st_drop_geometry(P23sf)
P23sf <- st_as_sf(P23sf, coords = c("X", "Y"), crs = 4326)

tiger22 <- st_as_sf(tiger22)
tiger22 <- rename(tiger22, "X.id" = "X")
tiger_coords <- st_coordinates(st_transform(tiger22, 4326))
tiger_coords[, c("X", "Y")] <- tiger_coords[, c("Y", "X")] # Swap X and Y
tiger_coords <- as.data.frame(tiger_coords)
tiger_coords <- tiger_coords[!duplicated(tiger_coords$L1),]
tiger22 <- st_drop_geometry(tiger_fix)
tiger22$X <- tiger_coords$X
tiger22$Y <- tiger_coords$Y
tiger22 <- st_as_sf(tiger22, coords = c("X", "Y"), crs = 4326)



# Take new observations from P23sf and put them into the for loop
## get the last year parcel data
LandParcels_2022 <- read.csv("~/Desktop/BARI/GI-2023/new datasets/LandParcels.2022/LandParcels.2022.csv")
#take new observations by LAND_PARCEL_ID
P23sf_new <- P23sf[which(P23sf$GIS_ID %in% setdiff(P23sf$GIS_ID, LandParcels_2022$Land_Parcel_ID)),]
#P23sf_old$TLID <- NA 
#794 new observations
length(setdiff(LandParcels_2022$Land_Parcel_ID, P23sf$GIS_ID))
#305 retired observations
nrow(P23sf) - nrow(LandParcels_2022)
#489 overall difference 
nrow(P23sf) - nrow(LandParcels_2022) + length(setdiff(LandParcels_2022$Land_Parcel_ID, P23sf$GIS_ID))
#794 actual new observations


# If CRS do not match, transform the CRS of tiger22 to match P23sf_new
tiger22 <- st_as_sf(tiger22, wkt = "geometry", crs = st_crs(P23sf_new))
tiger22 <- st_transform(tiger22, crs = st_crs(P23sf_new))
#### Match parcels which have same FULLNAME as the street file####

## running all parcels through matching:
## Note: takes ~ 25 minutes to run this loop ~55min on 2020 data, tablet Alina
start1 <- Sys.time()
pb <- progress_estimated(nrow(P23sf_new),0)

P23sf_new$TLID <- NA

for(i in 1:nrow(P23sf_new)){
  # i = 12
  matching_streets <- tiger22 %>%
    filter(FULLNAM %in% P23sf_new$FULLNAME[i])
  if(nrow(matching_streets)>0){
    P23sf_new$TLID[i] <- as.character(matching_streets$TLID[which.min(st_distance(P23sf_new[i,"geometry"], matching_streets))])
  }
  pb$pause(0.1)$tick()$print() # update progress bar
}

end1 <- Sys.time()
end1-start1 
#Time difference of 1.664658 mins Sep21 23
#Time difference of 1.54573 mins in Sep, 23
#Time difference of 1.920024 mins in Sep 15, 2023 
#Time difference of 3.315381 hours


sum(is.na(P23sf_new$TLID))/nrow(P23sf_new) #0.01259446 for new 0.0009732459 for all #0.04030227 for new obs. 0.001196281 for all in Sep 23 #0.04015056 in Aug 23 #  0.01430806 in Feb 19th,2023  
# 0.01690084 in 2020 #0.0173456 in 2021 # 0.01544526 in 2022
table(P23sf_new$FULLNAM[which(is.na(P23sf_new$TLID))]) %>% sort()

#join TLID for old ones
P23sf_old <- P23sf[which(P23sf$GIS_ID %in% setdiff(P23sf$GIS_ID, P23sf_new$GIS_ID)),]
#or 
# P23sf_old <- P23sf[which(!P23sf$GIS_ID %in% setdiff(P23sf$GIS_ID, LandParcels_2022$Land_Parcel_ID)),]
prop22 <- read.csv("~/Desktop/BARI/GI-2023/new datasets/Last Year/Properties.2022.csv")
prop22 <- prop22 |> dplyr::select(GIS_ID, TLID)
prop22u <- distinct(prop22)
P23sf_old <- left_join(P23sf_old, prop22u, join_by(GIS_ID))
P23sf_old <- P23sf_old[,colnames(P23sf_new)]


#TG_join <- tiger22 |> select(FULLNAM, TLID) |> rename(FULLNAME = FULLNAM)
#P23sf_tgjoin <- P23sf[which(P23sf$GIS_ID %in% setdiff(P23sf$GIS_ID, P23sf_new$GIS_ID)),]
#P23sf_tgjoin <- left_join(P23sf_tgjoin, TG_join, join_by(FULLNAME))

#merge the new TLIDs back to P23sf
P23sf_wTLID <- rbind(P23sf_new, P23sf_old)
#P23sf <- P23sf_wTLID



#### Add blocks groups, blocks ####


length(unique(P23sf_wTLID$FULLNAME[which(is.na(P23sf_wTLID$TLID) & grepl("ISLAND",P23sf_wTLID$FULLNAME)==F)])) 
#45 in Sep 23
#65 in Sep 23
#68 in Aug 23
#206 in Feb 2023
#198 in 2019 data #221 in 2020 #203 in 2021 # 159 in 2022
#nrow(unique(P23sf[which(is.na(P23sf$TLID) & grepl("ISLAND",P23sf$FULLNAME)==F),])) # 404 # error in 2019 data #error in 2020

# 2021 is the latest it seems. Toshi comment on Feb 19th, 2023
#2022 is avaliable on August 2023 - YET
blocks <- tigris::blocks(state = "MA",county = "Suffolk",year = 2022)
blocks <- st_transform(blocks,crs = st_crs(P23sf_wTLID))
blocks$Blk_ID_20 <- blocks$GEOID20
blocks$CT_ID_20 <- str_sub(blocks$GEOID20,end = 11)
blocks <- blocks %>% dplyr::select(Blk_ID_20,CT_ID_20)

bgs <- tigris::block_groups(state = "MA",county = "Suffolk",year = 2022)
bgs <- st_transform(bgs,crs = st_crs(P23sf_wTLID))
bgs$BG_ID_20 <- bgs$GEOID
bgs <- bgs %>% dplyr::select(BG_ID_20)

# 2019 block to get BG_ID_10

blocks_10 <- tigris::blocks(state = "MA",county = "Suffolk",year = 2019)
blocks_10 <- st_transform(blocks_10,crs = st_crs(P23sf_wTLID))
blocks_10$Blk_ID_10 <- blocks_10$GEOID10
blocks_10$CT_ID_10 <- str_sub(blocks_10$GEOID10,end = 11)
blocks_10$BG_ID_10 <- str_sub(blocks_10$GEOID10,end = 12)
blocks_10 <- blocks_10 %>% dplyr::select(CT_ID_10, BG_ID_10, Blk_ID_10)


#this was needed to join block data
P23sf_wTLID$X <- st_coordinates(P23sf_wTLID)[,2]
P23sf_wTLID$Y <- st_coordinates(P23sf_wTLID)[,1]
P23sf_wTLID <- st_drop_geometry(P23sf_wTLID)
P23sf_wTLID <- st_as_sf(P23sf_wTLID, coords = c("X", "Y"), crs = 4326)


P23sf_geo <- st_join(P23sf_wTLID,blocks_10,join=st_within,left=T)
P23sf_geo <- st_join(P23sf_geo,blocks,join=st_within,left=T)
P23sf_geo <- st_join(P23sf_geo,bgs,join=st_within,left=T)

sum(is.na(P23sf_geo$Blk_ID_20))/nrow(P23sf_geo)  #3.041393e-05 in Sep23 #0.000191989 in Aug 23 #0.0002225437 in March 2023#0.0002425958 in 2020
sum(is.na(P23sf_geo$BG_ID_20))/nrow(P23sf_geo) #3.041393e-05 in Sep23  #0.000191989 in Aug 23 #0.0002225437 in March 2023
sum(is.na(P23sf_geo$CT_ID_20))/nrow(P23sf_geo) #3.041393e-05 in Sep23  #0.000191989 in Aug 23 #0.0002225437 in March 2023 #0.00024259 in 2020

#P23sf_geo <- st_transform(P23sf_geo,4326)
P23sf_geo$X <- st_coordinates(P23sf_geo)[,2]
P23sf_geo$Y <- st_coordinates(P23sf_geo)[,1]
P23sf_geo <- st_drop_geometry(P23sf_geo)
P23sf_geo <- st_as_sf(P23sf_geo, coords = c("X", "Y"), crs = 4326)

#P23sf_geo <- P23sf_geo %>% dplyr::select(-c("...1"))
P23sf_geo$X <- st_coordinates(P23sf_geo)[,1]
P23sf_geo$Y <- st_coordinates(P23sf_geo)[,2]

# save parcels updated
write_csv(as.data.frame(st_set_geometry(P23sf_geo,NULL)),"~/Desktop/BARI/GI-2023/Outputs_Feb24/parcels_updatedTLID_2023.csv")



#load in the parcels updated file

P23sf_geo <- read.csv("~/Desktop/BARI/GI-2023/new-2/parcels_updatedTLID_083023_YET.csv")



# ##drop the .y for block, bg, ct id's and rename the original
# P23sf_geo <- P23sf_geo %>% dplyr::select(-c("Blk_ID_10", "BG_ID_10", "CT_ID_10") )


#### WHY WE ARE DOING THIS?????? ######
identical(P23sf$GIS_ID,P23sf_geo$GIS_ID)
identical((P23sf$GIS_ID |> sort()), (P23sf_geo$GIS_ID |> sort()))
#TRUE

#P23sf_geo <- read.csv("../Data/parcels_updatedTLID_190410.csv",stringsAsFactors = F)
P23sf$TLID <- P23sf_geo$TLID[match(P23sf$MAP_PAR_ID,P23sf_geo$MAP_PAR_ID)]
P23sf$Blk_ID_20 <- P23sf_geo$Blk_ID_20[match(P23sf$MAP_PAR_ID,P23sf_geo$MAP_PAR_ID)]
P23sf$BG_ID_20 <- P23sf_geo$BG_ID_20[match(P23sf$MAP_PAR_ID,P23sf_geo$MAP_PAR_ID)]
P23sf$CT_ID_20 <- P23sf_geo$CT_ID_20[match(P23sf$MAP_PAR_ID,P23sf_geo$MAP_PAR_ID)]
P23sf_geo <- P23sf

# save the workspace
#save(P23sf_geo,file = "C:\\Users\\alina.ristea\\Google Drive\\Work\\NEU_BARI\\Projects\\Geographical_Infrastructure\\Data\\results\\parcels_updatedTLID_sf_09262019.RData")
#load(file = "../Data/Geographical Infrastructure 2018/Data/parcels_updatedTLID_sf.RData")

# read hte P23sf if it is not read previously (it is created in the upper rows)
#P23sf <- read.csv("C:\\Users\\alina.ristea\\Google Drive\\Work\\NEU_BARI\\Projects\\Geographical_Infrastructure\\Boston Geographical Infrastructure 2019\\parcels_updatedTLID_10102019.csv")

#NEW
#P23sf <- read.csv("C:\\Users\\bariuser2\\Google Drive\\BARI Research Team Data Library\\Geographical Infrastructure\\Boston Geographical Infrastructure 2020\\Outputs\\parcels_updatedTLID_04292020.csv")

P23 <- st_as_sf(P23sf_geo, coords = c("X", "Y"), crs = 26986)
sum(is.na(P23$TLID)) #434 in Aug 23, #1149 in March 2023. 
#1528 in 2019 data #1672 in 2020 #1716 in 2021 #1391 in 2022


#### DO NOT RUN: old loop from the code trying to connect streets and parcels ####

# %>%
#  head(n = 20)
# TLID_NA <- st_join(TLID_NA, P23[,"ZIPCODE"],join=st_within,left=T)
# TLID_NA <- P23[(is.na(P23$TLID)),]
# # TLID_NA <- head(TLID_NA, 3)
# # P23 <- st_as_sf(P23sf, coords = c("X", "Y"), crs = 26986)
# TLID_NA <- st_transform(TLID_NA, 26986)
# tiger22$zip <- ifelse(is.na(tiger22$ZIPL), as.character(tiger22$ZIPR), as.character(tiger22$ZIPL))
# tiger22$zip <- as.numeric(tiger22$zip)

#pb <- progress_estimated(nrow(TLID_NA),0)

#TLID_NA$TLID <- NA
#TLID_NA$dist <- NA

## P23_sub <- P23[(is.na(P23$TLID)),]
## 
## nonmatching <- P23_sub %>%
##   filter(!(FULLNAME %in% tiger22$FULLNAME))

## nonmatching_streets <- tiger22 %>%
## filter(zip %in% P23_sub$ZIPCODE)

## TLID_NA <- head(TLID_NA, 3)

# for (i in 1:nrow(TLID_NA)) {
#   # nonmatching_streets <- tiger22 %>%
#   #           filter(zip %in% TLID_NA$ZIPCODE[i])
#   # if(nrow(nonmatching_streets)>0) {
#   temp <-st_distance(TLID_NA[i,"geometry"], tiger22, by_element = TRUE)
#   TLID_NA$TLID[i] <- as.character(tiger22$TLID[which.min(temp)])
#   TLID_NA$dist[i] <- min(temp)
#   # }
#   # pb$tick()$print()
# }


#### TO RUN: find streets close to parcels by applying a buffer over the parcels ####

# create a df for parcels which do not have TLID associated
TLID_NA <- P23shp_centroid%>%
  filter(MAP_PAR_ID %in% P23$MAP_PAR_ID[is.na(P23$TLID)]) # this will have 9 columns
# is not this 19 columns?

# try to use only the TLID column from tiger22 to work faster
tiger22_short <- tiger22[,c("TLID")]

# function creating buffers around each point and trying to find the shortest distance between what intersects each buffer
points_to_near_poly <- function(points_transf, poly_transf){
  # transform points and polygons
  points <- st_as_sf(points_transf)
  polygons <- st_as_sf(poly_transf)
  # buffer around points
  points_buffer <- st_buffer(points, dist = 100)
  # create new variable for the joined info points and poly
  mixed_var <- data.frame(matrix(ncol = dim(points)[2]+dim(polygons)[2]-1, nrow = dim(points)[1]))
  for (i in 1:dim(points)[1]){
    intersect_buffer_poly <- st_intersection(polygons, points_buffer[i,])
    shortdist <- set_units(10000000000,cm)
    if (dim(intersect_buffer_poly)[1] != 0){
      for (j in 1:dim(intersect_buffer_poly)[1]){
        dst <- st_distance(points[i,], intersect_buffer_poly[j,])#,byid=TRUE)
        if (dst < shortdist){
          shortdist <- dst
          mixed_var[i,] <- intersect_buffer_poly[j,]
        }
      }
    } else {
      # change here according to the dataframe size
      mixed_var[i,2:19] <- points[i,]
    }
  }
  return(mixed_var)
}

# run the buffer analysis
start1 <- Sys.time()
TLID_NA <- points_to_near_poly(TLID_NA, tiger22_short) # Time difference of 4.991917 mins #10.53832 min on tablet 2020
# Time difference of 49.39196 secs in 2023
# 1418 ?
# 43 TLIDs NA for 100m
end1 <- Sys.time()
end1-start1


sum(is.na(TLID_NA$X1))
#64 in Aug 23 # 45 in Feb 2023
#65 in 2020 for 100m #48 in 2021 #66 TLIDs NA for 100m in 2022

# the function is losing the headers; add names to header (TLID from tiger22 + names from P22sh)
names = c(colnames(P23shp_centroid))
colnames(TLID_NA) = c("TLID", names)

# create copy variable TLID_new to be used further


# we use character for geometry column, if not we cannot save it well as csv
TLID_NA$geometry <- as.character(TLID_NA$geometry)

# deleting empty, unnecessary rows
TLID_NA_2 <- TLID_NA[,-c(8,10)]
TLID_new <- TLID_NA_2
# debugging the error to write.csv

typeof(TLID_NA_2$TLID)
typeof(TLID_NA_2$MAP_PAR_ID)
typeof(TLID_NA_2$Ln_P_ID) #no column like that 
typeof(TLID_NA_2$geometry)

# code for this error Error in utils::write.table(TLID_NA_2, "TLIDs_20230329.TY.csv", row.names = FALSE,  : 
# unimplemented type 'list' in 'EncodeElement'
TLID_NA_2$geometry <- vapply(TLID_NA_2$geometry, paste, collapse = ", ", character(1L))


# geometry is the suspect, we cannot have a list in write.csv
# save as csv 
write.csv(TLID_NA_2, "~/Desktop/BARI/GI-2023/outputs/TLIDs_091023_YET.csv", row.names = FALSE)


#### Join data: parcels with TLID, parcels run in the buffer loop and resulted with or without TLIDs ####

dt <- left_join(as.data.frame(P23), as.data.frame(TLID_new[,c("TLID", "MAP_PAR_ID")]), by = "MAP_PAR_ID", 
                relationship =  "many-to-many")
#@AR: the left join created two different columns tlid.x and tlid.y; we need to join them removing the NA
dt$TLID[!is.na(dt$TLID.x)] = dt$TLID.x[!is.na(dt$TLID.x)]  
dt$TLID[!is.na(dt$TLID.y)] = dt$TLID.y[!is.na(dt$TLID.y)]  
# remove unnaccesary columns
drops <- c("TLID.y", "TLID.x")
dt <- dt[ , !(names(dt) %in% drops)]

# save a csv including the TLID NAs and non NAs (i.e. parcels connected to streets or not)
#names(dt)[95] <- "TLID"
#names(dt)[94] <- "geometry"

#in 2020
#names(dt)[95] <- "TLID"
#names(dt)[89] <- "geometry"

dt <- st_as_sf(dt)
#dt <- st_transform(dt,4326)
dt$X <- st_coordinates(dt)[,1]
dt$Y <- st_coordinates(dt)[,2]

#dt <- dt %>% dplyr::select(-c("...1") )

# save as csv
write.csv(as.data.frame(st_set_geometry(dt,NULL)),"~/Desktop/BARI/GI-2023/outputs/parcels_fullupdatedTLID_091023_YET.csv", row.names = FALSE)



## ERRORS WHILE TRYING TO WRITE AS SHP, finally it did not get saved as shp <-- we do not need shp for script 03 ##

## Error in CPL_write_ogr(obj, dsn, layer, driver, as.character(dataset_options),  : 
#Write error


# save as shp
st_write(dt, "parcels_fullupdatedTLID_August23_YET.gpkg")

# writeOGR(dt, "parcels_fullupdatedTLID_02202023_TY.shp", driver="ESRI Shapefile")

st_write(LandParcels_2023_sf, "LandParcels_2023.shp", driver="ESRI Shapefile", overwrite_layer=TRUE)



