#Description- From Parcels to Streets
#- This script is assigning street information (from tiger data) to parcels.
#- In this script, we are changing PAD data and tiger csv with the road from last year.

# 0. Preliminary Stuff

# List of required packages
packages <- c("sp", "sf", "plyr", "raster", "rgdal", 'rgeos',  "lubridate", "dplyr", "dbplyr",  "here","tidyverse","lwgeom", "tigris", "nngeo", "units")

# Install and load packages
lapply(packages, function(pkg) {
  if (!require(pkg, character.only = TRUE)) install.packages(pkg, dependencies = TRUE)
  library(pkg, character.only = TRUE, quietly = TRUE)
})

# Set global options
options(scipen = 15, digits = 10)
options(tigris_class = "sf",tigris_use_cache = T)

# Define custom negation operator
`%notin%` <- Negate(`%in%`)

# Define file paths using 'here' for consistency and flexibility
input_dir <- "~/BARI/911/Inputs"
parcels24_shp_path <- file.path(input_dir, "Parcels_(2024).shp")


#Read BARI data}
# Parcel Data - from the city of Boston 
parcels24_shp <- st_read("~/Desktop/Academic/BARI/Scripts/GI/Inputs/Parcels_(2024)/Parcels_(2024).shp")
## get the last year parcel data
LandParcels_2023 <- read.csv("~/Desktop/Academic/BARI/Scripts/GI/Inputs/LandParcels.2023.csv")

# PAD unit from Property assessment data for # this PAD script is run by Saina
propassraw <- read.csv("~/Desktop/Academic/BARI/Scripts/GI/Inputs/PAD.Record.wUnit.csv")

# read street data #we are using the csv created for the GI2019 release
tiger23 <- read.csv("~/Desktop/Academic/BARI/Scripts/GI/Inputs/Roads.2023.csv")

# read shp street data and connect geometry with csv (((UPDATE YEARS HERE< TAKE them from drive)))
tiger23_shp <- st_read("~/Desktop/Academic/BARI/Scripts/GI/Inputs/Roads.2023/Roads.2023.shp")
tiger23$geometry <- tiger23_shp$geometry[match(tiger23$TLID, tiger23_shp$TLID)]

# 1. Data Manipulation


# Cleaning up the properties for centroids
propass24 <- propassraw[!duplicated(propassraw$GIS_ID),] #98531 in 2024 #98639 in Aug 2023 #98873, as of 2022
#98873, # 98907 in 2019 #99777 in 2020, #98943 in 2021

#remove NAs
parcels24_shp <- parcels24_shp[!is.na(as.numeric(as.character(parcels24_shp$MAP_PAR_ID))),]

# Finding the centroid and merging it back unto the parcel shapefile
P24shp_centroid <- st_centroid(parcels24_shp %>%  st_transform(26986))

# Merging the property data with the parcels shapefile and 
# converting them to spatial objects (needed to be re-projected)
P24shp_centroid$GIS_ID <- as.numeric(as.character(P24shp_centroid$MAP_PAR_ID))
#????? no column like GIS_ID - create a new column and rename it as GIS_ID -- changed it to MAP_PAR_ID

#?propass24$GIS_ID <- as.numeric(propass24$GIS_ID) #they are identical
P24sf <- left_join(P24shp_centroid, propass24, by=c("GIS_ID"="GIS_ID"))
a <- left_join(propass24, P24shp_centroid, by=c("GIS_ID"="GIS_ID"))
# a is # 98644, 82 variable in Aug 23
# a is # 98908, 82 Variable in March 2023

#store NAs
P24sf_NAs <- P24sf[is.na(P24sf$ST_NAME),]
#eliminate NAs
P24sf <- P24sf[!is.na(P24sf$ST_NAME),]
#remove duplicates
P24sf <- P24sf[!duplicated(P24sf$PID),]
#98639 after cleaning in Sep 23 #98529 in 2024

#### Street analysis ####

## we remove every suf part because it does not exist in pad 2021 ##

#P24sf$ST_NAME_SUF <- gsub("XT","EXD",P24sf$ST_NAME_SUF)
#P24sf$ST_NAME_SUF2 <- P24sf$ST_NAME_SUF
#P24sf$ST_NAME_SUF2[is.na(P24sf$ST_NAME_SUF2)] <- ""
#P24sf$FULLNAME <- str_c(P24sf$ST_NAME, P24sf$ST_NAME_SUF2, sep = " ") #no ST_NAME_SUF2 column anymore in 2023
P24sf$FULLNAME <- P24sf$ST_NAME
#P24sf$FULLNAME <- str_trim(P24sf$FULLNAME) # trim whitespace
P24sf$ST_NAME <- toupper(P24sf$ST_NAME)
P24sf$FULLNAME <- P24sf$ST_NAME

P24sf$FULLNAME <- recode(P24sf$FULLNAME, "WM T MORRISSEY BL" = "WILLIAM T MORRISSEY BLVD" )
P24sf$FULLNAME <- recode(P24sf$FULLNAME, "JAMES ONEILL ST" = "JAMES O'NEIL ST" )
P24sf$FULLNAME <- recode(P24sf$FULLNAME, "DRYDOCK AV" = "DRY DOCK AV" )
P24sf$FULLNAME <- recode(P24sf$FULLNAME, "GEN JOZEF PILSUDSKI WAY" = "PILSUDSKI WAY")
P24sf$FULLNAME <- recode(P24sf$FULLNAME, "REV R M COSTELLO PL" = "REV ROBERT M COSTELLO PL")
P24sf$FULLNAME <- recode(P24sf$FULLNAME, "MSGR REYNOLDS WAY" = "MONSIGNOR REYNOLDS WAY")
P24sf$FULLNAME <- recode(P24sf$FULLNAME, "WOODDALE AV" = "WOODALE AV")
P24sf$FULLNAME <- recode(P24sf$FULLNAME, "SURREYHILL LA" = "SURREY HILL LA" )
P24sf$FULLNAME <- recode(P24sf$FULLNAME, "RINGGOLD ST" = "RINGOLD ST" )
P24sf$FULLNAME <- recode(P24sf$FULLNAME, "O DONNELL TE" = "ODONNELL TE" )
P24sf$FULLNAME <- recode(P24sf$FULLNAME, "GEN WM H DEVINE WY" = "DEVINE WY" )
P24sf$FULLNAME <- recode(P24sf$FULLNAME, "HAZLETON ST" = "HAZELTON ST")
P24sf$FULLNAME <- recode(P24sf$FULLNAME, "MORRISON ST" = "SELWYN ST") # per BostonMap
P24sf$FULLNAME <- recode(P24sf$FULLNAME, "MALCOLM X BL" = "NEW DUDLEY ST") # per BostonMap
P24sf$FULLNAME <- recode(P24sf$FULLNAME, "LANE PK" = "LN PARK") # per BostonMap
P24sf$FULLNAME <- recode(P24sf$FULLNAME, "ALWIN ST" = "ALWYN ST") # per BostonMap
P24sf$FULLNAME <- recode(P24sf$FULLNAME, "SILVIA CT" = "SYLVIA CT") # per BostonMap
P24sf$FULLNAME <- recode(P24sf$FULLNAME, "JESHURUN RD" = "JESHURUN ST") # per BostonMap
P24sf$FULLNAME <- recode(P24sf$FULLNAME, "GARDEN COURT ST" = "GARDEN CT") # per BostonMap
P24sf$FULLNAME <- recode(P24sf$FULLNAME, "WAYMOUNT ST" = "PARKER HILL AV") # per BostonMap
P24sf$FULLNAME <- recode(P24sf$FULLNAME, "POND VIEW AV" = "PONDVIEW AV") # per BostonMap
P24sf$FULLNAME <- recode(P24sf$FULLNAME, "ABBY RD" = "ABBEY RD") # per BostonMap
P24sf$FULLNAME <- recode(P24sf$FULLNAME, "ALTACREST RD" = "ALTA CREST RD") # per BostonMap
P24sf$FULLNAME <- recode(P24sf$FULLNAME, "TERAGRAM ST" = "TEREGRAM ST") # per BostonMap
P24sf$FULLNAME <- recode(P24sf$FULLNAME, "JOANNE TE" = "JO ANNE TE") # per BostonMap
P24sf$FULLNAME <- recode(P24sf$FULLNAME, "DELANY CI" = "DELANEY CT") # per BostonMap
P24sf$FULLNAME <- recode(P24sf$FULLNAME, "BONELL TE" = "BONNEL TE") # per BostonMap
P24sf$FULLNAME <- recode(P24sf$FULLNAME, "AVENUE LOUIS PASTEUR" = "AVE LOUIS PASTEUR") # per BostonMap
P24sf$FULLNAME <- recode(P24sf$FULLNAME, "ST A" = "A ST") # per BostonMap
P24sf$FULLNAME <- recode(P24sf$FULLNAME, "ANTHONY R VALENTI WY" = "TRAVERSE ST") # per BostonMap TLID matching
P24sf$FULLNAME <- recode(P24sf$FULLNAME, "ADRIAN ST" = "ADRAIN ST") # per BostonMap
P24sf$FULLNAME <- recode(P24sf$FULLNAME, "UFFORD ST" = "DYER ST") # per BostonMap, this is closest
P24sf$FULLNAME <- recode(P24sf$FULLNAME, "ST PAULS AV" = "ST PAUL'S AV") # per BostonMap
P24sf$FULLNAME <- recode(P24sf$FULLNAME, "ORGAN PARK ST" = "ORGAN PARK") # per BostonMap
P24sf$FULLNAME <- recode(P24sf$FULLNAME, "O DONNELL SQ" = "ODONNELL SQ") # per BostonMap
P24sf$FULLNAME <- recode(P24sf$FULLNAME, "WM CARD OCONNELL WY" = "WILLIAM CARDINAL O'CONNELL WAY") # per BostonMap
P24sf$FULLNAME <- recode(P24sf$FULLNAME, "MARYANNA RD" = "MARY ANNA RD") # per BostonMap
P24sf$FULLNAME <- recode(P24sf$FULLNAME, "LEFEVRE ST" = "LE FEVRE ST") # per BostonMap
P24sf$FULLNAME <- recode(P24sf$FULLNAME, "CENTER PZ" = "CAMBRIDGE ST") # per BostonMap
P24sf$FULLNAME <- recode(P24sf$FULLNAME, "YEOMAN PL" = "YEOMAN ST") # per google
P24sf$FULLNAME <- recode(P24sf$FULLNAME, "THEODORE A GLYNN WY" = "THEODORE GLYNN WY") # per bostonmap
P24sf$FULLNAME <- recode(P24sf$FULLNAME, "RIVERBANK PL" = "RIVERBANK PL PVT WAY") # per bostonmap
P24sf$FULLNAME <- recode(P24sf$FULLNAME, "REV RICHARD A BURKE" = "REV RICHARD A BURKE ST") # per bostonmap
P24sf$FULLNAME <- recode(P24sf$FULLNAME, "JEANNE DR" = "BEECHWOOD ST") # per bostonmap
P24sf$FULLNAME <- recode(P24sf$FULLNAME, "GEN JOZEF PILSUDSKI WY" = "PILSUDSKI WY") # per bostonmap
P24sf$FULLNAME <- recode(P24sf$FULLNAME, "FIDELIS WA" = "FIDELIS WAY") # per BostonMap
P24sf$FULLNAME <- recode(P24sf$FULLNAME, "DR MARY M BEATTY CI" = "MARY MOORE BEATTY CIR") # per BostonMap
P24sf$FULLNAME <- recode(P24sf$FULLNAME, "CORNAUBA EXD" = "CORNAUBA ST EXD") # per BostonMap
P24sf$FULLNAME <- recode(P24sf$FULLNAME, "AVENUE DE LAFAYETTE" = "AVE DE LAFAYETTE") # per BostonMap
P24sf$FULLNAME <- recode(P24sf$FULLNAME, "WILLIAM E DOUCETTE SQ" = "WM E DOUCETTE SQ") # per BostonMap
P24sf$FULLNAME <- gsub(" AV$"," AVE",P24sf$FULLNAME)
P24sf$FULLNAME <- gsub(" AV "," AVE ",P24sf$FULLNAME)
P24sf$FULLNAME <- gsub(" HW$"," HWY",P24sf$FULLNAME)
P24sf$FULLNAME <- gsub(" WY$"," WAY",P24sf$FULLNAME)
P24sf$FULLNAME <- gsub(" BL$"," BLVD",P24sf$FULLNAME)
P24sf$FULLNAME <- gsub(" CI$"," CIR",P24sf$FULLNAME)
P24sf$FULLNAME <- gsub(" CR$"," CRES",P24sf$FULLNAME)
P24sf$FULLNAME <- gsub(" TE$"," TER",P24sf$FULLNAME)
P24sf$FULLNAME <- gsub(" PZ$"," PLZ",P24sf$FULLNAME)
P24sf$FULLNAME <- gsub(" PA$"," PATH",P24sf$FULLNAME)
P24sf$FULLNAME <- gsub(" WH$"," WHARF",P24sf$FULLNAME)
P24sf$FULLNAME <- gsub(" LA$"," LN",P24sf$FULLNAME)
P24sf$FULLNAME <- gsub(" RO$"," ROW",P24sf$FULLNAME)
P24sf$FULLNAME <- gsub("FIRST","1ST",P24sf$FULLNAME) # making changes to parcel address names to match census:
P24sf$FULLNAME <- gsub("SECOND","2ND",P24sf$FULLNAME) 
P24sf$FULLNAME <- gsub("THIRD","3RD",P24sf$FULLNAME)
P24sf$FULLNAME <- gsub("FOURTH","4TH",P24sf$FULLNAME) 
P24sf$FULLNAME <- gsub("FIFTH","5TH",P24sf$FULLNAME) 
P24sf$FULLNAME <- gsub("SIXTH","6TH",P24sf$FULLNAME) 
P24sf$FULLNAME <- gsub("SEVENTH","7TH",P24sf$FULLNAME)
P24sf$FULLNAME <- gsub("EIGHTH","8TH",P24sf$FULLNAME) 
P24sf$FULLNAME <- gsub("NINTH","9TH",P24sf$FULLNAME)
# P24sf$FULLNAME <- gsub("TENTH","10TH",P24sf$FULLNAME)

P24sf$FULLNAME <- str_trim(P24sf$FULLNAME)
P24sf$FULLNAME <- gsub(' +',' ', P24sf$FULLNAME)

# Saina's portion
P24sf$FULLNAME <- str_replace(P24sf$FULLNAME, "PW$", "PKWY")
P24sf$FULLNAME <- str_replace(P24sf$FULLNAME, "PK$", "PARK")
P24sf$FULLNAME <- str_replace(P24sf$FULLNAME, "CC$", "CIRCUIT")
# P24sf$FULLNAME <- str_replace(P24sf$FULLNAME, "CT$", "CIRCUIT")
P24sf$FULLNAME <- str_replace(P24sf$FULLNAME, "WM C KELLY SQ", "CENTRAL SQ")
P24sf$FULLNAME <- str_replace(P24sf$FULLNAME, "DELOS ST", "Grove Terrace")
P24sf$FULLNAME <- str_replace(P24sf$FULLNAME, "SEA VIEW AVE", "SEAVIEW AVE")
P24sf$FULLNAME <- str_replace(P24sf$FULLNAME, "MSGR P J LYDON WAY", "MSGR PATRICK J LYDON WAY")
P24sf$FULLNAME <- gsub("^MT ","MOUNT ",P24sf$FULLNAME)

# P24sf$FULLNAME <- str_replace(P24sf$FULLNAME, "MOUNT", "MT")
P24sf$FULLNAME <- str_replace(P24sf$FULLNAME, "DRAYTON ST", "DRAYTON AVE")
P24sf$FULLNAME <- str_replace(P24sf$FULLNAME, "ARBORWAY ST", "ARBORWAY")
P24sf$FULLNAME <- str_replace(P24sf$FULLNAME, "CASTLE ROCK ST", "CASTLEROCK ST")
P24sf$FULLNAME <- str_replace(P24sf$FULLNAME, "NAVARRE PL", "NAVARRE ST")
P24sf$FULLNAME <- str_replace(P24sf$FULLNAME, "WM F MCCLELLAN HWY", "WILLIAM F MCCLELLAN HWY")
P24sf$FULLNAME <- str_replace(P24sf$FULLNAME, "ELM LAWN", "ELM LAWN ST")
P24sf$FULLNAME <- str_replace(P24sf$FULLNAME, "GROTTO GLEN AVE", "GROTTO GLEN RD")
P24sf$FULLNAME <- str_replace(P24sf$FULLNAME, "ROSEBERY", "ROSEBERRY")
P24sf$FULLNAME <- str_replace(P24sf$FULLNAME, "JAMAICAWAY ST", "JAMAICAWAY")
P24sf$FULLNAME <- str_replace(P24sf$FULLNAME, "MARTIN LUTHER KING JR BLVD", "MARTIN LUTHER KING BLVD")
P24sf$FULLNAME <- str_replace(P24sf$FULLNAME, "ROCK AVE", "ROCK AVE PRIVATE WAY")
P24sf$FULLNAME <- str_replace(P24sf$FULLNAME, "CHARLESGATE EAST", "CHARLESGATE E")
P24sf$FULLNAME <- str_replace(P24sf$FULLNAME, "KEYES STREET PL", "KEYES PL")
P24sf$FULLNAME <- str_replace(P24sf$FULLNAME, "PIERSON ST", "PEIRSON ST")
P24sf$FULLNAME <- str_replace(P24sf$FULLNAME, "FENWAY ST", "FENWAY")
P24sf$FULLNAME <- str_replace(P24sf$FULLNAME, "TRUMAN PW", "TRUMAN HWY")
P24sf$FULLNAME <- str_replace(P24sf$FULLNAME, "TALBOT PL", "TALBOT AVE")
P24sf$FULLNAME <- str_replace(P24sf$FULLNAME, "ORCHARD AVE", "ORCHARDHILL RD")
## round two
P24sf$FULLNAME <- str_replace(P24sf$FULLNAME, "NEWBURY ST", "NEWBURY")
P24sf$FULLNAME <- str_replace(P24sf$FULLNAME, "NEWBURY  ST", "NEWBURY")
tiger23$FULLNAM <- str_replace(tiger23$FULLNAM, "NEWBURY ST", "NEWBURY")
P24sf[90105,]$FULLNAME
# 98779 what is this number and code 

# Emma's portion
# P24sf$FULLNAME  <- gsub("CHARLES STREET SOUTH","CHARLES ST S", P24sf$FULLNAME)
P24sf$FULLNAME <- str_replace(P24sf$FULLNAME, "CHARLES STREET SOUTH", "CHARLES ST S")
P24sf$FULLNAME <- str_replace(P24sf$FULLNAME, "OAK STREET WEST","OAK ST W")
tiger23[13579,]$FULLNAM
P24sf[40364,]$FULLNAME

P24sf$FULLNAME <- str_replace(P24sf$FULLNAME, "SIXTEENTH ST","16TH ST")
P24sf$FULLNAME <- str_replace(P24sf$FULLNAME, "TE$"," TER")
P24sf$FULLNAME <- str_replace(P24sf$FULLNAME, "FR FRANCIS GILDAY ST","FATHER FRANCIS GILDAY ST")
# P24sf$FULLNAME <- str_replace(P24sf$FULLNAME, "CEMETERY LN","CEMETERY LA")
# P24sf$FULLNAME <- str_replace(P24sf$FULLNAME, "CHISHOLM LN","CHISHOLM LA")
P24sf$FULLNAME <- str_replace(P24sf$FULLNAME, " WEST"," W")
P24sf$FULLNAME <- str_replace(P24sf$FULLNAME, "MSGR ","MONSIGNOR ")

# P24sf$FULLNAME  <- gsub(" PK"," PARK", P24sf$FULLNAME)
# P24sf$FULLNAME  <- gsub(" WY"," WAY", P24sf$FULLNAME)
# P24sf$FULLNAME  <- gsub(" CI"," CIR", P24sf$FULLNAME)
# P24sf$FULLNAME  <- gsub(" AV"," AVE", P24sf$FULLNAME)
# P24sf$FULLNAME  <- gsub(" TE"," TER", P24sf$FULLNAME)
# P24sf$FULLNAME  <- gsub("FR ","FATHER ", P24sf$FULLNAME)
# P24sf$FULLNAME  <- gsub(" LA"," LN", P24sf$FULLNAME)
# P24sf$FULLNAME  <- gsub(" WH"," WHARF", P24sf$FULLNAME)
# P24sf$FULLNAME  <- gsub(" WEST"," W", P24sf$FULLNAME)
# P24sf$FULLNAME  <- gsub("MSGR ","MONSIGNOR ", P24sf$FULLNAME)

# Railey's portion
# P24sf$FULLNAME<- str_replace(P24sf$FULLNAME, "PK$", "PARK")
P24sf$FULLNAME<- str_replace(P24sf$FULLNAME, "CHICKAMAUGA", "CHICAMAUGA")
P24sf$FULLNAME<- str_replace(P24sf$FULLNAME, "LAGRANGE", "LA GRANGE")
P24sf$FULLNAME<- str_replace(P24sf$FULLNAME, "MORTON VILLAGE", "MORTON VILLIAGE")
P24sf$FULLNAME<- str_replace(P24sf$FULLNAME, "CEDAR GROVE", "CEDAR GROVE ST")
P24sf$FULLNAME<- str_replace(P24sf$FULLNAME, "N BENNET CT", "N BENNETT CT")
P24sf$FULLNAME<- str_replace(P24sf$FULLNAME, "VAN BRUNT", "VAN BRUNT ST")
P24sf$FULLNAME<- str_replace(P24sf$FULLNAME, "WALES PL", "WALES ST")
P24sf$FULLNAME<- str_replace(P24sf$FULLNAME, "UPHAM CT", "UPHAMS CT")
P24sf$FULLNAME<- str_replace(P24sf$FULLNAME, "PARK VALE", "PARKVALE")
P24sf$FULLNAME<- str_replace(P24sf$FULLNAME, "THIRTEENTH", "13th")
P24sf$FULLNAME<- str_replace(P24sf$FULLNAME, "POWER HOUSE", "POWER HOUSE ST")
P24sf$FULLNAME<- str_replace(P24sf$FULLNAME, "SOLDIERS FIELD RD EXD", "SOLDIERS FIELD RD")
P24sf$FULLNAME<- str_replace(P24sf$FULLNAME, "STREET A", "A ST")
P24sf$FULLNAME<- str_replace(P24sf$FULLNAME, "O CONNELL", "OCONNELL")
P24sf$FULLNAME<- str_replace(P24sf$FULLNAME, "PORT NORFOLK", "PORT NORFOLK ST")
P24sf$FULLNAME<- str_replace(P24sf$FULLNAME, "TAFTHILL", "TAFT HILL")
P24sf$FULLNAME<- str_replace(P24sf$FULLNAME, "E BOUNDARY RD", "ENNEKING PKWY")
P24sf$FULLNAME<- str_replace(P24sf$FULLNAME, "PIE AL", "PIE ALY")
P24sf$FULLNAME<- str_replace(P24sf$FULLNAME, "MATIGNON", "MATIGON")
# Courts/very small streets that are being grouped with the larger streets they are connected to 
P24sf$FULLNAME<- str_replace(P24sf$FULLNAME, "RUGGLES CT", "RUGGLES ST")
P24sf$FULLNAME<- str_replace(P24sf$FULLNAME, "JUNIPER TER", "JUNIPER ST")
P24sf$FULLNAME<- str_replace(P24sf$FULLNAME, "MEYER CT", "MEYER ST")
P24sf$FULLNAME<- str_replace(P24sf$FULLNAME, "DUDLEY PL", "DUDLEY ST")
P24sf$FULLNAME<- str_replace(P24sf$FULLNAME, "NEAL CT", "SHORT ST")
P24sf$FULLNAME<- str_replace(P24sf$FULLNAME, "LOWLAND PL", "EVERETT ST")
P24sf$FULLNAME<- str_replace(P24sf$FULLNAME, "MONMOUTH SQ", "MONMOUTH ST")
P24sf$FULLNAME<- str_replace(P24sf$FULLNAME, "FOSTER CT", "FOSTER ST")

#Zakhire <- P24sf
#P24sf <- Zakhire
# Mehrnaz's portion

# P24sf$FULLNAME <- gsub("BOARD AL", "BOARD ST", P24sf$FULLNAME)
P24sf$FULLNAME <- gsub("DEDHAM LINE", "DEDHAM ST", P24sf$FULLNAME) ## done
# P24sf$FULLNAME <- gsub("WARREN PK", "WARREN PARK", P24sf$FULLNAME)
P24sf$FULLNAME <- gsub("HAYDN ST", "HAYDEN ST", P24sf$FULLNAME) ## done
P24sf$FULLNAME <- gsub("NORTHDALE ST", "NORTHDALE RD", P24sf$FULLNAME) ## done

P24sf$FULLNAME <- gsub("E BROADWAY ST", "E BROADWAY", P24sf$FULLNAME) ## done
P24sf$FULLNAME <- gsub("BROADWAY ST", "BROADWAY", P24sf$FULLNAME) ## done
P24sf$FULLNAME <- gsub("W BROADWAY ST", "W BROADWAY", P24sf$FULLNAME) ## done

P24sf$FULLNAME <- gsub("RAILROAD RD", "RAILROAD ST", P24sf$FULLNAME) ## done
P24sf$FULLNAME <- gsub("ADAMS$", "ADAMS ST", P24sf$FULLNAME)  ## done
P24sf$FULLNAME <- gsub("MOTHER JULIA RD", "MOTHER JULIAN RD", P24sf$FULLNAME)  ## done
P24sf$FULLNAME <- gsub("CHARLES RIVER DAM", "CHARLES RIVER DAM RD",P24sf$FULLNAME)  ## done & save
P24sf$FULLNAME <- gsub("SNOW$", "SNOW ST", P24sf$FULLNAME) ## done
P24sf$FULLNAME <- gsub("CHARLESGATE ST", "CHARLESGATE", P24sf$FULLNAME) ## done
P24sf$FULLNAME <- gsub("BELMONT PL", "BELMONT ST", P24sf$FULLNAME) ## done
P24sf$FULLNAME <- gsub("LOTHROP PL", "LOTHROP ST", P24sf$FULLNAME) ## done
P24sf$FULLNAME <- gsub("LEAMINGTON ST", "LEAMINGTON RD", P24sf$FULLNAME)## done & save
P24sf$FULLNAME <- gsub("NEW PICKERTS WHARF", "CENTRAL WHARF", P24sf$FULLNAME) ## done
P24sf$FULLNAME <- gsub("BOWDOIN SQ", "BOWDOIN ST", P24sf$FULLNAME) ## done
P24sf$FULLNAME <- gsub("COMMERCIAL WHARF EAST", "COMMERCIAL WHARF", P24sf$FULLNAME)  ## done
P24sf$FULLNAME <- gsub("GROVE PL", "GROVE ST", P24sf$FULLNAME)  ## done
P24sf$FULLNAME <- gsub("LEVERETT ST", "LEVERETT AVE", P24sf$FULLNAME)  ## done
P24sf$FULLNAME <- gsub("HOWE AVE", "HOWE ST", P24sf$FULLNAME)  ## done
P24sf$FULLNAME <- gsub("BRADEEN STREET FW", "BRADEEN ST", P24sf$FULLNAME)  ## done
P24sf$FULLNAME <- gsub("BAKERS AL", "BAKERS ALY", P24sf$FULLNAME)  ## done
P24sf$FULLNAME <- gsub("BATTERY WHARF ST", "BATTERY WHARF", P24sf$FULLNAME)  ## done
P24sf$FULLNAME <- gsub("PINEFIELD LN", "PINEFIELD RD", P24sf$FULLNAME) ## done
P24sf$FULLNAME <- gsub("OAKLAND PARK", "OAKLAND PL", P24sf$FULLNAME)  ## done
P24sf$FULLNAME <- gsub("READVILLE TROTTING PARK", "READVILLE ST", P24sf$FULLNAME) ## done & save
P24sf$FULLNAME <- gsub("^1ST ST", "E 1ST ST", P24sf$FULLNAME) ## done
# P24sf$FULLNAME <- gsub("HESTIA PK", "HESTIA PARK", P24sf$FULLNAME)
P24sf$FULLNAME <- gsub("^PARLEY VALE ST", "PARLEY VALE", P24sf$FULLNAME) ## done
P24sf$FULLNAME <- gsub("^PARLEY VALE AVE", "PARLEY VALE", P24sf$FULLNAME) ## done
P24sf$FULLNAME <- gsub("CUMMINGS HWY", "CUMMINGS ST", P24sf$FULLNAME) ## done
P24sf$FULLNAME <- gsub("MASSPORT BYPASS RD", "MASSPORT HAUL RD", P24sf$FULLNAME) ## done
P24sf$FULLNAME <- gsub("FID KENNEDY DR", "FID KENNEDY AVE", P24sf$FULLNAME) ## done
P24sf$FULLNAME <- gsub("DRAPER RD", "DRAPER ST", P24sf$FULLNAME) ## done
P24sf$FULLNAME <- gsub("GARDEN COURT CT", "GARDEN CT ST", P24sf$FULLNAME) ## done
P24sf$FULLNAME <- gsub("ALPINE PL", "ALPINE ST", P24sf$FULLNAME)## done
P24sf$FULLNAME <- gsub("HAMILTON TER", "HAMILTON PL", P24sf$FULLNAME) ## done
P24sf$FULLNAME <- gsub("NEW DORCHESTER AVE BRIDGE", "DORCHESTER AVE", P24sf$FULLNAME) ## done & save
P24sf$FULLNAME <- gsub("DEDHAM PARISH RD", "PARISH ST", P24sf$FULLNAME) ## done
P24sf$FULLNAME <- gsub("ELLERY CT", "ELLERY ST", P24sf$FULLNAME) ## done
P24sf$FULLNAME <- gsub("COLUMBUS SQ", "COLUMBUS AVE", P24sf$FULLNAME)  ## done

# P24sf$FULLNAME <- gsub("COLUMBIA CIR", "COLUMBIA RD", P24sf$FULLNAME)
P24sf$FULLNAME <- gsub("^7TH ST", "E 7TH ST", P24sf$FULLNAME) ## done
# P24sf$FULLNAME <- gsub("RUNDEL PK", "RUNDEL PARK", P24sf$FULLNAME)
P24sf$FULLNAME <- gsub("HICKORY AVE", "HICKORY LN", P24sf$FULLNAME) ## done
P24sf$FULLNAME <- gsub("WOODLAND AVE", "WOODLAND RD", P24sf$FULLNAME)  ## done
P24sf$FULLNAME <- gsub("NEW NORTHERN AVE", "NORTHERN AVE", P24sf$FULLNAME)  ## done
P24sf$FULLNAME <- gsub("HOYT PL", "HOYT ST", P24sf$FULLNAME) ## done
P24sf$FULLNAME <- gsub("DEDHAM BRANCH", "DEDHAM ST", P24sf$FULLNAME) ## done
P24sf$FULLNAME <- gsub("LAMARTINE CT", "LAMARTINE ST", P24sf$FULLNAME)## done & save

# The offending TLID in 2019 is 85701050
# View(prop23[prop23$TLID==85701050 & !is.na(prop23$TLID),])
#unique(prop23$ST_NAME[prop23$TLID==85701050 & !is.na(prop23$TLID)]) #length == 401
#View(tiger23[grepl('MC',tiger23$FULLNAME),])
# added at end of subs: 
P24sf$FULLNAME <- gsub("^MT ","MOUNT ",P24sf$FULLNAME)
# added after import of tiger23 in order to create consistency of ST/SAINT, which is tricky. All properties, though, are ST:
tiger23$FULLNAM <- toupper(as.character(tiger23$FULLNAM)) # this was needed
tiger23$FULLNAM <- gsub("^SAINT ","ST ",tiger23$FULLNAM)
# Those two fixes get us down to 382.

length(unique(P24sf$FULLNAME[!P24sf$FULLNAME %in% tiger23$FULLNAM])) #233 in 2024
#211 in Sep, 23
#217???? in August, 2023
#4060 in March,2023
#4087 in 2020 
#4056 in 2021 
#4061 in 2022 

sum(is.na(P24sf$FULLNAME))
#[1] 0
length(which(tiger23$FULLNAM ==  "0"))
#4658
#eliminate NAs as "0"s
tiger23 <- tiger23[which(!tiger23$FULLNAM ==  "0"),]
# 1.1. 

#correct the geometry variables
P24sf$Y <- st_coordinates(st_transform(P24sf, 4326))[,1]
P24sf$X <- st_coordinates(st_transform(P24sf, 4326))[,2]
P24sf <- st_drop_geometry(P24sf)
P24sf <- st_as_sf(P24sf, coords = c("X", "Y"), crs = 4326)

tiger23 <- st_as_sf(tiger23)
tiger23 <- rename(tiger23, "X.id" = "X")
tiger_coords <- st_coordinates(st_transform(tiger23, 4326))
tiger_coords[, c("X", "Y")] <- tiger_coords[, c("Y", "X")] # Swap X and Y
tiger_coords <- as.data.frame(tiger_coords)
tiger_coords <- tiger_coords[!duplicated(tiger_coords$L1),]
tiger23 <- st_drop_geometry(tiger_fix)
tiger23$X <- tiger_coords$X
tiger23$Y <- tiger_coords$Y
tiger23 <- st_as_sf(tiger23, coords = c("X", "Y"), crs = 4326)



# Take new observations from P24sf and put them into the for loop

#take new observations by LAND_PARCEL_ID
P24sf_new <- P24sf[which(P24sf$GIS_ID %in% setdiff(P24sf$GIS_ID, LandParcels_2023$Land_Parcel_ID)),]
#P24sf_old$TLID <- NA 
#794 new observations #181 in 2024
length(setdiff(LandParcels_2023$Land_Parcel_ID, P24sf$GIS_ID))
#305 retired observations #181 in 2024
nrow(P24sf) - nrow(LandParcels_2023)
#489 overall difference  #635 in 2024
nrow(P24sf) - nrow(LandParcels_2023) + length(setdiff(LandParcels_2023$Land_Parcel_ID, P24sf$GIS_ID))
#794 actual new observations #816 in 2024


# If CRS do not match, transform the CRS of tiger23 to match P24sf_new
tiger23 <- st_as_sf(tiger23, wkt = "geometry", crs = st_crs(P24sf_new))
tiger23 <- st_transform(tiger23, crs = st_crs(P24sf_new))
#### Match parcels which have same FULLNAME as the street file####

## running all parcels through matching:
## Note: takes ~ 25 minutes to run this loop ~55min on 2020 data, tablet Alina
start1 <- Sys.time()
pb <- progress_estimated(nrow(P24sf_new),0)

P24sf_new$TLID <- NA

for(i in 1:nrow(P24sf_new)){
  # i = 12
  matching_streets <- tiger23 %>%
    filter(FULLNAM %in% P24sf_new$FULLNAME[i])
  if(nrow(matching_streets)>0){
    P24sf_new$TLID[i] <- as.character(matching_streets$TLID[which.min(st_distance(P24sf_new[i,"geometry"], matching_streets))])
  }
  pb$pause(0.1)$tick()$print() # update progress bar
}

end1 <- Sys.time()
end1-start1 
#Time difference of 1.840781697 mins in 2024
#Time difference of 1.664658 mins Sep21 23
#Time difference of 1.54573 mins in Sep, 23
#Time difference of 1.920024 mins in Sep 15, 2023 
#Time difference of 3.315381 hours


sum(is.na(P24sf_new$TLID))/nrow(P24sf_new) #0.03799019608 in 2024 #0.01259446 for new 0.0009732459 for all #0.04030227 for new obs. 0.001196281 for all in Sep 23 #0.04015056 in Aug 23 #  0.01430806 in Feb 19th,2023  
# 0.01690084 in 2020 #0.0173456 in 2021 # 0.01544526 in 2022
table(P24sf_new$FULLNAM[which(is.na(P24sf_new$TLID))]) %>% sort()

#join TLID for old ones
P24sf_old <- P24sf[which(P24sf$GIS_ID %in% setdiff(P24sf$GIS_ID, P24sf_new$GIS_ID)),]
#or 
# P24sf_old <- P24sf[which(!P24sf$GIS_ID %in% setdiff(P24sf$GIS_ID, LandParcels_2023$Land_Parcel_ID)),]
prop23 <- read.csv("~/Desktop/Academic/BARI/Scripts/GI/Inputs/Properties.2023.csv")
prop23 <- prop23 |> dplyr::select(GIS_ID, TLID)
prop23u <- distinct(prop23)
P24sf_old <- left_join(P24sf_old, prop23u, join_by(GIS_ID))
P24sf_old <- P24sf_old[,colnames(P24sf_new)]


#TG_join <- tiger23 |> select(FULLNAM, TLID) |> rename(FULLNAME = FULLNAM)
#P24sf_tgjoin <- P24sf[which(P24sf$GIS_ID %in% setdiff(P24sf$GIS_ID, P24sf_new$GIS_ID)),]
#P24sf_tgjoin <- left_join(P24sf_tgjoin, TG_join, join_by(FULLNAME))

#merge the new TLIDs back to P24sf
P24sf_wTLID <- rbind(P24sf_new, P24sf_old)
#P24sf <- P24sf_wTLID



#### Add blocks groups, blocks ####


length(unique(P24sf_wTLID$FULLNAME[which(is.na(P24sf_wTLID$TLID) & grepl("ISLAND",P24sf_wTLID$FULLNAME)==F)])) 
#46 in 2024
#45 in Sep 23
#65 in Sep 23
#68 in Aug 23
#206 in Feb 2023
#198 in 2019 data #221 in 2020 #203 in 2021 # 159 in 2022
#nrow(unique(P24sf[which(is.na(P24sf$TLID) & grepl("ISLAND",P24sf$FULLNAME)==F),])) # 404 # error in 2019 data #error in 2020

# 2021 is the latest it seems. Toshi comment on Feb 19th, 2023
#2022 is avaliable on August 2023 - YET
blocks <- tigris::blocks(state = "MA",county = "Suffolk",year = 2023)
blocks <- st_transform(blocks,crs = st_crs(P24sf_wTLID))
blocks$Blk_ID_20 <- blocks$GEOID20
blocks$CT_ID_20 <- str_sub(blocks$GEOID20,end = 11)
blocks <- blocks %>% dplyr::select(Blk_ID_20,CT_ID_20)

bgs <- tigris::block_groups(state = "MA",county = "Suffolk",year = 2023)
bgs <- st_transform(bgs,crs = st_crs(P24sf_wTLID))
bgs$BG_ID_20 <- bgs$GEOID
bgs <- bgs %>% dplyr::select(BG_ID_20)

# 2019 block to get BG_ID_10

blocks_10 <- tigris::blocks(state = "MA",county = "Suffolk",year = 2019)
blocks_10 <- st_transform(blocks_10,crs = st_crs(P24sf_wTLID))
blocks_10$Blk_ID_10 <- blocks_10$GEOID10
blocks_10$CT_ID_10 <- str_sub(blocks_10$GEOID10,end = 11)
blocks_10$BG_ID_10 <- str_sub(blocks_10$GEOID10,end = 12)
blocks_10 <- blocks_10 %>% dplyr::select(CT_ID_10, BG_ID_10, Blk_ID_10)


#this was needed to join block data
P24sf_wTLID$X <- st_coordinates(P24sf_wTLID)[,2]
P24sf_wTLID$Y <- st_coordinates(P24sf_wTLID)[,1]
P24sf_wTLID <- st_drop_geometry(P24sf_wTLID)
P24sf_wTLID <- st_as_sf(P24sf_wTLID, coords = c("X", "Y"), crs = 4326)


P24sf_geo <- st_join(P24sf_wTLID,blocks_10,join=st_within,left=T)
P24sf_geo <- st_join(P24sf_geo,blocks,join=st_within,left=T)
P24sf_geo <- st_join(P24sf_geo,bgs,join=st_within,left=T)

sum(is.na(P24sf_geo$Blk_ID_20))/nrow(P24sf_geo)   #0.00003044788844 in 2024 #3.041393e-05 in SeP24 #0.000191989 in Aug 23 #0.0002225437 in March 2023#0.0002425958 in 2020
sum(is.na(P24sf_geo$BG_ID_20))/nrow(P24sf_geo) #0.00003044788844 in 2024 #3.041393e-05 in SeP24  #0.000191989 in Aug 23 #0.0002225437 in March 2023
sum(is.na(P24sf_geo$CT_ID_20))/nrow(P24sf_geo) #0.00003044788844 in 2024 #3.041393e-05 in SeP24  #0.000191989 in Aug 23 #0.0002225437 in March 2023 #0.00024259 in 2020

#P24sf_geo <- st_transform(P24sf_geo,4326)
P24sf_geo$X <- st_coordinates(P24sf_geo)[,2]
P24sf_geo$Y <- st_coordinates(P24sf_geo)[,1]
P24sf_geo <- st_drop_geometry(P24sf_geo)
P24sf_geo <- st_as_sf(P24sf_geo, coords = c("X", "Y"), crs = 4326)

#P24sf_geo <- P24sf_geo %>% dplyr::select(-c("...1"))
P24sf_geo$X <- st_coordinates(P24sf_geo)[,1]
P24sf_geo$Y <- st_coordinates(P24sf_geo)[,2]

# save parcels updated
write_csv(as.data.frame(st_set_geometry(P24sf_geo,NULL)),"~/Desktop/Academic/BARI/Scripts/GI/Outputs/parcels_updatedTLID_2024.csv")



#load in the parcels updated file

P24sf_geo <- read.csv("~/Desktop/BARI/GI-2023/new-2/parcels_updatedTLID_083023_YET.csv")



# ##drop the .y for block, bg, ct id's and rename the original
# P24sf_geo <- P24sf_geo %>% dplyr::select(-c("Blk_ID_10", "BG_ID_10", "CT_ID_10") )


#### WHY WE ARE DOING THIS?????? ######
identical(P24sf$GIS_ID,P24sf_geo$GIS_ID)
identical((P24sf$GIS_ID |> sort()), (P24sf_geo$GIS_ID |> sort()))
#TRUE

#P24sf_geo <- read.csv("../Data/parcels_updatedTLID_190410.csv",stringsAsFactors = F)
P24sf$TLID <- P24sf_geo$TLID[match(P24sf$MAP_PAR_ID,P24sf_geo$MAP_PAR_ID)]
P24sf$Blk_ID_20 <- P24sf_geo$Blk_ID_20[match(P24sf$MAP_PAR_ID,P24sf_geo$MAP_PAR_ID)]
P24sf$BG_ID_20 <- P24sf_geo$BG_ID_20[match(P24sf$MAP_PAR_ID,P24sf_geo$MAP_PAR_ID)]
P24sf$CT_ID_20 <- P24sf_geo$CT_ID_20[match(P24sf$MAP_PAR_ID,P24sf_geo$MAP_PAR_ID)]
P24sf_geo <- P24sf

# save the workspace
#save(P24sf_geo,file = "C:\\Users\\alina.ristea\\Google Drive\\Work\\NEU_BARI\\Projects\\Geographical_Infrastructure\\Data\\results\\parcels_updatedTLID_sf_09262019.RData")
#load(file = "../Data/Geographical Infrastructure 2018/Data/parcels_updatedTLID_sf.RData")

# read hte P24sf if it is not read previously (it is created in the upper rows)
#P24sf <- read.csv("C:\\Users\\alina.ristea\\Google Drive\\Work\\NEU_BARI\\Projects\\Geographical_Infrastructure\\Boston Geographical Infrastructure 2019\\parcels_updatedTLID_10102019.csv")

#NEW
#P24sf <- read.csv("C:\\Users\\bariuser2\\Google Drive\\BARI Research Team Data Library\\Geographical Infrastructure\\Boston Geographical Infrastructure 2020\\Outputs\\parcels_updatedTLID_04292020.csv")

P24 <- st_as_sf(P24sf_geo, coords = c("X", "Y"), crs = 26986)
sum(is.na(P24$TLID)) #124 in 2024 #434 in Aug 23, #1149 in March 2023. 
#1528 in 2019 data #1672 in 2020 #1716 in 2021 #1391 in 2022




#### TO RUN: find streets close to parcels by applying a buffer over the parcels ####

# create a df for parcels which do not have TLID associated
TLID_NA <- P24shp_centroid%>%
  filter(MAP_PAR_ID %in% P24$MAP_PAR_ID[is.na(P24$TLID)]) # this will have 9 columns
# is not this 19 columns?

# try to use only the TLID column from tiger23 to work faster
tiger23_short <- tiger23[,c("TLID")]

points_to_near_poly <- function(points_transf, poly_transf) {
  # Convert to sf and align CRS
  points <- st_transform(st_as_sf(points_transf), st_crs(poly_transf))
  polygons <- st_as_sf(poly_transf)
  
  # Buffer points and find intersections
  points_buffer <- st_buffer(points, dist = 100)
  intersections <- st_intersection(polygons, points_buffer)
  
  # Calculate distances and find the nearest polygon for each point
  distances <- st_distance(points, intersections)
  nearest_indices <- apply(distances, 1, which.min)
  
  # Create result by selecting the nearest polygon
  result <- intersections[nearest_indices, ]
  result <- st_set_geometry(result, st_geometry(points))
  
  return(result)
}

# run the buffer analysis
start1 <- Sys.time()
TLID_NA <- points_to_near_poly(TLID_NA, tiger23_short) 
#Time difference of 52.19130397 secs in 2024
# Time difference of 4.991917 mins #10.53832 min on tablet 2020
# Time difference of 49.39196 secs in 2023
# 1418 ?
# 43 TLIDs NA for 100m
end1 <- Sys.time()
end1-start1


sum(is.na(TLID_NA$X1))
#0 in 2024
#64 in Aug 23 # 45 in Feb 2023
#65 in 2020 for 100m #48 in 2021 #66 TLIDs NA for 100m in 2022

# # the function is losing the headers; add names to header (TLID from tiger23 + names from P22sh)
# names = c(colnames(P24shp_centroid))
# colnames(TLID_NA) = c("TLID", names)
# TLID_NA <- TLID_NA[,c("TLID", names)]

# create copy variable TLID_new to be used further


# we use character for geometry column, if not we cannot save it well as csv
# TLID_NA$geometry <- as.character(TLID_NA$geometry)

# deleting empty, unnecessary rows
TLID_NA_2 <- TLID_NA[,-c(8,9,10, 13,14,15)]
# TLID_new <- TLID_NA_2
# debugging the error to write.csv

typeof(TLID_NA_2$TLID)
typeof(TLID_NA_2$MAP_PAR_ID)
typeof(TLID_NA_2$Ln_P_ID) #no column like that 
typeof(TLID_NA_2$geometry)

# code for this error Error in utils::write.table(TLID_NA_2, "TLIDs_20230329.TY.csv", row.names = FALSE,  : 
# unimplemented type 'list' in 'EncodeElement'
#TLID_NA_2$geometry <- vapply(TLID_NA_2$geometry, paste, collapse = ", ", character(1L))

# Ensure the geometry column is correctly set
st_geometry(TLID_NA_2) <- "geometry"

# Extract latitude and longitude from the geometry column
coords <- st_coordinates(TLID_NA_2)
TLID_NA_2$long <- coords[, 1]
TLID_NA_2$lat <- coords[, 2]

# Drop the geometry column

TLID_new <- as.data.frame(st_drop_geometry(TLID_NA_2))

# geometry is the suspect, we cannot have a list in write.csv
# save as csv 
write.csv(as.data.frame(st_drop_geometry(TLID_NA_2)), "~/Desktop/Academic/BARI/Scripts/GI/Outputs/TLIDs_2024_YET.csv", row.names = FALSE)


#### Join data: parcels with TLID, parcels run in the buffer loop and resulted with or without TLIDs ####

dt <- left_join(as.data.frame(P24), as.data.frame(TLID_new[,c("TLID", "MAP_PAR_ID")]), by = "MAP_PAR_ID", 
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
write.csv(as.data.frame(st_set_geometry(dt,NULL)),"~/Desktop/Academic/BARI/Scripts/GI/Outputs/parcels_fullupdatedTLID_2024_YET.csv", row.names = FALSE)






