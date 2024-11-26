##### PREP 0: Load Neccesary Libraries and Functions#### 
# List of required packages
packages <- c("sf", "wk", "lubridate", "glue", "tidyverse")

# Install and load packages
lapply(packages, function(pkg) {
  if (!require(pkg, character.only = TRUE)) install.packages(pkg, dependencies = TRUE)
  library(pkg, character.only = TRUE, quietly = TRUE)
})

# Set global options
options(scipen = 100000000)

######### # PREP 0: Read Files ###########

# Input Paths
input_dir <- "~/BARI/Permits/Inputs"
permits_path = file.path(input_dir, "approved-building-permits.csv") # downloaded from Boston City data portal: https://data.boston.gov/dataset/approved-building-permits
permits_last_path = file.path(input_dir, "Permits Records 2023.csv")  #last years' data
properties_path = file.path(input_dir, "Properties_2023.csv") # from BARI dataverse
landParcels_path = file.path(input_dir, "LandParcels_2023.csv")  # from BARI dataverse
landParcelsShpPath = file.path(input_dir, "LandParcels_2023.shp")
cleaning_functions_path <- file.path(input_dir, "Cleaning_functions_20190326.R")
geocoding_functions_path <- file.path(input_dir, "Geocoding_functions_20190326.R")
helper_functions_path <- file.path(input_dir, "Helper_functions_20190326.R")

# Output Path
output_dir <- "~/BARI/Permits/Outputs"
date <- format(Sys.Date(), "(%m-%Y)")
record_name <- sprintf("Permits.Records.Geocoded.%s.csv", date)
record_path <- file.path(output_dir, record_name)


# Read Files
permits_2024 <- read.csv(permits_path)
permits_2023 <- read.csv(permits_last_path)
properties <- read.csv(properties_path)
landParcels <- read.csv(landParcels_path)
landParcels_shp <- st_read(landParcelsShpPath)

# Loading functions
source(cleaning_functions_path)
source(geocoding_functions_path)
source(helper_functions_path)

##### PREP I: Filter and Transform Data ######

# Filter out observations until 2024
permits <- permits_2024 %>% filter(year(ymd_hms(issued_date)) != 2024)

#permits_backup <- permits #612083, 24 in 2024 with NA # Backup the filtered data

########## PREP II: Transform geo info ##############

# Rename lat and long columns
permits <- permits %>% rename("lat" = "gpsy",
                              "long" = "gpsx")

# how many NA values do we have for geo information
length(which(permits$geom_point_4326 == "")) #14373 in 2024
length(which(permits$geom_point_4326 != ""))/nrow(permits) #0.9765179 in 2024

# Split the dataset based on geom_point_4326 availability
permits_wgeo <- permits[which(permits$geom_point_4326 != ""),]
permits_wogeo <- permits[which(permits$geom_point_4326 == ""),]

# Obtain valid WKB strings, Convert Hexadecimal WKB to Raw and Convert raw binary data to 'sfc' object
permits_wgeo <- permits_wgeo %>%
  mutate(
    wkb_raw = map(geom_point_4326, ~ as.raw(strtoi(paste0("0x", substring(.x, seq(1, nchar(.x) - 1, 2), seq(2, nchar(.x), 2)))))),
    geometry = st_as_sfc(wkb_raw, EWKB = TRUE)
  )

# Combine the geometry with the subset data frame to create an sf data frame
permits_wgeo <- st_sf(permits_wgeo)
permits_wgeo <- st_transform(permits_wgeo, 4326)

# Remove unnecessary columns 
permits_wgeo <- permits_wgeo %>% 
  select(-c(lat, long, wkb_raw, geom_point_2249, geom_point_4326))
permits_wogeo <- permits_wogeo %>% 
  select(-c(geom_point_2249, geom_point_4326))

# add lat/long from geometry
permits_wgeo <- permits_wgeo %>% 
  mutate(long = st_coordinates(geometry)[, 'X'], lat = st_coordinates(geometry)[, 'Y']) %>% 
  st_set_geometry(NULL)

# Combine the two datasets
permits_geo <- rbind(permits_wgeo, permits_wogeo)
permits <- permits_geo

######### # PREP III: Process Date Variables and Rename Columns ###########

#rename columns
permits <- permits %>%
  rename(
    PermitNumber = permitnumber,       # Different
    WORKTYPE = worktype,             # Same, different case
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
    Property_ID = property_id,        # Same, different case
  )



# Convert ISSUED_DATE to POSIXct
sum(is.na(permits$ISSUED_DATE)) #0
permits$ISSUED_DATE = ymd_hms(permits$ISSUED_DATE)

# Handle missing EXPIRATION_DATE
permits$EXPIRATION_DATE[permits$EXPIRATION_DATE==""|permits$EXPIRATION_DATE==" "]=NA
sum(is.na(permits$EXPIRATION_DATE)) #25400 in 2024
permits$EXPIRATION_DATE = ymd_hms(permits$EXPIRATION_DATE)

# Calculate PermitDuration
permits$PermitDuration = permits$EXPIRATION_DATE - permits$ISSUED_DATE
permits$PermitDuration[!is.na(permits$PermitDuration) & permits$PermitDuration<0]= NA
permits$PermitDuration <- as.numeric(permits$PermitDuration)/86400  #NA's :25497 in 2024
summary(permits) 

# Convert DECLARED_VALUATION and total_fees to numeric
permits <- permits %>%
  mutate(
    DECLARED_VALUATION = as.numeric(gsub("[\\$,]", "", DECLARED_VALUATION)),
    total_fees = as.numeric(gsub("[\\$,]", "", total_fees))
  )
summary(permits$total_fees)
permits$"total_fees" = ifelse(!is.na(permits$"total_fees") & permits$"total_fees"<0,NA, permits$"total_fees")

# Remove duplicate rows
permits_dedupe <- permits %>%
  distinct(
    PermitNumber, WORKTYPE, permittypedescr, description, NOTES,
    APPLICANT, DECLARED_VALUATION, total_fees, ISSUED_DATE, EXPIRATION_DATE,
    STATUS, owner, OCCUPANCY, sq_feet, .keep_all = TRUE
  )

# # Remove duplicate rows
# permits_dedupe <- permits[!duplicated(cbind(permits$PermitNumber,permits$WORKTYPE,permits$permittypedescr,permits$description,permits$NOTES,
#                                             permits$APPLICANT, permits$DECLARED_VALUATION, permits$total_fees, permits$ISSUED_DATE, permits$EXPIRATION_DATE,
#                                             permits$STATUS, permits$owner, permits$OCCUPANCY, permits$sq_feet)),]
# permits_dedupe_backup <- permits_dedupe #600123 in 2024

######## # PREP IV: Categorize Permit Types ############## 
#put this at the end because it omits the missing data from previous year

permits_dedupe$permittypedescr = trim(str_replace_all(permits_dedupe$permittypedescr,
                                                      "\xe4\xf3\xf1",""))


NEWCON = (!is.na(permits_dedupe$description) & 
            (permits_dedupe$description == "New construction" | 
               permits_dedupe$description == "Erect")) | 
  (!is.na(permits_dedupe$permittypedescr) & 
     (permits_dedupe$permittypedescr=="Erect/New Construction" |
        permits_dedupe$permittypedescr == "Foundation Permit"))

DEMO = (!is.na(permits_dedupe$description) & 
          (permits_dedupe$description =="Demolition - Exterior" | 
             permits_dedupe$description=="Demolition - Interior")) 


ADD = (!is.na(permits_dedupe$description) & 
         (permits_dedupe$description == "Addition")) | 
  (!is.na(permits_dedupe$permittypedescr) & 
     (permits_dedupe$permittypedescr == "Amendment to a Long Form" | 
        permits_dedupe$permittypedescr == "Long Form/Alteration Permit"))



RENO = (!is.na(permits_dedupe$description) & 
          (permits_dedupe$description == "Electrical" | 
             permits_dedupe$description == "Gas" | 
             permits_dedupe$description == "Plumbing" |
             permits_dedupe$description == "Renovations - Exterior"|
             permits_dedupe$description == "Renovations - Interior NSC")) |
  (!is.na(permits_dedupe$permittypedescr) & 
     (permits_dedupe$permittypedescr == "Electrical Permit" | 
        permits_dedupe$permittypedescr == "Gas Permit" | 
        permits_dedupe$permittypedescr == "Plumbing Permit" | 
        permits_dedupe$permittypedescr == "Short Form Bldg Permit" | 
        permits_dedupe$permittypedescr == "Use of Premises" |
        permits_dedupe$permittypedescr== "Electrical Fire Alarms" | 
        permits_dedupe$permittypedescr== "Electrical Low Voltage" | 
        permits_dedupe$permittypedescr== "Electrical Temporary Service"))


#combining the two categorizations
permits_dedupe$newcon<-ifelse(NEWCON, 1,0)
permits_dedupe$demo<-ifelse(!NEWCON & DEMO, 1,0)
permits_dedupe$addition<-ifelse(!NEWCON & !DEMO & ADD, 1,0)
permits_dedupe$reno<-ifelse(!NEWCON & !DEMO & !ADD & RENO, 1,0)

##“Government” Variable
permits_dedupe$government = ifelse(permits_dedupe$owner == "CITY OF BOSTON" |
                                     permits_dedupe$owner == "BOSTON REDEVELOPMNT AUTH" |
                                     permits_dedupe$owner == "BOSTON REDEVELOPMENT AUTH" |
                                     permits_dedupe$owner == "BOSTON REDEVELOPMENT" |
                                     permits_dedupe$owner == "BOSTON HOUSING AUTHORITY" |
                                     permits_dedupe$owner == "BOSTON HOUSING AUTH" |
                                     permits_dedupe$owner == "BOSTON POLICE POST 1018 VFW" |
                                     permits_dedupe$owner == "CITY OF BOSTON SCHOOL DEPT" |
                                     permits_dedupe$owner == "CITY OF BOSTON PUBLIC HEALTH" |
                                     permits_dedupe$owner == "CITY OF BOSTON SCHOOL DEPT" |
                                     permits_dedupe$owner == "CITY OF BOSTON BY FCL" |
                                     permits_dedupe$owner == "CITY OF BOSTON PUB FACIL" |
                                     permits_dedupe$owner == "BOSTON REDEVLOPMENT AUTHORIT" |
                                     permits_dedupe$owner == "BOSTON POLICE DETECTIVE" |
                                     permits_dedupe$owner == "CITY OF BOSTON PARKS" |
                                     permits_dedupe$owner == "BOSTON REDEVELOP AUTHORITY" |
                                     permits_dedupe$owner == "CITY OF BOSTON PARKS AND" |
                                     permits_dedupe$owner == "THE BOSTON REDEVELOPMENT" |
                                     permits_dedupe$owner == "BOSTON REDEVOPMENT AUTH" |
                                     permits_dedupe$owner == "BOSTON REDEVLPMNT AUTHOR" |
                                     permits_dedupe$owner == "BOSTON REDEVLOPMENT AUTHOR" |
                                     permits_dedupe$owner == "MBTA" |
                                     permits_dedupe$owner == "BOSTON PUBLIC HEALTH COMM" |
                                     permits_dedupe$owner == "CITY OF BOSTON PUBLIC HEALTH" |
                                     permits_dedupe$owner == "CITY OB BOSTON PUBLIC HEALTH" |
                                     permits_dedupe$owner == "PUBLIC FACILITIES COMM" |
                                     permits_dedupe$owner== "BOSTON DEVELOPMENT" |
                                     permits_dedupe$owner== "BOSTON FIRE DEPARTMENT" |
                                     permits_dedupe$owner== "BOSTON HOUSING" |
                                     permits_dedupe$owner== "BOSTON MUNRCIPA" |
                                     permits_dedupe$owner== "BOSTON POLICE DEPARTMENT" |
                                     permits_dedupe$owner== "BOSTON PORT * S" |
                                     permits_dedupe$owner== "BOSTON PUBLIC HEALTH COMM" |
                                     permits_dedupe$owner== "BOSTON REDEVELO" |
                                     permits_dedupe$owner== "BOSTON REDEVELOPMENT AUTH" |
                                     permits_dedupe$owner== "BOSTON REDEVELP" |
                                     permits_dedupe$owner== "CITY OF BOSTON" |
                                     permits_dedupe$owner== "CITY OF BOSTON - DND" |
                                     permits_dedupe$owner== "CITY OF BOSTON - PUB FAC " |
                                     permits_dedupe$owner== "CITY OF BOSTON (REO)" |
                                     permits_dedupe$owner== "CITY OF BOSTON BY FCL" |
                                     permits_dedupe$owner== "CITY OF BOSTON PROP MGMT DEPT" |
                                     permits_dedupe$owner== "CITY OF BOSTON-GEORGE WHITE FUND" |
                                     permits_dedupe$owner== "COMMONWLTH OF M" |
                                     permits_dedupe$owner== "COMMWLTH OF MAS" |
                                     permits_dedupe$owner== "M B T A" |
                                     permits_dedupe$owner== "MASS BAY TRANSP" |
                                     permits_dedupe$owner== "MASS BAY TRANSPORTATION AUTH" |
                                     permits_dedupe$owner== "MASS PORT AUTHO" |
                                     permits_dedupe$owner== "MASS PORT AUTHORITY" |
                                     permits_dedupe$owner== "MASS TURNPIKE A" |
                                     permits_dedupe$owner== "MASS TURNPIKE AUTHORITY" |
                                     permits_dedupe$owner== "MASSACHUSETTS BAY TRANS AUTH" |
                                     permits_dedupe$owner== "MASSACHUSETTS PORT AUTHORITY" |
                                     permits_dedupe$owner== "MASSPORT AUTHOR" |
                                     permits_dedupe$owner== "MBTA" |
                                     permits_dedupe$owner== "MSS PORT AUTHOR" |
                                     permits_dedupe$owner== "COMMMONWEALTH O" |
                                     permits_dedupe$owner== "COMMONWEALTH FL" |
                                     permits_dedupe$owner== "COMMONWEALTH OF" |
                                     permits_dedupe$owner== "UNITED STATES OF AMER" |
                                     permits_dedupe$owner== "FEDERAL HOME LOAN MORTGAGE" |
                                     permits_dedupe$owner== "FEDERAL HOME LOAN MTG CORP" |
                                     permits_dedupe$owner== "FEDERAL MORTGAGE ASSOC" |
                                     permits_dedupe$owner== "FEDERAL NATIONAL MORTGAGE ASSO" |
                                     permits_dedupe$owner== "FEDERAL NATIONAL MTG ASSOC",1,0)


permits <- permits_dedupe

######### STEP I: ADDING IDs From Properties by merging #####

# properties for merging 
properties_merge <- properties %>%
  select(PID, GIS_ID, Land_Parcel_ID, TLID, Blk_ID_10, BG_ID_10, CT_ID_10,
         Blk_ID_20, BG_ID_20, CT_ID_20, NSA_Name, BRA_PD, X, Y) %>% #Get a subset of properties dataset
  rename(parcel_id = PID) %>% #rename column
  distinct(parcel_id, .keep_all = TRUE)  # Remove duplicates

# Merge with permits data
permits1 <- permits %>%
  left_join(properties_merge, by = "parcel_id")

# Split merged data into subsets based on missing Land_Parcel_ID and geo info
permits_full <- permits1[!is.na(permits1$Land_Parcel_ID) ,] #this is the base dataset
permits_missid <- permits1[is.na(permits1$Land_Parcel_ID) ,]
permits_missid_wgeo <- permits_missid[!is.na(permits_missid$long),] #will merge back in Step II
permits_missid_wogeo <- permits_missid[is.na(permits_missid$long),] #will merge back in Step IV

# Fill missing geo information in permits_full using coalesce
permits_full <- permits_full %>%
  mutate(
    long = coalesce(long, X),
    lat = coalesce(lat, Y)
  ) %>%
  select(-X, -Y)  # Remove unnecessary columns

########## STEP II: Spatial Join to obtain IDs ########


#delete the null ID columns
permits_missid_wgeo <- permits_missid_wgeo[,-c(29:41)]

# Convert to `sf` object with geometry from lat/long
permits_missid_wgeo <- st_as_sf(permits_missid_wgeo, coords = c("long", "lat"), crs = 4326)


#get a subset of landparcels for spatial join
parcels <- st_transform(landParcels_shp, 4326) 
parcels_merge <- parcels[,c(1,32:40)]
names(parcels_merge) <- c(names(permits_missid_wogeo[,c(30:39)]), "geometry")

#apply spatial join for obtaning missing IDs
permits_missid_wgeo2 <- st_join(permits_missid_wgeo,parcels_merge,join=st_nearest_feature)

# Add `GIS_ID` from `properties` based on `Land_Parcel_ID`
permits_missid_wgeo2 <- permits_missid_wgeo2 %>%
  mutate(GIS_ID = properties$GIS_ID[match(Land_Parcel_ID, properties$Land_Parcel_ID)])

# Drop geometry and reorder columns to match `permits_full`
permits_missid_wgeo2 <- permits_missid_wgeo2 %>%
  st_set_geometry(NULL) %>%
  select(all_of(names(permits_full)))

#merge back to permits_full
permits_full2 <- bind_rows(permits_full, permits_missid_wgeo2)

########### STEP III: Obtain Missing Observations From Last Year's Data ###########

# Get the missing permits from last year's data
permits_missing_23 <- permits_2023 %>%
  filter(!PermitNumber %in% permits_full2$PermitNumber) %>%
  distinct(PermitNumber, WORKTYPE, permittypedescr, description, NOTES,
           APPLICANT, DECLARED_VALUATION, total_fees, ISSUED_DATE, EXPIRATION_DATE,
           STATUS, owner, OCCUPANCY, sq_feet, .keep_all = TRUE)

# Add 2020 geographies to the missing cases
permits_missing_23 <- permits_missing_23 %>%
  mutate(
    Blk_ID_20 = properties$Blk_ID_20[match(GIS_ID, properties$GIS_ID)],
    BG_ID_20 = properties$BG_ID_20[match(GIS_ID, properties$GIS_ID)],
    CT_ID_20 = properties$CT_ID_20[match(GIS_ID, properties$GIS_ID)]
  )

# Convert date columns to POSIXct format
permits_missing_23 <- permits_missing_23 %>%
  mutate(
    ISSUED_DATE = as.POSIXct(ISSUED_DATE, format = "%Y-%m-%d %H:%M:%S"),
    EXPIRATION_DATE = as.POSIXct(paste(EXPIRATION_DATE, "00:00:00"), format = "%Y-%m-%d %H:%M:%S")
  )

# Rename columns for consistency and reorder for merging
permits_full2 <- permits_full2 %>%
  rename(X = long, Y = lat, parcel_num = parcel_id, NSA_NAME = NSA_Name)

varnames <- c("PermitNumber", "WORKTYPE", "permittypedescr", "description", "NOTES",
              "APPLICANT", "DECLARED_VALUATION", "total_fees", "ISSUED_DATE", "EXPIRATION_DATE",
              "STATUS", "owner", "OCCUPANCY", "sq_feet", "ADDRESS", "CITY", "STATE", "ZIP",
              "Property_ID", "GIS_ID", "parcel_num", "X", "Y", "Land_Parcel_ID", "TLID",
              "Blk_ID_10", "BG_ID_10", "CT_ID_10", "Blk_ID_20", "BG_ID_20", "CT_ID_20",
              "NSA_NAME", "BRA_PD", "newcon", "addition", "demo", "reno",
              "PermitDuration", "government")

# Select the necessary columns and merge
permits_full2 <- permits_full2 %>% select(all_of(varnames))
permits_missing_23 <- permits_missing_23 %>% select(all_of(varnames))

# Combine the datasets
permits_full3 <- bind_rows(permits_full2, permits_missing_23)

# Print the number of rows in the final dataset (for confirmation)
nrow(permits_full3)  # was 615,747 in 2024

############### STEP IV: Rbind the remaning subset in step I ###############
# Filter out records already included in `permits_full3` using `anti_join()`
permits_missid_wogeo2 <- permits_missid_wogeo %>%
  anti_join(permits_full3, by = "PermitNumber")

permits_missid_wogeo2 <- permits_missid_wogeo2[,-c(40:41)] # Remove unnecessary columns

# Rename columns for consistency
permits_missid_wogeo2 <- permits_missid_wogeo2 %>%
  rename(X = long, Y = lat, parcel_num = parcel_id, NSA_NAME = NSA_Name)

# Reorder columns to match `varnames`
permits_missid_wogeo2 <- permits_missid_wogeo2 %>%
  select(all_of(varnames))

# Combine the datasets
permits_full4 <- bind_rows(permits_full3, permits_missid_wogeo2)

# Print the number of rows in the final dataset (for confirmation)
nrow(permits_full4)  #was 616,038 in 2024


########## Check the final Data ##########
# List of columns to check
columns_to_check <- c("X", "Land_Parcel_ID", "BG_ID_20", "CT_ID_20", "GIS_ID")

# Function to print missing and non-missing statistics
check_missing <- function(col) {
  cat("Missing", col, ":", sum(is.na(permits_full4[[col]])), "\n")
  cat("Percentage with", col, ":", sum(!is.na(permits_full4[[col]])) / nrow(permits_full4), "\n")
}

# Apply the check function to each column
purrr::walk(columns_to_check, check_missing)



######### Save the Final Data ###############

write.csv(permits_full4, record_path,row.names=F) 

