## Create full 911 call record-level file (including BARI types and geographical information) ##

### EDIT ####
### THIS SECTION NEEDS TO BE EDITED BEFORE RUNNING THE CODE ###
# List of required packages
packages <- c("sp", "sf", "plyr", "lubridate", "dplyr", "here", "nngeo", "units")

# Install and load packages
lapply(packages, function(pkg) {
  if (!require(pkg, character.only = TRUE)) install.packages(pkg, dependencies = TRUE)
  library(pkg, character.only = TRUE, quietly = TRUE)
})

# Set global options
options(scipen = 15, digits = 10)

# Define custom negation operator
`%notin%` <- Negate(`%in%`)

# Define file paths using 'here' for consistency and flexibility
input_dir <- "~/BARI/911/Inputs"
common_event_path <- file.path(input_dir, "2023_ag_common.csv")
agency_event_path <- file.path(input_dir, "2023_ag_event.csv")
parcels_path <- file.path(input_dir, "LandParcels_2023.shp")
roads_path <- file.path(input_dir, "Roads.2023.shp")
blocks_path <- file.path(input_dir, "Blocks_Boston_2020_BARI.shp")
types_path <- file.path(input_dir, "911_Types_2014_to_2022.csv") 
boston_boundary_path <- file.path(input_dir, "City_of_Boston_Boundary_(Water_Included).shp")


# Load raw 911 data
common_event <- read.csv(common_event_path)
agency_event <- read.csv(agency_event_path)

# Load spatial data (parcels, roads, census blocks)
par <- st_read(parcels_path, quiet = TRUE)
road <- st_read(roads_path, quiet = TRUE)
cb <- st_read(blocks_path, quiet = TRUE)

# Define date for saving files (e.g., "Dec2023")
date <- "Dec2023"

# Define date and save file names
output_dir <- "~/BARI/911/Outputs"
date <- format(Sys.Date(), "(%m-%Y)")
record_name <- sprintf("911_Calls_From_Jan-Dec_2023-%s.csv", date)
record_path <- file.path(output_dir, record_name)
recordwll_name <- sprintf("911_Calls_From_Jan-Dec_2023_wll-%s.csv", date)
recordwll_path <- file.path(output_dir, recordwll_name)

# Load the 911 types file
typ <- read.csv(types_path)


## ****   END OF EDITING   **** ##


### PREPARE DATA ######

# Define custom negation operator
`%notin%` <- Negate(`%in%`)

### LOAD SHAPEFILES AND TRANSFORM PROJECTIONS

# Standardize column names for merging
colnames(par)[1] <- "Land_Parcel_ID"

# Filter out parcels with missing Land_Parcel_ID
par <- par[!is.na(par$Land_Parcel_ID), ]

# Define UTM projection (UTM Zone 19 for Boston)
utm_projection <- "+proj=utm +zone=19 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

# Check initial CRS and transform to UTM
st_crs(par)
st_crs(road)
st_crs(cb)

par_utm <- st_transform(par, utm_projection)
road_utm <- st_transform(road, utm_projection)
cb_utm <- st_transform(cb, utm_projection)

# Print a summary of transformed road data
table(nchar(road_utm$TLID))

# Load existing data set from BARI Dataverse
previous <- typ

# Load new datasets: common event (CE) and agency event (AE)
ce <- common_event
ae <- agency_event

### REMOVE DUPLICATES

# Function to remove duplicates and return cleaned data
remove_duplicates <- function(data, id_col) {
  dup_sum <- sum(duplicated(data[[id_col]], fromLast = TRUE) | duplicated(data[[id_col]], fromLast = FALSE))
  dup_ids <- unique(data[[id_col]][duplicated(data[[id_col]], fromLast = TRUE) | duplicated(data[[id_col]], fromLast = FALSE)])
  
  # Extract duplicate rows and remove additional duplicates
  dup_data <- data[data[[id_col]] %in% dup_ids, ]
  dup_data <- dup_data[!duplicated(dup_data), ]
  
  # Create cleaned dataset by combining non-duplicate and cleaned duplicate rows
  cleaned_data <- rbind.fill(data[!data[[id_col]] %in% dup_ids, ], dup_data)
  
  # Validation check
  stopifnot(length(unique(data[[id_col]])) == length(unique(cleaned_data[[id_col]])))
  return(cleaned_data)
}

# Apply duplicate removal to CE and AE datasets
ce <- remove_duplicates(ce, "eid")
ae <- remove_duplicates(ae, "eid")

# Save a backup copy of the original datasets
save.ce <- common_event
save.ae <- agency_event


#### REDUCE DATA ####

# Reduce 'ae' dataset to relevant columns
ae <- ae[, c(
  "eid", "ag_id", "num_1", "priority", "rev_num", "dgroup",
  "sdts", "ad_ts", "cdts", "ds_ts", "en_ts", "ar_ts", "tr_ts", "ta_ts", "xdts",
  "tycod", "typ_eng", "sub_tycod", "sub_eng"
)]

# Reduce 'ce' dataset to relevant columns
ce <- ce[, c(
  "eid", "rev_num", "ani_num_old", "dow", "cdts", "rev_dts", "udts",
  "call_sour_old", "clname_old", "clrnum_old", "cstr_add_old", "ccity_old", "cob_911_name",
  "cob_911_phone", "cob_911_addr", "estnum", "edirpre", "efeanme", "efeatyp", "edirsuf", "eapt",
  "earea", "emun", "zip", "ecompl", "eloc_fld1", "eloc_fld2", "eloc_fld3", "eloc_fld4",
  "xstreet1", "xstreet2", "x_cord", "y_cord", "fea_mslink", "hash",
  "loc_com", "loc_ver", "loi_event", "loi_inf", "loi_specsit", "parse_type"
)]



#### FORMAT DATA ####

# Create 'date' and 'year' variables for agency event data based on 'sdts'
ae$date <- parse_date_time(gsub("ED", "", as.character(ae$sdts)), "ymd HMS") 
ae$year <- year(ae$date)

# Standardize column names for common variables in both datasets
names(ae)[names(ae) == "cdts"] <- "a_cdts"
names(ce)[names(ce) == "cdts"] <- "c_cdts"
names(ae)[names(ae) == "rev_num"] <- "a_rev_num"
names(ce)[names(ce) == "rev_num"] <- "c_rev_num"

# Format type and subtype codes and their text descriptions
ae$tycod <- toupper(as.character(ae$tycod))
ae$sub_tycod <- toupper(as.character(ae$sub_tycod))
ae$typ_eng <- toupper(as.character(ae$typ_eng))
ae$sub_eng <- toupper(as.character(ae$sub_eng))

# Create a combined, finalized 'TYPE' column
ae$TYPE <- paste0(ae$tycod, ae$sub_tycod)


## CHECK IF THERE ARE NEW TYPES AND MERGE TYPE CATEGORIES WITH DATA ###

### GET NEW TYPE FILE #####

# Check if all types in ae exist in typ and print missing types if any are found
if (length(setdiff(ae$TYPE, typ$TYPE)) > 0) {
  stop(paste("The following types are missing from 'typ':", paste(setdiff(ae$TYPE, typ$TYPE), collapse = ", ")))
}

# Print new types found in ae that are not in typ
message("New types in AE data: ", paste(setdiff(ae$TYPE, typ$TYPE), collapse = ", "))

# Print types in typ that are missing from ae
message("Missing types from AE data: ", paste(setdiff(typ$TYPE, ae$TYPE), collapse = ", "))

# Check for new types in 'ae' not present in 'typ'
length(unique(ae$TYPE[!ae$TYPE %in% typ$TYPE]))  # Example: 6 in 2021 update, 23 in 2024
new.typ <- unique(ae$TYPE[!ae$TYPE %in% typ$TYPE])

# Create new 'ae.typ' dataframe for the new types
ae.typ <- data.frame(
  distinct(ae[ae$TYPE %in% new.typ, c("TYPE", "tycod", "typ_eng", "sub_tycod", "sub_eng")]),
  SocDis = 0, PrivateConflict = 0, Violence = 0, Guns = 0, Used = 0
)

# Combine with the existing 'typ' data
typs.new <- rbind.fill(typ, ae.typ)
typs.new <- typs.new[order(typs.new$TYPE), ]

# Update ecometrics categories based on 911 documentation
typs.new$SocDis <- ifelse(typs.new$TYPE %in% c("DISTRBDRUNKS", "DISTRBPANHAN", "IVPERLEWD"), 1, 0)
typs.new$Violence <- ifelse(typs.new$TYPE %in% c("AB===>>>", "ABIP", "ABRPT", "ARMROBDEFAULT", "EDP2", "FIGHTDEFAULT", "PKNIFEDEFAULT"), 1, 0)
typs.new$PrivateConflict <- ifelse(typs.new$TYPE %in% c("LANTENDEFAULT"), 1, 0)
typs.new$Guns <- ifelse(typs.new$TYPE %in% c("ABDWIP", "PERGUNDEFAULT", "SHOTSDEFAULT", "PSHOTDEFAULT", "FDWEAPGUN"), 1, 0)

# New 2024 categories
typs.new$Larceny <- ifelse(typs.new$TYPE %in% unique(ae$TYPE[grepl('^LARC', ae$TYPE)]), 1, 0)
typs.new$Burglary <- ifelse(typs.new$TYPE %in% unique(ae$TYPE[grepl('^BE', ae$TYPE)]), 1, 0)
typs.new$Vandalism <- ifelse(typs.new$TYPE %in% unique(ae$TYPE[grepl('^VAN', ae$TYPE)]), 1, 0)
typs.new$Robbery <- ifelse(typs.new$TYPE %in% c(unique(ae$TYPE[grepl('^ROB', ae$TYPE)]), 'ARMROBDEFAULT'), 1, 0)

# Create frequency column for 2023 and initialize year tracking columns
types <- typs.new$TYPE
typs.new$Frequency_2023 <- NA
typs.new$yr.intro2 <- NA
typs.new$last.yr2 <- NA

# Populate frequency and year information for 2023 only
for (i in seq_along(types)) {
  type <- types[i]
  typs.new$yr.intro2[i] <- ifelse(is.na(typs.new$yr.intro[i]),
                                  min(d911$year[d911$TYPE == type]),
                                  typs.new$yr.intro[i])
  typs.new$last.yr2[i] <- ifelse(type %in% d911$TYPE,
                                 max(d911$year[d911$TYPE == type]),
                                 typs.new$last.yr[i])
  typs.new$Frequency_2023[i] <- ifelse(type %in% d911$TYPE,
                                       length(unique(d911$eid[d911$TYPE == type & d911$year == 2023])),
                                       0)
}

# Remove old year columns and rename new ones
typs.new$yr.intro <- NULL
typs.new$last.yr <- NULL
typs.new <- typs.new %>%
  dplyr::rename(last.yr = last.yr2, yr.intro = yr.intro2)

# Populate 'Used' column based on ecometric categories
typs.new$Used <- ifelse(
  typs.new$SocDis == 1 | typs.new$PrivateConflict == 1 | typs.new$Violence == 1 |
    typs.new$Guns == 1 | typs.new$Larceny == 1 | typs.new$Burglary == 1 |
    typs.new$Vandalism == 1 | typs.new$Robbery == 1, 1, 0
)

# Reorder columns and keep only relevant ones
order <- c("TYPE", "tycod", "typ_eng", "sub_tycod", "sub_eng",
           "Frequency_2023", "yr.intro", "last.yr",
           "PrivateConflict", "Violence", "SocDis", "Guns",
           "Larceny", "Burglary", "Vandalism", "Robbery", "Used")
typs.new <- typs.new[, order]

# Save the updated types file
write.csv(typs.new, types_path, row.names = FALSE)



### MERGE TYPE CATEGORIES WITH DATA #########

# Merge 'ae' data with 'typs.new' to include ecometric categories
ae <- merge(
  ae,
  typs.new[, c("TYPE", "SocDis", "PrivateConflict", "Violence", "Guns", "Larceny", "Burglary", "Vandalism", "Robbery")],
  by = "TYPE",
  all.x = TRUE
)


### ADD GEOGRAPHICAL INFORMATION #########


# Assign NA to zero coordinates in 'ce' geographic data
stopifnot(sum(is.na(ce$x_cord)) == 0, sum(is.na(ce$y_cord)) == 0)
ce$y_cord[ce$y_cord == 0] <- NA
ce$x_cord[ce$x_cord == 0] <- NA

# Filter for rows with valid geographic information
ce.togeo <- ce[!is.na(ce$x_cord) & !is.na(ce$y_cord), c("eid", "x_cord", "y_cord")]

# Correct coordinates and transform to spatial data
ce.togeo$x_fix <- ce.togeo$x_cord / 100
ce.togeo$y_fix <- ce.togeo$y_cord / 100
ce.togeo$x_cord <- NULL
ce.togeo$y_cord <- NULL

# Convert to spatial data and transform to WGS84 (EPSG: 4326)
ce.togeo.proj <- st_as_sf(ce.togeo, coords = c("x_fix", "y_fix"), crs = 2249)
ce.togeo.proj <- st_transform(ce.togeo.proj, 4326)


plot(ce.togeo.proj[1])

#### REMOVE POINTS OUTSIDE A 50M BUFFER AROUND BOSTON CITY BOUNDARY #########

# Read Boston boundary shapefile and create a 50m buffer
boston_boundary <- st_read(boston_boundary_path)
buffer_50m <- st_buffer(boston_boundary, dist = 50)
buffer_50m <- st_transform(buffer_50m, st_crs(ce.togeo.proj))

# Filter points that are within the 50m buffer
within_buffer <- apply(st_within(ce.togeo.proj, buffer_50m, sparse = FALSE), 1, any)
ce.togeo.proj <- ce.togeo.proj[within_buffer, ]

#### JOIN PARCEL DATA TO POINTS ##########

# Transform 'par' to match CRS of 'ce.togeo.proj'
par <- st_transform(par, st_crs(ce.togeo.proj))
names(par)[1] <- "Land_Parcel_ID"
par_join <- par[, c("Land_Parcel_ID", "TLID", "B_ID_10", "BG_ID_1", "CT_ID_1", "B_ID_20", "BG_ID_2", "CT_ID_2", "NSA_Nam", "BRA_PD")]

# Join parcel data to 'ce.togeo.proj' using nearest feature with a maximum distance of 100 meters
geo.data_par <- st_join(ce.togeo.proj, par_join, join = st_nearest_feature, maxdist = 100)


#### MERGE GEO DATA WITH ORIGINAL ########


# Remove geometry and merge with original 'ce.togeo.proj' by 'eid'
geo.data_par$geometry <- NULL
ce.geo <- merge(ce.togeo.proj, geo.data_par, by = "eid", all.x = TRUE)





### FINAL STEP: MERGE AGENCY EVENT AND COMMON EVENT DATA ######


# Ensure only 'eid' overlaps between 'ae' and 'ce.geo.full'
ce.geo.full <- ce.geo
stopifnot(sum(colnames(ae) %in% colnames(ce.geo.full)) == 1)

# Merge agency event and geographic data
data911 <- merge(ae, ce.geo.full, by = "eid", all = TRUE)

# Parse 'sdts' date column and order by date
data911$date <- parse_date_time(gsub("ED", "", as.character(data911$sdts)), "ymd HMS")
data911 <- data911[order(data911$date), ]

# Add indicator for pre-system change (2014 and earlier)
data911$pre_sys_change <- 0

# Handle inconsistencies in 'NSA_Name'if exists
# data911$NSA_Name <- ifelse(is.na(data911$NSA_Name), data911$NSA_NAME, data911$NSA_Name)
# data911$NSA_NAME <- NULL


#### RENAME COLUMNS AND EXTRACT COORDINATES ####


# Rename old columns to standardize identifiers
names(data911)[names(data911) == "B_ID_10"] <- "Blk_ID_10"
names(data911)[names(data911) == "BG_ID_1"] <- "BG_ID_10"
names(data911)[names(data911) == "CT_ID_1"] <- "CT_ID_10"
names(data911)[names(data911) == "B_ID_20"] <- "Blk_ID_20"
names(data911)[names(data911) == "BG_ID_2"] <- "BG_ID_20"
names(data911)[names(data911) == "CT_ID_2"] <- "CT_ID_20"
names(data911)[names(data911) == "NSA_Nam"] <- "NSA_NAME"

# Extract coordinates from geometry and add to data
coordinates <- st_coordinates(st_as_sf(data911))
data911$x_cord <- coordinates[, 1]  # Longitude
data911$y_cord <- coordinates[, 2]  # Latitude
data911$geometry <- NULL

#### SUBSET DATA FOR 2023 AND TRIM COLUMNS ####


# Filter data for the year 2023
data911_2023 <- data911[data911$year == 2023, ]

# Define columns to keep for final output
trim_cols <- c(
  "eid", "date", "year", "pre_sys_change", "TYPE", "SocDis", "Guns",
  "PrivateConflict", "Violence", "Larceny", "Burglary", "Vandalism", "Robbery",
  "Land_Parcel_ID", "TLID", "Blk_ID_10", "BG_ID_10", "CT_ID_10",
  "Blk_ID_20", "BG_ID_20", "CT_ID_20", "x_cord", "y_cord"
)

# Subset and remove duplicates
d911_2023 <- data911_2023[, trim_cols]
d911_2023 <- d911_2023[!duplicated(d911_2023), ]

#### SAVE THE CLEANED 2023 DATA ##############

# Save the full 911 calls dataset for 2023
write.csv(d911_2023, record_path, row.names = FALSE)

# Filter data with minimum one geographic characteristic
data911_ll <- d911_2023[
  !is.na(d911_2023$CT_ID_10) | !is.na(d911_2023$BG_ID_10) | !is.na(d911_2023$Blk_ID_10) |
    !is.na(d911_2023$CT_ID_20) | !is.na(d911_2023$BG_ID_20) | !is.na(d911_2023$Blk_ID_20) |
    !is.na(d911_2023$Land_Parcel_ID) | !is.na(d911_2023$TLID),
]

# Remove duplicates and save the filtered data with latitude and longitude
data911_ll <- data911_ll[!duplicated(data911_ll), ]
write.csv(data911_ll, recordwll_name, row.names = FALSE)
