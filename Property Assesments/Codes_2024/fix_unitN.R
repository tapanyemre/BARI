LandParcels2023_1 <- read.csv("~/Downloads/LandParcels2023 (1).csv")
landparcels2023 <- read.csv("~/Downloads/LandParcels.2023 (1).csv")
landparcels2021 <- read.csv("~/Downloads/Land.Parcels.2021.csv")
Properties_2021 <- read.csv("~/Downloads/Properties.2021.csv")
Properties_2023_1 <- read.csv("~/Downloads/Properties.2023.csv")
names(landparcels2023)

summary(landparcels2023$New_unit)
properties_match <- Properties_2023_1[match(LandParcels2023_1$Land_Parcel_ID, Properties_2023_1$PID), c("PID", "unit_N_orig")] 
properties_match <- rename(properties_match, "Land_Parcel_ID" = "PID")

order <- names(LandParcels2023_1)
order
LandParcels2023_1 <- select(LandParcels2023_1, -unit_N_orig)
LandParcels.2023 <- left_join(LandParcels2023_1, properties_match, by="Land_Parcel_ID") 
LandParcels.2023 <- LandParcels.2023[,order]
write.csv(LandParcels.2023, "/Users/yet/Desktop/BARI/GI-2023/Outputs_Feb24/LandParcels.2023.csv")
names(LandParcels.2023)
library(sf)
LandParcels.2023_shp <- st_as_sf(LandParcels.2023, coords = c("X", "Y"), crs = 26986)
st_write(LandParcels.2023_shp, "LandParcels.2023",
         driver = "ESRI Shapefile")
PADCross_Record_2022 <- read.csv("~/Desktop/BARI/PA-2023/outputs_Feb24/PADCross.Record.2022.csv")
(mean(is.na(PADCross_Record_2022$unit_N_orig)))
