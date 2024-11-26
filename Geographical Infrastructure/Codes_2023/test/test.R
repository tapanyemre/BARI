roads <- read.csv("roads_fin_clust_Dec2023.csv")

summary(parcels$unit_N_orig)


pad_unit <- read.csv("~/Desktop/BARI/PA-2023/outputs2/others/PAD.Record.wUnit.2022.csv")
parcels2 <-read.csv("~/Desktop/BARI/GI-2023/drive/Sanity Checks/Parcel_final_11082023_postsanitychecks_YET.csv")
parcels <- read.csv("~/Desktop/BARI/GI-2023/drive/Sanity Checks/LandParcels.2023.csv")
prop <- read.csv("~/Desktop/BARI/GI-2023/drive/Sanity Checks/Properties.2023.csv")
parcels3 <- read.csv("~/Desktop/BARI/GI-2023/drive/Sanity Checks/Parcel_final_11082023_postsanitychecks_YET.csv")

prop_sub <- prop %>% select(Land_Parcel_ID, unit_N, unit_N_orig)
parcels_sub <- parcels2 %>% select(-c(unit_N, unit_N_orig))
parcels_m <- left_join(parcels_sub, prop_sub, by="Land_Parcel_ID")
parcels <- parcels_m


landparcels <- read.csv("~/Desktop/BARI/GI-2023/drive/Sanity Checks/LandParcels.2023.csv")
landparcels_sub <- landparcels %>% select( "TLID", "CT_ID_10", "BG_ID_10","CT_ID_20", "BG_ID_20")
landparcels_sf <- st_as_sf(landparcels, coords = c("X", "Y"), crs = 26986)
parcels_shp <- st_as_sf(landparcels, coords = c("X", "Y"), crs = 26986)
test <- st_read("~/Desktop/BARI/GI-2023/new datasets/Parcel Datasets/Parcels_2023/parcels_2023.shp")
test <- read.csv("~/Desktop/BARI/GI-2023/new datasets/Parcel Datasets/Parcels_2023.csv")
test <- read.csv("~/Desktop/BARI/GI-2023/new datasets/Street Data/roads_fin_clust_2022_12132022.csv")




