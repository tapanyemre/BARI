parcel_final <- read.csv("GI-2023/drive/Sanity Checks/Parcel_final_11082023_postsanitychecks_YET.csv")
LandParcels <- read.csv("GI-2023/drive/Sanity Checks/LandParcels.2023.csv")
properties <- read.csv("GI-2023/drive/Sanity Checks/Properties.2023.csv")
pad_unit <- read.csv("PA-2023/outputs2/others/PAD.Record.wUnit.2022.csv")
pad_cross <- read.csv("PA-2023/outputs2/PADCross/PADCross.Record.2022.csv")
roads_final <- read.csv("GI-2023/drive/ParcelToStreets_final/Roads.2023_dec/roads_fin_clust_Dec2023.csv")
write_csv(pad_cross, "~/Downloads/PADCross.Record.2022(new).csv")
roads_last <- read.csv("~/Desktop/BARI/GI-2023/new datasets/Street Data/roads_fin_clust_2022_12132022.csv")
properties22 <- read.csv("~/Downloads/dataverse/GI-2022/Properties.2022.csv")
LandParcels22 <- read.csv("~/Downloads/dataverse/GI-2022/LandParcels.2022.csv")
roads22 <- read.csv("~/Downloads/dataverse/GI-2022/Roads.2022.csv")
street_segments <- st_read("~/Desktop/BARI/GI-2023/new datasets/Street Data/Boston_Street_Segments/Boston_Street_Segments.shp")
roads_23 <- st_read("~/Desktop/BARI/GI-2023/drive/ParcelToStreets_final/Roads.2023_dec/roads_fin_clust_Dec2023.shp")
census_20 <- st_read("~/Desktop/BARI/GI-2023/new datasets/CENSUS2020TIGERROADS_SHP/CENSUS2020TIGERROADS_ARC_GC.shp")


parcel_final_sub <- parcel_final %>% select(Land_Parcel_ID, property_N, unit_N,
                                            unit_N_orig, X, Y, TLID, 
                                            BG_ID_10, CT_ID_10,
                                            BG_ID_20, CT_ID_20)
LandParcels_sub <- LandParcels %>% select(GIS_ID, PID, unit_N, unit_N_orig, 
                                          TLID, X, Y,
                                          BG_ID_10, CT_ID_10,
                                          BG_ID_20, CT_ID_20)
properties_sub <- properties %>% select(Land_Parcel_ID, GIS_ID, PID, 
                                        unit_N, unit_N_orig, 
                                        TLID, X, Y,
                                        BG_ID_10, CT_ID_10,
                                        BG_ID_20, CT_ID_20)
pad_unit <- pad_unit %>% select(GIS_ID, PID, unit_N, unit_N_orig, )


# Select only GIS_ID, PID, and Land_Parcel_ID from properties_sub
properties_sub_slim <- properties_sub %>% select(Land_Parcel_ID, GIS_ID, PID)

# Join the datasets
LandParcels_sub <- left_join(LandParcels_sub, properties_sub_slim, by = c("GIS_ID", "PID"))

# Join the datasets based on Land_Parcel_ID, keeping only desired columns
LandParcels_sub <- left_join(LandParcels_sub, parcel_final_sub[, c("Land_Parcel_ID", "property_N")], by = "Land_Parcel_ID")


parcels <-LandParcels_sub 
