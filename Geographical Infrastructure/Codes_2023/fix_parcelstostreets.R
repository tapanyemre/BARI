########### Manipulating Datasets ##########
street_census <- read.csv("~/Downloads/roads_fin_clust_2023_.csv")


# load datasets
parcel_final <- read.csv("~/Desktop/BARI/GI-2023/drive/Sanity Checks/Parcel_final_11082023_postsanitychecks_YET.csv")
LandParcels <- read.csv("GI-2023/drive/Sanity Checks/LandParcels.2023.csv")
properties <- read.csv("~/Desktop/BARI/GI-2023/drive/Sanity Checks/Properties.2023.csv")
pad_unit <- read.csv("~/Desktop/BARI/PA-2023/outputs2/others/PAD.Record.wUnit.2022.csv")
pad_cross <- read.csv("~/Desktop/BARI/PA-2023/outputs2/PADCross/PADCross.Record.2022.csv")
roads_final <- read.csv("GI-2023/drive/ParcelToStreets_final/Roads.2023_dec/roads_fin_clust_Dec2023.csv")

parcel

#get subset of datasets
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
pad_unit <- pad_unit %>% select(GIS_ID, PID, unit_N, unit_N_orig)


# Select only GIS_ID, PID, and Land_Parcel_ID from properties_sub
properties_sub_slim <- properties_sub %>% select(Land_Parcel_ID, GIS_ID, PID)

# Join the datasets
LandParcels_sub <- left_join(LandParcels_sub, properties_sub_slim, by = c("GIS_ID", "PID"))

# Join the datasets based on Land_Parcel_ID, keeping only desired columns
LandParcels_sub <- left_join(LandParcels_sub, parcel_final_sub[, c("Land_Parcel_ID", "property_N")], by = "Land_Parcel_ID")


parcels <-LandParcels_sub 


test_parcels <- read.csv("~/Desktop/BARI/GI-2023/drive/Sanity Checks/Parcel_final_11082023_postsanitychecks_YET.csv")

########### ADDING CENSUS 2020 BLOCK GROUPS ##########

#agregate by TLID
#parcels_sf <- st_as_sf(parcels, coords = c("X", "Y"), crs = 26986)
streets_bgs_20 <-aggregate(BG_ID_20~TLID, data=parcels, FUN=Mode)

#merge with streets from last year by TLID
streets_prefinal <- read.csv("~/Desktop/BARI/GI-2023/new datasets/Street Data/roads_fin_clust_2022_12132022.csv")
streets_census<-merge(streets_prefinal,streets_bgs_20,by='TLID',all.x=TRUE)
table(is.na(streets_census$BG_ID_20))

# Merge the two data frames with all.x = TRUE to keep all rows from streets_census

# Check the number of rows to ensure it matches the original streets_census

#extract centroids
roads_shp <- st_read("~/Desktop/BARI/GI-2023/new datasets/Street Data/Roads_2021.shp")
roads_centroid <- st_centroid(roads_shp)
plot(roads_centroid)

##import block groups 2020 .shp
cbg_2020 <- st_read("~/Desktop/BARI/PA-2023/base_data/Block_Groups_Boston_2020/Block_Groups_Boston_2020.shp")


##st_join centroids with blocks groups as "nearest_feature"
roads_centroid_trans <- st_transform(roads_centroid, st_crs(cbg_2020))
joined_data = st_join(roads_centroid_trans, cbg_2020, join = st_nearest_feature)

#### explore the datasets
plot(cbg_2020)
plot(roads_centroid_trans)
plot(joined_data['GEOID20'])
table(is.na(joined_data$GEOID20))
plot(st_geometry(joined_data[is.na(joined_data$GEOID20),]))
plot(joined_data[joined_data$Main==1,])
names(joined_data)
plot(st_geometry(joined_data))

##subset those with NA on BG_ID_20 in streets_census
#streets_census_subset = subset(streets_census, is.na(BG_ID_20))
streets_census_merged
##merge aggregate and spatial join census geographies into a single variable
streets_spjoined <- merge(streets_census_merged, joined_data[c("TLID", "GEOID20")], by='TLID', all.y=TRUE)
#View(streets_census[is.na(streets_census$BG_ID_20),])
streets_spjoined$BG_ID_20final<-ifelse(!is.na(streets_spjoined$BG_ID_20),
                                     streets_spjoined$BG_ID_20, 
                                     streets_spjoined$GEOID20)
##determine CT_ID based on all but the last digit of BG_ID
streets_spjoined$CT_ID_20final <- str_sub(streets_spjoined$BG_ID_20final,end = 11)



##rename the variables and finalize the version
streets_spjoined <- streets_spjoined %>%
  dplyr::select(-c(BG_ID_20,CT_ID_20,GEOID20)) # Deleting the columns "BG_ID_20" and "GEOID20"

# Renaming "BG_ID_final" to "BG_ID_20" and "CT_ID_final" to "CT_ID_20"
streets_spjoined <- streets_spjoined %>%
  dplyr::rename(BG_ID_20 = BG_ID_20final, CT_ID_20 = CT_ID_20final)

streets_last <- streets_spjoined %>%
  dplyr::select(-geometry)
# Save road files
# export also as csv
write.csv(streets_last,"roads_fin_clust_2023.csv", row.names = F)

# read streets as shapefile with geometry
roads <- st_read("~/Desktop/BARI/GI-2023/new datasets/Street Data/Roads_2021.shp")
roads <- roads[c("geometry", "TLID")] 
# # merge streets_prefinal with roads for having geometry
#streets_prefinal <- left_join(roads, streets_census, by="TLID") #, all.x = TRUE)
streets_last_shp <- left_join(streets_last, roads, by="TLID")
streets_last_shp <- st_as_sf(streets_last_shp)
#streets_prefinal <- st_as_sf(streets_prefinal)
#streets_prefinal <- st_as_sf(streets_census)
# export the new shapefile
## run this code in console; otherwise will never finish
st_write(streets_last_shp,"roads_fin_clust_2023.shp")
