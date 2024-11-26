View(streets_last)

#replace 0 columns with NAs
streets_last[which(streets_last$BG_ID_10==0),"BG_ID_10"] <- NA
streets_last[which(streets_last$CT_ID_10==0),"CT_ID_10"] <- NA
streets_last[which(streets_last$CT_ID_10==0),"CT_ID_10"]
streets_last[which(streets_last$BG_ID_10==0),"BG_ID_10"]
#check the number of NAs
sum(is.na(streets_last$CT_ID_10))
sum(is.na(streets_last$BG_ID_10))




#check parcel_N
summary(streets_last$parcel_N)
names(streets_last)
streets_last <- dplyr::rename(streets_last, parcel_N_old = parcel_N)
streets_fixed <- merge(streets_last, streets_prefinal[c("TLID", "parcel_N")], by="TLID")
names(streets_fixed)
summary(streets_fixed)
streets_fixed <- dplyr::select(streets_fixed, -parcel_N_old)
# fix order of columns
new_order <- c("TLID", "STATEFP", "COUNTYFP", "TFIDL", "TFIDR", "MTFCC", "FULLNAM", "SMID", "LFROMADD", 
               "LTOADD", "RFROMADD", "RTOADD", "ZIPL", "ZIPR", "Length", "CLASS", "RDTYPE", "Main", 
               "DeadEnd", "Cluster", "unit_N", "unit_N_orig", "property_N", "parcel_N", "BG_ID_10", "CT_ID_10", 
               "BG_ID_20", "CT_ID_20")
# Reordering the columns in the data frame
streets_fixed <- streets_fixed[, new_order]

#do this with earlier version
street_kmeans <- read.csv("~/Desktop/BARI/GI-2023/drive/Street_Kmeans_fix/roads_fin_clust_2023_kmeans.csv")
streets_fixed_recent <- merge(streets_last, street_kmeans[c("TLID", "parcel_N")], by="TLID")
streets_fixed_recent <- dplyr::rename(streets_fixed_recent, property_N_old = property_N)
streets_fixed_recent <- merge(streets_fixed_recent, street_kmeans[c("TLID", "property_N")], by="TLID")
names(streets_fixed_recent)
summary(streets_fixed_recent)
streets_fixed_recent <- dplyr::select(streets_fixed_recent, -parcel_N_old)
streets_fixed_recent <- dplyr::select(streets_fixed_recent, -property_N_old)
# fix order of columns
new_order <- c("TLID", "STATEFP", "COUNTYFP", "TFIDL", "TFIDR", "MTFCC", "FULLNAM", "SMID", "LFROMADD", 
               "LTOADD", "RFROMADD", "RTOADD", "ZIPL", "ZIPR", "Length", "CLASS", "RDTYPE", "Main", 
               "DeadEnd", "Cluster", "unit_N", "unit_N_orig", "property_N", "parcel_N", "BG_ID_10", "CT_ID_10", 
               "BG_ID_20", "CT_ID_20")
# Reordering the columns in the data frame
streets_fixed_recent <- streets_fixed_recent[, new_order]

write.csv(streets_fixed_recent,"roads_fin_clust_2023.csv", row.names = F)

# read streets as shapefile with geometry
roads <- st_read("~/Desktop/BARI/GI-2023/new datasets/Street Data/Roads_2021.shp")
roads <- roads[c("geometry", "TLID")] 
# # merge streets_prefinal with roads for having geometry
#streets_prefinal <- left_join(roads, streets_census, by="TLID") #, all.x = TRUE)
streets_fixed_shp <- left_join(streets_fixed_recent, roads, by="TLID")
streets_fixed_shp <- st_as_sf(streets_fixed_shp)
#streets_prefinal <- st_as_sf(streets_prefinal)
#streets_prefinal <- st_as_sf(streets_census)
# export the new shapefile
## run this code in console; otherwise will never finish
st_write(streets_fixed_shp,"roads_fin_clust_2023.shp")

