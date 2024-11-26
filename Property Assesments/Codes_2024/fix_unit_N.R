#parcel_final_v <-  read.csv("~/Desktop/BARI/GI-2023/drive/Sanity Checks/Parcel_final_11082023_postsanitychecks_YET.csv")
options(scipen = 999)
#dataframe$column <- format(dataframe$column, scientific = FALSE)


#pad_unit <- read.csv("~/Desktop/BARI/PA-2023/outputs2/others/PAD.Record.wUnit.2022.csv")
#pad_cross <- read.csv("~/Desktop/BARI/PA-2023/outputs2/PADCross/PADCross.Record.2022.csv")
#property_unitN <- as.data.frame(property_dt[,c("PID", "UNIT_NUM")])
#pad_unit <- as.data.frame(pad_unit[,c("PID", "UNIT_NUM", "unit_N_orig")])
#pad_cross$unit_N_org <- ifelse(pad_cross$UNIT_NUM == "", NA, pad_cross$unit_N)

#### FIX PAD_CROSS
pad_cross <- read.csv("PA-2023/Assesments_Dec22/PADCross.Record.2022.csv")
colorder <- names(pad_cross)
pad_cross$unit_N_org <- ifelse(pad_cross$UNIT_NUM == "", NA, pad_cross$unit_N)
pad_cross <- dplyr::select(pad_cross, -c("unit_N_orig")) 
pad_cross <- dplyr::rename(pad_cross, unit_N_orig = unit_N_org)
# Reordering the columns in the data frame
pad_cross <- pad_cross[, colorder]
write.csv(pad_cross, "~/Desktop/BARI/PA-2023/outputs_Feb24/PADCross.Record.2022.csv", row.names=F)

### FIX PAD_UNIT
pad_unit <- read.csv("~/Downloads/PAD.Record.wUnit11012023 (1).csv")
colorder <- names(pad_unit)
pad_unit$unit_N_org <- ifelse(pad_unit$UNIT_NUM == "", NA, pad_unit$unit_N)
pad_unit <- dplyr::select(pad_unit, -c("unit_N_orig")) 
pad_unit <- dplyr::rename(pad_unit, unit_N_orig = unit_N_org)
# Reordering the columns in the data frame
pad_unit <- pad_unit[, colorder]
write.csv(pad_unit, "~/Desktop/BARI/PA-2023/outputs_Feb24/PAD.Record.wUnit.2022.csv", row.names=F)

mean(is.na(pad_cross$unit_N_orig))
### FIX PROPERTIES
properties <- read.csv("GI-2023/GI_drive/Properties/Properties.2023.csv")
colorder <- names(properties)
properties$unit_N_org <- ifelse(properties$UNIT_NUM == "", NA, properties$unit_N)
properties <- dplyr::select(properties, -c("unit_N_orig")) 
properties <- dplyr::rename(properties, unit_N_orig = unit_N_org)
# Reordering the columns in the data frame
properties <- properties[, colorder]
write.csv(properties, "~/Desktop/BARI/GI-2023/Outputs_Feb24/Properties.2023.csv", row.names=F)

### FIX Parcels
parcels <- read.csv("~/Desktop/BARI/GI-2023/drive/LandParcels_2024/LandParcels2023.csv")
colorder <- names(parcels)
joint_parcel <- properties[,c("Land_Parcel_ID", "unit_N_orig")]
joint_parcel <- joint_parcel[!duplicated(joint_parcel$Land_Parcel_ID), ]
parcels$unit_N_org <- ifelse(parcels$UNIT_NUM == "", NA, parcels$unit_N)
parcels <- dplyr::select(parcels, -c("unit_N_orig")) 
parcels <- dplyr::rename(parcels, unit_N_orig = unit_N_org)
# Reordering the columns in the data frame
parcels <- parcels[, colorder]
write.csv(parcels, "~/Desktop/BARI/GI-2023/Outputs_Feb24/LandParcels2023.csv", row.names=F)


### FIX ROADS
roads2023 <- read.csv("GI-2023/GI_drive/Roads/roads_fin_clust_2023.csv")
joint_road <- pad_cross[,c("TLID", "UNIT_NUM")]
joint_road <- joint_road[unique(joint_road$TLID),]
roads2023_wun <- left_join(roads2023, joint, by = "TLID")
colorder <- names(read.csv("GI-2023/GI_drive/Roads/roads_fin_clust_2023.csv"))
roads2023$unit_N_org <- ifelse(pad_unit$UNIT_NUM == "", NA, pad_unit$unit_N)
roads2023 <- dplyr::select(pad_unit, -c("unit_N_orig")) 
roads2023 <- dplyr::rename(pad_unit, unit_N_orig = unit_N_org)
# Reordering the columns in the data frame
roads2023 <- roads2023[, colorder]
write.csv(roads2023, "~/Desktop/BARI/GI-2023/Outputs_Feb24/Roads.2023.csv", row.names=F)






