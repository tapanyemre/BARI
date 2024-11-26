#latest changes in PADunit

#read csvs
PADUnit <- read.csv("PA-2023/outputs2/PAD.Record.wUnit11012023.csv") #done
PropertiesToParcels <- read.csv("GI-2023/drive/PropertiesToParcels_10032023_YET.csv") #done
parcels_updatedTLID <- read.csv("GI-2023/drive/parcels_updatedTLID_postsanitycheck_10032023_YET.csv") #done
properties_geo <- read.csv("GI-2023/drive/properties_geo_10032023_YET_postsanitychecks.csv") #done
Parcel_final <- read.csv("GI-2023/drive/Parcel_final_10032023_YET_postsanitychecks.csv") #done


#flip coordinates on Parcels_updatedTLID
tmp <- parcels_updatedTLID$X
parcels_updatedTLID$X <- parcels_updatedTLID$Y
parcels_updatedTLID$Y <- tmp
rm(tmp)
#match unit_N
parcels_updatedTLID$unit_N <- PADUnit$unit_N[match(parcels_updatedTLID$PID,PADUnit$PID)]


#flip coordinates on Parcels_updatedTLID
tmp <- properties_geo$X
properties_geo$X <- properties_geo$Y
properties_geo$Y <- tmp
rm(tmp)
#match unit_N
properties_geo$unit_N <- PADUnit$unit_N[match(properties_geo$PID,PADUnit$PID)]

#flip coordinates on Parcels_updatedTLID
tmp <- Parcel_final$X
Parcel_final$X <- Parcel_final$Y
Parcel_final$Y <- tmp
rm(tmp)
#match unit_N
Parcel_final$unit_N <- PADUnit$unit_N[match(Parcel_final$Land_Parcel_ID,PADUnit$PID)]


#write csvs
write.csv(parcels_updatedTLID, "~/Desktop/BARI/GI-2023/drive/Sanity Checks/parcels_updatedTLID_postsanitycheck_11082023_YET.csv", row.names=F)
write.csv(properties_geo, "~/Desktop/BARI/GI-2023/drive/Sanity Checks/properties_geo_11082023_postsanitychecks_YET.csv", row.names=F)
write.csv(Parcel_final, "~/Desktop/BARI/GI-2023/drive/Sanity Checks/Parcel_final_11082023_postsanitychecks_YET.csv", row.names=F)
