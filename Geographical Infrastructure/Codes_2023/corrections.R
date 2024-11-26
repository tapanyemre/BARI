Parcel_final_09302023_YET <- read.csv("GI-2023/outputs/Parcel_final_09302023_YET.csv")
Parcel_final_09302023_YET$owner_address <- toupper(Parcel_final_09302023_YET$owner_address)
Parcel_final_09302023_YET$full_address <- toupper(Parcel_final_09302023_YET$full_address)
write.csv(Parcel_final_09302023_YET, paste0("~/Desktop/BARI/GI-2023/drive/","Parcel_final_10032023_YET",".csv"),row.names=F)

parcels_updatedTLID_092123_YET <- read.csv("GI-2023/outputs/parcels_updatedTLID_092123_YET.csv")
parcels_updatedTLID_092123_YET$ST_NAME <- toupper(parcels_updatedTLID_092123_YET$ST_NAME)
parcels_updatedTLID_092123_YET$OWNER.MAIL.ADDRESS <- toupper(parcels_updatedTLID_092123_YET$OWNER.MAIL.ADDRESS)
parcels_updatedTLID_092123_YET$FULLNAME <- toupper(parcels_updatedTLID_092123_YET$FULLNAME)
write.csv(parcels_updatedTLID_092123_YET, paste0("~/Desktop/BARI/GI-2023/drive/","parcels_updatedTLID_10032023_YET",".csv"),row.names=F)

properties_geo_09302023_YET <- read.csv("GI-2023/outputs/properties_geo_09302023_YET.csv")
properties_geo_09302023_YET$ST_NAME <- toupper(properties_geo_09302023_YET$ST_NAME)
write.csv(properties_geo_09302023_YET, paste0("~/Desktop/BARI/GI-2023/drive/","properties_geo_10032023_YET",".csv"),row.names=F)

PropertiesToParcels_091423_YET <- read.csv("GI-2023/outputs/PropertiesToParcels-091423_YET.csv")
PropertiesToParcels_091423_YET$ST_NAME <- toupper(PropertiesToParcels_091423_YET$ST_NAME)
PropertiesToParcels_091423_YET$fulladress <- toupper(PropertiesToParcels_091423_YET$fulladress)
write.csv(PropertiesToParcels_091423_YET, paste0("~/Desktop/BARI/GI-2023/drive/","PropertiesToParcels_10032023_YET",".csv"),row.names=F)

Parcel_final_20230921_YET_postsanitychecks <- read.csv("GI-2023/outputs/Sanity Checks/Parcel_final_20230921_YET_postsanitychecks.csv")
Parcel_final_20230921_YET_postsanitychecks$owner_address <- toupper(Parcel_final_20230921_YET_postsanitychecks$owner_address)
Parcel_final_20230921_YET_postsanitychecks$full_address <- toupper(Parcel_final_20230921_YET_postsanitychecks$full_address)
write.csv(Parcel_final_20230921_YET_postsanitychecks, paste0("~/Desktop/BARI/GI-2023/drive/","Parcel_final_10032023_YET_postsanitychecks",".csv"),row.names=F)

parcels_updatedTLID_postsanitycheck_20230921 <- read.csv("GI-2023/outputs/Sanity Checks/parcels_updatedTLID_postsanitycheck_20230921.csv")
parcels_updatedTLID_postsanitycheck_20230921$ST_NAME <- toupper(parcels_updatedTLID_postsanitycheck_20230921$ST_NAME)
parcels_updatedTLID_postsanitycheck_20230921$FULLNAME <- toupper(parcels_updatedTLID_postsanitycheck_20230921$FULLNAME)
write.csv(parcels_updatedTLID_postsanitycheck_20230921, paste0("~/Desktop/BARI/GI-2023/drive/","parcels_updatedTLID_postsanitycheck_10032023_YET",".csv"),row.names=F)

properties_geo_20230921_YET_postsanitychecks <- read.csv("GI-2023/outputs/Sanity Checks/properties_geo_20230921_YET_postsanitychecks.csv")
properties_geo_20230921_YET_postsanitychecks$ST_NAME <- toupper(properties_geo_20230921_YET_postsanitychecks$ST_NAME)
write.csv(properties_geo_20230921_YET_postsanitychecks, paste0("~/Desktop/BARI/GI-2023/drive/","properties_geo_10032023_YET_postsanitychecks",".csv"),row.names=F)

