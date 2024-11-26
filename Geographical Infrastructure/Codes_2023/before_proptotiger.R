LandParcels_2022 <- read.csv("~/Desktop/BARI/GI-2023/new datasets/LandParcels.2022/LandParcels.2022.csv")
prop23 <- read.csv("~/Desktop/BARI/GI-2023/outputs/PropertiesToParcels-091423_YET.csv")
prop22 <- read.csv("~/Desktop/BARI/GI-2023/new datasets/Last Year/Properties.2022.csv")
#take new observations by LAND_PARCEL_ID
prop23_new <- prop23[which(prop23$Land_Parcel_ID %in% setdiff(prop23$Land_Parcel_ID, LandParcels_2022$Land_Parcel_ID)),] 