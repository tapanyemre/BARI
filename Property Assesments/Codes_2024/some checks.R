props_2023 <- read.csv("~/Desktop/BARI/GI-2023/new datasets/Property Assesments/fy2023-property-assessment-data.csv")
props_2023_unit <- read.csv("~/Desktop/BARI/PA-2023/outputs/PAD.Record.wUnit09082023.csv")
props_2023_parcel <- read.csv("~/Desktop/BARI/GI-2023/outputs/PropertiesToParcels-090823_YET.csv")
props_2022 <- read.csv("~/Desktop/BARI/GI-2023/new datasets/Property Assesments/fy2022pa-4.csv")
props_2022_final <- read.csv("~/Desktop/BARI/GI-2023/new datasets/Last Year/Properties.2022.csv")
LandParcels_2022 <- read.csv("~/Desktop/BARI/GI-2023/new datasets/LandParcels.2022/LandParcels.2022.csv")
streets_2023 <- read.csv("~/Downloads/Live_Street_Address_Management_(SAM)_Addresses.csv")
census2020 <- st_read("~/Desktop/BARI/GI-2023/new datasets/CENSUS2020TIGERROADS_SHP/CENSUS2020TIGERROADS_ARC_GC.shp")
table(props_2023$PID %in% props_2022$PID)
table(props_2022$PID %in% props_2023$PID)
streets_23 <- read





props_2022_final %>%
  group_by(TLID) %>%
  summarise(n_distinct(ST_NAME))%>%
  View()

st22 <- props_2022_final %>%
  group_by(TLID) %>%
  summarise(n_distinct(ST_NAME))

PAD_unit %>%
  group_by(TLID) %>%
  summarise(n_distinct(ST_NAME))%>%
  View()
st23 <- PAD_unit %>%
  group_by(TLID) %>%
  summarise(n_distinct(ST_NAME))

P23sf_geo %>%
  group_by(TLID) %>%
  summarise(n_distinct(ST_NAME))%>%
  View()

View(PAD_unit[PAD_unit$TLID == 85711227,])
View(streets_2023[streets_2023$TLID == 85711227,])
View(props_2022_final[props_2022_final$TLID == 85711227,])


View(props_2022[props_2022$PID==1401574000,])
View(props_2022[props_2022$PID==306927000,])

PAD_unit_1 <- PAD_unit
