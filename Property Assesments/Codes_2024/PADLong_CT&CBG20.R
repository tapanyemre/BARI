### libraries ###
library(sf)
library(tidyverse)
library(data.table)

### to prevent scientific notation issues ###
options(scipen=100000000)



####################
pal <- read.csv("~/Downloads/PADLong.Record.2022.csv")
cts_20_shp <- tigris::tracts(state = "MA", county = "Suffolk", year = 2022) %>% dplyr::rename(CT_ID_20 = GEOID)
bgs_20_shp <- tigris::block_groups(state = "MA",county = "Suffolk",year = 2022) %>%  dplyr::rename(BG_ID_20 = GEOID)

lastyear = 2022
#make new condo vars for aggregation
for (year in c(2001:lastyear)) {
  nc = paste("NewCondo",year,sep="")
  prevyear = year -1
  thisyearLU = paste(paste("FY",year,sep=""),".LU",sep="")
  prevyearLU = paste(paste("FY",prevyear,sep=""),".LU",sep="")
  thisyearmatch = match(pal[,thisyearLU],c("CD","CP","CM"))
  prevyearmatch = match(pal[,prevyearLU],c("CD","CP","CM"))
  pal[,nc] = ifelse(!is.na(thisyearmatch) & is.na(prevyearmatch),1,
                    ifelse(!is.na(prevyearmatch) & is.na(thisyearmatch), -1, 0))
}


## Aggregate sum change in assessed value over each year by census tract
# pal.CT <- aggregate(cbind(as.numeric(DiffAV2001),as.numeric(DiffAV2002),
#                           as.numeric(DiffAV2003),as.numeric(DiffAV2004),
#                           as.numeric(DiffAV2005),as.numeric(DiffAV2006),
#                           as.numeric(DiffAV2007),as.numeric(DiffAV2008),
#                           as.numeric(DiffAV2009),as.numeric(DiffAV2010),
#                           as.numeric(DiffAV2011),as.numeric(DiffAV2012),
#                           as.numeric(DiffAV2013),as.numeric(DiffAV2014),
#                           as.numeric(DiffAV2015),as.numeric(DiffAV2016),
#                           as.numeric(DiffAV2017),as.numeric(DiffAV2018),
#                           NewCondo2001, NewCondo2002, NewCondo2003, NewCondo2004, 
#                           NewCondo2005, NewCondo2006, NewCondo2007, NewCondo2008, 
#                           NewCondo2009, NewCondo2010, NewCondo2011, NewCondo2012, 
#                           NewCondo2013, NewCondo2014, NewCondo2015, NewCondo2016, 
#                           NewCondo2017, NewCondo2018) ~ CT_ID_20, pal, sum, na.rm = TRUE)

pal.CT <- pal %>%
  filter(!is.na(CT_ID_20)) %>%
  group_by(CT_ID_20) %>%
  summarise(DiffAV2001 = sum(DiffAV2001, na.rm = T), DiffAV2002 = sum(DiffAV2002, na.rm = T), 
            DiffAV2003 = sum(DiffAV2003, na.rm = T), DiffAV2004 = sum(DiffAV2004, na.rm = T), 
            DiffAV2005 = sum(DiffAV2005, na.rm = T), DiffAV2006 = sum(DiffAV2006, na.rm = T), 
            DiffAV2007 = sum(DiffAV2007, na.rm = T), DiffAV2008 = sum(DiffAV2008, na.rm = T), 
            DiffAV2009 = sum(DiffAV2009, na.rm = T), DiffAV2010 = sum(DiffAV2010, na.rm = T), 
            DiffAV2011 = sum(DiffAV2011, na.rm = T), DiffAV2012 = sum(DiffAV2012, na.rm = T), 
            DiffAV2013 = sum(DiffAV2013, na.rm = T), DiffAV2014 = sum(DiffAV2014, na.rm = T), 
            DiffAV2015 = sum(DiffAV2015, na.rm = T), DiffAV2016 = sum(DiffAV2016, na.rm = T), 
            DiffAV2017 = sum(DiffAV2017, na.rm = T), DiffAV2018 = sum(DiffAV2018, na.rm = T), 
            DiffAV2019 = sum(DiffAV2019, na.rm = T), DiffAV2020 = sum(DiffAV2020, na.rm = T), 
            DiffAV2021 = sum(DiffAV2021, na.rm = T), DiffAV2022 = sum(DiffAV2022, na.rm = T),
            NewCondo2001 = sum(NewCondo2001, na.rm = T), NewCondo2002 = sum(NewCondo2002, na.rm = T), 
            NewCondo2003 = sum(NewCondo2003, na.rm = T), NewCondo2004 = sum(NewCondo2004, na.rm = T), 
            NewCondo2005 = sum(NewCondo2005, na.rm = T), NewCondo2006 = sum(NewCondo2006, na.rm = T), 
            NewCondo2007 = sum(NewCondo2007, na.rm = T), NewCondo2008 = sum(NewCondo2008, na.rm = T), 
            NewCondo2009 = sum(NewCondo2009, na.rm = T), NewCondo2010 = sum(NewCondo2010, na.rm = T), 
            NewCondo2011 = sum(NewCondo2011, na.rm = T), NewCondo2012 = sum(NewCondo2012, na.rm = T), 
            NewCondo2013 = sum(NewCondo2013, na.rm = T), NewCondo2014 = sum(NewCondo2014, na.rm = T), 
            NewCondo2015 = sum(NewCondo2015, na.rm = T), NewCondo2016 = sum(NewCondo2016, na.rm = T), 
            NewCondo2017 = sum(NewCondo2017, na.rm = T), NewCondo2018 = sum(NewCondo2018, na.rm = T), 
            NewCondo2019 = sum(NewCondo2019, na.rm = T), NewCondo2020 = sum(NewCondo2020, na.rm = T), 
            NewCondo2021 = sum(NewCondo2021, na.rm = T), NewCondo2022 = sum(NewCondo2022, na.rm = T))

names(pal.CT)[1:(length(c(2001:lastyear))+1)] <- c("CT_ID_20",
                                                   "SumDiffAV2001","SumDiffAV2002",
                                                   "SumDiffAV2003","SumDiffAV2004",
                                                   "SumDiffAV2005","SumDiffAV2006",
                                                   "SumDiffAV2007","SumDiffAV2008",
                                                   "SumDiffAV2009","SumDiffAV2010",
                                                   "SumDiffAV2011","SumDiffAV2012",
                                                   "SumDiffAV2013","SumDiffAV2014",
                                                   "SumDiffAV2015","SumDiffAV2016",
                                                   "SumDiffAV2017","SumDiffAV2018",
                                                   "SumDiffAV2019","SumDiffAV2020",
                                                   "SumDiffAV2021", "SumDiffAV2022")


## Aggregate median percentage change in assessed value over each year by census tract
# Median_PercentValueChangeYear_CT_Yearly <- aggregate(cbind(PercChangeAV2001,
#                                                            PercChangeAV2002,
#                                                            PercChangeAV2003,
#                                                            PercChangeAV2004,
#                                                            PercChangeAV2005,
#                                                            PercChangeAV2006,
#                                                            PercChangeAV2007,
#                                                            PercChangeAV2008,
#                                                            PercChangeAV2009,
#                                                            PercChangeAV2010,
#                                                            PercChangeAV2011,
#                                                            PercChangeAV2012,
#                                                            PercChangeAV2013,
#                                                            PercChangeAV2014,
#                                                            PercChangeAV2015,
#                                                            PercChangeAV2016,
#                                                            PercChangeAV2017,
#                                                            PercChangeAV2018,
#                                                            GrowthPercChangeAV,
#                                                            CrashPercChangeAV,
#                                                            RecoveryPercChangeAV) 
#                                                      ~ CT_ID_20, pal, median, na.rm=TRUE)

Median_PercentValueChangeYear_CT_Yearly <- pal %>%
  filter(!is.na(CT_ID_20)) %>%
  group_by(CT_ID_20) %>%
  summarise(PercChangeAV2001 = median(PercChangeAV2001, na.rm = T), PercChangeAV2002 = median(PercChangeAV2002, na.rm = T), 
            PercChangeAV2003 = median(PercChangeAV2003, na.rm = T), PercChangeAV2004 = median(PercChangeAV2004, na.rm = T), 
            PercChangeAV2005 = median(PercChangeAV2005, na.rm = T), PercChangeAV2006 = median(PercChangeAV2006, na.rm = T), 
            PercChangeAV2007 = median(PercChangeAV2007, na.rm = T), PercChangeAV2008 = median(PercChangeAV2008, na.rm = T), 
            PercChangeAV2009 = median(PercChangeAV2009, na.rm = T), PercChangeAV2010 = median(PercChangeAV2010, na.rm = T), 
            PercChangeAV2011 = median(PercChangeAV2011, na.rm = T), PercChangeAV2012 = median(PercChangeAV2012, na.rm = T), 
            PercChangeAV2013 = median(PercChangeAV2013, na.rm = T), PercChangeAV2014 = median(PercChangeAV2014, na.rm = T), 
            PercChangeAV2015 = median(PercChangeAV2015, na.rm = T), PercChangeAV2016 = median(PercChangeAV2016, na.rm = T), 
            PercChangeAV2017 = median(PercChangeAV2017, na.rm = T), PercChangeAV2018 = median(PercChangeAV2018, na.rm = T), 
            PercChangeAV2019 = median(PercChangeAV2019, na.rm = T), PercChangeAV2020 = median(PercChangeAV2020, na.rm = T), 
            PercChangeAV2021 = median(PercChangeAV2021, na.rm = T), PercChangeAV2022 = median(PercChangeAV2022, na.rm = T), 
            GrowthPercChangeAV = median(GrowthPercChangeAV, na.rm = T), 
            CrashPercChangeAV = median(CrashPercChangeAV, na.rm = T), 
            RecoveryPercChangeAV = median(RecoveryPercChangeAV, na.rm = T))

pal.CT <- merge(pal.CT,Median_PercentValueChangeYear_CT_Yearly,by = "CT_ID_20")


#keeping only certain vars (so... didn't need to make condo vars? )
pal.CT = pal.CT[,c(
  "CT_ID_20",
  "SumDiffAV2001","PercChangeAV2001","NewCondo2001" ,
  "SumDiffAV2002","PercChangeAV2002","NewCondo2002" ,
  "SumDiffAV2003","PercChangeAV2003","NewCondo2003" ,
  "SumDiffAV2004","PercChangeAV2004","NewCondo2004" ,
  "SumDiffAV2005","PercChangeAV2005","NewCondo2005" ,
  "SumDiffAV2006","PercChangeAV2006","NewCondo2006" ,
  "SumDiffAV2007","PercChangeAV2007","NewCondo2007" ,
  "SumDiffAV2008","PercChangeAV2008","NewCondo2008" ,
  "SumDiffAV2009","PercChangeAV2009","NewCondo2009" ,
  "SumDiffAV2010","PercChangeAV2010","NewCondo2010" ,
  "SumDiffAV2011","PercChangeAV2011","NewCondo2011" ,
  "SumDiffAV2012","PercChangeAV2012","NewCondo2012" ,
  "SumDiffAV2013","PercChangeAV2013","NewCondo2013" ,
  "SumDiffAV2014","PercChangeAV2014","NewCondo2014" ,
  "SumDiffAV2015","PercChangeAV2015","NewCondo2015" ,
  "SumDiffAV2016","PercChangeAV2016","NewCondo2016" ,
  "SumDiffAV2017","PercChangeAV2017","NewCondo2017" ,
  "SumDiffAV2018","PercChangeAV2018","NewCondo2018" ,
  "SumDiffAV2019","PercChangeAV2019","NewCondo2019" ,
  "SumDiffAV2020","PercChangeAV2020","NewCondo2020" ,
  "SumDiffAV2021","PercChangeAV2021","NewCondo2021" ,
  "SumDiffAV2021","PercChangeAV2022","NewCondo2022" ,
  "GrowthPercChangeAV","CrashPercChangeAV" , "RecoveryPercChangeAV" )]

#pal.CT <- pal.CT[-1,]
# write csv
#write.csv(pal.CT,paste0(BARI, pal_ct_path) , row.names=F)
write.csv(pal.CT, "PADLong.CT20.2022.csv",
          row.names=F)

#names(tractsShp)[4] <- "CT_ID_20"
# merge onto ct shp file
pal.CT.shp = merge(pal.CT,cts_20_shp,by="CT_ID_20",all.x=T)

# save shp file
st_write(pal.CT.shp,paste0(BARI, pal_ct_shp_path) ,pal_ct_shp_name,driver="ESRI Shapefile",
         overwrite_layer=TRUE)

st_write(pal.CT.shp,"PADLong.CT20.2022",driver="ESRI Shapefile",
         overwrite_layer=TRUE)


#writeOGR(pal.CT.shp,paste0(BARI, pal_ct_shp_path) ,pal_ct_shp_name,driver="ESRI Shapefile",
# overwrite_layer=TRUE)


#aggregate to BG

# pal.BG <- aggregate(cbind(as.numeric(DiffAV2001),as.numeric(DiffAV2002),
#                           as.numeric(DiffAV2003),as.numeric(DiffAV2004),
#                           as.numeric(DiffAV2005),as.numeric(DiffAV2006),
#                           as.numeric(DiffAV2007),as.numeric(DiffAV2008),
#                           as.numeric(DiffAV2009),as.numeric(DiffAV2010),
#                           as.numeric(DiffAV2011),as.numeric(DiffAV2012),
#                           as.numeric(DiffAV2013),as.numeric(DiffAV2014),
#                           as.numeric(DiffAV2015),as.numeric(DiffAV2016),
#                           as.numeric(DiffAV2017),as.numeric(DiffAV2018),
#                           NewCondo2001, NewCondo2002, NewCondo2003, NewCondo2004, 
#                           NewCondo2005, NewCondo2006, NewCondo2007, NewCondo2008, 
#                           NewCondo2009, NewCondo2010, NewCondo2011, NewCondo2012, 
#                           NewCondo2013, NewCondo2014, NewCondo2015, NewCondo2016,
#                           NewCondo2017, NewCondo2018) ~ BG_ID_20, pal, sum, na.rm = TRUE)


pal.BG <- pal %>%
  filter(!is.na(BG_ID_20)) %>%
  group_by(BG_ID_20) %>%
  summarise(DiffAV2001 = sum(DiffAV2001, na.rm = T), DiffAV2002 = sum(DiffAV2002, na.rm = T), 
            DiffAV2003 = sum(DiffAV2003, na.rm = T), DiffAV2004 = sum(DiffAV2004, na.rm = T), 
            DiffAV2005 = sum(DiffAV2005, na.rm = T), DiffAV2006 = sum(DiffAV2006, na.rm = T), 
            DiffAV2007 = sum(DiffAV2007, na.rm = T), DiffAV2008 = sum(DiffAV2008, na.rm = T),
            DiffAV2009 = sum(DiffAV2009, na.rm = T), DiffAV2010 = sum(DiffAV2010, na.rm = T), 
            DiffAV2011 = sum(DiffAV2011, na.rm = T), DiffAV2012 = sum(DiffAV2012, na.rm = T), 
            DiffAV2013 = sum(DiffAV2013, na.rm = T), DiffAV2014 = sum(DiffAV2014, na.rm = T), 
            DiffAV2015 = sum(DiffAV2015, na.rm = T), DiffAV2016 = sum(DiffAV2016, na.rm = T), 
            DiffAV2017 = sum(DiffAV2017, na.rm = T), DiffAV2018 = sum(DiffAV2018, na.rm = T), 
            DiffAV2019 = sum(DiffAV2019, na.rm = T), DiffAV2020 = sum(DiffAV2020, na.rm = T), 
            DiffAV2021 = sum(DiffAV2021, na.rm = T), DiffAV2022 = sum(DiffAV2022, na.rm = T), 
            NewCondo2001 = sum(NewCondo2001, na.rm = T), NewCondo2002 = sum(NewCondo2002, na.rm = T),
            NewCondo2003 = sum(NewCondo2003, na.rm = T), NewCondo2004 = sum(NewCondo2004, na.rm = T), 
            NewCondo2005 = sum(NewCondo2005, na.rm = T), NewCondo2006 = sum(NewCondo2006, na.rm = T), 
            NewCondo2007 = sum(NewCondo2007, na.rm = T), NewCondo2008 = sum(NewCondo2008, na.rm = T), 
            NewCondo2009 = sum(NewCondo2009, na.rm = T), NewCondo2010 = sum(NewCondo2010, na.rm = T), 
            NewCondo2011 = sum(NewCondo2011, na.rm = T), NewCondo2012 = sum(NewCondo2012, na.rm = T), 
            NewCondo2013 = sum(NewCondo2013, na.rm = T), NewCondo2014 = sum(NewCondo2014, na.rm = T), 
            NewCondo2015 = sum(NewCondo2015, na.rm = T), NewCondo2016 = sum(NewCondo2016, na.rm = T), 
            NewCondo2017 = sum(NewCondo2017, na.rm = T), NewCondo2018 = sum(NewCondo2018, na.rm = T), 
            NewCondo2019 = sum(NewCondo2019, na.rm = T), NewCondo2020 = sum(NewCondo2020, na.rm = T), 
            NewCondo2021 = sum(NewCondo2021, na.rm = T), NewCondo2022 = sum(NewCondo2022, na.rm = T))


names(pal.BG)[1:(length(c(2001:lastyear))+1)]  <- c("BG_ID_20",
                                                    "SumDiffAV2001","SumDiffAV2002",
                                                    "SumDiffAV2003","SumDiffAV2004",
                                                    "SumDiffAV2005","SumDiffAV2006",
                                                    "SumDiffAV2007","SumDiffAV2008",
                                                    "SumDiffAV2009","SumDiffAV2010",
                                                    "SumDiffAV2011","SumDiffAV2012",
                                                    "SumDiffAV2013","SumDiffAV2014",
                                                    "SumDiffAV2015","SumDiffAV2016",
                                                    "SumDiffAV2017","SumDiffAV2018",
                                                    "SumDiffAV2019","SumDiffAV2020",
                                                    "SumDiffAV2021", "SumDiffAV2022")


# Median_PercentValueChangeYear_BG_Yearly <- aggregate(cbind(PercChangeAV2001,
#                                                            PercChangeAV2002,
#                                                            PercChangeAV2003,
#                                                            PercChangeAV2004,
#                                                            PercChangeAV2005,
#                                                            PercChangeAV2006,
#                                                            PercChangeAV2007,
#                                                            PercChangeAV2008,
#                                                            PercChangeAV2009,
#                                                            PercChangeAV2010,
#                                                            PercChangeAV2011,
#                                                            PercChangeAV2012,
#                                                            PercChangeAV2013,
#                                                            PercChangeAV2014,
#                                                            PercChangeAV2015,
#                                                            PercChangeAV2016,
#                                                            PercChangeAV2017,
#                                                            PercChangeAV2018,
#                                                            GrowthPercChangeAV,
#                                                            CrashPercChangeAV, 
#                                                            RecoveryPercChangeAV)
#                                                      ~ BG_ID_20, pal, median, na.rm=TRUE)

Median_PercentValueChangeYear_BG_Yearly <- pal %>%
  filter(!is.na(BG_ID_20)) %>%
  group_by(BG_ID_20) %>%
  summarise(PercChangeAV2001 = median(PercChangeAV2001, na.rm = T), PercChangeAV2002 = median(PercChangeAV2002, na.rm = T), 
            PercChangeAV2003 = median(PercChangeAV2003, na.rm = T), PercChangeAV2004 = median(PercChangeAV2004, na.rm = T), 
            PercChangeAV2005 = median(PercChangeAV2005, na.rm = T), PercChangeAV2006 = median(PercChangeAV2006, na.rm = T), 
            PercChangeAV2007 = median(PercChangeAV2007, na.rm = T), PercChangeAV2008 = median(PercChangeAV2008, na.rm = T), 
            PercChangeAV2009 = median(PercChangeAV2009, na.rm = T), PercChangeAV2010 = median(PercChangeAV2010, na.rm = T), 
            PercChangeAV2011 = median(PercChangeAV2011, na.rm = T), PercChangeAV2012 = median(PercChangeAV2012, na.rm = T), 
            PercChangeAV2013 = median(PercChangeAV2013, na.rm = T), PercChangeAV2014 = median(PercChangeAV2014, na.rm = T), 
            PercChangeAV2015 = median(PercChangeAV2015, na.rm = T), PercChangeAV2016 = median(PercChangeAV2016, na.rm = T), 
            PercChangeAV2017 = median(PercChangeAV2017, na.rm = T), PercChangeAV2018 = median(PercChangeAV2018, na.rm = T), 
            PercChangeAV2019 = median(PercChangeAV2019, na.rm = T), PercChangeAV2020 = median(PercChangeAV2020, na.rm = T), 
            PercChangeAV2021 = median(PercChangeAV2021, na.rm = T), PercChangeAV2022 = median(PercChangeAV2022, na.rm = T), 
            GrowthPercChangeAV = median(GrowthPercChangeAV, na.rm = T), 
            CrashPercChangeAV = median(CrashPercChangeAV, na.rm = T), 
            RecoveryPercChangeAV = median(RecoveryPercChangeAV, na.rm = T))


pal.BG <- merge(pal.BG,Median_PercentValueChangeYear_BG_Yearly,by = "BG_ID_20")



pal.BG = pal.BG[,c(
  "BG_ID_20",
  "SumDiffAV2001","PercChangeAV2001","NewCondo2001" ,
  "SumDiffAV2002","PercChangeAV2002","NewCondo2002" ,
  "SumDiffAV2003","PercChangeAV2003","NewCondo2003" ,
  "SumDiffAV2004","PercChangeAV2004","NewCondo2004" ,
  "SumDiffAV2005","PercChangeAV2005","NewCondo2005" ,
  "SumDiffAV2006","PercChangeAV2006","NewCondo2006" ,
  "SumDiffAV2007","PercChangeAV2007","NewCondo2007" ,
  "SumDiffAV2008","PercChangeAV2008","NewCondo2008" ,
  "SumDiffAV2009","PercChangeAV2009","NewCondo2009" ,
  "SumDiffAV2010","PercChangeAV2010","NewCondo2010" ,
  "SumDiffAV2011","PercChangeAV2011","NewCondo2011" ,
  "SumDiffAV2012","PercChangeAV2012","NewCondo2012" ,
  "SumDiffAV2013","PercChangeAV2013","NewCondo2013" ,
  "SumDiffAV2014","PercChangeAV2014","NewCondo2014" ,
  "SumDiffAV2015","PercChangeAV2015","NewCondo2015" ,
  "SumDiffAV2016","PercChangeAV2016","NewCondo2016" ,
  "SumDiffAV2017","PercChangeAV2017","NewCondo2017" ,
  "SumDiffAV2018","PercChangeAV2018","NewCondo2018" ,
  "SumDiffAV2019","PercChangeAV2019","NewCondo2019" ,
  "SumDiffAV2020","PercChangeAV2020","NewCondo2020" ,
  "SumDiffAV2021","PercChangeAV2021","NewCondo2021" ,
  "SumDiffAV2022","PercChangeAV2022","NewCondo2022" ,
  "GrowthPercChangeAV","CrashPercChangeAV" , "RecoveryPercChangeAV" )]
#pal.BG <- pal.BG[-1,]
# write bg csv
write.csv(pal.BG, paste0(BARI, pal_bg_path) , row.names=F)
write.csv(pal.BG, "PADLong.CBG20.2022.csv" , row.names=F)

# merge to bg shpfile
#names(bgsShp)[5] <- "BG_ID_20"
pal.BG.shp = merge(pal.BG,bgs_20_shp,by="BG_ID_20",all.x=T)

# write bg shpfile
st_write(pal.BG.shp, "PADLong.CBG20.2022",driver="ESRI Shapefile",
         overwrite_layer=TRUE)
