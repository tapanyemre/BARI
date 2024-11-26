pal_colnames <- c("parcel_num", "CM_ID", "ST_NUM", "ST_NAME_SUF", "ZIPCODE", "X", "Y", 
             "GIS_ID", "Land_Parcel_ID", "TLID", "Blk_ID_10", "BG_ID_10", "CT_ID_10", 
             "Blk_ID_20", "BG_ID_20", "CT_ID_20", 
             "FY2000.LU", "FY2000.AV", "FY2000.RESEX", "DiffAV2000", "PercChangeAV2000", "LU2000FourCat",
             "FY2001.LU", "FY2001.AV", "FY2001.RESEX", "DiffAV2001", "PercChangeAV2001", "LU2001FourCat", 
             "FY2002.LU", "FY2002.AV", "FY2002.RESEX",  "DiffAV2002", "PercChangeAV2002", "LU2002FourCat", 
             "FY2003.LU", "FY2003.AV", "FY2003.RESEX", "DiffAV2003", "PercChangeAV2003", "LU2003FourCat", 
             "FY2004.LU", "FY2004.AV", "FY2004.RESEX", "DiffAV2004", "PercChangeAV2004", "LU2004FourCat", 
             "FY2005.LU", "FY2005.AV", "FY2005.RESEX", "DiffAV2005", "PercChangeAV2005", "LU2005FourCat", 
             "FY2006.LU", "FY2006.AV", "FY2006.RESEX", "DiffAV2006",  "PercChangeAV2006", "LU2006FourCat", 
             "FY2007.LU", "FY2007.AV", "FY2007.RESEX", "DiffAV2007", "PercChangeAV2007", "LU2007FourCat", 
             "FY2008.LU", "FY2008.AV",  "FY2008.RESEX", "DiffAV2008", "PercChangeAV2008", "LU2008FourCat", 
             "FY2009.LU",  "FY2009.AV", "FY2009.RESEX", "DiffAV2009", "PercChangeAV2009", "LU2009FourCat", 
             "FY2010.LU", "FY2010.AV", "FY2010.RESEX", "DiffAV2010", "PercChangeAV2010",  "LU2010FourCat", 
             "FY2011.LU", "FY2011.AV", "FY2011.RESEX", "DiffAV2011", "PercChangeAV2011", "LU2011FourCat", 
             "FY2012.LU", "FY2012.AV", "FY2012.RESEX", "DiffAV2012", "PercChangeAV2012", "LU2012FourCat", 
             "FY2013.LU", "FY2013.AV",  "FY2013.RESEX", "DiffAV2013", "PercChangeAV2013", "LU2013FourCat", 
             "FY2014.LU",  "FY2014.AV", "FY2014.RESEX", "DiffAV2014", "PercChangeAV2014", "LU2014FourCat", 
             "FY2015.LU", "FY2015.AV", "FY2015.RESEX", "DiffAV2015", "PercChangeAV2015",  "LU2015FourCat",
             "FY2016.LU", "FY2016.AV", "FY2016.RESEX", "DiffAV2016",  "PercChangeAV2016", "LU2016FourCat", 
             "FY2017.LU", "FY2017.AV", "FY2017.RESEX",  "DiffAV2017", "PercChangeAV2017", "LU2017FourCat", 
             "FY2018.LU", "FY2018.AV","FY2018.RESEX", "DiffAV2018", "PercChangeAV2018", "LU2018FourCat", 
             "FY2019.LU",  "FY2019.AV", "FY2019.RESEX", "DiffAV2019", "PercChangeAV2019", "LU2019FourCat", 
             "FY2020.LU", "FY2020.AV", "FY2020.RESEX", "DiffAV2020", "PercChangeAV2020", "LU2020FourCat", 
             "FY2021.LU", "FY2021.AV", "FY2021.RESEX", "DiffAV2021", "PercChangeAV2021" , "LU2021FourCat",  
             "FY2022.LU",  "FY2022.AV", "FY2022.RESEX", "DiffAV2022", "PercChangeAV2022" , 
             "GrowthDiffAV", "GrowthPercChangeAV",   
             "CrashDiffAV", "CrashPercChangeAV" ,   
             "RecoveryDiffAV", "RecoveryPercChangeAV")


tmp <- landParcels$X
landParcels$X <- landParcels$Y
landParcels$Y <- tmp
rm(tmp)


