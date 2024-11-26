# simple regex trims the string, found online
trim <- function (x) gsub("^\\s+|\\s+$", "", x)  

# finds the mode of a vector (how does r not have this built in)
Mode <- function(x,na.rm=T) {
  if (na.rm) {
    x = x[!is.na(x)]
  }
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


# this merges on a dataset that has variables that are already present in the first one, then adds in those variables 
# where they were missing in the first one and cleans up the variable names
# this is useful for when you have to add data for a bunch of variables from a bunch of sources, for example geographic data from multiple geocodes
# in practice i stopped using this, in order to be more transparent
merge_and_move <- function(dataset1,dataset2,byx,varList,byy=NA,allx=T,ally=F) {
  if (is.na(byy)) {byy=byx}
  merged_data = merge(dataset1, dataset2[,c(byy,varList)],by.x=byx,by.y=byy,all.x=allx,all.y=ally )
  for (var in varList) {
    varx = paste(var,".x",sep="")
    vary = paste(var,".y",sep="")
    merged_data[,var]=ifelse(!is.na(merged_data[,varx]),merged_data[,varx],merged_data[,vary])
    merged_data[,varx] <- NULL
    merged_data[,vary] <- NULL
  }
  return(merged_data)
}



# these two functions combine several vectors of string variables, while not including any that were blank or NA
# actually quite useful for recombining pieces of addresses into a full address, because if the suffix is missing you don't want an NA or an extra space stuck in the middle
# i may have found this online
mass_combine <- function(df,X) {
  result = rep(NA,length(df[,1]))
  for (i in 1:length(df[1,])) {
    result = combine_NA(result,df[,i],X)
  }  
  return(result)
}

# this just combines two vectors of string variables 
combine_NA <- function(a,b,X) {
  a <- lapply(a,as.character)
  a= trim(a)
  a[a==""] <- NA
  b <- lapply(b,as.character)
  b = trim(b)
  b[b==""]<-NA
  c = paste(a,b,sep=X)
  c[is.na(b)] <- a[is.na(b)]
  c[is.na(a)] <- b[is.na(a)]
  c = trim(c)
  return(c)
}




## creates a Z score, with percentage limits allow you to cap extreme values 
ZScore <- function(x,pLimits=c(NA,NA)) {
  if (!is.na(pLimits[1]) & !is.na(pLimits[2])) {
    df = data.frame(x = x, ID = c(1:length(x)))
    pMin = pLimits[1]
    pMax = pLimits[2]
    orderedX = x[!is.na(x)][order(x[!is.na(x)])]
    df$newX = df$x
    df[!is.na(df$x) & df$x< orderedX[floor(pMin*length(orderedX)/100)+1],"newX"]=orderedX[floor(pMin*length(orderedX)/100)+1]
    df[!is.na(df$x) & df$x> orderedX[floor(pMax*length(orderedX)/100)],"newX"]=orderedX[floor(pMax*length(orderedX)/100)]
  }
  else {
    df = data.frame(newX = x, ID = c(1:length(x)))
  }
  x_real = df$newX[!is.na(df$newX)]
  mean_calc<-sum(as.numeric(x_real))/length(x_real)
  stddev_calc<-(sum((as.numeric(x_real)-mean_calc)^2)/(length(x_real)-1))^.5 
  df$x_zscore = (as.numeric(df$newX)-mean_calc)/stddev_calc
  return(df[order(df$ID),"x_zscore"])
}

# gives the number of missing and percent nonmissing for our common geographic indicators
check_geo <- function(x) {
  for (columnName in c("X","Y","Land_Parcel_ID","GIS_ID","Property_ID","parcel_num","TLID","Blk_ID_10","BG_ID_10","CT_ID_10","BRA_PD","NSA_Name")) {
    if (columnName %in% names(x)) {
      print(paste(columnName,"-", sum(is.na(x[,columnName]) | x[,columnName] == 0) ) )
      print(paste("    ",(1-(sum(is.na(x[,columnName]) | x[,columnName] == 0)/nrow(x)))))
    } else {
      print(paste(columnName, " not found",sep=""))
    }
  }
}

placeInGI = function(df,IDConnectorPath,fuzzyMatching=F,fuzzyMatchDBPath="",projection = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
                     landParcelsPath,landParcelsShpName="",landParcelsShpPath="",
                     roadsPath="",roadsShpPath="",roadsShpName="",
                     samPath="",
                     blkShpPath="", blkShpName="",
                     bgShpPath="", bgShpName="",
                     ctShpPath="", ctShpName="") {
  geoVars = c("Land_Parcel_ID","X","Y","TLID","Blk_ID_10","BG_ID_10","CT_ID_10","NSA_Name","BRA_PD")
  # remove old geographic data from df
  for (var in geoVars) { df[,var] = NA}
  df$geoType = NA
  
  df = standardizeGeoNames(df)
  # make unique ID
  df$uniqueGeocodeID = row.names(df)
  
  # reads in ID connector and attaches geo data
  IDconnector = read.csv(IDConnectorPath,stringsAsFactors=F)
  landParcels = read.csv(landParcelsPath,stringsAsFactors=F)
  # landParcels$TLID = landParcels$TLID_1
  IDconnector.geo = merge(IDconnector, landParcels,by="Land_Parcel_ID",all.x=T)
  
  # attempts to connect geographic data based on Property_ID
  if ("Property_ID" %in% names(df)) {
    df = merge_and_move(dataset1 = df,
                        dataset2 = IDconnector.geo[!duplicated(IDconnector.geo$Property_ID),],
                        byx = "Property_ID",allx=T,
                        varList=geoVars)
    df$geoType[is.na(df$geoType)&!is.na(df$Land_Parcel_ID)] = "Property_ID"
    print("Property_ID")
  } else {print("No Property_ID")}
  
  # attempts to connect geographic data based on parcel_num
  if ("parcel_num" %in% names(df)) {
    df = merge_and_move(df,
                        IDconnector.geo[!duplicated(IDconnector.geo$parcel_num),],
                        byx = "parcel_num",allx=T,
                        varList=geoVars)  
    df$geoType[is.na(df$geoType)&!is.na(df$Land_Parcel_ID)] = "parcel_num"
    print("parcel_num")
  } else {print("No parcel_num")}
  
  # attempts to connect geographic data based on GIS_ID (very unlikely to do much beyond parcel_num)
  if ("GIS_ID" %in% names(df)) {
    df = merge_and_move(df,
                        IDconnector.geo[!duplicated(IDconnector.geo$GIS_ID),],
                        byx = "GIS_ID",allx=T,
                        varList=geoVars)
    df$geoType[is.na(df$geoType)&!is.na(df$Land_Parcel_ID)] = "GIS_ID"
    print("GIS_ID")
  }  else {print("No GIS_ID")}
  
  # creates shapefile for geographic geocoding
  if (sum(c("lat","lng") %in% names(df))==2) {
    df.shp = df[ !is.na(df$lat),]
    coordinates(df.shp) = ~lng+lat 
    proj4string(df.shp) = projection
  }
  
  # attemptes to connect geographic data based on geocode
  if (sum(c("num1","num2","street_c","suffix_c","zip_c","city_c") %in% names(df))==6) {
    # SAM NO GEO
    #############
      if (samPath!="") {
        geocoded.s.ng = geocode(toGeocode =df[ is.na(df$Land_Parcel_ID),],tgID = "uniqueGeocodeID",refName = "Sam",smallestGeo = "Land_Parcel_ID",fuzzyMatching = fuzzyMatching,fuzzyMatchDBPath = fuzzyMatchDBPath,
                                geographies = c("X","Y","TLID","Blk_ID_10","BG_ID_10","CT_ID_10","NSA_Name","BRA_PD"), refCSVPath = samPath)
        df = merge_and_move(df,
                            geocoded.s.ng,
                            byx = "uniqueGeocodeID",allx=T,
                            varList=geoVars)
        print("Geocode - Sam no geo")
      
      } else {
        print("Sam path not found")
      }
      
    # LP NO GEO
    ###########
    geocoded.lp.ng = geocode(toGeocode = df[ is.na(df$Land_Parcel_ID),],tgID = "uniqueGeocodeID",refName = "LandParcels",smallestGeo = "Land_Parcel_ID",fuzzyMatching = fuzzyMatching,fuzzyMatchDBPath = fuzzyMatchDBPath,
                       geographies = c("X","Y","TLID","Blk_ID_10","BG_ID_10","CT_ID_10","NSA_Name","BRA_PD"), refCSVPath = landParcelsPath)
    df = merge_and_move(df,
                        geocoded.lp.ng,
                        byx = "uniqueGeocodeID",allx=T,
                        varList=geoVars)
    
    print("Geocoded - lp no geo")
    
    # LP WITH GEO
    #############
    if (sum(c("lat","lng") %in% names(df))==2 & landParcelsShpPath!="" & landParcelsShpName!="") {
      
      geocoded.lp.g = geocode(toGeocode = df[ is.na(df$Land_Parcel_ID),],toGeocodeShp = df.shp[ df.shp@data$uniqueGeocodeID %in% df$uniqueGeocodeID[ is.na(df$Land_Parcel_ID)],],tgID = "uniqueGeocodeID",refName = "LandParcels",smallestGeo = "Land_Parcel_ID",
              geographies = c("X","Y","TLID","Blk_ID_10","BG_ID_10","CT_ID_10","NSA_Name","BRA_PD"),fuzzyMatching = fuzzyMatching,fuzzyMatchDBPath = fuzzyMatchDBPath,
              refCSVPath = landParcelsPath,refShpPath = landParcelsShpPath,refShpName = landParcelsShpName,
              matches = list(
                list(c("street_c","num1","suffix_c","zip_c"),NA,40),
                list(c("street_c","num1","suffix_c","city_c"),NA,40),
                list(c("street_c","num1","suffix_c"),NA,40),
                list(c("street_c","suffix_c","city_c"),NA,40),
                list(c("street_c","suffix_c","zip_c"),NA,40),
                list(c("street_c","num1","city_c"),NA,40),
                list(c("street_c","suffix_c"),NA,40),
                list(c("street_c","num1"),NA,40),
                list(c("street_c","zip_c"),NA,40),
                list(c("street_c","city_c"),NA,40),
                list(c("street_c"),NA,40)))
      
      df = merge_and_move(df,
                          geocoded.lp.g,
                          byx = "uniqueGeocodeID",allx=T,
                          varList=geoVars)
      print("Geocoded - lp with geo")
      
    } else { print("Lat/Lng vars or land parcels shapefile path not found")}
    
    
    # ROADS NO GEO
    #############
    if (roadsPath!="") {
      
      geocoded.r.ng = geocode(toGeocode =df[ is.na(df$Land_Parcel_ID),],tgID = "uniqueGeocodeID",refName = "Roads",smallestGeo = "TLID",fuzzyMatching = fuzzyMatching,fuzzyMatchDBPath = fuzzyMatchDBPath,
                         geographies = c("BG_ID_10","CT_ID_10","NSA_Name","BRA_PD"), refCSVPath = roadsPath)
      df = merge_and_move(df,
                          geocoded.r.ng,
                          byx = "uniqueGeocodeID",allx=T,
                          varList=c("TLID","BG_ID_10","CT_ID_10","NSA_Name","BRA_PD"))

       print("Geocode - roads no geo")
      
      
      # ROADS W GEO
      #############
      if (sum(c("lat","lng") %in% names(df))==2 & roadsShpPath!="" & roadsShpName!="") {
        
        geocoded.r.g = geocode(toGeocode =df[ is.na(df$Land_Parcel_ID),],toGeocodeShp = df.shp[ df.shp@data$uniqueGeocodeID %in% df$uniqueGeocodeID[ is.na(df$Land_Parcel_ID)],],fuzzyMatching = fuzzyMatching,fuzzyMatchDBPath = fuzzyMatchDBPath,
                               tgID = "uniqueGeocodeID",refName = "Roads",smallestGeo = "TLID",
                geographies = c("BG_ID_10","CT_ID_10","NSA_Name","BRA_PD"),
                refCSVPath = roadsPath,refShpPath = roadsShpPath,refShpName = roadsShpName,
                matches = list(
                  list(c("street_c","num1","suffix_c","zip_c"),NA,40),
                  list(c("street_c","num1","suffix_c","city_c"),NA,40),
                  list(c("street_c","num1","suffix_c"),NA,40),
                  list(c("street_c","suffix_c","city_c"),NA,40),
                  list(c("street_c","suffix_c","zip_c"),NA,40),
                  list(c("street_c","num1","city_c"),NA,40),
                  list(c("street_c","suffix_c"),NA,40),
                  list(c("street_c","num1"),NA,40),
                  list(c("street_c","zip_c"),NA,40),
                  list(c("street_c","city_c"),NA,40),
                  list(c("street_c"),NA,40)))
        df = merge_and_move(df,
                            geocoded.r.g,
                            byx = "uniqueGeocodeID",allx=T,
                            varList=c("TLID","BG_ID_10","CT_ID_10","NSA_Name","BRA_PD"))
        
        # df = merge_and_move(df,
        #                     geocoded.r.g,
        #                     byx = "uniqueGeocodeID",allx=T,
        #                     varList=c("TLID","BG_ID_10","CT_ID_10"))
        print("Geocode - roads with geo")
        
      }  else { print("Lat/Lng vars or roads shapefile path not found")}
    } else { print("Roads path not found")}  
      
    df$geoType[is.na(df$geoType)&!is.na(df$Land_Parcel_ID)] = "Geocode"
  } else { print("Street address vars not found")}
  
  if (sum(c("lat","lng") %in% names(df))==2 ) {
    if (blkShpPath!="" & blkShpName!="") {
      blkShp = readOGR(path.expand(blkShpPath), blkShpName,stringsAsFactors=F)
      df.shp.blk <- spTransform(df.shp[ df.shp$uniqueGeocodeID %in% df$uniqueGeocodeID[ is.na(df$Blk_ID_10)],], proj4string(blkShp)) 
    
      df.shp.overBlk = over(df.shp.blk,blkShp)
      df.shp.overBlk$uniqueGeocodeID = df.shp.blk$uniqueGeocodeID
      ## SSH columns not exist in new dts
      df = merge_and_move(df,
                          df.shp.overBlk,
                          byx = "uniqueGeocodeID",byy="uniqueGeocodeID",allx=T,ally=T,
                          varList=c("Blk_ID_10","BG_ID_10","CT_ID_10","NSA_Name","BRA_PD"))
      # df = merge_and_move(df,
      #                     df.shp.overBlk,
      #                     byx = "uniqueGeocodeID",byy="uniqueGeocodeID",allx=T,ally=T,
      #                     varList=c("Blk_ID_10","BG_ID_10","CT_ID_10")) 
      print("Block overlay")
      
    } else { print("Blocks not found")}
    if (bgShpPath!="" & bgShpName!="") {
      bgShp = readOGR(path.expand(bgShpPath), bgShpName,stringsAsFactors=F)
      df.shp.bg <- spTransform(df.shp[ df.shp$uniqueGeocodeID %in% df$uniqueGeocodeID[ is.na(df$BG_ID_10)],], proj4string(bgShp)) 
      
      df.shp.overBG = over(df.shp.bg,bgShp)
      df.shp.overBG$uniqueGeocodeID = df.shp.bg$uniqueGeocodeID
      df = merge_and_move(df,
                          df.shp.overBG,
                          byx = "uniqueGeocodeID",byy="uniqueGeocodeID",allx=T,ally=T,
                          varList=c("BG_ID_10","CT_ID_10","NSA_Name","BRA_PD"))
      # df = merge_and_move(df,
      #                     df.shp.overBlk,
      #                     byx = "uniqueGeocodeID",byy="uniqueGeocodeID",allx=T,ally=T,
      #                     varList=c("Blk_ID_10","BG_ID_10","CT_ID_10")) 
      print("BG overlay")
      
    } else { print("BG not found")}
    if (ctShpPath!="" & ctShpName!="") {
      ctShp = readOGR(path.expand(ctShpPath), ctShpName,stringsAsFactors=F)
      df.shp.ct <- spTransform(df.shp[ df.shp$uniqueGeocodeID %in% df$uniqueGeocodeID[ is.na(df$CT_ID_10)],], proj4string(ctShp)) 
      
      df.shp.overCT = over(df.shp.ct,ctShp)
      df.shp.overCT$uniqueGeocodeID = df.shp.ct$uniqueGeocodeID
      df = merge_and_move(df,
                          df.shp.overCT,
                          byx = "uniqueGeocodeID",byy="uniqueGeocodeID",allx=T,ally=T,
                          varList=c("CT_ID_10"
                                    #,"NSA_NAME","BRA_PD"
                                    )) 
      print("CT overlay")   
    } else { print("CTs not found")}
  }
 
  print(table(df$geoType))
  df$uniqueGeocodeID = NULL
  return(df)
  
}

