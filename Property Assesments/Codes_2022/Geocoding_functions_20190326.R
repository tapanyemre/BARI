# Henry Gomory - July 27, 2017

library(rgeos)
library(dplyr)
library(maptools)
library(rgdal)
library(stringdist)

############
# OVERVIEW #
############
# This geocoder assigns geographic data from a reference dataset to rows in a "toGeocode" dataset, based on matches between components of a street address
# The components of the street address are num1, num2, street_c, suffix_c, unit_c, zip_c, city_c. 

# First, the geocoder gets a reference dataset, based on refName. It is either the land parcels, roads, or sam file
    # this could be very easily altered to include more datasets or a generic one. 
    # that dataset has expanded out its addresses from 1-5 Maple St. to 3 rows: 1, 3, 5 Maple St., so that all possible matches are made
    # if geographic matching is enabled, a corresponding shapefile must be included as well
# A series of merges are then attempted based on the matches argument.
    # if the default list of matches is specified, first it merges based on street_c, num1, suffix_c, and zip_c, then it tries with city instead of zip, etc. 
    # this generates "rawMatches" which are then processed into real data
    # if numer differences are specified, then imperfect matches between numbers are allowed in the raw match, deferring to the smallest difference
# If a value of tgID is matched to only only one smallestGeo, that is deemed a unique match
    # if there is no unique match, if geographic matching is enabled then out of these matches, the one that is geographically closest is chosen
    # otherwise, for each geography, the geocoder checks whether there is a unique value of that geography among the matches
    # for example, if there are ten land parcels that match to an address but they all have the same tract ID or street segment ID, then those geographies are captured
    # if a unique match or geo match is found, then no other matches are attempted for that entry
# after iterating through all of the types of matches, the geocoder returns a data frame with an entry for all of the items, geographic data, and information about the results
  
#############
# ARGUMENTS #
#############
# Important arguments
    # toGeocode: the data frame to be gecoded. must include some of the street address components (num1, num2, street_c, suffix_c, unit_c, zip_c, city_c)
            # the street address components can be created using the cleaning functions 
        # toGeocodeShp is a spatial object that is used for geographic matches. If geographic matching is enabled such an object must be provided. 
            # toGeocodeShp must hold the tgID in its @data slot, and tgID MUST BE UNIQUE FOR THE SHPFILE
    # tgID: ID for the data frame. Records will be returned with this as a unique ID. 
            # It does not need to be unique in the toGeocode data frame. but if it is not, then two records with the same ID will be treated as one
            # Accordingly, if they have different street addresses, it will be very unlikely that a unique match is found for them
    # refName: the database to match to, can either be "Roads","Sam", or "LandParcels". The refpaths (below) must correspond to the database indicated in refName.
        # refCSVPath: a path to the CSV file holding the reference file. The file must have at least some of the street address components.
        # refShpPath: a path to the folder of the shapefile holding the reference shape file (only needed for geographic matches)
        # refShpName: the name of the shapefile in the folder indicated by refShpPath (only needed for geographic matches)
    # smallestGeo: the smallest geography. a variable within the reference file. 
            # If a unique value of smallestGeo is found for a row in toGeocode, then it is deemed geocoded and no more matches are attempted.
            # 99% of the time this should be TLID for roads, Land_Parcel_ID for LandParcels, and either SAM_ADDRESS_ID (Property_ID) or Land_Parcel_ID for Sam
    # geographies: a vector of column names of geographic variables you want to find out about through the geocoding
            # typically: c("X","Y","TLID","Blk_ID_10","BG_ID_10","CT_ID_10","NSA_NAME","BRA_PD")
            # all of the geographies must exist in the reference file
    # matches: this is a list of sets of variables to match on, as described in the overview. the matches are tried in order, so the stricter matches should be placed first
            # each element of the matches list has three values: a vector and then two values, each either NA or a number
            # the first NA or number is the maximum difference allowed between numbers in an address that will still be counted as a match
            # if it is zero or NA, then only exact matches will be allowed 2 Maple St. -> 2 Maple St.
            # however, a value of 4 will allow matches like 2 Maple St. -> 6 Maple St., but choosing the match with the smallest difference
            # the second NA or number is the maximum geographic difference allowed between matches. 
            # when the geocoder chooses between matches based on geography it will throw out any matches where the distance is too high, as described above
            # including a value for the second number for any match enables the geographic matching mode
    # fuzzyMatching: whether fuzzy matching should be used to match streets, requires human oversight
    # fuzzyMatchDBPath: a path to a csv file that holds previously-decided fuzzy matching decisions. it will be saved again with new decisions after. 
            # requires variables: street1, street2, decision. 

# Less-important arguments
    # expand: determines whether a range of values in toGeocode should all be attempted. So if toGeocode is 1-5, 1, 3, and 5 will be attempted
            # should almost always be True
    # weirdRange: this determines whether number combinations like 25-3 (num1 = 25, num2 = 3) should be interpreted as 3-25, thus expanding to 3, 5, 7...
            # this should almost always be False, since those combinations are almost always street numbers then apartment units
    # fullRange: when expanding the addresses in the reference file, determines whether 1-5 should be expanded to 1, 3, 5 or 1, 2, 3, 4, 5
            # this is especially important when geocoding to roads
            # i think the best way to use this is to first geocode with it off, then geocode the remains with it on
            # if you are doing geographic matches it doesn't matter so much, because it will match based on street and then find the closest
            # likewise, it doesn't matter so much if you are doing inexact number matches
    # oddsAndEvens: very minor, used when expanding the addresses in the reference file, determines whether the two numbers need to be the same sign
            # if enabled, 2-5 changes to 2-4
    # buffer: also very minor, used when expanding addresses in the reference file, if it is 2, 3-7 will expand to 1, 3, 5, 7, 9, allowing for close matches
            # this was originally implemented before the imperfect number matches and so is not very useful now. This is less useful because it does not defer to the closest match
            # it could be useful if you want to check for a subset that don't match whether all of the addresses that are close to it match on some geograhpy
            # for example, you might only want to match to a street segment if all of the addresses within 10 of your target have the same street, rather than 
            # just finding the closest address and taking its street segment ID (realistically i don't know if i would ever really do this)
    # batchSize: the number of toGeocode cases that are geocoded in a batch. 3000 is the default size because larger ones made my computer crash. 
            # can be increased based on computer power
    # planarCRS: the CRS to be used for geographical matches. 
  


##########
# OUTPUT #
##########

# Output during the geocoding
    # the geocoder prints some status updates as it progresses
    # fuzzy matches are printed and if they have not been encountered before they require user input
    # many problems are checked for and if they are encountered the function will exit
    # if a large number of addresses are being geocoded, the geocoder will work through them in batches. 
    # after each bath, the number processes and the number fully geocoded will be printed
    # fully geocoded means that a unique value for smallestGeo has been attached to the address being geocoded
    # a table of the number of full geocodes that came from each type of match is printed
    # this is a cross tabulation of the two fields fm_type and fm_vars

# the output object 
    # the returned object has a row for every unique value of tgID, whether or not a match was found
    # the first column is the ID specified in the field tgID
    # next is smallestGeo then the geographies
    # next is fm_type, which is an important variable summarizing the match
        # Unique means that a unique value for smallestGeo was found in the reference file, this is the best type of match
        # Geo means that although no unique reference item was found based on the matches, we found the geographically-closest one that matched
        # Non-Unique matches means that some matches were found, but we couldn't narrow it to a single item in the reference file. There may still be extensive geographic
            # information for these records, just no definitive "smallestGeo"
        # No matches means that in all of the types of matches attempted, none of them had any matches - this suggests that the record is either junky or not in Boston
    # fm_vars: the list of variables on which the match that found the item was made. It only exists for matches with fm_type == Unique or Geo
    # fm_numMatches: the number of matches that were narrowed down to a single one for a match that found smallestGeo. This is relevant only for geo matches because it must be 1 for all others or NA if no full match was found
    # fm_numDiff: the difference between numbers in a full match that used numbers
    # fm_geoDiff: the geographic distance in a geo match
    # next is a column for each combination of variables that were matched on, with the number of matches found using those variables
        # some are 0 because an earlier match was completely successful, and so the match was not attempted


geocode <- function(toGeocode,tgID,refName,smallestGeo,geographies=c(),refCSVPath,
                    toGeocodeShp = NA, weirdRange=F,fullRange=F,oddsAndEvens=T,buffer=0, expand = T,
                    refShpPath = NA, refShpName = NA, fuzzyMatching = F,fuzzyMatchDBPath =NA, batchSize = 3000,planarCRS = "+init=epsg:32619 +units=m",
                    matches = list(
                      list(c("street_c","num1","suffix_c","zip_c"),NA,NA), # first NA is num, second is geo
                      list(c("street_c","num1","suffix_c","city_c"),NA,NA),
                      list(c("street_c","num1","suffix_c"),NA,NA),
                      list(c("street_c","suffix_c","city_c"),NA,NA),
                      list(c("street_c","suffix_c","zip_c"),NA,NA),
                      list(c("street_c","num1","city_c"),NA,NA),
                      list(c("street_c","suffix_c"),NA,NA),
                      list(c("street_c","num1"),NA,NA),
                      list(c("street_c","zip_c"),NA,NA),
                      list(c("street_c","city_c"),NA,NA),
                      list(c("street_c"),NA,NA))) {
  
  # Part of a series of little messages, since geocoding can take a while
  print("Starting geocoder")
  
  # doing some manipulations of the arguments
  xy = (sum(!is.na(lapply(matches,'[[',3)))>0)
  if (length(geographies)==0 | (length(geographies == 1) & smallestGeo %in% geographies)) {
    geographies = c()
  } else {
    geographies = setdiff(geographies,smallestGeo)
  }
  
  # doing an initial check of the arguments
  if (!initialCheck(xy = xy,refShpPath = refShpPath,refShpName = refShpName,toGeocodeShp = toGeocodeShp,toGeocode = toGeocode,refName = refName,tgID = tgID)){return(NA)}
  
  # this returns the data frame that will be geocoded again
  print("Loading reference files")
  reference = getReferenceFile(refName = refName,weirdRange=weirdRange,buffer=buffer,fullRange=fullRange,refCSVPath = refCSVPath,oddsAndEvens=oddsAndEvens)
  
  # if xy, get reference shp and make sure toGeocodeShp is in correct projection
  if (xy) {
    referenceShp = getReferenceShpFile(refShpPath = refShpPath,refShpName = refShpName,planarCRS = planarCRS,refName =refName)
    # projection is changed to planar in order to do gDistance
    toGeocodeShp = spTransform(toGeocodeShp,CRS(planarCRS))
  } else {
    referenceShp = NA
    toGeocodeShp = NA
  }
  print("   Reference loaded")
  
  toGeocode = prepareFileToGeocode(toGeocode = toGeocode,fuzzyMatching = fuzzyMatching,reference = reference,fuzzyMatchDBPath = fuzzyMatchDBPath)
  
  if(!secondaryCheck(smallestGeo = smallestGeo,referenceShp = referenceShp,toGeocodeShp = toGeocodeShp,geographies = geographies,reference = reference,tgID = tgID,matches = matches,toGeocode = toGeocode,xy=xy)) {return(NA)}
  
  print("Starting geocoding")
  # iterating in batches of 3000
  base = 1
  while (base < nrow(toGeocode)) {
    
    # take subset, change base and toRow
    toRow = ifelse((base+batchSize-1)>nrow(toGeocode),nrow(toGeocode),(base+batchSize-1))
    toGeocode_sub = toGeocode[c(base:toRow),]
    base = toRow+1
    
    # expand toGeocode if that option is enabled
    if (expand) {
      toGeocode_sub = expandAddresses(toGeocode_sub,tgID)
    }
    
    
    #prepares the dataframe that will hold the geocode information
    # it has: a unique ID
    #         a row for each geography, including smallest geography
    #         the classifying vars: fm_numDiff, fm_geoDist, fm_type, fm_vars, fm_numMatches
    #         a var for each match type
    full_geocode_sub = data.frame(unique(toGeocode_sub[[tgID]]))
    names(full_geocode_sub ) = tgID
    for (var in c(smallestGeo,geographies,"fm_type","fm_geoDist","fm_numDiff","fm_vars","fm_numMatches",
                  unlist(lapply(matches,FUN = function(x){paste(x[[1]],collapse="; ")})))) {full_geocode_sub[,var]=NA}
    
    # iterating through each match
    for (matchVars in matches) {
      match = matchVars[[1]]
      maxNumDifference = matchVars[[2]]
      maxGeoDistance = matchVars[[3]]
      
      # get all of the potential matches, that will then be reduced down to real data
      # uniqueMatches, geoMatches, nonUnique matches are non-overlapping and each follow the same variable format, so they are then rbinded
      rawMatches = getRawMatches(toGeocode = toGeocode_sub,reference = reference,match = match,smallestGeo = smallestGeo,geographies = geographies,maxNumDifference = maxNumDifference,tgID = tgID)
      
      if (nrow(rawMatches)>0) {
        # first processing of raw matches, gets unique matches
        uniqueMatches = getUniqueMatches(rawMatches = rawMatches,tgID = tgID,smallestGeo = smallestGeo,reference = reference,geographies = geographies)
        
        # removes unique matches, gets geo matches
        remainingRawMatches = rawMatches[ !rawMatches[[tgID]] %in% uniqueMatches[[tgID]],]
        if (!is.na(maxGeoDistance)) {
          geoMatches = getGeoMatches(remainingRawMatches = remainingRawMatches,tgID = tgID,smallestGeo = smallestGeo,geographies =  geographies,referenceShp = referenceShp,toGeocodeShp = toGeocodeShp,maxGeoDistance = maxGeoDistance,reference = reference)
          remainingRawMatches = remainingRawMatches[ !remainingRawMatches[[tgID]] %in% geoMatches[[tgID]],]
        } else {
          geoMatches = emptyDF(c(tgID,smallestGeo,geographies,"fm_type","fm_geoDist"))
        }
        
        # removes geo matches, gets non unique matches
        nonUniqueMatches = getNonUniqueMatches(remainingRawMatches = remainingRawMatches,tgID = tgID,geographies = geographies,smallestGeo = smallestGeo)
        
        # create allMatches, add the meta variables
        allMatches = rbind(uniqueMatches,geoMatches,nonUniqueMatches)
        allMatches = addMetaVars(allMatches = allMatches,match = match,tgID = tgID,smallestGeo = smallestGeo,rawMatches = rawMatches)
        
        
        # add data into the full_geocode file
        full_geocode_sub = merge_and_move(full_geocode_sub, allMatches,byx=tgID,allx=T,varList = c(smallestGeo,geographies,paste(match,collapse="; "), "fm_type","fm_geoDist","fm_numDiff","fm_vars","fm_numMatches"))
        
        # remove matches that we have smallestGeo for from toGeocode file
        toGeocode_sub = toGeocode_sub[ ! toGeocode_sub[[tgID]] %in% allMatches[[tgID]][!is.na(allMatches[[smallestGeo]])],]
      }
      
      # if nothing was merged on, the number of matches is made 0
      full_geocode_sub[[paste(match,collapse="; ")]][is.na(full_geocode_sub[[paste(match,collapse="; ")]])]=0
    }
    
    if (!exists("full_geocode")) {
      full_geocode = full_geocode_sub
    } else {
      full_geocode = rbind(full_geocode,full_geocode_sub)
    }
    print(paste(toRow,"/",nrow(toGeocode)," processed. ",sum(!is.na(full_geocode[[smallestGeo]])),"/",toRow," fully geocoded.",sep=""))
  }
  #return the geocoded file and the original file
  full_geocode$fm_type = ifelse(!is.na(full_geocode$fm_type),full_geocode$fm_type,
                                ifelse(rowSums(full_geocode[,unlist(lapply(matches,FUN = function(x){paste(x[[1]],collapse="; ")}))])>0,"Non-Unique matches","No matches"))
  print("Finished geocode: ")
  print(table(full_geocode$fm_vars,full_geocode$fm_type,useNA = "always"))
  
  #rearranges the vars before returning
  full_geocode = full_geocode[,c( c(tgID,smallestGeo,geographies,"fm_type","fm_geoDist","fm_numDiff","fm_vars","fm_numMatches") ,
                   setdiff(names(full_geocode), c(tgID,smallestGeo,geographies,"fm_type","fm_geoDist","fm_numDiff","fm_vars","fm_numMatches")))]
  
  return(full_geocode)
}
emptyDF = function(varNames){
  emptyDF = data.frame()
  for (var in varNames) {emptyDF[,var]=logical(0)}
  return(emptyDF)
}

getReferenceShpFile = function(refShpPath,refShpName,planarCRS,refName) {
    # if geographic data is included it pulls the geographic reference data
    referenceShp = readOGR(path.expand(refShpPath),refShpName,stringsAsFactors=F,verbose = F)
    referenceShp = spTransform(referenceShp,CRS(planarCRS))
    # this will have to be changed if the name changes
    if (refName == "LandParcels") {referenceShp@data$Land_Parcel_ID = referenceShp@data$Ln_P_ID}    
    return(referenceShp)
}

prepareFileToGeocode = function(toGeocode,fuzzyMatching,reference,fuzzyMatchDBPath) {
  for (var in c("num1","num2","street_c","suffix_c","unit_c","zip_c","city_c")) {
    if (!var %in% names(toGeocode)) {
      print(paste(var, "not found in toGeocode; set to NA"))
      toGeocode[,var]=NA
    }
  }
  
  toGeocode$num1 = clean_num(toGeocode$num1)[,2]
  toGeocode$num2 = clean_num(toGeocode$num2)[,2]
  toGeocode$street_c = clean_streetName(toGeocode$street_c)
  toGeocode$unit_c = (clean_unit(unit = toGeocode$unit_c, num = toGeocode$num1))
  toGeocode$suffix_c = clean_suffix(toGeocode$suffix_c)
  toGeocode$zip_c = clean_zip(toGeocode$zip_c)
  toGeocode$city_c = clean_city(toGeocode$city_c)
  
  if (fuzzyMatching) {
    toGeocode = fuzzymatchNames(df = toGeocode,reference = reference,referenceType="df",fuzzyMatch = Inf,fuzzyMatchDBPath=fuzzyMatchDBPath)
  }
  return(toGeocode)
}

getRawMatches = function(toGeocode_sub,reference,match,smallestGeo,geographies,maxNumDifference,tgID) {
  rawMatches = merge(
    toGeocode_sub[complete.cases(toGeocode_sub[,match]), c(match,tgID)],
    reference[complete.cases(reference[,match]), c(match,smallestGeo,geographies)],
    by=match
  )
  
  # make numDiff field, which is NA if num is not in the match
  if ( "num1" %in% match) {
    rawMatches = rename(rawMatches, num1.x = num1)
    rawMatches$num1.y = rawMatches$num1.x
    if (nrow(rawMatches)>0) {
      rawMatches$numDiff = 0
    } else {
      rawMatches$numDiff=logical(0)
    }
  }
  # match not on num, to get imperfect num matches
  # if maxNumDifference is not NA, then these matches are allowed
  if (!is.na(maxNumDifference) & "num1" %in% match & length(match) != 1) {
    toGeocode_sub_notMatched = toGeocode_sub[ !toGeocode_sub[[tgID]] %in% rawMatches[,tgID],]
    match_noNum = setdiff(match,"num1")
    rawMatches_noNum = merge(
      toGeocode_sub_notMatched[complete.cases(toGeocode_sub_notMatched[,match_noNum]), c(match,tgID)],
      reference[complete.cases(reference[,match_noNum]), c(match,smallestGeo,geographies)],
      by=match_noNum
    )
    
    # find the closest match
    rawMatches_noNum$numDiff  = abs(as.numeric(rawMatches_noNum$num1.x)-as.numeric(rawMatches_noNum$num1.y))
    # change something here if we want it to only get matches that are both even or both odd
    rawMatches_noNum = rawMatches_noNum[ !is.na(rawMatches_noNum$numDiff) & 
                                                       rawMatches_noNum$numDiff <= maxNumDifference,]
    rawMatches_noNum = rawMatches_noNum[ order(rawMatches_noNum$numDiff),]
    rawMatches_noNum = rawMatches_noNum[ !duplicated(rawMatches_noNum[,tgID]),]
    
    # add the num matches back in 
    rawMatches = rbind(rawMatches, rawMatches_noNum)
  }
  return(rawMatches)
}


getUnique = function(df,idVar,targetVar) {
  uniqueVals = by(data = df,INDICES = df[[idVar]],FUN = function(x,targetVar){return(oneOrNone(x[[targetVar]]))},targetVar=targetVar)
  uniqueVals = data.frame(ph1 = names(uniqueVals),ph2=as.vector(uniqueVals),stringsAsFactors=F)
  names(uniqueVals) = c(idVar,targetVar)
  return(uniqueVals)
}

getUniqueMatches = function(rawMatches,tgID,smallestGeo,reference,geographies) {
  uniqueMatches = getUnique(rawMatches,tgID,smallestGeo)
  uniqueMatches = uniqueMatches[ !is.na(uniqueMatches[[smallestGeo]]),]
  # get geographies
  uniqueMatches = merge(uniqueMatches, reference[!duplicated(reference[[smallestGeo]]),c(smallestGeo,geographies)],by=smallestGeo,all.x=T)
  if (nrow(uniqueMatches)>0) {
    uniqueMatches$fm_type = "Unique"
    uniqueMatches$fm_geoDist = NA
  } else {
    uniqueMatches$fm_type = character(0)
    uniqueMatches$fm_geoDist = logical(0)
  }
  return(uniqueMatches)
}
# gets geographic matches
getGeoMatches = function(remainingRawMatches,tgID,smallestGeo, geographies,referenceShp,toGeocodeShp,maxGeoDistance,reference) {
  # removing matches that don't have geographic data
  toAggregate = remainingRawMatches[ remainingRawMatches[[smallestGeo]] %in% referenceShp@data[[smallestGeo]] & 
                                       remainingRawMatches[[tgID]] %in% toGeocodeShp@data[[tgID]],]
  if (nrow(toAggregate)>0) {
    geoMatches = by(toAggregate,INDICES = toAggregate[[tgID]],findClosestGeo,referenceShp = referenceShp, toGeocodeShp=toGeocodeShp,smallestGeo=smallestGeo,tgID=tgID)     
    geoMatches = data.frame(ph1 = names(geoMatches),ph2 = unlist(lapply(geoMatches,'[[',1)),ph3 =unlist(lapply(geoMatches,'[[',2)),stringsAsFactors=F)
    names(geoMatches) = c(tgID,smallestGeo,"fm_geoDist")
    geoMatches = geoMatches[ as.numeric(geoMatches$fm_geoDist)<maxGeoDistance,]
    geoMatches = merge(geoMatches,reference[!duplicated(reference[[smallestGeo]]),c(smallestGeo,geographies)],by=smallestGeo,all.x=T)
    if (nrow(geoMatches)>0) {
      geoMatches$fm_type = "Geo"
    } else {
      geoMatches$fm_type = character(0)
    }
    
    return(geoMatches)
  } else {
    return(emptyDF(c(tgID,smallestGeo,geographies,"fm_type","fm_geoDist")))
  }
}

getNonUniqueMatches = function(remainingRawMatches,tgID,geographies,smallestGeo) {
  if (nrow(remainingRawMatches)>0) {
    nonUniqueMatches = data.frame(unique(remainingRawMatches[[tgID]]))
    names(nonUniqueMatches)=tgID
    nonUniqueMatches[,smallestGeo]=NA
    for (geo in geographies) {
      nonUniqueMatches = merge(nonUniqueMatches,getUnique(remainingRawMatches,tgID,geo),by=tgID,all.x=T)
    }
    nonUniqueMatches$fm_type = NA
    nonUniqueMatches$fm_geoDist = NA
    return(nonUniqueMatches)
  } else {
    return(emptyDF(c(tgID,smallestGeo,geographies,"fm_type","fm_geoDist")))
  }
}

addMetaVars = function(allMatches,match,tgID,smallestGeo,rawMatches) {
  if ("num1" %in% match) {
    # get numDiff
    minNumDiff = aggregate(rawMatches[["numDiff"]], by = list(rawMatches[[tgID]]),FUN = min,na.rm=T)
    names(minNumDiff)=c(tgID,"fm_numDiff")
    allMatches = merge(allMatches,minNumDiff,by=tgID,all.x=T)
    # fm_numDiff should be NA if there was not a final match (fm)
    allMatches$fm_numDiff[is.na(allMatches[[smallestGeo]])]=NA
  } else {
    allMatches$fm_numDiff = NA
  }
  
  allMatches$fm_vars = ifelse(is.na(allMatches[[smallestGeo]]),NA,paste(match,collapse="; "))
  
  numMatches = by(rawMatches,INDICES = rawMatches[[tgID]],function(df,x){length(unique(df[[x]][!is.na(df[[x]])]))},x=smallestGeo) 
  numMatches = data.frame(ph1 = names(numMatches),ph2 = as.vector(numMatches),stringsAsFactors=F)
  names(numMatches) = c(tgID,paste(match,collapse="; "))
  allMatches = merge(allMatches,numMatches,by=tgID,all=T)
  # fm_numMatches only exists for final match (fm)
  allMatches$fm_numMatches = ifelse(is.na(allMatches[[smallestGeo]]),NA,allMatches[[paste(match,collapse="; ")]])
  return(allMatches)
}

initialCheck = function(xy, refShpPath, refShpName, toGeocodeShp,toGeocode,refName,tgID){
 
  if (refName != "LandParcels" & refName != "Roads" & refName != "Sam") {
    print("Invalid reference name provided - EXITING")
    return(0)
  }
  if (!tgID %in% names(toGeocode)) {
    print("tgID not in toGeocode data frame - EXITING")
    return(0)
  }
  if (xy) {
    if ( (is.na(refShpPath) | is.na(refShpName))) {
      print("Geographic mode indicated but no spatial reference provided - EXITING")
      return(0)
    }
    if (!isS4(toGeocodeShp)) {
      print("Geographic mode indicated but missing or invalid toGeocodeShp - EXITING")
      return(0)
    }
    if ( !"data" %in% slotNames(toGeocodeShp)) {
      print("toGeocodeShp has no slot data, likely wrong type of file - EXITING")
      return(0)
    }
    if ( "data" %in% slotNames(toGeocodeShp)) {
      if (!tgID %in% names(toGeocodeShp@data)) {
        print("tgID not in toGeocodeShp - EXITING")
        return(0)
      }
    }
  }

    return(1)
}

secondaryCheck = function(smallestGeo,referenceShp,toGeocodeShp,geographies,reference,tgID,matches,toGeocode,xy) {
  if (xy) {
    if (! smallestGeo %in% names(referenceShp@data)) {
      print("Reference shapefile missing smallestGeo - EXITING")
      return(0)
    }
  }
  # check that toGeocode and reference each have the right variables
  if (sum(c(geographies,smallestGeo) %in% names(reference)) < length(c(geographies,smallestGeo))) {
    print("Reference file missing some geographies - EXITING")
    print(names(reference))
    print(c(geographies,smallestGeo))
    return(0)
  }
  
  if (sum(unique(unlist(lapply(matches,'[[',1))) %in% names(reference)) < length(unique(unlist(lapply(matches,'[[',1))))) {
    print("Reference file missing some matching fields - EXITING")
    return(0)
  }
  if (sum(unique(unlist(lapply(matches,'[[',1))) %in% names(toGeocode)) < length(unique(unlist(lapply(matches,'[[',1))))) {
    print("toGeocode file missing some matching fields - EXITING")
    return(0)
  }
  return(1)
}



# a function used in the aggregation step of geographic matches, finds the closest polygon in the refernce file
findClosestGeo = function(df,referenceShp,toGeocodeShp,smallestGeo,tgID) {
    referenceShp_sub  = referenceShp[ referenceShp@data[[smallestGeo]] %in% df[[smallestGeo]],]
    toGeocodeShp_sub = toGeocodeShp[ toGeocodeShp@data[[tgID]] %in% df[[tgID]],]
    minDist = Inf
    theRow = NA
    for (i in c(1:nrow(referenceShp_sub@data))) {
      dist = gDistance(toGeocodeShp_sub,referenceShp_sub[i,])
      if (dist < minDist) {
        theRow = i
        minDist = dist
      }
    }
    return(list(referenceShp_sub@data[theRow,smallestGeo],minDist)) 
}


# a function used in the aggregation step of geographic matches, finds the closest polygon in the refernce file
findClosestNum = function(df,smallestGeo,tgID) {
  df= df[order(as.numeric(df$num1.x)),]
  temp = abs(as.numeric(df$num1.x) - as.numeric(df$num1.y))
  return(list(df[which.min(temp),smallestGeo],min(temp),length(unique(df[,smallestGeo]))))
}
          
fuzzymatchNames = function(df, reference,referenceType="df",fuzzyMatch=Inf,fuzzyMatchDBPath=NA) {
  # get reference df
  if (referenceType == "path") {
    reference = read.csv(reference,stringsAsFactors=F)    
  } 
  if ("street_1" %in% names(reference)) {
    reference$street_c = reference$street_1
  }
  
  # get fuzzy match DB
  if (!is.na(fuzzyMatchDBPath)) {
    fmdb= read.csv(fuzzyMatchDBPath,stringsAsFactors=F)
    print("Fuzzy matches being recorded")
  } else {
    print("Fuzzy matches not being recorded")
    fmdb = data.frame(street1=NA,street2=NA,decision=NA)
  }
  
  uniqueStreet = unique(df$street_c[! df$street_c %in% reference$street_c])
  for (i in c(1:length(uniqueStreet))) {
    temp = stringdist(uniqueStreet[i],reference$street_c)/nchar(uniqueStreet[i])
    if (min(temp,na.rm = T) < fuzzyMatch) {
      # check if comparison already exists
      dbCheck = tryCatch({
        grepl(uniqueStreet[i],fmdb$street1)*grepl(reference$street_c[which.min(temp)],fmdb$street2)
      },error = function(e){
        rep(0,length(fmdb$street1))
      })  
      if (sum(dbCheck)>0) {
        # there should only be one, but just in case I add the [1]
        decision = fmdb$decision[dbCheck==1][1]
      } else {
        decision = 2-menu(c("Accept", "Reject","End fuzzy matching"), title=paste(c(i,"/",length(uniqueStreet),": ",uniqueStreet[i],"--->",reference$street_c[which.min(temp)]),collapse=""))
        if (decision == -1) {
          print("Ending fuzzy matching")
          break
        }
        fmdb[nrow(fmdb)+1,]=c(uniqueStreet[i],reference$street_c[which.min(temp)],decision)
      }
      
      if (as.numeric(decision)==1) {
        df[!is.na(df$street_c) & df$street_c == uniqueStreet[i],"street_c"] = reference$street_c[which.min(temp)]
        if (sum(dbCheck)>0){print(paste(c(uniqueStreet[i],"-----YES---->",reference$street_c[which.min(temp)]),collapse=""))}
      } else {
        if (sum(dbCheck)>0){print(paste(c(uniqueStreet[i],"-----NO------",reference$street_c[which.min(temp)]),collapse=""))}
      }
    }
    if (i%%50==0) {
      if (!is.na(fuzzyMatchDBPath)) {
        write.csv(fmdb,fuzzyMatchDBPath,row.names=F) 
      }
    }
  }
  if (!is.na(fuzzyMatchDBPath)) {
    write.csv(fmdb,fuzzyMatchDBPath,row.names=F) 
  }
  
  return(df)
}


# returnFullReference and expandAddresses are used to call the reference dataframe
getReferenceFile = function(refName,weirdRange=F,buffer=0,fullRange=F,oddsAndEvens=F, refCSVPath) {
  
  # path to roads file or landparcels file; roads file needs to have NSA, BRA_PD, etc. attached - if it doesn't first merge it on from a BG file
  referenceOriginal = read.csv(refCSVPath,stringsAsFactors=F)
  
  if (refName == "LandParcels") {  
    reference_raw = "placeholder"
    
    ## SSH: these columns are present in landparcel2017 but not after: "minNum_","maxNum_","street_","suffix_","TLID_" "num, Zip. So I need to remove them to make the function work
    ## SSH: created some of them by using the cleaning address functions
    #loops through the two sets of address variables in a land parcels to get all possible places
    for (num in c(1:2)) {
      # parcels_sub = referenceOriginal[,c("Land_Parcel_ID",paste(c("minNum_","maxNum_","street_","suffix_","TLID_"),num,sep="" ),"zip","X","Y","Blk_ID_10","BG_ID_10","CT_ID_10","NSA_Name","BRA_PD")]
      # names(parcels_sub)[2:7]<-c("num1","num2","street_c","suffix_c","TLID","zip_c")
      parcels_sub = referenceOriginal[,c("Land_Parcel_ID",paste(c("num1","num2","street_c","suffix_c","TLID"),sep="" ),"X","Y","Blk_ID_10","BG_ID_10","CT_ID_10","NSA_Name","BRA_PD")]
      names(parcels_sub)[2:6]<-c("num1","num2","street_c","suffix_c","TLID") # not necessary but...
      if (!is.data.frame(reference_raw)) {
        reference_raw =  parcels_sub
      } else {
        reference_raw = rbind(reference_raw,parcels_sub)
      }
      reference_raw = reference_raw[!is.na(reference_raw$num1),]
    }
    # cleans here, in case any changes to the cleaning has been made since constructing the land parcels file, for example i just added something to take out apostrophes in street names
    reference_raw$zip_c = clean_zip(reference_raw$zip_c)
    reference_raw$street_c = clean_streetName(reference_raw$street_c)
    reference_raw$suffix_c = clean_suffix(reference_raw$suffix_c)
    reference_raw$city_c=NA
    
    #gives a generic name to the ID
    reference_raw$ReferenceID = reference_raw$Land_Parcel_ID
    
  } else if (refName=="Roads") {
    
    # cleans the roads file, the land parcels were already cleaned from when it was created
    temp = separate_suffix(referenceOriginal$FULLNAME)
    referenceOriginal$street_c = temp[,2]
    referenceOriginal$suffix_c = temp[,3]
    # separates the left and right side of the street into different rows
    ## SSH: again, some of these columns do not exist anymore
    # roads_r = rename(referenceOriginal[, c("TLID","RFROMADD","RTOADD","street_c","suffix_c","ZIPR","BG_ID_10","CT_ID_10","NSA_Name","BRA_PD")],num1 = RFROMADD,num2 = RTOADD,zip = ZIPR)
    # roads_r$side = "R"
    # roads_l = rename(referenceOriginal[, c("TLID","LFROMADD","LTOADD","street_c","suffix_c","ZIPL","BG_ID_10","CT_ID_10","NSA_Name","BRA_PD")],num1 = LFROMADD,num2 = LTOADD,zip = ZIPL)
    # roads_l$side = "L"
    roads_r = rename(referenceOriginal[, c("TLID","RFROMADD","RTOADD","ZIPR","BG_ID_10","CT_ID_10")],num1 = RFROMADD,num2 = RTOADD,zip = ZIPR)
    roads_r$side = "R"
    roads_l = rename(referenceOriginal[, c("TLID","LFROMADD","LTOADD","ZIPL","BG_ID_10","CT_ID_10")],num1 = LFROMADD,num2 = LTOADD,zip = ZIPL)
    roads_l$side = "L"
    reference_raw = rbind(roads_r,roads_l)
    # cleans the rest (i'm not sure if there's any reason  cleaned the street and suffix first above)
    reference_raw$zip_c = clean_zip(reference_raw$zip)
    reference_raw$num1 = clean_num(reference_raw$num1)[,2]
    reference_raw$num2 = clean_num(reference_raw$num2)[,2]
    reference_raw$city_c = NA
    # its referenceID is the TLID + a letter specifying the side
    reference_raw$ReferenceID = paste(reference_raw$TLID,reference_raw$side,sep="")
    ## SSH: again, some of these columns do not exist anymore
    # reference_raw = reference_raw[ ,c("ReferenceID", "num1","num2","street_c","suffix_c","zip_c","city_c","TLID","BG_ID_10","CT_ID_10","NSA_Name","BRA_PD")]
    reference_raw = reference_raw[ ,c("ReferenceID", "num1","num2","TLID","BG_ID_10","CT_ID_10")]
    reference_raw = reference_raw[!is.na(reference_raw$num1),]
  } else if (refName == "Sam") {
    # reup the cleaning
    reference_raw = referenceOriginal
    reference_raw$street_c = clean_streetName(reference_raw$street_c)
    reference_raw$suffix_c = clean_suffix(reference_raw$suffix_c)
    reference_raw$zip_c = clean_zip(reference_raw$zip_c)
    reference_raw$city_c = clean_city(reference_raw$city_c)
    reference_raw$unit_c = clean_unit(unit = reference_raw$unit_c,num = reference_raw$num1)
    reference_raw$ReferenceID = reference_raw$SAM_ADDRESS_ID
  } else {
    print("REFERENCE NAME NOT FOUND")
  }
  
  # after this point reference_raw needs to be in cleaned form
  # this then expands addresses like 1-5 Maple St. to 3 rows: 1, 3, 5 Maple St.
  reference = expandAddresses(reference_raw,weirdRange=weirdRange,buffer=buffer,fullRange=fullRange,oddsAndEvens=oddsAndEvens)
  
  return( reference)
}

# expands addresses like 1-5 Maple St. to 3 rows: 1, 3, 5 Maple St.
# used in other places besides the geocoder (in creating the geographical infrastructure for example, actually might be the only other place)
expandAddresses = function(reference_raw,ReferenceID = "ReferenceID",weirdRange=F,buffer=0,fullRange=F,oddsAndEvens=T) {
  #this expands out addresses that cover multiple numbers into multiple rows
  #if weird range is true it allows ranges where the second number is smaller than the first
  #the problem with this is it interprets 112-3 as 3-112, when it should probably be 112-113!
  
  
  # have to do this stupid thing because later in the group_by you can't pass a variable
  reference_raw$ReferenceID = reference_raw[,ReferenceID]
  
  # some of these number manipulations could be made more efficient, but this feels like clear steps
  # 1. makes them numeric, makes num2 = num1 if num2 is missing, so that adding the buffer will work correctly
  reference_raw$num1 = as.numeric(reference_raw$num1)
  reference_raw$num2 = ifelse(is.na(reference_raw$num2),reference_raw$num1,as.numeric(reference_raw$num2))
  # 2. if weirdRange is enabled, it makes num1 the minimum, num2 the maximum, otherwise it makes num2 = num1 if it is less than it
  if (weirdRange) {
    num2 = reference_raw$num2
    num1 = reference_raw$num1   
    reference_raw$num1 = pmin(num1,num2)
    reference_raw$num2 = pmax(num1,num2)
  } else {
    reference_raw$num2 = ifelse(reference_raw$num2 < reference_raw$num1, reference_raw$num1, reference_raw$num2)
  }
  
  # 3. oddsAndEvens determines whether both numbers in the address must be the same sign, whether 2-5 should be changed to 2-4
  if (oddsAndEvens) {
    reference_raw$num2 = ifelse((reference_raw$num2-reference_raw$num1)%%2 == 0, reference_raw$num2, reference_raw$num2-1)
  }
  
  
  # 4. with the numbers set, it incorporates the buffer
  # right now, if the we allow numbers with the buffer to go down to 0, which might be dangerous
  reference_raw$num1 = ifelse((reference_raw$num1 - buffer)>=0,reference_raw$num1 - buffer,
                              ifelse(reference_raw$num1%%2==1,1,0))

  reference_raw$num2 = reference_raw$num2 + buffer
  
  reference_raw$num_addr = 1
  #calculates the number of addresses that will be created out of each row
  if (fullRange) {
    reference_raw$num_addr = ifelse((reference_raw$num2-reference_raw$num1+1)<1,1,
                                (reference_raw$num2-reference_raw$num1+1))
  } else {
    reference_raw$num_addr = ifelse((ceiling((reference_raw$num2-reference_raw$num1)/2)+1)<1,1,
                                (ceiling((reference_raw$num2-reference_raw$num1)/2)+1))      
  }

  reference_raw$num_addr[ is.na(reference_raw$num_addr)]=1
  # expands the rows with multiple numbers into multiple rows then binds them back in
  reference_raw = reference_raw[rep(row.names(reference_raw),reference_raw$num_addr),]
  if (fullRange) {
    reference_raw = reference_raw %>%
      group_by(ReferenceID) %>%
      mutate(addOn = ((row_number())-1)) %>%
      ungroup()    
  } else {
    reference_raw = reference_raw %>%
      group_by(ReferenceID) %>%
      mutate(addOn = ((row_number()*2)-2)) %>%
      ungroup()  
  }
  
  reference_raw$num1 = as.character(ifelse((reference_raw$num1 + reference_raw$addOn)<=reference_raw$num2,
                                           reference_raw$num1 + reference_raw$addOn,
                                           reference_raw$num2))
  reference_raw$num2 <- NULL
  reference_raw$addOn <- NULL

  return(reference_raw)
}

oneOrNone <- function(x) {
  uniqueVals = unique(x)
  return(ifelse(
    length(uniqueVals) == 1,
    uniqueVals,
    NA
  )
  )
}
# another helper function
merge_and_move <- function(dataset1,dataset2,byx,varList,byy=NA,allx=T,ally=F) {
  if (is.na(byy)) {byy=byx}
  merge = merge(dataset1, dataset2[,c(byy,varList)],by.x=byx,by.y=byy,all.x=allx,all.y=ally )
  for (var in varList) {
    varx = paste(var,".x",sep="")
    vary = paste(var,".y",sep="")
    merge[,var]=ifelse(!is.na(merge[,varx]),merge[,varx],merge[,vary])
    merge[,varx] <- NULL
    merge[,vary] <- NULL
  }
  return(merge)
}
