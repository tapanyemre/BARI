library(stringr)
library(dplyr)

###### HG 6/29 
# A huge amount of this should be replaced by libPostal :L
### HG 7/27 - although.... i used a different library in python and it wasn't all that effective



#--------------------------------------#
#         Cleaning Functions           #
#--------------------------------------#
trim <- function (x) gsub("^\\s+|\\s+$", "", x)  

splitAddress = function(addrVarName,dataset) {
  
  dataset$addressSearch = toupper(dataset[,addrVarName])
  
  
  # zip
  zip_reg = "( [0-9]{1,5})([ -])?([0-9]{1,5})?$"
  dataset$zip_c = clean_zip(str_match(dataset$addressSearch,zip_reg)[,2])
  dataset$addressSearch = trim(gsub(zip_reg,"",dataset$addressSearch))
  
  # MA
  ma_reg = " MA$"
  dataset$addressSearch = trim(gsub(ma_reg,"",dataset$addressSearch))
  
  
  # CITY
  city_reg = "( MISSION HILL| SOUTH BOSTON| MATTAPAN| DORCHESTER| HYDE PARK| ROXBURY| JAMAICA PLAIN| BOSTON| CHESTNUT HILL| ALLSTON| BRIGHTON| CHARLESTOWN| ROSLINDALE| REVERE| BROOKLINE| WINTHROP| CHELSEA| DORCHESTER CENTER| ROXBURY CROSSING| SPRINGFIELD)$"
  dataset$city_c = clean_city(str_match(dataset$addressSearch,city_reg))[,2]
  dataset$addressSearch = trim(gsub(city_reg,"",dataset$addressSearch))
  
  
  # SUFFIX
  suffix_unit_reg = "(.*)( PKWY | PKWY$| LN | LN$| WHF | WHF$| TER | TER$| RD | RD$| PL | PL$| CIR | CIR$| BLVD | BLVD$| WAY | WAY$| DR | DR$| CT | CT$| HWY | HWY$| ST | ST$| ALLEY | AVE | AVENUE | BOULEVARD | CIRCLE | COURT | DRIVE | EXT\\. | HIGHWAY | LANE | PARK | PARKWAY | PLACE | ROAD | ROADWAY | ROW | SQUARE | STREET | TERRACE | WAY | ALLEY$| AVE$| AVENUE$| BOULEVARD$| CIRCLE$| COURT$| DRIVE$| EXT\\.$| HIGHWAY$| LANE$| PARK$| PARKWAY$| PLACE$| ROAD$| ROADWAY$| ROW$| SQUARE$| STREET$| TERRACE$| WAY$)([-A-Z0-9# .,/]*)?$"
  temp = str_match(dataset$addressSearch, suffix_unit_reg)
  dataset$unit = temp[,4]
  dataset$suffix_c = clean_suffix(temp[,3])
  dataset$addressSearch = ifelse(!is.na(temp[,2]),temp[,2],dataset$addressSearch)
  rm(temp)
  
  # replace street names that have a number that will mess this up ("PIER 4")
  dataset$addressSearch = gsub("PIER 4|PIERRE 4","PIER FOUR",dataset$addressSearch)
  dataset$addressSearch = gsub("1ST$","FIRST",
                               gsub("2ND$","SECOND",
                                    gsub("3RD$","THIRD",
                                         gsub("4TH$","FOURTH",
                                              gsub("5TH$","FIFTH",
                                                   gsub("6TH$","SIXTH",
                                                        gsub("7TH$","SEVENTH",
                                                             gsub("8TH$","EIGHTH",
                                                                  gsub("9TH$","NINTH",
                                                                       gsub("10TH$","TENTH",
                                                                            gsub("11TH$","ELEVENTH",
                                                                                 gsub("12TH$","TWELTH",
                                                                                      gsub("13TH$","THIRTEENTH",
                                                                                           gsub("14TH$","FOURTEENTH",
                                                                                                gsub("15TH$","FIFTEENTH",
                                                                                                     gsub("16TH$","SIXTEENTH",
                                                                                                          gsub("17TH$","SEVENTEENTH",
                                                                                                               gsub("18TH$","EIGHTEENTH",
                                                                                                                    gsub("19TH$","NINETEENTH",
                                                                                                                         gsub("20TH$","TWENTIETH",
                                                                                                                              dataset$addressSearch))))))))))))))))))))
  
  
  # get other ones that are missed because they don't have a suffix ("170 PARKER HILL 170-43")
  #unit_reg2 = "(UNIT)? ?[A-Z]?[0-9]+[A-Z]?[ -.]?([A-Z]?[0-9]+[A-Z]?)?$"
  #temp1 = str_match(dataset$addressSearch, unit_reg2)
  twoCharNotUnit = "(?![^A-Z]BLDG)(?![^A-Z]BLD)(?![^A-Z]UNIT)(?![^A-Z]FL)(?![^A-Z]RM)([^A-Z][A-Z]{2,})"
  # adds a space in front in case there is no number, and a space after for the split
  temp = strsplit(paste(paste(" ",dataset$addressSearch,sep="")," ",sep=""), twoCharNotUnit,perl = T)
  
  temp2 = unlist(
    lapply(temp,FUN=function(x){
      if (length(x)>1 & trim(as.character(x[length(x)])) != "" & as.character(x[length(x)]) != ".") {
        return(trim(as.character(x[length(x)])))
      } else {
        return(NA)
      }
    }))
  dataset$unit[is.na(dataset$unit)] = temp2[is.na(dataset$unit)]
  dataset$unit = trim(dataset$unit)
  
  dataset$addressSearch = trim(dataset$addressSearch)
  for (i in c(1:length(temp2))) {
    if (!is.na(temp2[i])) {
      dataset$addressSearch[i] = trim(substr(dataset$addressSearch[i],start = 1,stop=nchar(dataset$addressSearch[i])-nchar(temp2[i])))
    }
  }
  
  rm(temp,temp2)
  
  dataset$postDir = ifelse(   dataset$unit == "E" | dataset$unit == "E." | 
                                dataset$unit == "S" | dataset$unit == "S." | 
                                dataset$unit == "N" | dataset$unit == "N." | 
                                dataset$unit == "W" | dataset$unit == "W." ,gsub("\\.","",dataset$unit),NA)
  dataset$unit[ dataset$unit == "E" | dataset$unit == "E." | 
                  dataset$unit == "S" | dataset$unit == "S." | 
                  dataset$unit == "N" | dataset$unit == "N." | 
                  dataset$unit == "W" | dataset$unit == "W."]=NA
  
  
  
  # NUMBER
  dataset$backup = dataset$addressSearch
  
  dataset$addressSearch = dataset$backup
  
  #oneToNine = c("ONE","TWO","THREE","FOUR","FIVE","SIX","SEVEN","EIGHT","NINE")
  #tenToNineteen = c("TEN","ELEVEN","TWELVE","THIRTEEN","FOURTEEN","FIFTEEN","SIXTEEN","SEVENTEEN","EIGHTEEN","NINETEEN")
  #twentyToNinety = c("TWENTY","THIRTY","FORTY","FIFTY","SIXTY","SEVENTY","EIGHTY","NINETY")
  # these seem to be the only ones that were written as numbers, so no need for combining below
  
  #numbers= c(paste(rep(twentyToNinety,9)[order(rep(twentyToNinety,9))],oneToNine,sep = " "),
  #           paste(rep(twentyToNinety,9)[order(rep(twentyToNinety,9))],oneToNine,sep = ""),
  #           paste(rep(twentyToNinety,9)[order(rep(twentyToNinety,9))],oneToNine,sep = "-"))
  
  dataset$addressSearch = gsub("^ONE ","1 ",
                               gsub("^TWO ","2 ",
                                    gsub("^THREE ","3 ",
                                         gsub("^FOUR ","4 ",
                                              gsub("^FIVE ","5 ",
                                                   gsub("^SIX ","6 ",
                                                        gsub("^SEVEN ","7 ",
                                                             gsub("^EIGHT ","8 ",
                                                                  gsub("^NINE ","9 ",
                                                                       gsub("^TEN ","10 ",
                                                                            gsub("^ELEVEN ","11 ",
                                                                                 gsub("^TWELVE ","12 ",
                                                                                      gsub("^THIRTEEN " ,"13 ",
                                                                                           gsub("^FOURTEEN ","14 ",
                                                                                                gsub("^FIFTEEN ","15 ",
                                                                                                     gsub("^SIXTEEN ","16 ",
                                                                                                          gsub("^SEVENTEEN ","17 ",
                                                                                                               gsub("^EIGHTEEN ","18 ",
                                                                                                                    gsub("^NINETEEN ","19 ",
                                                                                                                         gsub("^TWENTY ","20 ",
                                                                                                                              gsub("^THIRTY ","30 ",
                                                                                                                                   gsub("^FORTY ","40 ",
                                                                                                                                        gsub("^FIFTY ","50 ",
                                                                                                                                             gsub("^SIXTY ","60 ",
                                                                                                                                                  gsub("^SEVENTY ","70 ",
                                                                                                                                                       gsub("^EIGHTY ","80 ",
                                                                                                                                                            gsub("^NINETY ","90 ",
                                                                                                                                                                 dataset$addressSearch)
                                                                                                                                                       ))))))))))))))))))))))))))
  
  
  
  numbers_reg = "^([0-9]+)([A-Z]{0,2})[ -]?([0-9]+([A-Z]{0,2}))?"
  
  temp = str_match(dataset$addressSearch,numbers_reg)
  dataset$num1 = (temp[,2])
  dataset$num2 = (temp[,4])
  temp[is.na(temp[,3]),3]=""
  temp[is.na(temp[,5]),5]=""
  dataset$unit[is.na(dataset$unit)] = ""
  dataset$unit = trim(paste(trim(paste(temp[,5],temp[,3],sep=" ")),dataset$unit,sep=" "))
  dataset$unit[dataset$unit ==""] = NA
  
  # probably a way to combine this into the earlier regex, but grabbing streets that have a single letter (unit) at the beginning but are not just a single letter
  temp = str_match(dataset$addressSearch,"^([ABCDEF]) ")
  
  dataset$street_c = clean_streetName(gsub(numbers_reg,"",dataset$addressSearch))
  rm(temp)
  dataset$addressSearch = NULL
  dataset$backup = NULL
  #View(head(dataset[!is.na(dataset$LOCATION) & is.na(dataset$suffix_c),c(addrVarName,"num1","num2","street_c","suffix_c","postDir","unit","city_c","zip_c")],200))
  
  
  #View(head(dataset[,c(addrVarName,"num1","num2","street_c","suffix_c","postDir","unit","city_c","zip_c")],200))
  
  #View(head(dataset[is.na(dataset$num1),c(addrVarName,"num1","num2","street_c","suffix_c","postDir","unit","city_c","zip_c")],200))
  
  #write.csv(dataset,"Documents/Research/My Research/Thesis/Scraping/dataset/asCSV/dataset_c.csv",row.names=F)
  return(dataset)
}



# this is dumb inefficient code, but it runs quickly enough and seems silly to go back and improve it now - HG 6/2017
# this fixes the name and some common formatting inconsistencies in geographic IDs
standardizeGeoNames <- function(df) {
  
  # property id
  if (!"Property_ID" %in% names(df)) {
    if ("propertyid" %in% names(df)) {df = rename(df, Property_ID = propertyid)}
    if ("property_id" %in% names(df)) {df = rename(df, Property_ID = property_id)} 
    if ("Property_id" %in% names(df)) {df = rename(df, Property_ID = Property_id)} 
    if ("PROPID" %in% names(df)) {df = rename(df, Property_ID = PROPID)}     
  }
  
  # parcel num
  if (!"parcel_num" %in% names(df)) {
    if ("PID" %in% names(df)) {df = rename(df,parcel_num = PID)}
    if ("ParcelNum" %in% names(df)) {df = rename(df,parcel_num = ParcelNum)}
    if ("parcelnum" %in% names(df)) {df = rename(df,parcel_num = parcelnum)}
    if ("Parcel_Num" %in% names(df)) {df = rename(df,parcel_num = Parcel_Num)}
    if ("Parcel_ID" %in% names(df)) {df = rename(df,parcel_num = Parcel_ID)} 
  	if ("parcel_id" %in% names(df)) {df = rename(df,parcel_num = parcel_id)} 
  }
  
  # x
  if (!"X" %in% names(df)) {
    if ("x" %in% names(df)) {df = rename(df,X = x)}
  }
  
  # y
  if (!"Y" %in% names(df)) {
    if ("y" %in% names(df)) {df = rename(df,Y = y)}
  }
  
  # block
  if (!"Blk_ID_10" %in% names(df)) {
    if ("Blk_ID" %in% names(df)) {df = rename(df,Blk_ID_10 = Blk_ID)}
    if ("BLK_ID_10" %in% names(df)) {df = rename(df,Blk_ID_10 = BLK_ID_10)}
    if ("BLK_ID" %in% names(df)) {df = rename(df,Blk_ID_10 = BLK_ID)}
  }
  
  # block group
  if (!"BG_ID_10" %in% names(df)) {
    if ("BG_ID" %in% names(df)) {df = rename(df,BG_ID_10 = BG_ID)}
  }
  
  #tract
  if (!"CT_ID_10" %in% names(df)) {
    if ("CT_ID" %in% names(df)) {df = rename(df,CT_ID_10 = CT_ID)}    
  }
  
  # cleaning values of GIS_ID and parcel_num
  if ("GIS_ID" %in% names(df)) { df$GIS_ID = as.character(as.numeric(gsub("_","",df$GIS_ID))) }
  if ("parcel_num" %in% names(df)) {  df$parcel_num = as.character(as.numeric(gsub("_","",df$parcel_num))) }
  
  return(df)
}

clean_unit = function(unit, num = NA) {
  
  unit_c = trim(gsub("^#|^APT|^UNIT|^STE","",trim(toupper(unit))))
  
  # get rid of street numbers in unit
  if (sum(!is.na(num)) > 0 ) {
    badUnits_cond = (is.na(suppressWarnings(as.numeric(unit_c))) | suppressWarnings(as.numeric(unit_c)) != unit_c) & (unit_c != "" & !is.na(unit_c))
    if (sum(badUnits_cond)>0) {
      badUnits = data.frame(num =   num[badUnits_cond], unit = unit_c[ badUnits_cond],stringsAsFactors = F)
      for (i in c(1:nrow(badUnits))) {
        # if the street number is big it removes it regardless
        if (nchar(badUnits$num[i])>1) {
          badUnits$unit[i] = gsub(paste("^",badUnits$num[i],"-? ?",sep=""),"",badUnits$unit[i],perl = T)
          # but if it's small it might be the floor number, so the regex checks some other stuff
        } else {
          badUnits$unit[i] = gsub(paste("^",badUnits$num[i],"(-| )|^",badUnits$num[i],"(?=[A-Z] ?-?[0-9])",sep=""),"",badUnits$unit[i],perl = T)
        }
      }
      unit_c[badUnits_cond] = badUnits$unit  
    }
  }
  
  # removes spaces and hyphens if it's not a number or a letter on both sides (1-2--->1-2; 1-A--->1A)
  unit_c = gsub(" {2,}"," ",unit_c)
  unit_c = gsub(" - | -|- ","-",unit_c)
  unit_c = gsub("((?<=[0-9])(-| )(?=[A-Z]))|((?<=[A-Z])(-| )(?=[0-9]))","",unit_c,perl = T)
  unit_c[ !is.na(suppressWarnings(as.numeric(unit_c)))] = suppressWarnings(as.numeric(unit_c))[!is.na(suppressWarnings(as.numeric(unit_c)))]
 

  unit_c = trim(gsub("^\\.","",unit_c))
  # LEFT or RIGHT at the end
  unit_c = gsub("LEFT$","L",unit_c)
  unit_c = gsub("RIGHT$","R",unit_c)
  
  # East or West at the end
  unit_c = gsub("EAST$|E.$","E",unit_c)
  unit_c = gsub("WEST$|W.$","E",unit_c)
  
  # 
  unit_c = gsub("^RIGHT SIDE$","R",unit_c)
  unit_c = gsub("^LEFT SIDE$","R",unit_c)
  
  temp = str_match(unit_c,"^(FLR|FL)(.+)(RM|#|APT|,)(.+)$")
  if (sum(!is.na(temp[,1]))>0) {
    temp[,3] = trim(gsub("\\.|,","",temp[,3]))
    temp[,5] = trim(gsub("\\.|,","",temp[,5]))
    temp2 = str_match(temp[,5],"([A-Z]?)([0-9]{0,2})([A-Z]?)")
    if (sum(!is.na(temp2[,1]))>0) {
        temp2[,3] = ifelse(is.na(temp2[,1]),
                     NA,
                     ifelse(nchar(temp2[,3])==1,
                            paste("0",temp2[,3],sep=""),
                            temp2[,3]))
        unit_c[!is.na(temp[,1])] = paste(temp[!is.na(temp[,1]),3],temp2[!is.na(temp[,1]),3],temp2[!is.na(temp[,1]),2],temp2[!is.na(temp[,1]),4],sep="")
    }
  }
  temp = str_match(unit_c,"(^([0-9]{1,4})([A-Z]{1})$)|(^([A-Z]{1})([0-9]{1,4})$)")
  if (sum(!is.na(temp[,1]))>0) {
    unit_c = ifelse(is.na(temp[,1]),unit_c,
                    ifelse(!is.na(temp[,2]),
                           paste(as.numeric(temp[,3]),temp[,4],sep=""),
                           paste(as.numeric(temp[,7]),temp[,6],sep="")))
  }
  unit_c = gsub("^RM.|^RM|^ROOM","",unit_c)
  #
  #temp = str_match(unit_c,"(.+)FLOOR$")
  
  
  temp = str_match(unit_c,"([0-9]{1,2})ROOM([0-9]{1,2})")
  if (sum(!is.na(temp[,1]))>0){
    unit_c[!is.na(temp[,1])]=as.numeric(temp[!is.na(temp[,1]),2])*100 + as.numeric(temp[!is.na(temp[,1]),3])
  }
  return(unit_c) 
}

# cleans a vector of number strings of the format: c("3","5", "3-5","3 5","3A","3-5A",etc.)
clean_num <- function(number) {
  
  #initial cleaning
  number = trim(toupper(as.character(number)))
  
  #this is a temporary fix because numbers like 0.21414 were becoming 0 and 21414
  #i should fix the regex but this is simpler for now
  number[number>0 & number<1]<-NA
  
  #separating numbers 
  # one pattern matches a single number, the other matches two numbers, I could and should combine them but I was dumb when i first wrote it
  pattern1 = "([0-9]+)[^0-9]+([0-9]+)"
  match1=str_match(number,pattern1)
  pattern2 = "([0-9]+)"
  match2=str_match(number,pattern2)
  num1 = rep(NA, length(number))
  num2 = rep(NA,length(number))
  # god this is alot of code that would be much simpler with a better regex
  if (ncol(match1)>1) {
    num1=as.integer(trim(match1[,2]))
  }
  if (ncol(match2)>1) {
    num1[is.na(num1)]=as.integer(trim(match2[is.na(num1),2]))
  }
  num1[is.na(num1)]=as.integer(number[is.na(num1)])
  if (ncol(match1)>2) {
    num2=as.integer(trim(match1[,3]))
  }
  
  # added this because otherwise we get 57-1, 57-2, etc. showing up as 1-57
  # there is a larger problem of numbers being written in as 153-5, meaning 153-155, that should be addressed
  num2[!is.na(num2) & !is.na(num1) & num2<num1 ] = NA
  
  # this returns a matrix! so you have to specify [,2] and [,3] to get num1 and num2 
  return(matrix(data = c(number,num1,num2),ncol=3))
}

# cleans and standardizes a vector of streetnames, returns a vector
# again, a lot of the regexes could be rewritten and combined to be much more efficient
clean_streetName <- function(streetName) {
  streetName <- trim(toupper(streetName))
  streetName <- gsub("&#039;","",streetName)
  streetName <- gsub("'","",streetName)
  streetName <- gsub("-","",streetName)
  
  streetName=gsub("^E ","EAST ",streetName)
  streetName=gsub(" E "," EAST ",streetName)
  streetName=gsub("^E\\. ","EAST ",streetName)
  streetName=gsub(" E\\. "," EAST ",streetName)
  
  streetName=gsub("^S ","SOUTH ",streetName)
  streetName=gsub(" S "," SOUTH ",streetName)
  streetName=gsub("^S\\. ","SOUTH ",streetName)
  streetName=gsub(" S\\. "," SOUTH ",streetName)
  
  streetName=gsub("^W ","WEST ",streetName)
  streetName=gsub(" W "," WEST ",streetName)
  streetName=gsub("^W\\. ","WEST ",streetName)
  streetName=gsub(" W\\. "," WEST ",streetName)
  
  streetName=gsub("^N ","NORTH ",streetName)
  streetName=gsub(" N "," NORTH ",streetName)
  streetName=gsub("^N\\. ","NORTH ",streetName)
  streetName=gsub(" N\\. "," NORTH ",streetName)
  
  streetName=gsub("^MT ","MOUNT ",streetName)
  streetName=gsub(" MT "," MOUNT ",streetName)
  streetName=gsub("^MT\\. ","MOUNT ",streetName)
  streetName=gsub(" MT\\. "," MOUNT ",streetName)
  
  streetName=gsub("^ST\\. ","SAINT ",streetName)
  streetName=gsub(" ST\\. "," SAINT ",streetName)
  streetName=gsub("^ST ","SAINT ",streetName)
  streetName=gsub(" ST "," SAINT ",streetName)
  
  streetName=gsub("^PK ","PARK ",streetName)
  streetName=gsub(" PK "," PARK ",streetName)
  
  streetName=gsub("^1(ST)? ","FIRST ",streetName)  
  streetName=gsub("^1(ST)?$","FIRST",streetName)  
  streetName=gsub(" 1(ST)? "," FIRST ",streetName)
  streetName=gsub(" 1(ST)?$"," FIRST",streetName)  
  
  streetName=gsub("^2(ND)? ","SECOND ",streetName)  
  streetName=gsub(" 2(ND)? "," SECOND ",streetName)  
  streetName=gsub("^2(ND)?$","SECOND",streetName)  
  streetName=gsub(" 2(ND)?$"," SECOND",streetName)
  
  streetName=gsub("^3(RD)? ","THIRD ",streetName)  
  streetName=gsub(" 3(RD)? "," THIRD ",streetName)  
  streetName=gsub("^3(RD)?$","THIRD",streetName)  
  streetName=gsub(" 3(RD)?$"," THIRD",streetName) 
  
  streetName=gsub("^4(TH)? ","FOURTH ",streetName)  
  streetName=gsub(" 4(TH)? "," FOURTH ",streetName)   
  streetName=gsub("^4(TH)?$","FOURTH",streetName)  
  streetName=gsub(" 4(TH)?$"," FOURTH",streetName) 
  
  streetName=gsub("^5(TH)? ","FIFTH ",streetName)  
  streetName=gsub(" 5(TH)? "," FIFTH ",streetName)  
  streetName=gsub("^5(TH)?$","FIFTH",streetName)  
  streetName=gsub(" 5(TH)?$"," FIFTH",streetName) 
  
  streetName=gsub("^6(TH)? ","SIXTH ",streetName)  
  streetName=gsub(" 6(TH)? "," SIXTH ",streetName)  
  streetName=gsub("^6(TH)?$","SIXTH",streetName)  
  streetName=gsub(" 6(TH)?$"," SIXTH",streetName)  
  
  streetName=gsub("^7(TH)? ","SEVENTH ",streetName)  
  streetName=gsub(" 7(TH)? "," SEVENTH ",streetName)  
  streetName=gsub("^7(TH)?$","SEVENTH",streetName)  
  streetName=gsub(" 7(TH)?$"," SEVENTH",streetName)  
  
  streetName=gsub("^8(TH)? ","EIGHTH ",streetName)  
  streetName=gsub(" 8(TH)? "," EIGHTH ",streetName)  
  streetName=gsub("^8(TH)?$","EIGHTH",streetName)  
  streetName=gsub(" 8(TH)?$"," EIGHTH",streetName)  
  
  streetName=gsub("^9(TH)? ","NINTH ",streetName)  
  streetName=gsub(" 9(TH)? "," NINTH ",streetName)  
  streetName=gsub("^9(TH)?$","NINTH",streetName)  
  streetName=gsub(" 9(TH)?$"," NINTH",streetName)  
  
  streetName=gsub("^13(TH)? ","THIRTEENTH ",streetName)  
  streetName=gsub(" 13(TH)? "," THIRTEENTH ",streetName)  
  streetName=gsub("^13(TH)?$","THIRTEENTH",streetName)  
  streetName=gsub(" 13(TH)?$"," THIRTEENTH",streetName)  
  
  streetName=gsub("^16(TH)? ","SIXTEENTH ",streetName)  
  streetName=gsub(" 16(TH)? "," SIXTEENTH ",streetName)
  streetName=gsub("^16(TH)?$","SIXTEENTH",streetName)  
  streetName=gsub(" 16(TH)?$"," SIXTEENTH",streetName)  
  
  #boston specific
  streetName[streetName=="MEDCALF"] = "METCALF"
  streetName[streetName=="JULIET"] = "JULIETTE"
  streetName[streetName=="MLK"] = "MARTIN LUTHER KING"
  streetName[streetName=="MONSIGNOR DENNIS F O'CALLAGHAN"|streetName=="O'CALLAGHAN"|streetName=="MSGR. DENNIS F. O'CALLAGHAN"|streetName=="MSGR DENNIS F. O'CALLAGHAN"] = "MSGR O'CALLAGHAN"
  streetName[streetName=="DE SOTO"] = "DESOTO"
  streetName[streetName=="HENRY STERLING"] = "STERLING"
  streetName[streetName=="PARK VALE"] = "PARKVALE"
  streetName[streetName=="MASS"] = "MASSACHUSETTS"
  streetName[streetName=="BARTLET"] = "BARTLETT"
  streetName[streetName=="CHESBOROUGH"] = "CHESBROUGH"
  streetName[streetName=="SELWIN"] = "SELWYN"
  streetName[streetName=="MARIBOSA"] = "MARIPOSA"
  streetName[streetName=="MCCORMICK"] = "MCCORMACK"
  streetName[streetName=="JULIET"] = "JULIETTE"
  streetName[streetName=="WILLOOWWOOD"] = "WILLOWWOOD"
  streetName[streetName=="ADAM"] = "ADAM"
  streetName[streetName=="MASCHSTS"] = "MASSACHUSETTS"
  streetName[streetName=="COMNWLTH"] = "COMMONWEALTH"
  streetName[streetName=="WM F MCCLELLAN"] = "WILLIAM F MCCLELLAN"
  streetName[streetName=="ROSEBERY"] = "ROSEBERRY"
  streetName[streetName=="MARTIN LUTHER KING JR"] = "MARTIN LUTHER KING"
  streetName[streetName=="CASTLE ROCK"] = "CASTLEROCK"
  streetName[streetName=="MSGR P J LYDON"] = "MSGR PATRICK J LYDON"
  streetName[streetName=="CASTLE ROCK"] = "CASTLEROCK"
  streetName[streetName=="O CONNELL"] = "OCONNELL"
  
  return(streetName)
}

# like the streetname function, cleans and standardizes a vector of suffixes and returns the cleaned vector
clean_suffix <- function(suffix) {
  suffix <- trim(toupper(suffix))
  
  suffix[suffix=="ST S"]<- "ST SOUTH"
  suffix[suffix=="ST EXN"|suffix=="ST EXD"]<- "ST EXTENSION"
  suffix[suffix=="ST W"] <-"ST WEST"
  suffix[suffix=="ST E"] <-"ST EAST"
  suffix[suffix=="ST N"] <-"ST NORTH"
  
  suffix[suffix=="PLAZA"|suffix=="PZ"]<- "PLZ"
  suffix[suffix=="ROWE"|suffix=="RO"] <- "ROW"
  suffix[suffix=="DRIVE"] <- "DR"
  suffix[suffix=="ROAD"] <- "RD"
  suffix[suffix=="AV"|suffix=="AVE."|suffix=="AVENUE"] <- "AVE"
  suffix[suffix=="TER"|suffix=="TE"|suffix=="TERRACE"] <- "TERR"
  suffix[suffix=="COURT"] <- "CT"
  suffix[suffix=="PARKWAY"] <- "PKWY"
  suffix[suffix=="PLACE"] <- "PL"
  suffix[suffix=="STREET"|suffix=="STRET"] <- "ST"
  suffix[suffix=="WY"] <- "WAY"
  suffix[suffix=="LA"| suffix=="LANE"] <- "LN"
  suffix[suffix=="CI"|suffix=="CIRCLE"] <- "CIR"
  suffix[suffix=="HW"|suffix=="HW."|suffix=="HIGHWAY"] <- "HWY" 
  suffix[suffix=="PARK"] <- "PK" 
  suffix[suffix=="CRESCENT"] <- "CRES" 
  suffix[suffix=="CIRCUIT"| suffix=="CC"] <- "CIRT"
  suffix[suffix=="BL"|suffix=="BOULEVARD"] <- "BLVD"
  suffix[suffix=="PKWY"] <- "PW"
  suffix[suffix=="WHARF"] <- "WHF"
  suffix[suffix=="ALLEY"] <- "ALY"
  suffix[suffix=="CROSSWAY"] <- "CWY"
  suffix[suffix=="DAM"] <- "DM"
  suffix[suffix=="DRIVEWAY"] <- "DRWY"
  suffix[suffix=="SQUARE"] <- "SQ"
  suffix[suffix=="VIEW"] <- "VW"
  suffix[suffix=="EXTENSION"] <- "EXT"
  suffix[suffix=="GARDEN"|suffix=="GARDENS"|suffix=="GDNS"] <- "GDN"
  suffix[suffix=="GREEN"] <- "GRN"
  
  suffix[suffix==""] <- NA
  return(suffix)
}

# cleans and standardizes a vector of city names - most of these are for cities/neighborhoods within Boston proper
clean_city <- function(city) {
  city = toupper(trim(city))
  city[city== "NULL"] <- NA
  city[city == "ALSTON"] <- "ALLSTON"
  city[city == "BACKBAY"] <- "BACK BAY"
  city[city == "BEACON HIL"] <- "BEACON HILL"
  city[!is.na(match(city,c("BORSOTN", "BOSTON WEST END", "BOSTON-DORCHESTER", "BOSTON02113", "BOSTON02129", "BOTON")))] <- "BOSTON"
  city[!is.na(match(city,c("BRGHTON","BRIGHTJTON","BRIGHJTON","BRIGTON","BRIGRTON","BTIGHTON","BR")))] <- "BRIGHTON"
  city[!is.na(match(city,c("CHRLESTOWN", "CHARLES TOWN","CHARLESTOEN","CHARLESTONW","CHARLESTOWEN","CHARLESTWON",
                           "CHARLSETOWN","CH")))] <- "CHARLESTOWN"
  city[!is.na(match(city,c("CHESNUT HILL")))] <- "CHESTNUT HILL"
  city[city=="LEATHER DISTR."] <- "CHINATOWN"
  city[!is.na(match(city,c("DORCHESER","DORCHESETER","DORCHESTERT", "DOCHESTER","DOR","DORC","DORC HESTER",
                           "DORCEHSTER","DORCESTER","DORCH","DORCHERSTER","DORCHESTE","DORCHESTER MA","DORCHSTERT",
                           "DORCHESTON","DORCHESTOR/BOSTON","DORCHESWTER","DORCHETSER","DORCHSETER","DORCHSTER",
                           "DORHESTER","DORSHESTER","DO")))] <- "DORCHESTER"
  city[!is.na(match(city,c("E BOSTON","E. BOSTON","E.BOSTON","EAST BOST5ON","EAST BOTON","E BSOTN","EAST BSOTN",
                           "EASTBOSTON","EB")))] <- "EAST BOSTON"
  city[!is.na(match(city,c("FENMORE","FENS/KENMORE","FENWAY","KENMORE")))] <- "FENWAY/KENMORE"
  city[!is.na(match(city,c("HYDE  PARK","HYDE PAEK","HYDE PARD","HYDE PARK 02136","HYDEPARK","HYDEPPARK" ,"HYE PARK",
                           "HYPE PARK","HP")))] <- "HYDE PARK"
  city[!is.na(match(city,c("J P","J.P","JAMCIA PLAIN","JAMAIC PLAIN","JAMAICA","JAMAICA BLAIN","JAMAICA PLAINI","JAMAICA PLAN",
                           "JAMAICAPLAIN","JAMAICIA PLAIN","JAMIACA PLAIN","JAMICA PLAIN","JP","JAMACIA PLAIN")))] <- "JAMAICA PLAIN"
  
  city[!is.na(match(city,c("MATAPABN","MATAPAN","MATTAAPAN","MATTAPN","MATTTAPAN","MT")))] <- "MATTAPAN"
  
  city[!is.na(match(city,c("N END","NO. END","NORTHEND")))] <- "NORTH END"
  city[!is.na(match(city,c("ROLINDALE","ROSINDALE","ROSLINDANLE","ROSLINADLE","ROSLINDAEL","ROSLINDALE ST","ROSLNDALE",
                           "ROSLIDANLE","RS")))] <- "ROSLINDALE"
  city[!is.na(match(city,c("ROXBURY CROSSING","ROX","ROXBURY02119","ROXBURY02120","TOXBURY")))] <- "ROXBURY"
  
  city[!is.na(match(city,c("S  BOSTON","S BOSTON","S. BOSTON","SO. BOSTON","SO.BOSTON","SOTH BOSTON","SOUHT BOSTON","SOUTH BOSOTN",
                           "SOUTH BOSTN","SOUTH BOSTOM","SOUTH BPSTOM","SOUTHBOSTON","BOUTH BOSTON","SB")))] <- "SOUTH BOSTON"
  
  city[!is.na(match(city,c("S END","SE","SO. END","SOUTHEND")))] <- "SOUTH END"
  
  city[!is.na(match(city,c("W ROXBURY","W  ROXBURY","W. ROX","W. ROXBURY","W.ROXBURY","WEST  ROXBURY","WEST ROXB URY",
                           "WEST ROXBRUY","WEST ROXBRY","WEST RXOBURY","WESTROXBURY","WR")))] <- "WEST ROXBURY"
  city[city == "" | city == "NA" | city == "N/A"] <- NA
  return(city)
}

# cleans and returns a vector of zips, again, crappy regex!
clean_zip <- function(zip) {
  
  zip = gsub(",","",gsub(",","",as.character(trim(zip))))
  zip[zip=="NULL"|zip=="NA"|zip=="0"]<-NA
  pattern1 = "^([0-9]{4})$"
  match1 = str_match(zip,pattern1)
  if (ncol(match1)>=2) {
    match1 = match1[,2]
  } else {
    match1= NA
  }
  pattern2 = "^([0-9]{5})$"
  match2 = str_match(zip,pattern2)
  if (ncol(match2)>=2) {
    match2 = match2[,2]
  } else {
    match2 = NA
  }
  
  pattern3 = "^([0-9]{4})-([0-9]{0,4})$"
  match3 = str_match(zip,pattern3)
  if (ncol(match3)>=2 ) {
    match3 = match3[,2]
  } else {
    match3 = NA
  }
  
  pattern4 = "^([0-9]{5})-([0-9]{0,4})$"
  match4 = str_match(zip,pattern4)
  if (ncol(match4)>=2) {
    match4 = match4[,2]  
  } else {
    match4 = NA
  }
  
  cleaned_zip = zip
  cleaned_zip[!is.na(match1)]=paste("0",match1[!is.na(match1)],sep="")
  cleaned_zip[!is.na(match2)]= match2[!is.na(match2)]
  cleaned_zip[!is.na(match3)]=paste("0",match3[!is.na(match3)],sep="")
  cleaned_zip[!is.na(match4)]= match4[!is.na(match4)]
  
  cleaned_zip = gsub("_","",cleaned_zip)
  return(cleaned_zip)
}


#separates and cleans a street and suffix (used inside the function clean_address, not necessarily useful on its own)
separate_suffix <- function(street) {
  street = trim(toupper(street))
  
  # this is a slightly better regex!
  # if any suffixes are added here, also remember to add them to the suffix cleaner to make sure they are in a standard form, if necessary (e.g. if you add RDE as a misspelling of road, make sure it gets corrected to RD in clean_suffix)
  pattern = "^(.*) (ST N|ST E|ST W|ST CON|ST EXD|ST EXN|ST S|STRET|ST|AVE|PL|AV|CIR|CI|LA|LN|DR|RD|WAY|BLVD|CT|TE|TER|TERR|ROAD|SQ|HWY|WY|PKWY|PW|COURT|PARKWAY|STREET|PLACE|AVENUE|AVE|HW|PK|PARK|CIRCUIT|CIRT|BL|RO|CC|ROW|ROWE|PLAZA|PLZ|PZ)[.]?$|(.*)$"
  match = str_match(street,pattern)
  street_name = trim(match[,2])
  street_name[is.na(street_name)] = match[is.na(street_name),1]
  street_suffix = trim(match[,3])
  street_suffix = clean_suffix(street_suffix)
  street_name = clean_streetName(street_name)
  
  return(matrix(data=c(street,street_name,street_suffix),ncol=3))
}

# separates and cleans an address vector that holds street number, street name, street suffix, possibly a PO box
# calls the other cleaning functions
clean_address <- function(address ){
  
  #removing a weird character that was causing problems, making all characters capital and trimming
  #address_raw <- as.character(gsub("\xae","R",address))
  address_raw <- as.character(address)
  address_raw <- toupper(trim(address_raw))
  
  #boston specific
  address_raw[address_raw=="1 SCHROEDER PLZ"|address_raw=="SCHROEDER PLZ"]="1201 TREMONT ST"
  
  #pull out a NSEW SUFFIX if it is there
  nsew_match = str_match(address_raw,"(.*) (E|EAST|W|WEST|S|SOUTH|N|NORTH)$")
  directional = nsew_match[,3]
  directional[directional=="E"] = "EAST"
  directional[directional=="W"] = "WEST"
  directional[directional=="S"] = "SOUTH"
  directional[directional=="N"] = "NORTH"
  address_raw = ifelse(!is.na(nsew_match[,2]),nsew_match[,2],address_raw)
  
  #splitting number and street
  pattern1 = "^([0-9]+[A-Z]?[A-Z]? ?- ?[0-9]*[A-Z]?[A-Z]?|[0-9]+[A-Z]?[A-Z]?) (.*)$"
  match = str_match(address_raw,pattern1)
  number = trim(match[,2])
  street = ifelse(is.na(trim(match[,3])),address_raw,trim(match[,3]))
  
  #for those that don't match the pattern, the original text is put in (I think this is when there was no street number)
  street[is.na(street)]=match[is.na(street),1]
  
  #getting suffix
  separated_suffix = separate_suffix(street)
  street_name = separated_suffix[,2]
  #if no suffix, get the whole
  street_name[is.na(separated_suffix[,2]) | (separated_suffix[,2] == "")] = separated_suffix[is.na(separated_suffix[,2]) | (separated_suffix[,2] == ""),1]
  street_suffix = separated_suffix[,3]
  
  #cleaning number
  cleaned_num = clean_num(number)
  num1 = cleaned_num[,2]
  num2 = cleaned_num[,3]
  
  
  #getting PO boxes
  pattern4 = "([P][O]|[P][.][O][.]) BOX +([0-9]+)"
  match=str_match(address_raw,pattern4)
  po_num = match[,3]
  
  
  returnMatrix = matrix(data = c(address,num1,num2,street_name,street_suffix,po_num,directional),ncol = 7)
  colnames(returnMatrix) <- c("address raw","num1","num2","street name","street suffix","PO num","directional")
  return(returnMatrix)
}

#from a clean state, reduces different names of cities within Boston to common grouping (e.g. Back Bay -> Back Bay/Allston)
reduce_cities <-function(cities) {
  cities = toupper(trim(cities))
  cities[!is.na(match(cities,c("ALLSTON","BRIGHTON")))] <- "ALLSTON/BRIGHTON"
  cities[!is.na(match(cities,c("NORTH DORCHESTER","SOUTH DORCHESTER")))] <- "DORCHESTER"
  cities[!is.na(match(cities,c("WEST ROXBURY","ROXBURY","EAST ROXBURY")))] <- "ROXBURY"
  cities[!is.na(match(cities,c("BACK BAY/BEACON HILL","CENTRAL","SOUTH END","FENWAY/KENMORE")))] <- "BOSTON"
  return(cities)
}

#checks if a zip code was accidentally put into the city variable
# i almost never use this, but at some point it was useful
city_zip <- function(city) {
  city = as.character(city)
  match1 = str_match(city,"^([0-9]{4})$")[,2]
  match2 = str_match(city,"^([0-9]{5})$")[,2]
  match3 = str_match(city,"^([0-9]{4})-([0-9]{4})$")[,2]
  match4 = str_match(city,"^([0-9]{5})-([0-9]{4})$")[,2]
  city_zip = NA
  city_zip[!is.na(match1)]=paste("0",match1[!is.na(match1)],sep="")
  city_zip[!is.na(match2)]= match2[!is.na(match2)]
  city_zip[!is.na(match3)]=paste("0",match3[!is.na(match3)],sep="")
  city_zip[!is.na(match4)]= match4[!is.na(match4)]
  
  return(city_zip)
  #return(cleaned_zip)
  
}



