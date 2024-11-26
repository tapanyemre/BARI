##Pad Cross Adjustments
PADcross_new <- read.csv("PA-2023/outputs/PADCross/PADCross.Record.DRFT1.09212023.csv")
PADcross_old <- read.csv("Archive/PA_2022/outputs /PADCross.Record.07132022.csv")

#RENAME COLUMNS
PADcross_new$X.1 <- 1:nrow(PADcross_new)
PADcross_new <- rename(PADcross_new, KITCHEN = KITCHENS)
PADcross_new <- rename(PADcross_new, ZIPCODE = ZIP_CODE)


##SPLITTING OWNER.MAIL.ADRESS
split_address <- function(addr) {
  # Split by commas
  parts <- unlist(strsplit(addr, ", "))
  
  # Check the number of parts
  if(length(parts) == 4) {
    # Address includes a unit number
    MAIL_ADDRESS <- paste(parts[1], parts[2], sep=", ")
    MAIL_CITY <- parts[3]
    state_zip <- unlist(strsplit(parts[4], " "))
  } else if(length(parts) == 3) {
    # Address doesn't include a unit number
    MAIL_ADDRESS <- parts[1]
    MAIL_CITY <- parts[2]
    state_zip <- unlist(strsplit(parts[3], " "))
  } else {
    # Handle cases where the address might not fit the expected format
    MAIL_ADDRESS <- NA
    MAIL_CITY <- NA
    state_zip <- c(NA, NA)
  }
  
  MAIL_STATE <- state_zip[1]
  MAIL_ZIPCODE <- state_zip[2]
  
  return(c(MAIL_ADDRESS, MAIL_CITY, MAIL_STATE, MAIL_ZIPCODE))
}

# Apply the function to the "OWNER.MAIL.ADDRESS" column
address_splits <- t(sapply(PADcross_new$`OWNER.MAIL.ADDRESS`, split_address))

# Convert the matrix to a dataframe
address_df <- as.data.frame(address_splits, stringsAsFactors = FALSE, row.names = FALSE)
colnames(address_df) <- c("MAIL_ADDRESS", "MAIL_CITY", "MAIL_STATE", "MAIL_ZIPCODE")

# Combine the original dataframe with the split columns
PADcross_new <- cbind(PADcross_new, address_df)




PADcross_new$`OWNER.MAIL.ADDRESS` <- NULL

extract_CO_and_address <- function(str) {
  if (is.na(str)) {
    return(list(CO_Info = NA, Address = NA))
  }
  
  # If "C/O" is at the beginning
  if (startsWith(str, "C/O")) {
    CO_info <- str_extract(str, "C/O[^0-9]*")
    address <- str_extract(str, "\\d.*")
  } else {
    CO_info <- str_extract(str, "C/O.*$")
    address <- str_replace(str, "C/O.*$", "")
  }
  return(list(CO_Info = CO_info, Address = address))
}


extracted_data <- t(sapply(PADcross_new$`MAIL_ADDRESS`, extract_CO_and_address, simplify = "data.frame"))
extracted_data <- as.data.frame(t(sapply(PADcross_new$`MAIL_ADDRESS`, extract_CO_and_address)))

extracted_data <- as.data.frame(extracted_data, stringsAsFactors = FALSE, row.names = FALSE)
extracted_data$CO_Info[extracted_data$CO_Info == "NANA"] <- NA
extracted_data<- data.table(extracted_data)
extracted_data$Address <- unlist(extracted_data$Address)
extracted_data$CO_Info <- unlist(extracted_data$CO_Info)


PADcross_new$MAIL_ADDRESS <- extracted_data$Address
PADcross_new$MAIL_ADDRESSEE <- extracted_data$CO_Info


#ADD 2020 CENSUS GEOGRAPHIES
parcel_final <- read.csv("GI-2023/drive/Parcel_final_10032023_YET_postsanitychecks.csv")
parcel_final_cen20 <- parcel_final






