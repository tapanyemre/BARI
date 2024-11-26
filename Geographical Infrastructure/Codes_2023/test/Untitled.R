#extract info from census
census_20_join <- census_20[c("TLID", "BLKGRP20_L")]
census_20_join <- census_20_join %>% rename(BG_ID_20 = BLKGRP20_L)
census_20_join_unique <- census_20_join[!duplicated(census_20_join$TLID), ]
streets_census_joined <- left_join(streets_census[c("TLID")], census_20_join_unique[c("TLID", "BG_ID_20")], by = "TLID")
streets_census$BG_ID_20 <- ifelse(is.na(streets_census$BG_ID_20), streets_census_joined$BG_ID_20, streets_census$BG_ID_20)


# Merge the two data frames with all.x = TRUE to keep all rows from streets_census
streets_census_joined <- merge(streets_census, census_20_join[c("TLID", "BG_ID_20")], by = "TLID", all.x = TRUE)

# Check the number of rows to ensure it matches the original streets_census
if (nrow(streets_census_joined) == nrow(streets_census)) {
  # Update BG_ID_20 in streets_census if the number of rows is correct
  streets_census$BG_ID_20 <- streets_census_joined$BG_ID_20.y
} else {
  # Handle the error if there's a row mismatch
  stop("Row mismatch after merge. Please check the uniqueness of TLID in both datasets.")
}
