merged <- left_join(streets_prefinal, parcels_merge, by="TLID")
merged_bg <- parcels_merge %>% select(BG_ID_10, BG_ID_20)
merged_ct <- parcels_merge %>% select(CT_ID_10, CT_ID_20)
parcels_merge2 <- parcels2 %>% select(TLID, BG_ID_10, CT_ID_10, BG_ID_20, CT_ID_20)
merged2 <- left_join(streets_prefinal, parcels_merge2, by="TLID")

roads_23 <- read.csv("~/Desktop/BARI/GI-2023/drive/ParcelToStreets_final/Roads.2023_dec/roads_fin_clust_Dec2023.csv")



# Assuming you've already loaded dplyr
library(dplyr)

landparcels_sub <- landparcels %>% select( "TLID", "CT_ID_10", "BG_ID_10","CT_ID_20", "BG_ID_20", 
                                           "X", "Y")


# Perform the left join
combined_data <- left_join(roads_23, landparcels_sub, by = c("TLID", "BG_ID_10"))

# Fill in missing values
roads_23_filled <- combined_data %>%
  mutate(
    BG_ID_20 = coalesce(BG_ID_20.x, BG_ID_20.y),
    CT_ID_20 = coalesce(CT_ID_20.x, CT_ID_20.y)
  ) %>%
  select(TLID, BG_ID_10, BG_ID_20 = BG_ID_20.x, CT_ID_20 = CT_ID_20.x)

# Note: .x and .y are suffixes automatically added by dplyr to distinguish 
# columns from the first and second data frames in the join.


library(dplyr)

# Summarize landparcels before joining
landparcels_summary <- landparcels_sub %>%
  group_by(BG_ID_10, BG_ID_20, CT_ID_10, CT_ID_20) %>%
  summarize(aggregate_column = mode(BG_ID_10)) # replace with appropriate aggregation

# Perform the left join
combined_data <- left_join(roads_23, landparcels_summary, by = c("BG_ID_10", "BG_ID_20", "CT_ID_10", "CT_ID_20"))

# Fill in missing values
roads_23_filled <- combined_data %>%
  mutate(
    BG_ID_20 = coalesce(BG_ID_20.x, BG_ID_20.y),
    CT_ID_20 = coalesce(CT_ID_20.x, CT_ID_20.y)
  )

