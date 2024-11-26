#coordinate fix
P23sf_new_gfix <- P23sf[which(P23sf$GIS_ID %in% setdiff(P23sf$GIS_ID, LandParcels_2022$Land_Parcel_ID)),]
P23sf_new_gfix$Y <- st_coordinates(st_transform(P23sf_new, 4326))[,1]
P23sf_new_gfix$X <- st_coordinates(st_transform(P23sf_new, 4326))[,2]
P23sf_new_gfix <- st_drop_geometry(P23sf_new_gfix)
P23sf_new_gfix <- st_as_sf(P23sf_new_gfix, coords = c("X", "Y"), crs = 4326)


reverse_linestring <- function(ls) {
  st_sfc(st_linestring(coords = matrix(st_coordinates(ls)[, 2:1], ncol = 2)), crs = st_crs(ls))
}

tiger_fix <- tiger22
tiger_coords <- st_coordinates(st_transform(tiger_fix, 4326))
tiger_coords[, c("X", "Y")] <- tiger_coords[, c("Y", "X")] # Swap X and Y
tiger_coords <- as.data.frame(tiger_coords)
tiger_coords <- tiger_coords[!duplicated(tiger_coords$L1),]
tiger_fix <- st_drop_geometry(tiger_fix)
tiger_fix$X <- tiger_coords$X
tiger_fix$Y <- tiger_coords$Y
tiger_fix <- st_as_sf(tiger_fix, coords = c("X", "Y"), crs = 4326)



# PARCEL TO STREETS FIX

streets_prefinal$Y <- st_coordinates(st_transform(streets_prefinal, 4326))[,1]
streets_prefinal$X <- st_coordinates(st_transform(streets_prefinal, 4326))[,2]
streets_prefinal <- st_drop_geometry(streets_prefinal)
streets_prefinal <- st_as_sf(streets_prefinal, coords = c("X", "Y"), crs = 4326)
streets_prefinal <- st_transform(streets_prefinal, 4326)
streets_prefinal <- as.data.frame(streets_prefinal) %>% select(-geometry)
streets_prefinal$geometry <- data_pre$geometry 
streets_prefinal <- st_sf(streets_prefinal)
streets_prefinal <- streets_prefinal %>% select(-BG_ID_10.y)

library(dplyr)
test <- as.data.table(st_coordinates(st_transform(streets_prefinal, 4326)))
data_pre <- data.frame(TLID = streets_prefinal$TLID)


grouped <- test %>% group_by(L1)
grouped$Z <- grouped$Y
grouped$Y <- grouped$X
grouped$X <- grouped$Z
grouped$X <- as.numeric(grouped$X)
grouped$Y <- as.numeric(grouped$Y)
data <- data.frame(X = as.numeric(grouped$X),Y = as.numeric(grouped$Y), L1 = as.numeric(grouped$L1))

# A function to create LINESTRINGs from points in each group
create_linestrings <- function(df) {
  # Split the data by the 'L1' column (or whichever column indicates the grouping for your LINESTRINGs)
  list_of_points <- split(df, df$L1)
  
  # Create LINESTRING geometries for each group of points
  list_of_linestrings <- lapply(list_of_points, function(points) {
    coords <- points[, c("X", "Y")]  # select the coordinate columns
    st_linestring(as.matrix(coords))  # create LINESTRING
  })
  
  # Combine the LINESTRINGs into a single 'sfc' column
  sfc_linestrings <- st_sfc(list_of_linestrings)
  
  return(sfc_linestrings)
}

# Assuming 'df' is your dataframe
data_sf <- data  # If you want to preserve the original df, otherwise you can use df directly

# Create LINESTRING geometries
data_pre$geometry <- create_linestrings(data)

# Convert the dataframe to a spatial dataframe (sf object)
df_sf <- st_sf(df_sf)

# Optionally, set the coordinate reference system if you know it
# df_sf <- st_set_crs(df_sf, <your_crs>)
