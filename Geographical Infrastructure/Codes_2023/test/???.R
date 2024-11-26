library(sf)
library(nngeo)

# Load your datasets
dataset1 <- st_read("path/to/dataset1")
dataset2 <- st_read("path/to/dataset2")

# Ensure both datasets have the same CRS
dataset1 <- st_transform(dataset1, crs = st_crs(dataset2))

# Perform k-NN spatial join
neighbors <- st_nn(dataset1, dataset2, k = 1, returnDist = FALSE)

# Assigning IDs
for (i in 1:nrow(dataset1)) {
  dataset1$CT_ID_20[i] <- dataset2$CT_ID_20[neighbors[[i]]]
  dataset1$BG_ID_20[i] <- dataset2$BG_ID_20[neighbors[[i]]]
}

# Handle edge cases if any

# Your dataset1 now has the estimated CT_ID_20 and BG_ID_20

