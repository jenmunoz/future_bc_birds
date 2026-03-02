
# ============================================================
# SPECIES RASTER STACK WORKFLOW
# ============================================================
# Purpose:
#   - Stack individual species rasters
#   - Compute richness and abundance summaries
#   - Identify contributing species at points and polygons
# ============================================================


# ============================================================
# 0) SETUP
# ============================================================

# Check working directory
getwd()

# Folder containing BC resident species rasters
folder_rasters_combos <- "data/output_bc_crop_named_residents"

# List all .tif files
files <- list.files(
  path = folder_rasters_combos,
  pattern = "\\.tif$",
  full.names = TRUE
)


# ============================================================
# 1) STACK RASTERS
# ============================================================

# Create SpatRaster stack (no calculations yet)
r_stack <- rast(files)

# Sanity check: layer names should correspond to species names
names(r_stack)
r_stack[[1]]


# ============================================================
# 2) PER-PIXEL METRICS
# ============================================================

# 2a) Per-pixel SUM (e.g., total abundance)
sum_r <- sum(r_stack, na.rm = TRUE)

# 2b) Per-pixel COUNT of non-zero layers (species richness)
count_r <- app(
  r_stack,
  fun = function(x) sum(x > 0, na.rm = TRUE)
)


# ============================================================
# 3) GLOBAL CHECK: WHICH SPECIES CONTRIBUTE ANYWHERE?
# ============================================================

# Per-layer maximum value
mx <- global(r_stack, max, na.rm = TRUE)

# Layers with any non-zero value across the entire map
layers_with_any_nz <- rownames(mx)[mx$max > 0]
layers_with_any_nz


# Optional: write outputs
# writeRaster(sum_r,   file.path(outdir, "sum_abundance.tif"),
#             overwrite = TRUE, gdal = "COMPRESS=LZW")
# writeRaster(count_r, file.path(outdir, "count_nonzero.tif"),
#             overwrite = TRUE, gdal = "COMPRESS=LZW")


# ============================================================
# 4) FUNCTION: SPECIES CONTRIBUTING AT POINT(S)
# ============================================================

contributors_at_points <- function(stack, pts_sf_or_df) {
  
  # Extract raster values at point locations
  vals <- terra::extract(stack, pts_sf_or_df)
  
  # Remove ID column
  vals_data <- vals[, -1, drop = FALSE]
  
  # Initialize output vector
  contributors <- character(nrow(vals_data))
  
  # Loop over points
  for (i in seq_len(nrow(vals_data))) {
    
    row_vals <- as.numeric(vals_data[i, ])
    
    # Species with values > 0
    contributing <- names(stack)[row_vals > 0 & !is.na(row_vals)]
    
    if (length(contributing) == 0) {
      contributors[i] <- NA_character_
    } else {
      contributors[i] <- paste(contributing, collapse = ";")
    }
  }
  
  contributors
}


# ============================================================
# 5) TEST: POINT EXTRACTION
# ============================================================

rasters_stacked <- r_stack  # ensure correct stack

# Example point (Vancouver area)
point <- st_as_sf(
  data.frame(
    lon = -123.12,
    lat = 49.28
  ),
  coords = c("lon", "lat"),
  crs = 4326
)

# Always check CRS before extraction
terra::crs(rasters_stacked)
st_crs(point)

# Reproject point to match raster CRS
point <- st_transform(point, terra::crs(rasters_stacked))

# Run function
contributors_at_points(rasters_stacked, point)



# ============================================================
# 6) FUNCTION: SPECIES CONTRIBUTING WITHIN A POLYGON
# ============================================================

contributors_at_polygon <- function(stack, polygon_sf) {
  
  # Extract raster values inside polygon
  vals <- terra::extract(stack, polygon_sf)
  
  # Remove ID column
  vals_data <- vals[, -1, drop = FALSE]
  
  # Initialize output vector
  contributing_layers <- character(0)
  
  # Loop over layers (columns)
  for (j in seq_len(ncol(vals_data))) {
    
    layer_vals <- vals_data[[j]]
    
    # Check if ANY positive value exists
    if (any(layer_vals > 0, na.rm = TRUE)) {
      contributing_layers <- c(
        contributing_layers,
        names(stack)[j]
      )
    }
  }
  
  # Return vector of species names
  if (length(contributing_layers) == 0) {
    NA_character_
  } else {
    contributing_layers
  }
}


# ============================================================
# 7) TEST: POLYGON EXTRACTION (BC BOUNDARY)
# ============================================================

rasters_stacked <- r_stack

polygon <- bc_boundary %>%
  st_transform(
    "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs"
  ) %>%
  vect()

# Check CRS
terra::crs(rasters_stacked)
st_crs(polygon)

# Reproject polygon to raster CRS
polygon <- st_transform(polygon, terra::crs(rasters_stacked))

# Run function
species_in_bc_test <- contributors_at_polygon(
  rasters_stacked,
  polygon
)

species_list <- as.list(species_in_bc_test)
print(species_list)


# ============================================================
# NOTE ON RESULTS
# ============================================================
# The function extracts 34 species while the stack contains 35.
# The missing species is "Spotted Owl".
#
# Inspection of its raster shows only zero values for BC.
# eBird 2022 records confirm no observations in BC for that year.
#
# Therefore, the function is behaving correctly.
# ============================================================

































































# Get the working directory
getwd()

folder_rasters_combos<-"data/output_bc_crop_named_residents"
files <- list.files(path = folder_rasters_combos, pattern = "\\.tif$", full.names = TRUE)

# ================================
# 1) STACKING
# ================================
# some stacking classical approach 
##############

r_stack<-rast(files)  # Create a stack of teh files without doing any math just a Stack Raster object
r_stack[[1]] #  check taht layers are named by species already 


# ================================
# 3) EXTRACT THE NAMES OF SPECIES PER PIXEL 
# ================================
##################### Function to extract the names of species per pixel 

names(r_stack)                            # sanity check

# 2) Per-pixel SUM (e.g., abundance sum)
sum_r <- sum(r_stack, na.rm = TRUE)

# 3) Per-pixel COUNT of non-zero layers (e.g., richness)
count_r <- app(r_stack, fun = function(x) sum(x > 0, na.rm = TRUE))

#Globally (anywhere in the map): which layers have any non-zero pixel?

# Per-layer max; >0 means the layer contributes somewhere
mx <- global(r_stack, max, na.rm = TRUE)
layers_with_any_nz <- rownames(mx)[mx$max > 0]
layers_with_any_nz

# ####
#   writeRaster(sum_r,   file.path(outdir, "sum_abundance.tif"),   overwrite = TRUE, gdal = "COMPRESS=LZW")
#   writeRaster(count_r, file.path(outdir, "count_nonzero.tif"),   overwrite = TRUE, gdal = "COMPRESS=LZW")


#Function to extract list of species at a given point 

contributors_at_points <- function(stack, pts_sf_or_df) {
  vals <- terra::extract(stack, pts_sf_or_df) # 1. Extract raster values at point locations
  vals_data <- vals[, -1, drop = FALSE]  # 2. Drop the ID column
  contributors <- character(nrow(vals_data))# 3. Initialize output vector
  
  # 4. Loop over points (rows)
  for (i in seq_len(nrow(vals_data))) { 
    row_vals <- as.numeric(vals_data[i, ])# Values from all raster layers at point i
    # Names of layers with values > 0 and not NA
    contributing <- names(stack)[row_vals > 0 & !is.na(row_vals) ]
    if (length(contributing) == 0) {
      contributors[i] <- NA_character_
    } else {
      contributors[i] <- paste(contributing, collapse = ";")
    }
  }
  
  contributors
}
##_###_###_##_##_###_##_##
# Stress test the fucntion
##_###_###_##_##_###_##_##

rasters_stacked <- r_stack            # check taht layers are named by species already

point <- st_as_sf(
  data.frame(
    lon = -123.12,
    lat = 49.28
  ),
  coords = c("lon", "lat"),
  crs = 4326
)
#Before extraction, always check:

terra::crs(rasters_stacked)
st_crs(point)
point <- st_transform(point, terra::crs(rasters_stacked))


contributors_at_points(rasters_stacked, point)

#Function to extract list of species at a polygon


contributors_at_polygon <- function(stack, polygon_sf) {
  
  # 1. Extract raster values for all cells inside the polygon
  vals <- terra::extract(stack, polygon_sf)
  
  # 2. Drop ID column
  vals_data <- vals[, -1, drop = FALSE]
  
  # 3. Initialize output
  contributing_layers <- character(0)
  
  # 4. Loop over raster layers (columns)
  for (j in seq_len(ncol(vals_data))) {
    
    layer_vals <- vals_data[[j]]
    
    # Does this layer have ANY positive, non-NA value?
    if (any(layer_vals > 0, na.rm = TRUE)) {
      contributing_layers <- c(
        contributing_layers,
        names(stack)[j]
      )
    }
  }
  
  # 5. Return vector (not collapsed string)
  if (length(contributing_layers) == 0) {
    NA_character_
  } else {
    contributing_layers
  }
}



# Using a polygon instead of a point 

# test the fucntion 

# To run this example I wanted to use all Bc POLYGONE BECAUSE I Have the whole list for BC, and compare them.

rasters_stacked <- r_stack            # check taht layers are named by species already

polygon <- bc_boundary %>% 
  st_transform("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs") %>%  # transform coordinate system to match the raster data from ebird
  vect()

#Before extraction, always check:

terra::crs(rasters_stacked)
st_crs(polygon )
polygon  <- st_transform(polygon , terra::crs(rasters_stacked))


species_in_bc_test<-contributors_at_polygon(rasters_stacked, polygon)

list<-as.list(species_in_bc_test)

print(list)




# Importnat it extacts the names of 34 species while the total number of raters that are for Bc in the stack is 35 and it shoudl extract them all.
# The one taht is missing is spotted owl 
# interestingly when I look at the ratser for this species only has zeros for BC, I confirm this by going into ebird and fro 2022 tehre are not registers. they exist fro other years


### This fucntion also works but gives me objects separated by ;

# contributors_at_polygon <- function(stack, polygon_sf) {
#   
#   # 1. Extract raster values for all cells inside the polygon
#   vals <- terra::extract(stack, polygon_sf)
#   
#   # 2. Drop ID column
#   vals_data <- vals[, -1, drop = FALSE]
#   
#   # 3. Initialize output
#   contributing_layers <- character(0)
#   
#   # 4. Loop over raster layers (columns)
#   for (j in seq_len(ncol(vals_data))) {
#     
#     layer_vals <- vals_data[[j]]
#     
#     # Does this layer have ANY positive, non-NA value?
#     if (any(layer_vals > 0, na.rm = TRUE)) {
#       contributing_layers <- c(
#         contributing_layers,
#         names(stack)[j]
#       )
#     }
#   }
#   
#   # 5. Return result
#   if (length(contributing_layers) == 0) {
#     NA_character_
#   } else {
#     paste(contributing_layers, collapse = ";")
#   }
# }

