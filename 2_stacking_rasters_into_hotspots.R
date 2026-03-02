

###_###_####_###_###_###_###_###_###_###_###_###_###_###_###_###_###_
## BC fund collaboration: future for Bc birds
## ##
## Objective:stacking selected e-bird rasters into single rasters for all Bc species
## ## Specifically calculating  number of species per pixel
## This code does the following:
## 0) Download e-bird 3*3 km rasters fro all BC birds ("Script 1_obtaining_ebird_data.R")
## i) Read tiff files into Rasters 
## ii) Reclassify rasters (0,1)
## iii) Sum rasters generating total number of species oer pixel
##
## Updated and annotated by Jenny Munoz
## Last updated: August 2025
###_###_####_###_###_###_###_###_###_###_###_###_###_###_###_###_###_

# ================================
# 0) SETUP
# ================================

# Install required Libraries 
# To access ebird data 

# Data Manipulation
install.packages("tidyverse")
install.packages("janitor")
install.packages("glue") # String Manipulation
install.packages("fs") # File Operations
install.packages("png") # Image Handling
# Data Visualization
install.packages("viridis")
install.packages("scales")
install.packages("fields")
install.packages("readr") # Data Input/Output
# Geospatial Data
install.packages("rnaturalearth")
install.packages("sf")
install.packages("raster")
install.packages("ebirdst")
install.packages("rmapshaper")
install.packages("terra")


# Load necessary libraries
library(dplyr) # Essential for data manipulation with functions to filter, arrange, summarize, etc.
library(janitor) # Functions for simple data cleaning
library(glue) # Useful for data-driven string interpolations
library(fs) # A cross-platform interface for file system operations
library(png) # Allows reading and writing PNG images
library(viridis) # Provides color scales for improving data visualization accessibility
library(scales) # Graphical scales for mapping data to aesthetics in visualizations
library(fields) # Tools for spatial data
library(readr) # Fast and friendly way to read rectangular data like CSV files
library(rnaturalearth) # Provides map data for creating high-quality maps
library(sf) # Used for handling simple features to work with geographic data
library(raster) # For raster data manipulation and analysis
library(ebirdst) # Tools for accessing and analyzing eBird Status and Trends data
library(rmapshaper) # Simplifies shapes for data visualizations
library(terra) # For working with raster and vector data in spatial analysis

library(ebirdst) # ebird data 
library(ggplot2) # for plots




# ================================
# 1) DATA
# ================================
# Get the working directory
getwd()

folder_rasters_combos<-"data/output_bc_crop_named_residents"
files <- list.files(path = folder_rasters_combos, pattern = "\\.tif$", full.names = TRUE)

# The original boundary is projected as NAD 1983 BC Environment Albers
# Read shapefile
bc_boundary <- sf::st_read("data/layers/BC_boundary_layer.shp") # vector file 

crs(bc_boundary)
ext(bc_boundary)      # spatial extent
res(bc_boundary)      # resolution

# # Ensure clip geometry matches each raster's CRS, so here we reprojet the Bc layer into the projection of teh rasters, alternatively i can reproject al the ebird  rasters to bc alberts, I decided that projecting one will save me some code 
# bc_boundary_proj <- bc_boundary %>% 
#   st_transform("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs") %>%  # transform coordinate system to match the raster data from ebird
#   vect() #vect() # transforms to terra object spat vector 
# # check the projection
# crs(bc_boundary_proj)


# ================================
# 1) STACKING
# ================================
# some stacking classical approach 
##############

r_stack<-rast(files)  # Create a stack of teh files without doing any math just a Stack Raster object
r_stack[[1]] #  check taht layers are named by species already 


process_stack <- function(r_stack) {
  
  # 1) Reclassify all layers: <=0 → 0, >0 → 1
  rcl <- matrix(c(-Inf, 0, 0,
                  0, Inf, 1),
                ncol = 3, byrow = TRUE)
  
  r_bin <- classify(r_stack, rcl)
  
  # 2) Pixel-wise sum across layers
  sum_raster <- app(r_bin, sum, na.rm = TRUE)
  
  # 3) Mask: TRUE if at least one layer has a 0
  zero_mask <- app(r_bin, function(x) any(x == 0))
  
  # 4) Replace NA in the sum only where zero_mask is TRUE
  sum_raster[is.na(sum_raster) & zero_mask] <- 0
  
  return(sum_raster)
}

result_raster <- process_stack(r_stack)

plot(result_raster )

# ================================
# 1) RE-PROJECT THE STACK TO MATCH BC BOUNDARY # NEED TO WORK ON THIS I THINK I AM MAKING  AMISTAKE HERE 
# ================================
# 1) Project raster to match BC boundary CRS 
##############
#result_proj <- project(result_raster, crs(bc_boundary_proj))
result_proj <- project(result_raster, crs(bc_boundary)) # PROJECTED TO WHAT WE NEED FOR THE PROJECT  The original boundary is projected as NAD 1983 BC Environment Albers

plot(result_proj)
# ## 2) Crop/mask to BC boundary  ----
result_bc <- mask(crop(result_proj, bc_boundary), bc_boundary) # to crop any extra outside the boundary
# 
# ## 3) Get sensible legend limits ----
 max_val <- global(result_proj, "max", na.rm = TRUE)[1, 1]
# 
# ## 4) Plot map ----
# par(mar = c(0, 0, 0, 0))
# 
# Base: BC boundary
plot(bc_boundary, col = "grey90", border = "grey40",
     axes = FALSE, main = "")

# Add raster on top
plot(result_proj,
     col = viridis(100),
     legend = FALSE,
     add = TRUE)
# 

# ================================
# 2) PLOTTING
# ================================
# some stacking classical approach 
##############
library(terra)
library(sf)
library(ggplot2)
library(viridis)


# Convert raster to dataframe
result_bc <- mask(crop(result_proj, bc_boundary), bc_boundary) # i dont think i need this step 

r_df <- as.data.frame(result_bc, xy = TRUE, na.rm = TRUE)
colnames(r_df)[3] <- "value"

# Convert BC boundary to sf
bc_sf <- st_as_sf(bc_boundary)
max_val <- global(result_proj, "max", na.rm = TRUE)[1, 1]


ggplot() + 
geom_raster(data = r_df, aes(x = x, y = y, fill = value)) +# Raster layer
geom_sf(data = bc_sf,  # BC boundary outline
          fill = NA,
          color = "grey40",
          linewidth = 0.4) +
scale_fill_viridis(
    name = "Number of resident species",
    limits = c(0, max_val),
    option = "D") +
  # Clean map theme
  coord_sf(expand = FALSE) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text  = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 9)
  ) +
  
  labs(title = "Number of resident species across BC")





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


#Per pixel: you can’t store a string list inside a raster cell, but you can query it at locations. Two handy helpers:

# A) For points: return contributing layer names for each point
contributors_at_points <- function(stack, pts_sf_or_df) {
  vals <- terra::extract(stack, pts_sf_or_df)         # first column is ID
  apply(vals[, -1, drop = FALSE], 1, function(row) {
    nm <- names(stack)[which(row > 0 & !is.na(row))]
    if (length(nm) == 0) NA_character_ else paste(nm, collapse = ";")
  })
}

outdir <- "data/derived"
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

contributors_at_points()

## a shorther version of it 

contributors_at_points <- function(stack, pts_sf) {
  
  # 1. Extract raster values at point locations
  vals <- terra::extract(stack, pts_sf_or_df)
  
  # 2. Drop the ID column
  vals_data <- vals[, -1, drop = FALSE]
  
  # 3. Initialize output vector
  contributors <- character(nrow(vals_data))
  
  # 4. Loop over points (rows)
  for (i in seq_len(nrow(vals_data))) {
    
    # Values from all raster layers at point i
    row_vals <- as.numeric(vals_data[i, ])
    
    # Names of layers with values > 0 and not NA
    contributing <- names(stack)[
      row_vals > 0 & !is.na(row_vals)
    ]
    
    if (length(contributing) == 0) {
      contributors[i] <- NA_character_
    } else {
      contributors[i] <- paste(contributing, collapse = ";")
    }
  }
  
  contributors
}

# the data 
stack <- rast(rasters_renamed_residents)   

pts_sf <- st_as_sf(
  data.frame(
    lon = -123.12,
    lat = 49.28
  ),
  coords = c("lon", "lat"),
  crs = 4326
)

#here is a new version in tidyverse 


contributors_at_points2 <- function(stack, pts_sf_or_df) {
  
  # 1. Extract raster values at point locations
  vals <- terra::extract(stack, pts_sf_or_df)
  
  # 2. Drop the ID column
  vals_data <- vals[, -1, drop = FALSE]
  
  # 3. Initialize output vector
  contributors <- character(nrow(vals_data))
  
  # 4. Loop over points (rows)
  for (i in seq_len(nrow(vals_data))) {
    
    # Values from all raster layers at point i
    row_vals <- as.numeric(vals_data[i, ])
    
    # Names of layers with values > 0 and not NA
    contributing <- names(stack)[
      row_vals > 0 & !is.na(row_vals)
    ]
    
    if (length(contributing) == 0) {
      contributors[i] <- NA_character_
    } else {
      contributors[i] <- paste(contributing, collapse = ";")
    }
  }
  
  contributors
}

# lets try some data 

stack <- rast(rasters_renamed_residents)              # check taht layers are named by species already


pts_sf <- st_as_sf(
  data.frame(
    lon = -123.12,
    lat = 49.28
  ),
  coords = c("lon", "lat"),
  crs = 4326
)
#Before extraction, always check:

terra::crs(stack)
st_crs(pts_sf)
pts_sf <- st_transform(pts_sf, terra::crs(stack))


contributors_at_points2(stack, pts_sf)


# ####
#   writeRaster(sum_r,   file.path(outdir, "sum_abundance.tif"),   overwrite = TRUE, gdal = "COMPRESS=LZW")
#   writeRaster(count_r, file.path(outdir, "count_nonzero.tif"),   overwrite = TRUE, gdal = "COMPRESS=LZW")

# Using a polygone instead of a point 
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
  
  # 5. Return result
  if (length(contributing_layers) == 0) {
    NA_character_
  } else {
    paste(contributing_layers, collapse = ";")
  }
}


species_in_bc_test<-contributors_at_polygon(stack, polygon)

polygon <- bc_boundary %>% 
  st_transform("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs") %>%  # transform coordinate system to match the raster data from ebird
  vect()

as.list(species_in_bc_test)

names(stack)

plot(stack$Spotted_Owl)

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


# To run this example I wanted to use all Bc POLYGONE BECAUSE I Have the whole list for BC, and compare them.
# 

polygon <- bc_boundary %>% 
  st_transform("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs") %>%  # transform coordinate system to match the raster data from ebird
  vect()

# here is the stack
stack <- rast(rasters_renamed_residents)   

# here I am running teh function 

species_in_bc_test<-contributors_at_polygon(stack, polygon)

# Importnat it extacts the names of 34 species while the total number of raters that are for Bc in the stack is 35 and it shoudl extract them all.
# The one taht is missing is spotted owl 
# interestingly when I look at the ratser for this species only has zeros for BC, I confirm this by going into ebird and fro 2022 tehre are not registers. they exist fro other years



