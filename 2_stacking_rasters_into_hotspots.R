

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
# ================================
#1a) STack Bc birds biodiversity
# ================================

# the folder with the rasters 
folder_rasters_combos<-"data/output_bc_crop_named_all"

#the function

load_reclass_and_sum_raster <- function(folder_rasters_combos) {
    files <- list.files(path = folder_rasters_combos, pattern = "\\.tif$", full.names = TRUE) # List all .tif raster files in the provided folder (full paths)
  # Safety check: if no files are found, return NULL to avoid crashing terra::rast()
  if (length(files) == 0) {
    warning("No raster files found in the folder")
    return(NULL)
  }
  raster_stack <- terra::rast(files) # Load all raster files into a SpatRaster (multi-layer stack)
  reclassified <- raster_stack > 0  # Reclassify raster values: # TRUE (1) where values > 0, FALSE (0) where values <= 0 # This creates a binary presence/absence raster for each layer
  sum_raster <- terra::app(reclassified, sum, na.rm = TRUE)# Sum across all raster layers:# Each pixel now represents the number of layers where value > 0
  zero_mask <- terra::app(reclassified == 0, sum, na.rm = TRUE) > 0# Create a mask identifying pixels where at least one layer had a 0
  sum_raster[is.na(sum_raster) & zero_mask] <- 0  # # Handle NA values in the summed raster: Replace NA with 0 ONLY where we know at least one layer had a valid 0
  # Return the final summed raster
  return(sum_raster)
}

bc_all_birds_hotspot<-load_reclass_and_sum_raster(folder_rasters_combos)

plot(bc_all_birds_hotspot)

# ANOTHER WAY OF DOING IT 

# # the folder with the rasters 
# folder_rasters_combos<-"data/output_bc_crop_named_all"
# # create a list of teh rasters   
# files <- list.files(path = folder_rasters_combos, pattern = "\\.tif$", full.names = TRUE) # make sure numbers make sense
# # If all rasters have the same extent, resolution, and projection, you can stack them:
# raster_stack <- terra::rast(files)
# 
# # this is teh fucntion 
# process_stack <- function(r_stack) {
#   rcl <- matrix(c(-Inf, 0, 0,0, Inf, 1),ncol = 3, byrow = TRUE)# 1) Reclassify all layers: <=0 → 0, >0 → 1
#   r_bin <- classify(r_stack, rcl)
#   sum_raster <- app(r_bin, sum, na.rm = TRUE)# 2) Pixel-wise sum across layers
#   zero_mask <- app(r_bin, function(x) any(x == 0))# 3) Mask: TRUE if at least one layer has a 0
#   sum_raster[is.na(sum_raster) & zero_mask] <- 0# 4) Replace NA in the sum only where zero_mask is TRUE
#   return(sum_raster)
# }
# 
# # Apply the function 
# bc_all_birds_hotspot <- process_stack(raster_stack )
# 
# plot(bc_all_birds_hotspot )

# ================================
# 1b) Write raster
# ================================

writeRaster(
  bc_all_birds_hotspot,
  "data/processed_bc_biod_rasters/bc_all_birds_hotspots_raster_2023.tif",
  overwrite = TRUE)

# ================================
# 1c) PLOTTING
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


# #=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
##_##_###_##_CODE ENDS HERE 
# #=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=



# Some extra code
# 8) Try some stacking
# ================================
# some stacking classical approach 
##############

# we need a list of rasters 
# for example the rasters_renamed_residents
class(rasters_renamed_residents)
r_stack <- rast(rasters_renamed_residents)              # check taht layers are named by species already
# raster_files_residents[[1]]

# We can also just stack teh rasters
#folder_rasters_combos<-"data/output_bc_crop_named_residents"
#files <- list.files(path = folder_rasters_combos, pattern = "\\.tif$", full.names = TRUE)
#raster_stack <- terra::rast(files)

# The function

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

###### here to create a nice plot 
# ## 1) Project raster to match BC boundary CRS ----
# # (safer than hard-coding a projection string)
# result_proj <- project(result_raster, crs(bc_boundary_proj))
# 
# ## 2) Crop/mask to BC boundary (optional but usually nicer) ----
# result_bc <- mask(crop(result_proj, bc_boundary_proj), bc_boundary_proj)
# 
# ## 3) Get sensible legend limits ----
# max_val <- global(result_bc, "max", na.rm = TRUE)[1, 1]
# 
# ## 4) Plot map ----
# par(mar = c(0, 0, 0, 0))
# 
# # Base: BC boundary
# plot(bc_boundary_proj, col = "grey90", border = "grey40",
#      axes = FALSE, main = "")
# 
# # Add raster on top
# plot(result_bc,
#      col = viridis(100),
#      legend = FALSE,
#      add = TRUE)
# 
# # Optional: add title separately (for nicer spacing)
# title("Number of resident species across BC", line = -2, cex.main = 1.2)
# 
# # 5) Legend with fields::image.plot ----
# fields::image.plot(
#   zlim       = c(0, max_val),
#   legend.only = TRUE,
#   col        = viridis(100),
#   breaks     = seq(0, max_val, length.out = 101),
#   smallplot  = c(0.15, 0.85, 0.12, 0.15),   # horizontal legend
#   horizontal = TRUE,
#   axis.args  = list(
#     at     = c(0, round(max_val / 2), max_val),
#     labels = c("Low", "Medium", "High"),
#     fg         = "black",
#     col.axis   = "black",
#     cex.axis   = 0.8,
#     lwd.ticks  = 0.5,
#     padj       = -1.5
#   ),
#   legend.args = list(
#     text = "Number of resident species",
#     side = 3,
#     col  = "black",
#     cex  = 1,
#     line = 0
#   )
# )




