

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

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# ---- 0) SETUP ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#

# ---- Install required libraries ----
# Data Manipulation
install.packages("tidyverse")
install.packages("janitor")
install.packages("glue")   # String Manipulation
install.packages("fs")     # File Operations
install.packages("png")    # Image Handling

# Data Visualization
install.packages("viridis")
install.packages("scales")
install.packages("fields")
install.packages("readr")  # Data Input/Output

# Geospatial Data
install.packages("rnaturalearth")
install.packages("sf")
install.packages("raster")
install.packages("ebirdst")
install.packages("rmapshaper")
install.packages("terra")

# ---- Load libraries ----
library(dplyr)        # Data manipulation
library(janitor)      # Data cleaning
library(glue)         # String interpolation
library(fs)           # File operations
library(png)          # Read/write PNG images
library(viridis)      # Color scales
library(scales)       # Graphical scales
library(fields)       # Spatial tools
library(readr)        # Read CSV, rectangular data
library(rnaturalearth)# Map data
library(sf)           # Simple features for geospatial data
library(raster)       # Raster data analysis
library(ebirdst)      # Access eBird Status and Trends data
library(rmapshaper)   # Simplify shapes
library(terra)        # Raster/vector spatial analysis
library(ggplot2)      # Plots

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# ---- 1) DATA PREPARATION ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#

# Get working directory
getwd()

# Define raster folder
folder_rasters_combos <- "data/output_bc_crop_named_residents"

# List raster files
files <- list.files(
  path = folder_rasters_combos,
  pattern = "\\.tif$",
  full.names = TRUE
)

# Read BC boundary shapefile
bc_boundary <- sf::st_read("data/layers/BC_boundary_layer.shp") 

# Check properties
crs(bc_boundary)
ext(bc_boundary)      
res(bc_boundary)      

# Optional: Reproject BC boundary to match raster CRS
# bc_boundary_proj <- bc_boundary %>% 
#   st_transform("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs") %>%  
#   vect() 
# crs(bc_boundary_proj)

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# ---- 2) STACKING WORKFLOW ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# ---- 2a) STACK BC BIRDS (ALL SPECIES) ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#

# Folder with rasters
folder_rasters_combos <- "data/output_bc_crop_named_all"

# ---- Function: Load, reclassify, and sum rasters ----
load_reclass_and_sum_raster <- function(folder_rasters_combos) {
  
  files <- list.files(
    path = folder_rasters_combos,
    pattern = "\\.tif$",
    full.names = TRUE
  ) # List all .tif raster files
  
  # Safety check
  if (length(files) == 0) {
    warning("No raster files found in the folder")
    return(NULL)
  }
  
  raster_stack <- terra::rast(files) # Load all raster files into a SpatRaster stack
  
  # Reclassify raster values
  # TRUE (1) where >0, FALSE (0) where <=0
  reclassified <- raster_stack > 0  
  
  # Sum across layers
  # Each pixel represents the number of layers with value >0
  sum_raster <- terra::app(reclassified, sum, na.rm = TRUE)
  
  # Mask: identify pixels where at least one layer had a 0
  zero_mask <- terra::app(reclassified == 0, sum, na.rm = TRUE) > 0
  
  # Replace NA with 0 where we know at least one layer had valid 0
  sum_raster[is.na(sum_raster) & zero_mask] <- 0  
  
  return(sum_raster)
}

# Apply function
bc_all_birds_hotspot <- load_reclass_and_sum_raster(folder_rasters_combos)

# Plot hotspot
plot(bc_all_birds_hotspot)

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# ---- 2b) WRITE OUTPUT RASTER ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#

writeRaster(
  bc_all_birds_hotspot,
  "data/processed_rasters/bc_all_birds_hotspots_raster_2023.tif",
  overwrite = TRUE
)

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# ---- 2c) PLOTTING ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#

# Convert raster to dataframe
result_bc <- mask(crop(result_proj, bc_boundary), bc_boundary) 

r_df <- as.data.frame(result_bc, xy = TRUE, na.rm = TRUE)
colnames(r_df)[3] <- "value"

# Convert BC boundary to sf
bc_sf <- st_as_sf(bc_boundary)
max_val <- global(result_proj, "max", na.rm = TRUE)[1, 1]

# Plot map
ggplot() + 
  geom_raster(data = r_df, aes(x = x, y = y, fill = value)) + 
  geom_sf(
    data = bc_sf,
    fill = NA,
    color = "grey40",
    linewidth = 0.4
  ) +
  scale_fill_viridis(
    name = "Number of resident species",
    limits = c(0, max_val),
    option = "D"
  ) +
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

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# ---- 3) SARA SPECIES ANALYSIS ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# species-at-risk listed under Schedule 1 of the federal Species at Risk Act
# Note:
# Resident species in eBird do not have full-year_max layer
# They have abundance_seasonal_max_3km.t
# Non-resident species have abundance_seasonal_max layer
# Goal: compare both types

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# ---- 3a) STACK BC SARA SPECIES ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
folder_rasters_combos_sara <- "data/output_bc_crop_named_sara"

# Function (same logic reused)
load_reclass_and_sum_raster <- function(folder_rasters_combos_sara) {
  files <- list.files(
    path = folder_rasters_combos_sara,
    pattern = "\\.tif$",
    full.names = TRUE
  )
  if (length(files) == 0) {
    warning("No raster files found in the folder")
    return(NULL)
  }
  raster_stack <- terra::rast(files)
  reclassified <- raster_stack > 0   # Reclassify: TRUE where >0, FALSE where <=0
  sum_raster <- terra::app(reclassified, sum, na.rm = TRUE)# Sum across layers
  zero_mask <- terra::app(reclassified == 0, sum, na.rm = TRUE) > 0 # Mask: identify pixels with at least one 0
  sum_raster[is.na(sum_raster) & zero_mask] <- 0  # Replace NA with 0 where appropriate
  return(sum_raster)
}

# Apply function
bc_sara_hotspot <- load_reclass_and_sum_raster(folder_rasters_combos_sara)

# Plot hotspot
plot(bc_sara_hotspot)

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# ---- 3b) WRITE OUTPUT RASTER ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#

writeRaster(
  bc_sara_hotspot,
  "data/processed_rasters/bc_sara_birds_hotspots_raster_2023.tif",
  overwrite = TRUE
)

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# ---- END OF SCRIPT ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#








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




