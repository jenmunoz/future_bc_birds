
###_###_####_###_###_###_###_###_###_###_###_###_###_###_###_###_###_
## BC fund collaboration: future for Bc birds
## ##
## Objective:stacking selected e-bird rasters into single rasters for 
##
## JV priority species for both PBHJV=-BC AND CIJV 
## Forest Birds
## Grassland-birds
## Long_distance_migrants " 
## ## Specifically calculating  number of species per pixel
## This code does the following:
## 0) Download e-bird 3*3 km rasters fro all BC birds ("Script 1_obtaining_ebird_data.R")
## i) Read tiff files into Rasters 
## ii) Reclassify rasters (0,1)
## iii) Sum rasters generating total number of species per pixel
## iii) Export raster Species Richness Density  per pixel

## Updated and annotated by Jenny Munoz
## Last updated: September 2026
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

#folder_rasters_combos <- "data/output_bc_crop_named_residents"
folder_rasters_combos<-"data/output_bc_crop_named_all"

# List raster files, of all bc species for which we have rasters
files <- list.files(path = folder_rasters_combos,pattern = "\\.tif$",full.names = TRUE)

# the species list
list_requested_data<-read.csv("data/list/birds_grouping_request_new/socb_groups.csv")
#View( list_requested_data)
# forest birds
forest_birds<-list_requested_data %>% filter(group_name=="forest_birds")
levels(list_requested_data$group_name)
unique(list_requested_data$ group_name)

# grassland_birds
grassland_birds<-list_requested_data %>% filter(group_name=="grassland_birds")

# long-distance migrants 
longdistance_migrants<-list_requested_data %>% filter(group_name=="long_distance_migrants")

#  migrants this is a request from Dan Dixon ECCC
overall_migrants<-read.csv("data/list/birds_grouping_request_new/MBCA_MigBirds_Dan_ECCC.csv")

#  priority species
priority_species_jv<-read.csv("data/list/birds_grouping_request_new/priority_species_JVs.csv")

priority_species_cijv <- read.csv( "data/list/birds_grouping_request_new/priority_species_JVs.csv") %>%
  filter( jv_priority %in% c("Both", "CIJV"))

priority_species_pbhjv <- read.csv( "data/list/birds_grouping_request_new/priority_species_JVs.csv") %>%
  filter( jv_priority %in% c("Both", "PBHJV-BC"))

head(priority_species_jv)

#names(list_requested_data$group_name)


#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# ---- 2) STACKING WORKFLOW ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# ---- 2a) STACK BC BIRDS (Forest birds) ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
list_requested_data<-read.csv("data/list/birds_grouping_request_new/socb_groups.csv")
forest_birds<-list_requested_data %>% filter(group_name=="forest_birds")

# Extract the species names 
species_names_forest_birds <- gsub(" ", "_", forest_birds$common_name)

# Create patterns and find them in the raster folders 
pattern_forest_birds <- paste0("^(", paste(species_names_forest_birds, collapse = "|"), ")")
selected_files_forest_birds <- files[ grepl(pattern_forest_birds, basename(files))]
basename(selected_files_forest_birds)

# Create destination folder for the forest birds rasters
output_folder_forest <- "data/output_bc_forest_birds_named_crop"

dir.create(output_folder_forest ,
  recursive = TRUE,
  showWarnings = FALSE)

# Copy selected files into the new folder
file.copy( from = selected_files_forest_birds,
  to = output_folder_forest,
  overwrite = TRUE)

# quick check

length(selected_files_forest_birds )
length(species_names_forest_birds)# this is longer because it also include species that are not in Bc 

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# ---- 2b) STACK BC BIRDS Function (Forest Birds) ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#


# ---- Function: Load, reclassify, and sum rasters ----
load_reclass_and_sum_raster <- function(folder_rasters_combos) {
  
  files <- list.files(
    path = folder_rasters_combos,
    pattern = "\\.tif$",
    full.names = TRUE
  ) # List Selected raster files  .tif raster files
  
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


# Folder with rasters

output_folder_forest <- "data/output_bc_forest_birds_named_crop"

# Apply function
bc_forest_birds_hotspot <- load_reclass_and_sum_raster(output_folder_forest)

# Plot hotspot
plot(bc_forest_birds_hotspot)
### I am working here 

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# ---- 2c) WRITE OUTPUT RASTER ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#

# NEED TO PROJECT IT to be used in ArcGis
bc_forest_birds_hotspot_projected <- terra::project( bc_forest_birds_hotspot,"EPSG:3005",method = "near") # EPSG:3005 is BC Albers, and method = "near" uses nearest-neighbour resampling. That means the reprojection will only assign existing raster values rather than creating intermediate values. Terra specifically recommends near for categorical/discrete rasters

# writeRaster(bc_forest_birds_hotspot_projected,
#   "data/processed_rasters/bc_forest_birds_hotspot_raster_NAD83_2023.tif",
#   datatype = "INT2S",  # or appropriate type
#   overwrite = TRUE)

writeRaster(bc_forest_birds_hotspot_projected,
  "data/processed_rasters/version2/3_bc_forest_birds_hotspot_raster_NAD83_2023_v2.tif",
  datatype = "INT2S",  # or appropriate type
  overwrite = TRUE)

# ---- 3a) STACK BC BIRDS (Grassland birds) ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
list_requested_data<-read.csv("data/list/birds_grouping_request_new/socb_groups.csv")
grassland_birds<-list_requested_data %>% filter(group_name=="grassland_birds")

# Create patterns and find them in the raster folders 
species_names_grassland_birds <- gsub(" ", "_", grassland_birds$common_name)
pattern_grassland_birds <- paste0("^(", paste(species_names_grassland_birds, collapse = "|"), ")")
selected_files_grassland_birds <- files[ grepl(pattern_grassland_birds, basename(files))]

# Create destination folder for the grassland birds rasters
output_folder_grassland <- "data/output_bc_grassland_birds_named_crop"

dir.create(output_folder_grassland,
           recursive = TRUE,
           showWarnings = FALSE)

# Copy selected files into the new folder
file.copy( from = selected_files_grassland_birds,
           to = output_folder_grassland ,
           overwrite = TRUE)

# quick check for the number of grassland birds in the list, tehn compare it with teh rasters extracted 

length(selected_files_grassland_birds )


#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# ---- 3b) STACK BC BIRDS~Function (Grassland_birds) ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# ---- Function: Load, reclassify, and sum rasters ----

load_reclass_and_sum_raster <- function(folder_rasters_combos) {
  files <- list.files(
    path = folder_rasters_combos,
    pattern = "\\.tif$",
    full.names = TRUE
  ) # List Selected raster files  .tif raster files
  
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

# Folder with rasters

output_folder_grassland <- "data/output_bc_grassland_birds_named_crop"

# Apply function
bc_grassland_birds_hotspot <- load_reclass_and_sum_raster(output_folder_grassland)

# Plot hotspot
plot(bc_grassland_birds_hotspot)
### I am working here 


#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# ---- 3c) WRITE OUTPUT RASTER ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#

# NEED TO PROJECT IT to be used in ArcGis
bc_grassland_birds_hotspot_projected <- terra::project(  bc_grassland_birds_hotspot,"EPSG:3005",method = "near") # EPSG:3005 is BC Albers, and method = "near" uses nearest-neighbour resampling. That means the reprojection will only assign existing raster values rather than creating intermediate values. Terra specifically recommends near for categorical/discrete rasters

# writeRaster(
#   bc_grassland_birds_hotspot_projected,
#   "data/processed_rasters/bc_grassland_birds_hotspot_raster_NAD83_2023.tif",
#   datatype = "INT2S",  # or appropriate type
#   overwrite = TRUE)

writeRaster(
  bc_grassland_birds_hotspot_projected,
  "data/processed_rasters/version2/4_bc_grassland_birds_hotspot_raster_NAD83_2023v2.tif",
  datatype = "INT2S",  # or appropriate type
  overwrite = TRUE)


# ---- 4a) STACK BC BIRDS (longdistance migrants) ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
list_requested_data<-read.csv("data/list/birds_grouping_request_new/socb_groups.csv")
longdistance_migrants<-list_requested_data %>% filter(group_name=="long_distance_migrants")

species_names_longdistance_migrants <- gsub(" ", "_", longdistance_migrants$common_name)

pattern_longdistance_migrants <- paste0("^(", paste(species_names_longdistance_migrants, collapse = "|"), ")")
selected_files_longdistance_migrants <- files[ grepl(pattern_longdistance_migrants, basename(files))]

# Create destination folder for the longdistance_migrants birds rasters
output_folder_longdistance_migrants <- "data/output_bc_longdistance_migrants_named_crop"

dir.create(output_folder_longdistance_migrants,
           recursive = TRUE,
           showWarnings = FALSE)

# Copy selected files into the new folder
file.copy( from = selected_files_longdistance_migrants,
           to = output_folder_longdistance_migrants ,
           overwrite = TRUE)

# quick check for the number of longdistance_migrants birds in the list, tehn compare it with teh rasters extracted 

length(selected_files_longdistance_migrants )


#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# ---- 4b) STACK BC BIRDS (longdistance_migrants) ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#

# ---- Function: Load, reclassify, and sum rasters ----

load_reclass_and_sum_raster <- function(folder_rasters_combos) {
  
  files <- list.files(
    path = folder_rasters_combos,
    pattern = "\\.tif$",
    full.names = TRUE
  ) # List Selected raster files  .tif raster files
  
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

# Folder with rasters

output_folder_longdistance_migrants <- "data/output_bc_longdistance_migrants_named_crop"

# Apply function
bc_longdistance_migrants_hotspot <- load_reclass_and_sum_raster(output_folder_longdistance_migrants) # max number shoudl be less than the total numbers of specie sin teh list 

# Plot hotspot
plot(bc_longdistance_migrants_hotspot)

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# ---- 4c) WRITE OUTPUT RASTER ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#

# NEED TO PROJECT IT to be used in ArcGis
bc_longdistance_migrants_hotspot_projected <- terra::project(  bc_longdistance_migrants_hotspot ,"EPSG:3005",method = "near") # EPSG:3005 is BC Albers, and method = "near" uses nearest-neighbour resampling. That means the reprojection will only assign existing raster values rather than creating intermediate values. Terra specifically recommends near for categorical/discrete rasters

# writeRaster(
#   bc_longdistance_migrants_hotspot_projected,
#   "data/processed_rasters/bc_longdistance_migrants_hotspot_raster_NAD83_2023.tif",
#   datatype = "INT2S",  # or appropriate type
#   overwrite = TRUE)

writeRaster(
  bc_longdistance_migrants_hotspot_projected,
  "data/processed_rasters/version2/5_bc_longdistance_migrants_hotspot_raster_NAD83_2023_v2.tif",
  datatype = "INT2S",  # or appropriate type
  overwrite = TRUE)





# ---- 5a) STACK BC BIRDS (Migratory birds for Dan) ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
#  migrants this is a request from Dan Dixon ECCC
overall_migrants<-read.csv("data/list/birds_grouping_request_new/MBCA_MigBirds_Dan_ECCC.csv")

species_names_overall_migrants <- gsub(" ", "_", overall_migrants$common_name)

pattern_overall_migrants <- paste0("^(", paste(species_names_overall_migrants, collapse = "|"), ")")
selected_files_overall_migrants <- files[ grepl(pattern_overall_migrants, basename(files))]

# Create destination folder for the overall_migrants birds rasters
output_folder_overall_migrants <- "data/output_bc_overall_migrants_named_crop"

dir.create(output_folder_overall_migrants,
           recursive = TRUE,
           showWarnings = FALSE)

# Copy selected files into the new folder
file.copy( from = selected_files_overall_migrants,
           to = output_folder_overall_migrants ,
           overwrite = TRUE)

# quick check for the number of overall_migrants birds in the list, tehn compare it with teh rasters extracted 

length(selected_files_overall_migrants )


#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# ---- 5b) STACK BC BIRDS (Migratory birds for Dan) ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#


# ---- Function: Load, reclassify, and sum rasters ----


load_reclass_and_sum_raster <- function(folder_rasters_combos) {
  
  files <- list.files(
    path = folder_rasters_combos,
    pattern = "\\.tif$",
    full.names = TRUE
  ) # List Selected raster files  .tif raster files
  
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

# Folder with rasters

output_folder_overall_migrants <- "data/output_bc_overall_migrants_named_crop"

# Apply function
bc_overall_migrants_hotspot <- load_reclass_and_sum_raster(output_folder_overall_migrants) # max number shoudl be less than the total numbers of specie sin teh list 

# Plot hotspot
plot(bc_overall_migrants_hotspot)


#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# ---- 5c) WRITE OUTPUT RASTER ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#

# NEED TO PROJECT IT to be used in ArcGis
bc_overall_migrants_hotspot_projected <- terra::project(bc_overall_migrants_hotspot,"EPSG:3005",method = "near") # EPSG:3005 is BC Albers, and method = "near" uses nearest-neighbour resampling. That means the reprojection will only assign existing raster values rather than creating intermediate values. Terra specifically recommends near for categorical/discrete rasters

# writeRaster(
#   bc_overall_migrants_hotspot_projected,
#   "data/processed_rasters/bc_overall_migrants_hotspot_raster_NAD83_2023.tif",
#   datatype = "INT2S",  # or appropriate type
#   overwrite = TRUE)

writeRaster(
  bc_overall_migrants_hotspot_projected,
  "data/processed_rasters/version2/10_bc_overall_migrants_hotspot_raster_NAD83_2023_v2.tif",
  datatype = "INT2S",  # or appropriate type
  overwrite = TRUE)


# 



# ---- 5a) STACK BC BIRDS (CIJV and pbhjv PRIORITY SPECIES ) ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
#  priority species
priority_species_jv<-read.csv("data/list/birds_grouping_request_new/priority_species_JVs.csv")



species_names_priority_species_jv <- gsub(" ", "_", priority_species_jv$common_name)

pattern_priority_species_jv <- paste0("^(", paste(species_names_priority_species_jv, collapse = "|"), ")")
selected_files_priority_species_jv <- files[ grepl(pattern_priority_species_jv, basename(files))]

# Create destination folder for the priority_species_jv birds rasters
output_folder_priority_species_jv <- "data/output_bc_priority_species_jv_named_crop"

dir.create(output_folder_priority_species_jv,
           recursive = TRUE,
           showWarnings = FALSE)

# Copy selected files into the new folder
file.copy( from = selected_files_priority_species_jv,
           to = output_folder_priority_species_jv ,
           overwrite = TRUE)

# quick check for the number of priority_species_jv birds in the list, tehn compare it with teh rasters extracted 

length(selected_files_priority_species_jv )


#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# ---- 5b) STACK BC BIRDS (priority_species_jv) ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#

# ---- Function: Load, reclassify, and sum rasters ----

load_reclass_and_sum_raster <- function(folder_rasters_combos) {
  
  files <- list.files(
    path = folder_rasters_combos,
    pattern = "\\.tif$",
    full.names = TRUE
  ) # List Selected raster files  .tif raster files
  
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

# Folder with rasters

output_folder_priority_species_jv <- "data/output_bc_priority_species_jv_named_crop"

# Apply function
bc_priority_species_jv_hotspot <- load_reclass_and_sum_raster(output_folder_priority_species_jv) # max number shoudl be less than the total numbers of specie sin teh list 

# Plot hotspot
plot(bc_priority_species_jv_hotspot)

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# ---- 5c) WRITE OUTPUT RASTER ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#

# NEED TO PROJECT IT to be used in ArcGis
bc_priority_species_jv_hotspot_projected <- terra::project(  bc_priority_species_jv_hotspot ,"EPSG:3005",method = "near") # EPSG:3005 is BC Albers, and method = "near" uses nearest-neighbour resampling. That means the reprojection will only assign existing raster values rather than creating intermediate values. Terra specifically recommends near for categorical/discrete rasters

# writeRaster(
#   bc_priority_species_jv_hotspot_projected,
#   "data/processed_rasters/bc_priority_species_jv_hotspot_raster_NAD83_2023_pixel3km.tif",
#   datatype = "INT2S",  # or appropriate type
#   overwrite = TRUE)

writeRaster(
  bc_priority_species_jv_hotspot_projected,
  "data/processed_rasters/version2/6_bc_priority_species_jv_hotspot_raster_NAD83_2023_pixel3km_v2.tif",
  datatype = "INT2S",  # or appropriate type
  overwrite = TRUE)

# ---- 6a) STACK BC BIRDS (priority_species_cijv) ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#

priority_species_cijv <- read.csv( "data/list/birds_grouping_request_new/priority_species_JVs.csv") %>%
  filter( jv_priority %in% c("Both", "CIJV"))
# 


species_names_priority_species_cijv <- gsub(" ", "_", priority_species_cijv$common_name)

pattern_priority_species_cijv <- paste0("^(", paste(species_names_priority_species_cijv, collapse = "|"), ")")
selected_files_priority_species_cijv <- files[ grepl(pattern_priority_species_cijv, basename(files))]

# Create destination folder for the priority_species_cijv birds rasters
output_folder_priority_species_cijv <- "data/output_bc_priority_species_cijv_named_crop"

dir.create(output_folder_priority_species_cijv,
           recursive = TRUE,
           showWarnings = FALSE)

# Copy selected files into the new folder
file.copy( from = selected_files_priority_species_cijv,
           to = output_folder_priority_species_cijv ,
           overwrite = TRUE)

# quick check for the number of priority_species_cijv birds in the list, tehn compare it with teh rasters extracted 

length(selected_files_priority_species_cijv )


#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# ---- 6b) STACK BC BIRDS (priority_species_cijv) ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#

# ---- Function: Load, reclassify, and sum rasters ----

load_reclass_and_sum_raster <- function(folder_rasters_combos) {
  
  files <- list.files(
    path = folder_rasters_combos,
    pattern = "\\.tif$",
    full.names = TRUE
  ) # List Selected raster files  .tif raster files
  
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

# Folder with rasters

output_folder_priority_species_cijv <- "data/output_bc_priority_species_cijv_named_crop"

# Apply function
bc_priority_species_cijv_hotspot <- load_reclass_and_sum_raster(output_folder_priority_species_cijv) # max number shoudl be less than the total numbers of specie sin teh list 

# Plot hotspot
plot(bc_priority_species_cijv_hotspot)

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# ---- 6c) WRITE OUTPUT RASTER ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#

# NEED TO PROJECT IT to be used in ArcGis
bc_priority_species_cijv_hotspot_projected <- terra::project(  bc_priority_species_cijv_hotspot ,"EPSG:3005",method = "near") # EPSG:3005 is BC Albers, and method = "near" uses nearest-neighbour resampling. That means the reprojection will only assign existing raster values rather than creating intermediate values. Terra specifically recommends near for categorical/discrete rasters

# writeRaster(
#   bc_priority_species_cijv_hotspot_projected,
#   "data/processed_rasters/bc_priority_species_cijv_hotspot_raster_NAD83_2023_pixel3km.tif",
#   datatype = "INT2S",  # or appropriate type
#   overwrite = TRUE)

writeRaster(
  bc_priority_species_cijv_hotspot_projected,
  "data/processed_rasters/version2/7_bc_priority_species_cijv_hotspot_raster_NAD83_2023_pixel3km.tif",
  datatype = "INT2S",  # or appropriate type
  overwrite = TRUE)

# ---- 7a) STACK BC BIRDS (priority_species_pbhjv) ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#

priority_species_pbhjv <- read.csv( "data/list/birds_grouping_request_new/priority_species_JVs.csv") %>%
  filter( jv_priority %in% c("Both", "PBHJV-BC"))

species_names_priority_species_pbhjv <- gsub(" ", "_", priority_species_pbhjv$common_name)

pattern_priority_species_pbhjv <- paste0("^(", paste(species_names_priority_species_pbhjv, collapse = "|"), ")")
selected_files_priority_species_pbhjv <- files[ grepl(pattern_priority_species_pbhjv, basename(files))]

# Create destination folder for the priority_species_pbhjv birds rasters
output_folder_priority_species_pbhjv <- "data/output_bc_priority_species_pbhjv_named_crop"

dir.create(output_folder_priority_species_pbhjv,
           recursive = TRUE,
           showWarnings = FALSE)

# Copy selected files into the new folder
file.copy( from = selected_files_priority_species_pbhjv,
           to = output_folder_priority_species_pbhjv ,
           overwrite = TRUE)

# quick check for the number of priority_species_pbhjv birds in the list, tehn compare it with teh rasters extracted 

length(selected_files_priority_species_pbhjv )


#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# ---- 7b) STACK BC BIRDS (priority_species_pbhjv) ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#

# ---- Function: Load, reclassify, and sum rasters ----

load_reclass_and_sum_raster <- function(folder_rasters_combos) {
  
  files <- list.files(
    path = folder_rasters_combos,
    pattern = "\\.tif$",
    full.names = TRUE
  ) # List Selected raster files  .tif raster files
  
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

# Folder with rasters

output_folder_priority_species_pbhjv <- "data/output_bc_priority_species_pbhjv_named_crop"

# Apply function
bc_priority_species_pbhjv_hotspot <- load_reclass_and_sum_raster(output_folder_priority_species_pbhjv) # max number shoudl be less than the total numbers of specie sin teh list 

# Plot hotspot
plot(bc_priority_species_pbhjv_hotspot)

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# ---- 7c) WRITE OUTPUT RASTER ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#

# NEED TO PROJECT IT to be used in ArcGis
bc_priority_species_pbhjv_hotspot_projected <- terra::project(  bc_priority_species_pbhjv_hotspot ,"EPSG:3005",method = "near") # EPSG:3005 is BC Albers, and method = "near" uses nearest-neighbour resampling. That means the reprojection will only assign existing raster values rather than creating intermediate values. Terra specifically recommends near for categorical/discrete rasters

# writeRaster(
#   bc_priority_species_pbhjv_hotspot_projected,
#   "data/processed_rasters/bc_priority_species_pbhjv_hotspot_raster_NAD83_2023_pixel3km.tif",
#   datatype = "INT2S",  # or appropriate type
#   overwrite = TRUE)

writeRaster(
  bc_priority_species_pbhjv_hotspot_projected,
  "data/processed_rasters/version2/bc_priority_species_pbhjv_hotspot_raster_NAD83_2023_pixel3km_v2.tif",
  datatype = "INT2S",  # or appropriate type
  overwrite = TRUE)



# ---- 8a) STACK BC BIRDS (coastal waterbird species ) ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
list_requested_data<-read.csv("data/list/birds_grouping_request_new/guild_list.csv")
coastal_waterbirds<-list_requested_data 

# Create patterns and find them in the raster folders 
species_names_coastal_waterbirds <- gsub(" ", "_", coastal_waterbirds$common_name)
pattern_coastal_waterbirds <- paste0("^(", paste(species_names_coastal_waterbirds, collapse = "|"), ")")
selected_files_coastal_waterbirds <- files[ grepl(pattern_coastal_waterbirds, basename(files))]

# Create destination folder for the grassland birds rasters
output_folder_coastal_waterbirds <- "data/output_bc_coastal_waterbirds_named_crop"

dir.create(output_folder_coastal_waterbirds,
           recursive = TRUE,
           showWarnings = FALSE)

# Copy selected files into the new folder
file.copy( from = selected_files_coastal_waterbirds,
           to = output_folder_coastal_waterbirds ,
           overwrite = TRUE)

# quick check for the number of grassland birds in the list, tehn compare it with teh rasters extracted 

length(selected_files_coastal_waterbirds )


#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# ---- 8b) STACK BC BIRDS~Function (coastal_waterbirds) ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# ---- Function: Load, reclassify, and sum rasters ----

load_reclass_and_sum_raster <- function(folder_rasters_combos) {
  files <- list.files(
    path = folder_rasters_combos,
    pattern = "\\.tif$",
    full.names = TRUE
  ) # List Selected raster files  .tif raster files
  
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

# Folder with rasters

output_folder_coastal_waterbirds <- "data/output_bc_coastal_waterbirds_named_crop"

# Apply function
bc_coastal_waterbirds_hotspot <- load_reclass_and_sum_raster(output_folder_coastal_waterbirds)

# Plot hotspot
plot(bc_coastal_waterbirds_hotspot)
### I am working here 


#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# ---- 3c) WRITE OUTPUT RASTER ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#

# NEED TO PROJECT IT to be used in ArcGis
bc_coastal_waterbirds_hotspot_projected <- terra::project(  bc_coastal_waterbirds_hotspot,"EPSG:3005",method = "near") # EPSG:3005 is BC Albers, and method = "near" uses nearest-neighbour resampling. That means the reprojection will only assign existing raster values rather than creating intermediate values. Terra specifically recommends near for categorical/discrete rasters

# writeRaster(
#   bc_coastal_waterbirds_hotspot_projected,
#   "data/processed_rasters/bc_coastal_waterbirds_hotspot_raster_NAD83_2023.tif",
#   datatype = "INT2S",  # or appropriate type
#   overwrite = TRUE)

writeRaster(
  bc_coastal_waterbirds_hotspot_projected,
  "data/processed_rasters/version2/9_bc_coastal_waterbirds_hotspot_raster_NAD83_2023v2.tif",
  datatype = "INT2S",  # or appropriate type
  overwrite = TRUE)




# # ----  WRITE OUTPUT RASTERS CODE CHUNK ----
# #=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# 
# # NEED TO PROJECT IT to be used in ArcGis
# bc_all_birds_hotspot_projected <- terra::project(  bc_all_birds_hotspot,"EPSG:3005",method = "near") # EPSG:3005 is BC Albers, and method = "near" uses nearest-neighbour resampling. That means the reprojection will only assign existing raster values rather than creating intermediate values. Terra specifically recommends near for categorical/discrete rasters
# 
# writeRaster(
#   bc_all_birds_hotspot_projected,
#   "data/processed_rasters/bc_all_birds_hotspots_raster_NAD83_2023.tif",
#   datatype = "INT2S",  # or appropriate type
#   overwrite = TRUE)

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# ---- extra) PLOTTING ----
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
# ---- END OF SCRIPT ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#



# Read the list for each species in the rasters created -------------------

bc_birds<-rast ("data/processed_rasters/version2/1_bc_all_birds_hotspots_raster_NAD83_2023_v2.tif")

bc_sara_birds <-rast ("data/processed_rasters/version2/2_bc_sara_birds_hotspots_raster_NAD83_2023_v2.tif")
bc_forest_birds<- rast("data/processed_rasters/version2/3_bc_forest_birds_hotspot_raster_NAD83_2023_v2.tif")
bc_forest_birds<-rast("data/processed_rasters/version2/4_bc_grassland_birds_hotspot_raster_NAD83_2023_v2.tif")
bc_longdistance_migrants<-rast("data/processed_rasters/version2/5_bc_longdistance_migrants_hotspot_raster_NAD83_2023_v2.tif")
bc_priority_species_pbhjvandcijv <-rast("data/processed_rasters/version2/6_bc_priority_species_jv_hotspot_raster_NAD83_2023_pixel3km_v2.tif")
bc_priority_species_cijv<-rast("data/processed_rasters/version2/7_bc_priority_species_cijv_hotspot_raster_NAD83_2023_pixel3km_v2.tif")
bc_priority_species_pbjv<-rast("data/processed_rasters/version2/8_bc_priority_species_pbhjv_hotspot_raster_NAD83_2023_pixel3km_v2.tif")
bc_waterfowl_coastal_waterbirds<-rast("data/processed_rasters/version2/9_bc_coastal_waterbirds_hotspot_raster_NAD83_2023v2.tif")
bc_migratory_birds_dan<-rast("data/processed_rasters/version2/10_bc_overall_migrants_hotspot_raster_NAD83_2023_v2.tif")


# lets try to extract all the names from each folder containing the rasters for each of the groups-------------------------

# Trial for one folder
# files1 <- list.files("data/output_bc_crop_named_all", recursive = TRUE, full.names = TRUE)
# 
# table1 <- data.frame(
#   file_name = basename(files1),
#   file_path = files1)
# 
# table1$species <- sub("abundance.*", "", table1$file_name)

# Function 

extract_species <- function(folder_path) {
  # Extract all files from the folder
  files <- list.files(folder_path,recursive = TRUE, full.names = TRUE)
  # Create a table
  species_table <- data.frame(file_name = basename(files),file_path = files)
  # Extract species names
  species_table$species <- sub("abundance.*",  "", species_table$file_name)
  
  return(species_table)
}


bc_birds <- extract_species("data/output_bc_crop_named_all")

bc_birds_sara <- extract_species("data/output_bc_crop_named_sara")

bc_forest_birds <- extract_species("data/output_bc_forest_birds_named_crop")

bc_grasslands_birds <- extract_species("data/output_bc_grassland_birds_named_crop")

bc_longdistance_migrants <- extract_species("data/output_bc_longdistance_migrants_named_crop")

bc_overall_migrants <- extract_species("data/output_bc_overall_migrants_named_crop")

bc_priority_species_cijv <- extract_species("data/output_bc_priority_species_cijv_named_crop")

bc_priority_species_pbhjv<- extract_species("data/output_bc_priority_species_pbhjv_named_crop")

bc_priority_species <- extract_species("data/output_bc_priority_species_jv_named_crop")

bc_coastal_waterbirds<-extract_species("data/output_bc_coastal_waterbirds_named_crop")


# Add folder names to each table
bc_birds$group <- "bc_birds_included"
bc_birds_sara$group <- "bc_birds_sara"
bc_forest_birds$group <- "bc_forest_birds"
bc_grasslands_birds$group <- "bc_grassland_birds"
bc_longdistance_migrants$group <- "bc_longdistance_migrants"
bc_priority_species_cijv$group <- "bc_priority_species_cijv"
bc_priority_species_pbhjv$group <- "bc_priority_species_pbhjv"
bc_priority_species$group <- "bc_priority_species_jv"
bc_coastal_waterbirds$group <- "bc_coastal_waterbirds"
bc_overall_migrants$group <- "bc_overall_migrants"


# Combine all tables
all_files <- rbind(
  bc_birds,
  bc_birds_sara,
  bc_forest_birds,
  bc_grasslands_birds,
  bc_longdistance_migrants,
  bc_overall_migrants,
  bc_priority_species_cijv,
  bc_priority_species_pbhjv,
  bc_priority_species,
  bc_coastal_waterbirds)


# View the combined table
#View(all_files)
write.csv(
  all_files,
  "metadata_outputs_JM/bird_species_list_per_rasters_v2.csv",
  row.names = FALSE)

# #=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# # ---- 3) SARA SPECIES ANALYSIS ----
# #=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# # species-at-risk listed under Schedule 1 of the federal Species at Risk Act
# # Note:
# # Resident species in eBird do not have full-year_max layer
# # They have abundance_seasonal_max_3km.t
# # Non-resident species have abundance_seasonal_max layer
# # Goal: compare both types
# 
# #=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# # ---- 3a) STACK BC SARA SPECIES ----
# #=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# folder_rasters_combos_sara <- "data/output_bc_crop_named_sara"
# 
# # Function (same logic reused)
# load_reclass_and_sum_raster <- function(folder_rasters_combos_sara) {
#   files <- list.files(
#     path = folder_rasters_combos_sara,
#     pattern = "\\.tif$",
#     full.names = TRUE
#   )
#   if (length(files) == 0) {
#     warning("No raster files found in the folder")
#     return(NULL)
#   }
#   raster_stack <- terra::rast(files)
#   reclassified <- raster_stack > 0   # Reclassify: TRUE where >0, FALSE where <=0
#   sum_raster <- terra::app(reclassified, sum, na.rm = TRUE)# Sum across layers
#   zero_mask <- terra::app(reclassified == 0, sum, na.rm = TRUE) > 0 # Mask: identify pixels with at least one 0
#   sum_raster[is.na(sum_raster) & zero_mask] <- 0  # Replace NA with 0 where appropriate
#   return(sum_raster)
# }
# 
# # Apply function
# bc_sara_hotspot <- load_reclass_and_sum_raster(folder_rasters_combos_sara)
# 
# # Plot hotspot
# plot(bc_sara_hotspot)
# 
# #=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# # ---- 3b) WRITE OUTPUT RASTER ----
# #=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# 
# # NEED TO PROJECT IT to be used in ArcGis
# bc_sara_hotspot_projected <- terra::project(
#   bc_sara_hotspot,
#   "EPSG:3005",method = "near")
# 
# 
# writeRaster(
#   bc_sara_hotspot_projected,
#   "data/processed_rasters/bc_sara_birds_hotspots_raster_NAD83_2023V2.tif",
#   datatype = "INT2S",  # or appropriate type
#   overwrite = FALSE
# )
# 
# #=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# 
# # ---- 3) Grasslands species analyses ----
# #=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# # 
# # Note:
# # Resident species in eBird do not have full-year_max layer
# # They have abundance_seasonal_max_3km.t
# # Non-resident species have abundance_seasonal_max layer
# # Goal: compare both types
# 
# #=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# # ---- 3a) STACK BC Grassland SPECIES ----
# #=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# folder_rasters_combos_sara <- "data/output_bc_crop_named_sara"
# 
# # Function (same logic reused)
# load_reclass_and_sum_raster <- function(folder_rasters_combos_sara) {
#   files <- list.files(
#     path = folder_rasters_combos_sara,
#     pattern = "\\.tif$",
#     full.names = TRUE
#   )
#   if (length(files) == 0) {
#     warning("No raster files found in the folder")
#     return(NULL)
#   }
#   raster_stack <- terra::rast(files)
#   reclassified <- raster_stack > 0   # Reclassify: TRUE where >0, FALSE where <=0
#   sum_raster <- terra::app(reclassified, sum, na.rm = TRUE)# Sum across layers
#   zero_mask <- terra::app(reclassified == 0, sum, na.rm = TRUE) > 0 # Mask: identify pixels with at least one 0
#   sum_raster[is.na(sum_raster) & zero_mask] <- 0  # Replace NA with 0 where appropriate
#   return(sum_raster)
# }
# 
# # Apply function
# bc_sara_hotspot <- load_reclass_and_sum_raster(folder_rasters_combos_sara)
# 
# # Plot hotspot
# plot(bc_sara_hotspot)
# 
# #=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# # ---- 3b) WRITE OUTPUT RASTER grasslands ----
# #=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# 
# # NEED TO PROJECT IT to be used in ArcGis
# bc_sara_hotspot_projected <- terra::project(
#   bc_sara_hotspot,
#   "EPSG:3005",method = "near")
# 
# 
# writeRaster(
#   bc_sara_hotspot_projected,
#   "data/processed_rasters/bc_sara_birds_hotspots_raster_NAD83_2023V2.tif",
#   datatype = "INT2S",  # or appropriate type
#   overwrite = FALSE
# )

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




