
###_###_####_###_###_###_###_###_###_###_###_###_###_###_###_###_###_
## BC fund collaboration: future for Bc birds
## ##
## Objective:stacking selected e-bird rasters into single rasters for all Bc species
## ## Specifically calculating  number of species per pixel
## This code does the following:
## 0) Download e-bird 3*3 km rasters fro all BC birds
## i) Read tiff files into Rasters 
## ii) Reclassify rasters (0,1)
## iii) Sum rasters generating total number of species oer pixel
##
## Updated and annotated by Jenny Munoz
## Last updated: August 2025
###_###_####_###_###_###_###_###_###_###_###_###_###_###_###_###_###_

# 0 Set up  -----------------------------------------------------------------
# Install required Libraries 
# To access ebird data 
library(ebirdst)
library(ggplot2)

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
library(ggplot2) # fpr plots
# 0.1 ebird Key  -----------------------------------------------------------------
#An access key is required to download eBird Status and Trends data
#1. Get a key by filling out the request form at https://ebird.org/st/request
#2. Save the key using 
set_ebirdst_access_key("f6me7thr51ul") # Valid until March 2026
getwd() 


# Part 1 explore and download the data  --------------------------------------------------

# Species list for BC



# Explore available status data products species
View(ebirdst_runs)
# examine seasonal dates and quality scores for chimney swift
glimpse(filter(ebirdst_runs, common_name == "Chimney Swift"))
# ├ Downloading data ----

# default data download location
ebirdst_data_dir()
# list available files for Golden Eagle
ebirdst_download_status("Golden Eagle", dry_run = TRUE)
# download 3 km estimates for Golden Eagle, a migrant
ebirdst_download_status("Golden Eagle", pattern = "3km")
# download 3 km estimates for Tui, a resident
ebirdst_download_status("Tui", pattern = "3km")


