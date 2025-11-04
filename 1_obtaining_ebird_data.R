
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

# ================================
# 0) SETUP
# ================================

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


# ================================
# 0) SETUP & ACCESS KEY
# ================================
# ---- eBird S&T access key ----
# An access key is required to download eBird Status & Trends data.
# 1) Request a key here: https://ebird.org/st/request
# 2) Save the key for this session with set_ebirdst_access_key().
#    NOTE: Do NOT hard-code real keys in scripts stored in repos. Prefer ~/.Renviron.
#    usethis::edit_r_environ(); add a line like: EBIRDST_KEY="your-key"
#    Then call set_ebirdst_access_key(Sys.getenv("EBIRDST_KEY")).

set_ebirdst_access_key("YOUR_KEY_HERE")  # <- replace for local testing only
# Where am I running this from (useful for path debugging)?
getwd()
# ebirdst package version (useful for reproducibility)
ebirdst_version()

# ================================
# 1) CHOOSING THE DATA DIRECTORY
# ================================

# ebirdst_data_dir() resolves the download directory using:
# 1) EBIRDST_DATA_DIR env var if set, otherwise
# 2) tools::R_user_dir("ebirdst", which = "data")
ebirdst_data_dir()

# If you want to override the default for THIS SESSION ONLY:
# (Pick a fast local SSD or a managed project folder.)
Sys.setenv(EBIRDST_DATA_DIR = "C:/Users/jmunoz/Documents/BirdsCanada/1_jv_science_coordinator_role/1_projects/9_future_for_bc_birds/e_bird_data_bc")
ebirdst::ebirdst_data_dir()

# ================================
# 2) SPECIES LIST FOR BC
# ================================

# This list is filtered to exclude rare/accidental, introduced, uncertain, and extirpated statuses.
# Assumes the CSV has at least columns: "status" and "common_name".
bc_list_full <- read.csv("data/bc_birds_checklist.csv", stringsAsFactors = FALSE) %>%
  as_tibble()
#View(bc_list_full)
# Safety checks in case the CSV schema changes
#stopifnot(all(c("status", "common_name") %in% names(bc_list)))
bc_list <- bc_list_full%>%
  filter(!status %in% c("Rare/Accidental", "Introduced", "Uncertain", "Extirpated"))
# Quick peeks
View(bc_list)
names(bc_list)
unique(bc_list$status)
unique(bc_list$common_name)

# Vector of species names we’ll potentially loop over later
bc_species <- unique(bc_list$common_name)

# ================================
# 3) EXPLORE AVAILABLE RUNS
# ================================
# If not attached, reference explicitly as ebirdst::ebirdst_runs
# Example: explore Cinnamon Teal availability
glimpse(dplyr::filter(ebirdst::ebirdst_runs, common_name == "Cinnamon Teal"))
# View(ebirdst_runs$common_name)
# View(ebirdst_runs)

# ================================
# 4) See FOR HOW MANY BC SPECIES WE HAVE EBIRD DATA 
# ================================

match_species_bc_ebird <- bc_list %>%
  left_join(ebirdst_runs, by = "common_name")
#View(match_species_bc_ebird )
# for which species we dont have it
missing_species_bc_ebird <- match_species_bc_ebird %>%
  filter(is.na(scientific_name))
#View(missing_species_bc_ebird )
print(missing_species_bc_ebird$sci_name)
# species for which we have the data  #### This is the list of species that I need for later 
list_species_bc_ebird <- bc_list %>%
  inner_join(ebirdst_runs, by = "common_name")
#House wren corrected manually
# Check manually those species with the ebirdst_runs

length(list_species_bc_ebird$common_name)
View(list_species_bc_ebird)

# ================================
# 4) DOWNLOAD DATA FOR ONE SPECIES
# ================================

# The function below checks what exists (dry_run = TRUE) before downloading.
# pattern = "full-year_max_3km" targets the 3-km full-year max product.
# Set download_occurrence=TRUE if you also want occurrence rasters.
# force = TRUE avoids prompts and overwrites if needed.

# DRY RUN: list what would be downloaded for Cinnamon Teal
ebirdst_download_status( "cinnamon teal",pattern = "full-year_max_3km",download_occurrence = TRUE,dry_run = TRUE)

# ACTUAL DOWNLOAD
ebirdst_download_status( "cinnamon teal",pattern = "full-year_max_3km", download_occurrence = TRUE,dry_run = TRUE,force = TRUE)
ebirdst_download_status( "cinnamon teal",pattern = "full-year_max_3km", download_occurrence = FALSE,dry_run = FALSE,force = TRUE)

# i THINK FOR THE RESIDENT SPECIES THE ABUNDANCE IS UNDER SEASONAL AND NO ANNUAL MAX FOR SOME REASON FOR EXAMPLE THIS TWO SPECIES DO NOT  HAVE FULL-YEAR MAX
ebirdst_download_status( "house finch",pattern = "abundance_seasonal_max_3km", download_occurrence = FALSE,dry_run = FALSE,force = TRUE)
ebirdst_download_status( "American Dipper ",pattern = "_3km", download_occurrence = FALSE,dry_run = TRUE,force = TRUE)

# ================================
# 5) LOAD RASTER PRODUCTS
# ================================

# load_raster() loads a specific raster from the downloaded products.
# Typical args:
# - species: common or scientific name (must match downloaded folder)
# - product: "abundance" or "occurrence"
#occurrence: the expected probability (0-1) of occurrence of a species.
#count: the expected count of a species, conditional on its occurrence at the given location.
#abundance: the expected relative abundance of a species, computed as the product of the probability of occurrence and the count conditional on occurrence.
#proportion-population: the proportion of the total relative abundance within each cell. This is a derived product calculated by dividing each cell value in the relative abundance raster by the total abundance summed across all cells.
# - period: "full-year" (or a season)
# - metric: for "abundance": e.g., "mean", "max"; for "occurrence": often "mean" or "max"
# Below we load occurrence "max" for full-year.

raster_cite_fullyear <- load_raster(species = "Cinnamon Teal", product = "abundance",period= "full-year",metric  = "max")

raster_housefinch_fullyear <- load_raster(species = "house finch", product = "abundance", period="seasonal", metric="max")  

# If you’re unsure about parameters or available products/periods/metrics:
 ?load_raster

# ================================
# 6) RUN A LOOP TO DOWNLOAD DATA FOR BC SPECIES 
# ================================
# Although we know there is not data for all BC species — only 301 out of 338 — we could still run it for the entire list so that when the package adds new data,
# the same code can automatically handle those species.  Just make sure to check which species were skipped.

# Option 1: Use all species from the BC list
# bc_species <- unique(bc_list$common_name)

# Option 2: Use only species that are present in both the BC list and eBird S&T runs
# list_species_bc_ebird <- bc_list %>%
#   inner_join(ebirdst_runs, by = "common_name")

bc_species <- unique(list_species_bc_ebird$common_name)

# Identify resident species (these have a different data product name)
bc_species_resident <- list_species_bc_ebird %>%
  filter(is_resident == "TRUE")

# LOOP over the list of species and download data, you will notice that it skipped some species, that do not have "full-year_max" data 

for (species in bc_species) {
  cat("\n>>> Downloading:", species, "\n")
  # Download full-year abundance data at 3 km
  try({
    ebirdst_download_status(species,pattern = "full-year_max_3km",download_occurrence = FALSE,dry_run = FALSE, force = TRUE)
  }, silent = TRUE)
}

# Resident species do not have a "full-year" abundance dataset.
# For some reason, their data product is named "abundance_seasonal_max_3km".
# Make sure force = FALSE so you don’t overwrite other species

for (species in bc_species_resident) {
  cat("\n>>> Downloading residents:", species, "\n")
  # Download seasonal abundance data for resident species at 3 km
  try({ ebirdst_download_status( species,pattern = "abundance_seasonal_max_3km",download_occurrence = FALSE,dry_run = FALSE,force = FALSE  )
  }, silent = TRUE)
}

# ================================
# 7) Load AND CROP TO BC BOUNDARIES FOR ALL BC SPECIES DOWNLOADED
# ================================

# try(..., silent = TRUE) means if one species fails (e.g., not available), the loop continues.

bc_boundary=


st_transform(st_crs(abd_seasonal)) |>
  

for (species in bc_species) {
  # load abundance at 3km
  try({   # load abundance at 3km
    abundance<-load_raster(species, product = "abundance",period= "full-year",metric  = "max")
    abundance_residents<-load_raster(species, product = "abundance",period= "seasonal",metric  = "max")
    # crop to bc boundary
    abundance_masked <- mask(crop(abundance, bc_boundary), bc_boundary)
    abundance_masked_residents <- mask(crop(abundance_residents, bc_boundary), bc_boundary)
     #rename teh file
    sp_name <- gsub(" ", "_", species)
    out_file <- file.path(output_dir, paste0(sp_name, "abundance_full-year_max_3km_BC.tif"))
    #Save to file 
    writeRaster(r, out_file, overwrite = TRUE)
    cat("✅ Saved:", out_file, "\n")
  }, silent = TRUE)
}
    
    


for (species in grassland_species) {
  # download seasonal abundance at 3km
  ebirdst_download_status(species, pattern = "abundance_seasonal_mean_3km")
  
  # load breeding season relative abundance
  abd <- load_raster(species, period = "seasonal") |>
    subset("breeding")
  # crop and mask to Montana
  abd_masked <- mask(crop(abd, mt_boundary), mt_boundary)
  # convert to binary, presence-absence
  range_mt[[species]] <- abd_masked > 0
}

ebirdst_download_status( "cinnamon teal",pattern = "full-year_max_3km", download_occurrence = TRUE,dry_run = FALSE,force = TRUE)


raster_cite_fullyear <- load_raster(species = "Cinnamon Teal", product = "abundance",period  = "full-year",metric  = "max")




# ================================
# 6) HELP / DOCS
# ================================

# Open help for ebirdst_download_status (function docs)
?ebirdst_download_status

# Open help for load_raster
?load_raster

# 0.1 ebird Key  -----------------------------------------------------------------
#An access key is required to download eBird Status and Trends data
#1. Get a key by filling out the request form at https://ebird.org/st/request
#2. Save the key using 
set_ebirdst_access_key("f6me7thr51ul") # Valid until March 2026
getwd() 

ebirdst_version()
# Part 1 explore and download the data  --------------------------------------------------




# A species list 

bc_species<-unique(bc_list$common_name)

bc_species <- c("Baird's Sparrow",
                       "Bobolink",
                       "Chestnut-collared Longspur",
                       "Sprague's Pipit",
                       "Upland Sandpiper",
                       "Western Meadowlark")



# Goal: map areas of importance during the breeding season for the set of six
# grassland species in Montana.

# start by producing a map of richness for these species
# produce binary range rasters for each species in Montana
range_mt <- list()
for (species in grassland_species) {
  # download seasonal abundance at 3km
  ebirdst_download_status(species, pattern = "abundance_seasonal_mean_3km")
  
  # load breeding season relative abundance
  abd <- load_raster(species, period = "seasonal") |>
    subset("breeding")
  # crop and mask to Montana
  abd_masked <- mask(crop(abd, mt_boundary), mt_boundary)
  # convert to binary, presence-absence
  range_mt[[species]] <- abd_masked > 0
}
# sum across species to calculate richness
richness <- sum(rast(range_mt), na.rm = TRUE)
# make a simple map
plot(richness, axes = FALSE)

abd[[1]]
abd_masked[[1]]
range_mt[[1]]














###_###_###_###_###_#
# The exercise code 
# Explore available status data products species
unique(ebirdst_runs$common_name)

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


