
###_###_####_###_###_###_###_###_###_###_###_###_###_###_###_###_###_
## BC fund collaboration: future for Bc birds
## ##
## Objective:stacking selected e-bird rasters into single rasters for all Bc species
## ## Specifically calculating  number of species per pixel
## This code does the following:
## 0) Download e-bird 3*3 km rasters fro all BC birds
## 1) It crops to Bc boundary 
## 2) It projects bc boundary to match ebird
## 3) In organizes data in folder for future analyses

## The subsequent scripts do the other steps 
##i) Read tiff files into Rasters 
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

#Install ebirdst from GitHub with:
  
if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes")
  }
# remotes::install_github("ebird/ebirdst")
install.packages("ebirdst", repos = c('https://ebird.r-universe.dev', 'https://cloud.r-project.org'))

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
# Check teh version of the data that we are using 
ebirdst::ebirdst_version() # $status_version_year  1] 2023
# ---- eBird S&T access key ----
# An access key is required to download eBird Status & Trends data.
# 1) Request a key here or look at your key here  https://ebird.org/st/request
# 2) Save the key for this session with set_ebirdst_access_key().
#    

set_ebirdst_access_key("f6me7thr51ul")  # <- replace for local testing only
# Where am I running this from (useful for path debugging)?
getwd() # shoudl be [1] "C:/Users/jmunoz/Local_BirdsCanada/1_JV_science_coordinator_role_local/1_Projects/9_future_for_bc_birds/analyses/future_bc_birds"
# ebirdst package version (useful for reproducibility)

# ================================
# 1) CHOOSING THE DATA DIRECTORY
# ================================

# ebirdst_data_dir() resolves the download directory using:
# 1) EBIRDST_DATA_DIR env var if set, otherwise
# 2) tools::R_user_dir("ebirdst", which = "data")
ebirdst_data_dir()

# If you want to override the default for THIS SESSION ONLY: this will be the folder where data is downloaded 
#Sys.setenv(EBIRDST_DATA_DIR = "C:/Users/jmunoz/Documents/BirdsCanada/1_jv_science_coordinator_role/1_projects/9_future_for_bc_birds/e_bird_data_bc")
Sys.setenv(EBIRDST_DATA_DIR = "C:/Users/jmunoz/Local_BirdsCanada/1_JV_science_coordinator_role_local/1_Projects/9_future_for_bc_birds/analyses/future_bc_birds/data/0_ebird_data_layers")
ebirdst::ebirdst_data_dir()

# ================================
# 2) SPECIES LIST FOR BC
# ================================
# This list is filtered to exclude rare/accidental, introduced, uncertain, and extirpated statuses.
# Assumes the CSV has at least columns: "status" and "common_name".

bc_list_full <- read.csv("data/list/bc_birds_checklist_avibase_reviewed.csv", stringsAsFactors = FALSE) %>%
  as_tibble()
#View( bc_list_full)
dim(bc_list_full)

# here we filter species that for any reason are not a contributor to Bc diversity
bc_list <- bc_list_full%>%
  filter(!conclusion_include %in% c("exclude"))

# Quick peeks
View(bc_list)
names(bc_list)
dim(bc_list)
unique(bc_list$status)
unique(bc_list$conclusion_include)
unique(bc_list$common_name)

# Vector of species names we’ll potentially loop over later # this is the whole list of BC bird species 
bc_species <- unique(bc_list$common_name)

# # ================================
# # 2) SPECIES AT RISK LIST FOR BC
# # ================================
# bc_SARA_list <- read.csv("data/list/bird_species_at_risk_list_02_18_2026.csv", stringsAsFactors = FALSE) %>%
#   as_tibble()
# 
# bc_list_SARA_join <- bc_list %>% 
#   full_join(bc_SARA_list, by = "common_name") %>%   # some species are not joining properly (e.g., owls)
#   filter(!is.na(Family)) %>% 
#   filter(!is.na(schedule)) 
# 
#   View(bc_list_SARA_join)
# ================================
# 3) EXPLORE AVAILABLE RUNS
# ================================
# If not attached, reference explicitly as ebirdst::ebirdst_runs
# Example: explore Cinnamon Teal availability
glimpse(dplyr::filter(ebirdst::ebirdst_runs, common_name == "Cinnamon Teal"))

# This shows all the species that have data in ebird
# View(ebirdst_runs$common_name)

# ================================
# 4) See FOR HOW MANY BC SPECIES WE HAVE EBIRD DATA 
# ================================
View(ebirdst_runs) # the overall list of ebird species 
# list of Bc birds
full_list<-bc_list %>% dplyr::select(sci_name, common_name,Family,Order,why)
#dim(full_list)
match_species_bc_ebird <- bc_list %>%
  left_join(ebirdst_runs, by = "common_name")
#View(match_species_bc_ebird )
#dim(match_species_bc_ebird)

# for which species we dont have it

missing_species_bc_ebird <- match_species_bc_ebird %>%
  filter(is.na(scientific_name)) %>% 
  dplyr::select(common_name, sci_name, is_resident) %>% 
  mutate(ebird_data="no")
dim(missing_species_bc_ebird)
#write.csv(missing_species_bc_ebird, "output_tables/Bc_species_without_ebird_data.csv", row.names = FALSE)

View(missing_species_bc_ebird )
# print(missing_species_bc_ebird$sci_name) # list pf secies for which we dont have the data 
# print(missing_species_bc_ebird$common_name) # list pf secies for which we dont have the data 

# species for which we have the data  #### This is the list of species that I need for later 
list_species_bc_ebird <- bc_list %>%
  inner_join(ebirdst_runs, by = "common_name") %>% 
  dplyr::select(common_name, sci_name, is_resident, species_code) %>% 
  mutate(ebird_data="yes")
dim(list_species_bc_ebird )

# Create a list that documents which species where included and which ones where not, plus which ones have ebird data 
annotated_list <- full_list %>% 
  left_join(list_species_bc_ebird, by = "common_name") %>% 
  left_join(missing_species_bc_ebird, by = "common_name") %>% 
  mutate(ebird_data = coalesce(ebird_data.x, ebird_data.y) ) %>%
  mutate(sci_name = coalesce(sci_name.x, sci_name.y) ) %>% # coalescence gives me the first value that exists
  mutate(is_resident = coalesce(is_resident.x, is_resident.y) ) %>%
  dplyr::select(-ebird_data.x, -ebird_data.y,-sci_name.x,-sci_name.y,-is_resident.x,-is_resident.y) %>% 
  dplyr::select(Order, Family, sci_name, common_name, ebird_data,is_resident,why )
  
dim(annotated_list)
#write.csv(annotated_list, "output_tables/annotated_list_Bc_species_status_2023.csv", row.names = FALSE)

# There  are some species that I need to correct manually 
# House wren corrected manually in teh Bc list to match the ebird list 
# Correct manually Western/Eastern Cattle Egret
# Checked manually those species with the ebirdst_runs

length(list_species_bc_ebird$common_name)
print(list_species_bc_ebird$common_name)
View(list_species_bc_ebird)

print(bc_list$common_name) # the whole list of bc species

# ================================
# 4) DOWNLOAD DATA FOR ONE SPECIES *** Practice
# ================================
# The function below checks what exists (dry_run = TRUE) before downloading.
# pattern = "full-year_max_3km" targets the 3-km full-year max product.
# Set download_occurrence=TRUE if you also want occurrence rasters.
# force = TRUE avoids prompts and overwrites if needed.

# DRY RUN: list what would be downloaded for Cinnamon Teal
ebirdst_download_status( "cinnamon teal",pattern = "full-year_max_3km",download_occurrence = TRUE,dry_run = TRUE)
ebirdst_download_status( "American Dipper",download_occurrence = FALSE,dry_run = TRUE)
ebirdst_download_status( "house finch",download_occurrence = FALSE,dry_run = TRUE)
ebirdst_download_status( "cinnamon teal",download_occurrence = FALSE,dry_run = TRUE)


View(ebirdst_runs)

# ACTUAL DOWNLOAD
ebirdst_download_status( "cinnamon teal",pattern = "full-year_max_3km", download_occurrence = TRUE,dry_run = TRUE,force = TRUE)
ebirdst_download_status( "cinnamon teal",pattern = "full-year_max_3km", download_occurrence = FALSE,dry_run = FALSE,force = TRUE)

# i THINK FOR THE RESIDENT SPECIES THE ABUNDANCE IS UNDER SEASONAL AND NO ANNUAL MAX FOR SOME REASON FOR EXAMPLE THIS TWO SPECIES DO NOT  HAVE FULL-YEAR MAX
ebirdst_download_status( "house finch",pattern = "abundance_seasonal_max_3km", download_occurrence = FALSE,dry_run = FALSE,force = TRUE)
ebirdst_download_status( "American Dipper ",pattern = "_3km", download_occurrence = FALSE,dry_run = TRUE,force = TRUE)

# ================================
# 5) LOAD RASTER PRODUCTS for one species *** Practice
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
load_raster()
raster_cite_fullyear <- load_raster(species = "Cinnamon Teal", product = "abundance",period= "full-year",metric  = "max")
raster_housefinch_fullyear <- load_raster(species = "house finch", product = "abundance", period="seasonal", metric="max")  

# If you’re unsure about parameters or available products/periods/metrics:
 ?load_raster

# ================================
# 6) RUN A LOOP TO DOWNLOAD DATA FOR BC SPECIES 
# ================================
# Although we know there is not data for all BC species — only 351
# we could still run it for the entire list so that when the package adds new data,
# the same code can automatically handle those species.  Just make sure to check which species were skipped and 
# Important make a note for the species that have not data on ebird but are included in teh Bc list

# Important note
# In the list of BC we have a total of 567 species including introduced species, rare and vagrant species
# Once we exclude those species that are not "conservation contributors" then we have 351 species 
# for example  those excluded include introduce, extinct, extirpated, and vagrants species ( this is after recommendation from Andrew, Bc Bird Atlas, Yousif and looking at the number of registers)
# Of those 354 we have data for 351 (see section# 4 See FOR HOW MANY BC SPECIES WE HAVE EBIRD DATA), of those  species 275 are flagged as non-residentare and downloaded, and 35 are flagged as resident. 
# 
# Option 1: Use all species from the BC list
#bc_species <- unique(bc_list$common_name) # this is teh full list 
#print(bc_species)

# Option 2: Use only species that are present in both the BC list and eBird S&T runs [This is the one we are using for the analyses]
# this includes only species for which we have ebird data 
# list_species_bc_ebird <- bc_list %>%
#   inner_join(ebirdst_runs, by = "common_name")

bc_species <- unique(list_species_bc_ebird$common_name)

# Identify resident species (these have a different data product name and that is why I am downloading them independently) 
# interestingly some species that are resident are not in the list of resident = yes such as crows  or bald eagle ## why???
# we take the list of bc species and filter residents 
bc_species_resident_df <- list_species_bc_ebird %>%
  filter(is_resident == "TRUE")

bc_species_resident<-unique(bc_species_resident_df$common_name)
bc_species_resident_code<-unique(bc_species_resident_df$species_code)


# LOOP over the list of all bc species and download data, you will notice that it skipped some species, that do not have "full-year_max" data ( those are  resident species )
# this only downloads data for 316 species 

for (species in bc_species) {
  cat("/n>>> Downloading:", species, "/n")
  # Download full-year abundance data at 3 km
  try({
    ebirdst_download_status(species,pattern = "full-year_max_3km",download_occurrence = FALSE,dry_run = FALSE, force = TRUE)
  }, silent = TRUE)
}

# Resident species do not have a "full-year" abundance dataset.
# For some reason, their data product is named "abundance_seasonal_max_3km".
# Make sure force = FALSE so you don’t overwrite other species

for (species in bc_species_resident) {
  cat("/n>>> Downloading residents:", species, "/n")
  # Download seasonal abundance data for resident species at 3 km
  try({ ebirdst_download_status( species,pattern = "abundance_seasonal_max_3km",download_occurrence = FALSE,dry_run =FALSE,force = FALSE  )
  }, silent = TRUE)
}

# ================================
# 7) READ BC BOUNDARIES and transform to ebird projection
# ================================
#Extract the projection form any raster  that you just downloaded
raster_ebird_projection <- crs( load_raster(species = "Cinnamon Teal", product = "abundance",period= "full-year",metric  = "max"))

# The original boundary is projected as NAD 1983 BC Environment Albers
# It is very important to check teh projection for ebird for every year because it changes between 2022 (UNIQUE PROJECTION) and 2023( WGC84)projectios
# Get the working document 
getwd()
# Read shapefile
bc_boundary <- sf::st_read("data/layers/BC_boundary_layer.shp") # vector file 

crs(bc_boundary)
ext(bc_boundary)      # spatial extent
#res(bc_boundary)      # resolution

# Ensure clip geometry matches each raster's CRS, so here we reprojet the Bc layer into the projection of teh rasters, alternatively i can reproject al the ebird  rasters to bc alberts, I decided that projecting one will save me some code 

bc_boundary_proj <- bc_boundary %>% 
  st_transform(raster_ebird_projection) %>%  # transform coordinate system to match the raster data from ebird
  vect()

# # alternatively you can do it directly 
# bc_boundary_proj <- bc_boundary %>% 
#   st_transform(8857) %>%  # Equal Earth projection (projected CRS, meters)
#   vect()

#vect() # transforms to terra object spat vector 
# check the projection
crs(bc_boundary_proj)

# ================================
# 7) Load AND CROP TO BC BOUNDARIES FOR ALL BC SPECIES DOWNLOADED
# ================================

# try(..., silent = TRUE) means if one species fails (e.g., not available), the loop continues.
# Note that here we work with residents and the non-resident species separated because the names of the files are different 

output_dir_residents<-"data/output_bc_crop/residents"

# Important you need to do resident species first, and they specify that you dont want them to overwrite it 
#abundance_residents <- load_raster("Black Oystercatcher", product = "abundance", period = "seasonal", metric = "max")


for (species in bc_species_resident) {
  try({
    # Load rasters
    abundance_residents <- load_raster(species, product = "abundance", period = "seasonal", metric = "max")
    
    # Crop/mask
    abundance_masked_residents <- mask(crop(abundance_residents, bc_boundary_proj), bc_boundary_proj)
    
    # Filenames
    sp_name <- gsub(" ", "_", species)
    out_file_season <- file.path(output_dir_residents, paste0(sp_name, "_abundance_resident_seasonal_max_3km_BC.tif"))
    
    # Save (important skip if exists OR set overwrite = TRUE)
    if (!file.exists(out_file_season)) {
      writeRaster(abundance_masked_residents, out_file_season, overwrite = FALSE)
    }
    
    cat("✅ Saved (or already existed):", basename(out_file_season), "\n")
  }, silent = TRUE)
}

warnings()
# For the rest of the species, this probably will include some empty rasters 
#output_dir <- "data/output_ebird_bc" # old folder 
output_dir<-"data/output_bc_crop/non-residents"


for (species in bc_species) {
  try({
    # Load raster (full-year abundance at 3 km)
    abundance <- load_raster(species, product = "abundance", period = "full-year", metric = "max")
    
    # Crop/mask to BC boundary
    abundance_masked <- mask(crop(abundance, bc_boundary_proj), bc_boundary_proj)
    
    # Filename
    sp_name <- gsub(" ", "_", species)
    out_file_full <- file.path(output_dir, paste0(sp_name, "_abundance_full-year_max_3km_BC.tif"))
    
    # Save (skip if exists OR set overwrite = TRUE)
    if (!file.exists(out_file_full)) {
      writeRaster(abundance_masked, out_file_full, overwrite = FALSE) # this makes sure we are not overwriting some species
    }
    
    cat("✅ Saved (or already existed):", basename(out_file_full), "\n")
  }, silent = TRUE)
}

# Important make sure that there are not warnings 
# ================================
# 6) RENAME THE RASTERS for the non resident
# ================================
getwd()
# 6a) RENAME THE RASTERS for non-resident species

#rasters_folder <- "data/output_ebird_bc"
rasters_folder<-"data/output_bc_crop/non-residents"

raster_files <- list.files(rasters_folder, pattern = "\\.tif$", full.names = TRUE)

# example of one raster
r1<- rast("data/output_bc_crop/other/Winter_Wren_abundance_full-year_max_3km_BC.tif" )

# the function to rename them including a name inside the raster
rasters_renamed <- lapply(raster_files, function(f) {
  r <- rast(f)
  # Extract the filename (without extension)
  fname <- tools::file_path_sans_ext(basename(f))
  # Split and get first two words
  parts <- strsplit(fname, "_")[[1]]
  newname <- paste(parts[1:min(2, length(parts))], collapse = "_")
  # Rename the layer inside the raster
  names(r) <- newname
  return(r)
})


####
# 6a)Export the raster renamed into a new directory
####
outdir <- "data/output_bc_crop_named"

if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE) # make sure directory exist 

# Loop through your list of rasters
for (r in rasters_renamed) {
  # Use the layer name as filename
  nm <- names(r)
  outfile <- file.path(outdir, paste0(nm, "abundance_full-year_max_3km_BC.tif"))
  writeRaster(r, outfile, overwrite = TRUE)
  cat("✅ Saved:", outfile, "\n")
}

r2<- rast("data/output_rasters_ebird_bc_named/Alder_Flycatcherabundance_full-year_max_3km_BC.tif" )

# ================================
# 7) RENAME THE RASTERS for the residents
# ================================

# 7a) RENAME THE RASTERS for non-resident species
r3<- rast("data/output_bc_crop/residents/American_Dipper_abundance_resident_seasonal_max_3km_BC.tif" )

#rasters_folder <- "data/output_ebird_bc_residents"
rasters_folder_residents <- "data/output_bc_crop/residents"
raster_files_residents <- list.files(rasters_folder_residents, pattern = "\\.tif$", full.names = TRUE)

outdir_res<- "data/output_bc_crop_named_all"

# the function to rename them 
rasters_renamed_residents <- lapply(raster_files_residents, function(f) {
  r <- rast(f)
  # Extract the filename (without extension)
  fname <- tools::file_path_sans_ext(basename(f))
  # Split and get first two words
  parts <- strsplit(fname, "_")[[1]]
  newname <- paste(parts[1:min(2, length(parts))], collapse = "_")
  # Rename the layer inside the raster
  names(r) <- newname
  return(r)
})

class(rasters_renamed_residents ) # list of rasters
####
# 7b)Export the raster renamed into a new directory
####

outdir_res<- "data/output_bc_crop_named_all"

if (!dir.exists(outdir_res)) dir.create(outdir_res, recursive = TRUE) # make sure directory exist 


# Loop through your list of rasters
for (r in rasters_renamed_residents) {
  # Use the layer name as filename
  nm <- names(r)
  outfile <- file.path(outdir_res, paste0(nm, "abundance_resident_max_3km_BC.tif"))
  writeRaster(r, outfile, overwrite = TRUE)
  cat("✅ Saved:", outfile, "\n")
}


#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# --------------- FOR SARA SPECIES ----------------
# #=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# Note to remember species considered resident species in the ebird list do not have the full-year_max layer, instead they have  abundance_seasonal_max_3km.t
# Interestingly non resident species fo ha a abundance_seasonal_max layer, I want to compare those two to see what difference it has 
# #=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=

# ================================
# 1) CHOOSING THE DATA DIRECTORY
# ================================
####### Set the directory to download teh data for SARA species 
Sys.setenv(EBIRDST_DATA_DIR = "C:/Users/jmunoz/Local_BirdsCanada/1_JV_science_coordinator_role_local/1_Projects/9_future_for_bc_birds/analyses/future_bc_birds/data/0_ebird_data_layers/SARA")

# ================================
# 2) THE DATA 
# ================================
bc_SARA_list <- read.csv("data/list/bird_species_at_risk_list_02_18_2026.csv", stringsAsFactors = FALSE) %>%
  as_tibble() %>% 
  rename(status_sara=status) %>% 
  rename(schedule_sara=schedule)

bc_list_SARA <-bc_list %>% 
  full_join(bc_SARA_list, by = "common_name")   # some species are not joining properly (e.g., owls)

bc_list_SARA_filtered<-bc_list %>% 
  full_join(bc_SARA_list, by = "common_name") %>%   # some species are not joining properly (e.g., owls)
  filter(!is.na(Family)) %>% 
  filter(!is.na(schedule_sara)) 

#write.csv(bc_list_SARA_filtered, "output_tables/list_Bc_species_SARA_status_2023.csv", row.names = FALSE)

#View(bc_list_SARA_join)

bc_list_SARA_join_ebird <-list_species_bc_ebird  %>% 
  left_join(bc_SARA_list, by = "common_name") %>%   # some species are not joining properly (e.g., owls)
  filter(!is.na(schedule_sara)) 

#View(bc_list_SARA_join_ebird)

bc_SARA_species<-unique(bc_list_SARA_filtered$common_name)

bc_SARA_species_residents<-bc_list_SARA_join_ebird %>% 
  filter(is_resident==TRUE) %>% 
  distinct(common_name) %>% 
  pull(common_name)

# ================================
# 3) DOWNLOAD RASTER
# ================================
# This runs for those specie staht are non resident 
for (species in bc_SARA_species) {
  cat("/n>>> Downloading:", species, "/n")
  # Download full-year abundance data at 3 km
  try({
    ebirdst_download_status(species,pattern = "full-year_max_3km",download_occurrence = FALSE,dry_run = FALSE, force = TRUE)
  }, silent = TRUE)
}

# There are two species in the SARA that are resident ad therefore I need to download "abundance_seasonal_max_3km" # species are Marbled Murrelet and Western Screech owl

# this complemenst for those species taht are resident 
for (species in bc_SARA_species_residents) {
  cat("/n>>> Downloading only residenst are new:", species, "/n")
  # Download seasonal abundance data for resident species at 3 km
  try({ ebirdst_download_status( species,pattern = "abundance_seasonal_max_3km",download_occurrence = FALSE,dry_run =FALSE,force = FALSE  )
  }, silent = TRUE)
}

# ================================
# 4) READ BC BOUNDARIES and transform to ebird projection
# ================================

# Read shapefile
bc_boundary <- sf::st_read("data/layers/BC_boundary_layer.shp") # vector file 

crs(bc_boundary)
ext(bc_boundary)      # spatial extent
#res(bc_boundary)      # resolution


raster_ebird_projection <- crs( load_raster(species = "Cinnamon Teal", product = "abundance",period= "full-year",metric  = "max"))
# The original boundary is projected as NAD 1983 BC Environment Albers
# It is very important to check teh projection for ebird for every year because it changes between 2022 (UNIQUE PROJECTION) and 2023( WGC84)projectios

# # do it directly to teh projection of interest 
bc_boundary_proj <- bc_boundary %>%
  st_transform(8857) %>%  # Equal Earth projection (projected CRS, meters)
  vect()

# ================================
# 4) CROP THE SARA RASTERS TO BC 
# ================================
# the output directory
output_dir_sara<-"data/output_bc_crop/sara"

for (species in bc_SARA_species) {
  try({
    abundance <- load_raster(species, product = "abundance", period = "full-year", metric = "max")# Load raster (full-year abundance at 3 km)
    abundance_masked <- terra::mask(crop(abundance, bc_boundary_proj), bc_boundary_proj)# Crop/mask to BC boundary
    sp_name <- gsub(" ", "_", species) # Filename
    out_file_full <- file.path(output_dir_sara, paste0(sp_name, "_abundance_full-year_max_3km_BC.tif"))
    # Save (skip if exists OR set overwrite = TRUE)
    if (!file.exists(out_file_full)) {
      writeRaster(abundance_masked, out_file_full, overwrite = FALSE) # this makes sure we are not overwriting some species
    }
    
    cat("✅ Saved (or already existed):", basename(out_file_full), "\n")
  }, silent = TRUE)
}

# Run again for resident species, baceuse of the difefrence in file names 
for (species in bc_SARA_species_residents) {
  try({
    abundance_residents <- load_raster(species, product = "abundance", period = "seasonal", metric = "max")
    abundance_masked_residents <- mask(crop(abundance_residents, bc_boundary_proj), bc_boundary_proj)    # Crop/mask
    sp_name <- gsub(" ", "_", species)# Filenames
    out_file_season <- file.path(output_dir_sara, paste0(sp_name, "_abundance_resident_seasonal_max_3km_BC.tif"))
    # Save (important skip if exists OR set overwrite = TRUE)
    if (!file.exists(out_file_season)) {
      writeRaster(abundance_masked_residents, out_file_season, overwrite = FALSE)
    }
    
    cat("✅ Saved (or already existed):", basename(out_file_season), "\n")
  }, silent = TRUE)
}

# ================================
# 4) RENAME THE FILES  
# ================================
# ================================
# 4a) RENAME THE RASTERS for the non resident
# ================================
# 6a) RENAME THE RASTERS for non-resident species and resident species. I am well aware taht there are two residents species but I just wnat to simplify this

rasters_folder<-"data/output_bc_crop/sara"
raster_files <- list.files(rasters_folder, pattern = "\\.tif$", full.names = TRUE)

# the function to rename them including a name inside the raster
rasters_renamed <- lapply(raster_files, function(f) {
  r <- rast(f)
  # Extract the filename (without extension)
  fname <- tools::file_path_sans_ext(basename(f))
  # Split and get first two words
  parts <- strsplit(fname, "_")[[1]]
  newname <- paste(parts[1:min(2, length(parts))], collapse = "_")
  # Rename the layer inside the raster
  names(r) <- newname
  return(r)
})

####
# 6a)Export the raster renamed into a new directory
####
outdir <- "data/output_bc_crop_named_sara"

if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE) # make sure directory exist 

# Loop through your list of rasters
for (r in rasters_renamed) {
  # Use the layer name as filename
  nm <- names(r)
  outfile <- file.path(outdir, paste0(nm, "abundance_full-year_or_seasonal_max_3km_BC.tif"))
  writeRaster(r, outfile, overwrite = TRUE)
  cat("✅ Saved:", outfile, "\n")
}


# #=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
##_##_###_##_CODE ENDS HERE 
# #=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=

