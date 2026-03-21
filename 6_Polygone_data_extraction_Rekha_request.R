#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# 7a) TEST: POLYGON EXTRACTION (Amos Creek)-------Rekha request -------------------
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# Questions for Rehka, how big is Amos creek 11.3 ha =0.11 kM2
# 3km* 3km is 900 hec

# Lets try another stuary taht is this size, I am wondering it it has sometihing to do with it being smaller tan teh pi

# Folder containing BC species rasters
folder_rasters_combos <- "data/output_bc_crop_named_all"
# List all .tif files
files <- list.files( path = folder_rasters_combos,pattern = "\\.tif$", full.names = TRUE)
# Create SpatRaster stack (no calculations yet)
r_stack <- rast(files)

# Sanity check: layer names should correspond to species names
names(r_stack)

# Read BC boundary shapefile
boundary <- sf::st_read("data/layers/amos_creek/Amos.Creek.Shapefile.shp") 

# Check properties
crs(boundary)

# transform to the projection of interest in this case WGC 84
polygon <- boundary %>%
  st_transform(8857) %>%  # Equal Earth projection (projected CRS, meters)
  vect()

# Check CRS
terra::crs(r_stack)
st_crs(polygon)

# Reproject polygon to raster CRS, IF NEEDED
#polygon <- st_transform(polygon, terra::crs(r_stacK))

# Run function
species_in_amoscreek_test <- contributors_at_polygon(
  r_stack,
  polygon)

# Cchek teh list of species that ocurr in Amos Creek 
species_list <- as.data.frame(species_in_amoscreek_test)
print(species_list)

write_csv(species_list, "data/species_list_amos_creek_layers_2023.csv")

################### Rekha request 
# lets look at the numbers that are in teh layer for Amos Creek rom a raster from the dashboard
# they give me an Na fro the Amos Creek 
raster_pbhjv<-rast("C:/Users/jmunoz/Local_BirdsCanada/1_JV_science_coordinator_role_local/1_Projects/9_future_for_bc_birds/analyses/future_bc_birds/data/PacificBirds_all-bird-groups_all-species-priority_annual_richness_annual.tif")
crs(raster_pbhjv)

boundary <- sf::st_read("data/layers/amos_creek/Amos.Creek.Shapefile.shp") 
boundary_proj <- boundary %>%
  st_transform(crs(raster_pbhjv)) # Equal Earth projection (projected CRS, meters)

abundance_masked <- terra::mask(crop(raster_pbhjv, boundary_proj ), boundary_proj)# Crop/mask to BC boundary
# test with loons 
raster_pbhjv<-rast("C:/Users/jmunoz/Local_BirdsCanada/1_JV_science_coordinator_role_local/1_Projects/9_future_for_bc_birds/analyses/future_bc_birds/data/pacloo_abundance_full-year_max_3km_2023.tif")
crs(raster_pbhjv)

boundary <- sf::st_read("data/layers/amos_creek/Amos.Creek.Shapefile.shp") 
boundary_proj <- boundary %>%
  st_transform(crs(raster_pbhjv)) # Equal Earth projection (projected CRS, meters)

abundance_masked <- terra::mask(crop(raster_pbhjv, boundary_proj ), boundary_proj)# Crop/mask to BC boundary

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# Now looking at individual rasters that I used for the dashboard from 2023

# test with loons 
raster_pbhjv<-rast("C:/Users/jmunoz/Local_BirdsCanada/1_JV_science_coordinator_role_local/1_Projects/9_future_for_bc_birds/analyses/future_bc_birds/data/PacificBirds_Pacific-Loon_mean_seasonal_abundance_2023_breeding.tif")
crs(raster_pbhjv)

boundary <- sf::st_read("data/layers/amos_creek/Amos.Creek.Shapefile.shp") 
boundary_proj <- boundary %>%
  st_transform(crs(raster_pbhjv)) # Equal Earth projection (projected CRS, meters)

abundance_masked <- terra::mask(crop(raster_pbhjv, boundary_proj ), boundary_proj)# Crop/mask to BC boundary # it shows Na! 

plot(raster_pbhjv)
plot(abundance_masked)

# test with loons 
raster_pbhjv<-rast("C:/Users/jmunoz/Local_BirdsCanada/1_JV_science_coordinator_role_local/1_Projects/9_future_for_bc_birds/analyses/future_bc_birds/data/PacificBirds_Pacific-Loon_mean_seasonal_abundance_2023_nonbreeding.tif")
crs(raster_pbhjv)

boundary <- sf::st_read("data/layers/amos_creek/Amos.Creek.Shapefile.shp") 
boundary_proj <- boundary %>%
  st_transform(crs(raster_pbhjv)) # Equal Earth projection (projected CRS, meters)

abundance_masked <- terra::mask(crop(raster_pbhjv, boundary_proj ), boundary_proj)# Crop/mask to BC boundary # it shows Na! 

########### looking at individual rasters for specific species

# this is if I download a raster for that species with max annual abundance from 2023

raster_pbhjv<-rast("C:/Users/jmunoz/Local_BirdsCanada/1_JV_science_coordinator_role_local/1_Projects/9_future_for_bc_birds/analyses/future_bc_birds/data/pacloo_abundance_full-year_max_3km_2023.tif")
crs(raster_pbhjv)

boundary <- sf::st_read("data/layers/amos_creek/Amos.Creek.Shapefile.shp") 
boundary_proj <- boundary %>%
  st_transform(crs(raster_pbhjv)) # Equal Earth projection (projected CRS, meters)

abundance_masked <- terra::mask(crop(raster_pbhjv, boundary_proj ), boundary_proj)# Crop/mask to BC boundary

plot(abundance_masked), it actually has a value on it 
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# End of script
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#