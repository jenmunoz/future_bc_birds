
library(raster)
library ( terra)




folder_path <- "data/output_ebird_bc_all_species"

# List all raster files (e.g., .tif)
raster_files <- list.files(folder_path, pattern = "\\.tif$", full.names = TRUE)

# Initialize list to store results
range_mt <- list()

# Loop through each raster
for (f in raster_files) {
  species <- tools::file_path_sans_ext(basename(f))  # use filename as species name
  abd_masked <- rast(f)
  
  # Convert raster values to 0/1 (TRUE/FALSE -> numeric)
  range_mt[[species]] <- abd_masked > 0
  range_mt[[species]] <- as.numeric(range_mt[[species]])
  
}

richness <- sum(rast(range_mt), na.rm = TRUE)
plot(richness, axes = FALSE)

alder<-range_mt[[1]]
b<-range_mt[[12]]
c<-range_mt[[23]]
d<-range_mt[[43]]


alderraw<-raster(raster_files[[1]])
################


#
# make a simple map
plot(richness, axes = FALSE)
plot(bc_boundary_proj, col = "grey", axes = FALSE, add = TRUE)
plot(richness, axes = FALSE, legend = FALSE, add = TRUE)

plot(alder)
alder_proj <- project(alder, "ESRI:102003", method = "near")
plot(alder_proj)

b_proj <- project(b, "ESRI:102003", method = "near")
plot(b_proj)

c_proj <- project(b, "ESRI:102003", method = "near")
plot(c_proj)

d_proj <- project(b, "ESRI:102003", method = "near")
plot(d_proj)



plot(alderraw)
alderraw_proj <- project(alderraw, "ESRI:102003", method = "near")
plot(alderraw_proj)



# make a slightly nicer map
# reproject
species_proj<- trim(project(richness, "ESRI:102003", method = "near"))
bc_boundary_proj2 <- project(bc_boundary_proj, "ESRI:102003", method = "near")
# basemap
par(mar = c(0, 0, 0, 0))
plot(bc_boundary_proj2, col = "grey", axes = FALSE,
     main = "Birds Hotspots in BC")
# add importance raster
plot(species_proj, legend = FALSE, add = TRUE)
# add legend

fields::image.plot(
  zlim   = c(0, 50),
  legend.only = TRUE,
  col    = viridis::viridis(100),
  breaks = seq(0, 170, length.out = 101),
  smallplot = c(0.15, 0.85, 0.12, 0.15),
  horizontal = TRUE,
  axis.args = list(
    at     = c(0, 25, 50,75, 100,125, 150, 175, 200),                 # ticks at 0, 25, 50 now
    labels = c("0", "25", "50", "75", "100","125", "150", "175", "200"),
    fg = "black", col.axis = "black",
    cex.axis = 0.75, lwd.ticks = 0.5,
    padj = -1.5
  ),
  legend.args = list(
    text = "Number of species",
    side = 3, col = "black",
    cex = 1, line = 0
  )
)

#7) READ BC BOUNDARIES and transform to ebird projection
# ================================

# The original boundary is projected as NAD 1983 BC Environment Albers
bc_boundary<-raster

# Read shapefile
getwd()
bc_boundary <- sf::st_read("data/layers/BC_boundary_layer.shp") # vector file 

crs(bc_boundary)
ext(bc_boundary)      # spatial extent
res(bc_boundary)      # resolution

# Ensure clip geometry matches each raster's CRS

bc_boundary_proj <- bc_boundary|>
  st_transform("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs") |> # transform coordinate system to match the raster data
  vect()

#vect() # transforms to terra object spat vector 
# check the projection
crs(bc_boundary_proj)
