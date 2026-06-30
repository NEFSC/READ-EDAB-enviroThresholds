library(dismapr)
library(arcgis)
library(RcppSimdJson)
library(terra)
library(raster)

datasets <- dm_datasets$dataset_code
layers <- c(dm_get_dataset_layers("NEUS_SPR"), dm_get_dataset_layers("NEUS_FAL"))

numLayers <- length(layers)

# Can loop over each layer (species) desired in further implementation

years_spr <- dm_get_dataset_layer_years("NEUS_SPR","Gadus morhua")
years_fall <- dm_get_dataset_layer_years("NEUS_FAL","Gadus morhua") # Possibly need to subset certain years
numYears_spr <- length(years_spr)
numYears_fall <- length(years_fall)
sf_tif <- dm_get_raster("NEUS_SPR","Gadus morhua", years_spr[1])
plot(sf_tif)

for (i in 2:numYears_spr) {
  year <- years_spr[i]
  sf_tif <- sf_tif + dm_get_raster("NEUS_SPR","Gadus morhua", year)
  plot(sf_tif)
}

sf_tif <- sf_tif + dm_get_raster("NEUS_FAL","Gadus morhua", years_fall[1])
plot(sf_tif)

for (i in 2:numYears_fall) {
  year <- years_fall[i]
  sf_tif <- sf_tif + dm_get_raster("NEUS_FAL","Gadus morhua", year)
  plot(sf_tif)
}

sf_shape <- as.polygons(sf_tif)
plot(sf_shape)

sf_tif_cubed<-sf_tif^(1/3)
plot(sf_tif_cubed)

sf_shape_cubed <- as.polygons(sf_tif_cubed)
plot(sf_shape_cubed)

sf_tif_cubed_2<-sf_tif_cubed^(1/3)
plot(sf_tif_cubed_2)

sf_shape_cubed_2 <- as.polygons(sf_tif_cubed_2)
plot(sf_shape_cubed_2)

# Probably need to reclassify all values above 0 to a specific number (1) to get a single shape file.
# Need to save shape file
