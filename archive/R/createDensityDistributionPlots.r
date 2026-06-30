library(dismapr)
library(arcgis)
library(RcppSimdJson)
library(terra)
library(raster)
library(sf)
library(concaveman)
library(dplyr)
library(here)

outdir <- here::here('data/DisMAP_Figures_test')
datasets <- dm_datasets$dataset_code


layers <- c(dm_get_dataset_layers("NEUS_SPR"), dm_get_dataset_layers("NEUS_FAL"))
numLayers <- length(layers)

# Can loop over each layer (species) desired in further implementation
for (spp in 1:numLayers) {
  species <- layers[spp]

  years_spr <- dm_get_dataset_layer_years("NEUS_SPR",species)
  years_fall <- dm_get_dataset_layer_years("NEUS_FAL",species) # Possibly need to subset certain years
  numYears_spr <- length(years_spr)
  numYears_fall <- length(years_fall)

  if (numYears_spr > 0) {
    filename <- paste0(outdir,species,'_Spring.tif')
    tiff(filename, compression = "lzw")
    sf_tif_spr <- dm_get_raster("NEUS_SPR",species, years_spr[1])
    plot(sf_tif_spr, axes=FALSE, bty='o', ann=FALSE, main = paste0(species, ' (Spring)'))

    for (i in 2:numYears_spr) {
      year <- years_spr[i]
      sf_tif_spr <- sf_tif_spr + dm_get_raster("NEUS_SPR",species, year)
      plot(sf_tif_spr, bty = 'o', ann=FALSE, main = paste0(species, ' (Spring)'))
    }
    dev.off()
  }

  if (numYears_fall > 0) {
    filename <- paste0(outdir,species,'_Fall.tif')
    tiff(filename, compression = "lzw")
    sf_tif_fall <- dm_get_raster("NEUS_FAL",species, years_fall[1])
    plot(sf_tif_fall, bty = 'o', ann=FALSE, main = paste0(species, ' (Fall)'))

    for (i in 2:numYears_fall) {
      year <- years_fall[i]
      sf_tif_fall <- sf_tif_fall + dm_get_raster("NEUS_FAL",species, year)
      plot(sf_tif_fall, bty = 'o', ann=FALSE, main = paste0(species, ' (Fall)'))
    }
    dev.off()
  }
  if ((numYears_fall > 0) && (numYears_spr > 0)) {
    filename <- paste0(outdir,species,'_Total.tif')
    tiff(filename, compression = "lzw")
    sf_tif_total <- sf_tif_fall + sf_tif_spr
    plot(sf_tif_total, bty = 'o', ann=FALSE, main = species)
    dev.off()
  }
}

