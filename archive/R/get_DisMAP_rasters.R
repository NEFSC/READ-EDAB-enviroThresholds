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

# Separate directories for raster data vs. plots ------
rasterdir <- here::here('data/DisMAP_Rasters')
dir.create(rasterdir, showWarnings = FALSE, recursive = TRUE)
dir.create(outdir,    showWarnings = FALSE, recursive = TRUE)

datasets <- dm_datasets$dataset_code
layers   <- c(dm_get_dataset_layers("NEUS_SPR"), dm_get_dataset_layers("NEUS_FAL"))
numLayers <- length(layers)

for (spp in 1:numLayers) {
  species <- layers[spp]
  
  years_spr  <- dm_get_dataset_layer_years("NEUS_SPR", species)
  years_fall <- dm_get_dataset_layer_years("NEUS_FAL", species)
  numYears_spr  <- length(years_spr)
  numYears_fall <- length(years_fall)
  
  # Spring --------
  if (numYears_spr > 0) {
    
    sf_tif_spr <- dm_get_raster("NEUS_SPR", species, years_spr[1])
    for (i in seq(2, numYears_spr)) {
      sf_tif_spr <- sf_tif_spr + dm_get_raster("NEUS_SPR", species, years_spr[i])
    }
    
    ## Save raster data -------------------
    raster_file_spr <- file.path(rasterdir, paste0(species, '_Spring_raster.tif'))
    terra::writeRaster(sf_tif_spr, filename = raster_file_spr,
                       overwrite = TRUE, datatype = "FLT4S")
    
    ## Save RGB plot image ---------------
    plot_file_spr <- file.path(outdir, paste0(species, '_Spring.tif'))
    tiff(plot_file_spr, compression = "lzw")
    plot(sf_tif_spr, axes = FALSE, bty = 'o', ann = FALSE,
         main = paste0(species, ' (Spring)'))
    dev.off()
  }
  
  # Fall ------------------
  if (numYears_fall > 0) {
    
    sf_tif_fall <- dm_get_raster("NEUS_FAL", species, years_fall[1])
    for (i in seq(2, numYears_fall)) {
      sf_tif_fall <- sf_tif_fall + dm_get_raster("NEUS_FAL", species, years_fall[i])
    }
    
    ## Save raster data ----------------
    raster_file_fall <- file.path(rasterdir, paste0(species, '_Fall_raster.tif'))
    terra::writeRaster(sf_tif_fall, filename = raster_file_fall,
                       overwrite = TRUE, datatype = "FLT4S")
    
    ## Save RGB plot image ---------------
    plot_file_fall <- file.path(outdir, paste0(species, '_Fall.tif'))
    tiff(plot_file_fall, compression = "lzw")
    plot(sf_tif_fall, bty = 'o', ann = FALSE,
         main = paste0(species, ' (Fall)'))
    dev.off()
  }
  
  
  
  # Combined ----------------------------
  if (numYears_fall > 0 && numYears_spr > 0) {
    
    sf_tif_total <- sf_tif_fall + sf_tif_spr
    
    ## Save raster data ------------
    raster_file_total <- file.path(rasterdir, paste0(species, '_Total_raster.tif'))
    terra::writeRaster(sf_tif_total, filename = raster_file_total,
                       overwrite = TRUE, datatype = "FLT4S")
    
    ## Save RGB plot image ---------------
    plot_file_total <- file.path(outdir, paste0(species, '_Total.tif'))
    tiff(plot_file_total, compression = "lzw")
    plot(sf_tif_total, bty = 'o', ann = FALSE, main = species)
    dev.off()
  }
}
