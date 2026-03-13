# Creating .gif files from .png files of thermal suitability
# to better visualize change over time

# load packages ---------
library(magick)
library(stringr)
library(dplyr)

# set directories ----------
input_dir  <- "images/suitable_habitat"
output_dir <- "images/animations"

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# list files -----------------
png_files <- list.files(
  input_dir,
  pattern = "\\.png$",
  full.names = TRUE
)

file_info <- tibble(
  file = png_files,
  filename = basename(png_files),
  species = str_extract(filename, "^[^_]+(?:_[^_]+)*(?=_\\d{4})"),
  year = as.numeric(str_extract(filename, "\\d{4}"))
)

# loop over species to create gif -------------
for (sp in unique(file_info$species)) {
  
  message("Creating animation for ", sp)
  
  sp_files <- file_info %>%
    filter(species == sp) %>%
    arrange(year)
  
  img_sequence <- NULL
  
  for (i in seq_along(sp_files$file)) {
    
    frame <- image_read(sp_files$file[i])
    
    # Downscale to reduce memory
    frame <- image_scale(frame, "800x")
    
    if (is.null(img_sequence)) {
      # First frame initializes object
      img_sequence <- frame
    } else {
      img_sequence <- c(img_sequence, frame)
    }
    
    rm(frame)
    gc()
  }
  
  # Animate
  img_animated <- image_animate(
    img_sequence,
    fps = 1
  )
  
  output_file <- file.path(
    output_dir,
    paste0(sp, "_animation.gif")
  )
  
  image_write(img_animated, path = output_file)
  
  rm(img_sequence, img_animated)
  gc()
}
