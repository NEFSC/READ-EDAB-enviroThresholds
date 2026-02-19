# Creating .gif files from .png files of thermal suitability
# to better visualize change over time

# load packages ----------
library(magick)
library(stringr)
library(dplyr)

# set directories ----------
input_dir <- "images/suitable_habitat"
output_dir <- "images/animations"

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Get all PNG files ----------
png_files <- list.files(
  input_dir,
  pattern = "\\.png$",
  full.names = TRUE
)

# Extract species names ------------
file_info <- tibble(
  file = png_files,
  filename = basename(png_files),
  species = str_extract(filename, "^[^_]+(?:_[^_]+)*(?=_\\d{4})"),
  year = as.numeric(str_extract(filename, "\\d{4}"))
)

# Loop over species -------
for (sp in unique(file_info$species)) {
  
  message("Creating animation for ", sp)
  
  sp_files <- file_info %>%
    filter(species == sp) %>%
    arrange(year)
  
  ## Read images in chronological order -----------
  img <- image_read(sp_files$file)
  
  ## Set frame rate --------
  img_animated <- image_animate(
    img,
    fps = 2   # 2 frames per second (0.5 sec per year)
  )
  
  ## Save GIF --------
  output_file <- file.path(
    output_dir,
    paste0(sp, "_animation.gif")
  )
  
  image_write(
    img_animated,
    path = output_file
  )
}
