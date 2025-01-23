
# calculate distance of 0 pixels to nearest 1
# we must do this in tiles because the original raster has super large extent and res

rm(list=ls())
library(terra)
library(dplyr)

# Load your raster
r <- rast('data/Settlement_2000.tif')

# ext(r)  # Extent (xmin, xmax, ymin, ymax)
# dim(r)  # Dimensions (rows, columns, layers)
# res(r)  # Resolution (cell size in x and y directions)
# 
# n_tiles = 100 # for debugging
# buffer_size = 10  # buffer around each tile

split_tiles <- function(r, n_tiles = 100, buffer_size = 10, output_dir = 'results') {
  
  # create directory if it does not exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }
  
  # extract raster extent 
  xmin_val <- xmin(r)
  xmax_val <- xmax(r)
  ymin_val <- ymin(r)
  ymax_val <- ymax(r)
  
  # calculate tile number in X and Y to get 100
  n_x_tiles <- ceiling(sqrt(n_tiles))
  n_y_tiles <- ceiling(n_tiles / n_x_tiles)
  
  # loop to generate tiles
  for (i in 0:(n_x_tiles - 1)) {
    for (j in 0:(n_y_tiles - 1)) {
      # calculate tile borders with a buffer
      # we need a buffer because a 0 pixel might have its nearest 1 in an adyacent tile
      # we calculate with a 10 degree buffer which I believe is enough
      tile_xmin <- xmin_val + (i * (xmax_val - xmin_val) / n_x_tiles) - buffer_size
      tile_xmax <- xmin_val + ((i + 1) * (xmax_val - xmin_val) / n_x_tiles) + buffer_size
      tile_ymin <- ymin_val + (j * (ymax_val - ymin_val) / n_y_tiles) - buffer_size
      tile_ymax <- ymin_val + ((j + 1) * (ymax_val - ymin_val) / n_y_tiles) + buffer_size
      
      # create tile extent based on calculated borders
      tile_ext <- ext(
        max(tile_xmin, xmin_val),
        min(tile_xmax, xmax_val),
        max(tile_ymin, ymin_val),
        min(tile_ymax, ymax_val)
      )
      
      # validate extent
      if (tile_ext[1] < tile_ext[2] && tile_ext[3] < tile_ext[4]) {
        # cut if valid
        tile <- crop(r, tile_ext)
        
        # check tile is not empty
        if (!is.null(tile) && ncell(tile) > 0) {
          # create filename (we cannot store in RAM)
          tile_filename <- paste0(output_dir, '/tile_', i, '_', j, '.asc')
          
          # save file
          writeRaster(tile, tile_filename, format='ascii', overwrite = TRUE)
        }
      }
    }
  }
}

# run
tiles <- split_tiles(r)

# calculate distance in each tile

output_dir <- 'results'
# get all paths to generated tiles
tile_files <- list.files(output_dir, pattern = 'tile_.*\\.asc$', full.names = TRUE)

# loop to load tile, calculate distance and save results
for (tile_file in tile_files) {
  # load tile
  tile <- rast(tile_file)
  
  # calculate distance
  dist_tile <- distance(tile)
  
  # create name
  dist_filename <- gsub('tile_', 'dist_', tile_file)
  
  # save
  writeRaster(dist_tile, dist_filename, format='ascii', overwrite = TRUE)
}

# merge distance tiles extracting minimum distance when pixels overlap (borders)
distance_tiles <- list.files(output_dir, pattern = 'dist_.*\\.asc$', full.names = TRUE) %>%
  lapply(rast)

# combine tiles
distance_raster <- do.call(mosaic, c(distance_tiles, fun = 'min'))

# save combined raster
writeRaster(distance_raster, 'distance_2000.asc',
            format='ascii', overwrite = TRUE)

# plot
plot(distance_raster)
