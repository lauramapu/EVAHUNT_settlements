
library(raster)
library(SpaDES)
library(doParallel)
library(foreach)
library(dplyr)
library(terra)

r <- raster('data/Settlement_2000.tif')
r@data@names <- 'Settlement_2000'

nx=10
ny=10
buffer=c(10,10)
path='results/splitRaster'

splitRaster(r, nx=nx, ny=ny, buffer=buffer, path=path, cl = parallel::makeCluster(5))

# calculate distance in each tile

output_dir <- 'results/dists'

# get all paths to generated tiles
tile_files <- list.files('results/splitRaster', pattern = 'tile', full.names = TRUE)

# loop to load tile, calculate distance and save results
for (tile_file in tile_files) {
  # load tile
  tile <- rast(tile_file)
  
  # calculate distance
  dist_tile <- distance(tile)
  
  # create name
  dist_filename <- paste0(outputdir, '/', gsub('dist_', tile_file))
  
  # save
  writeRaster(dist_tile, dist_filename, overwrite = TRUE)
}

# merge distance tiles extracting minimum distance when pixels overlap (borders)
distance_tiles <- list.files(output_dir, pattern = 'dist_.*\\.tif$', full.names = TRUE)
distance_tiles <- lapply(distance_tiles, rast)

# combine tiles
distance_raster <- do.call(mosaic, c(distance_tiles, fun = 'min'))

# save combined raster
writeRaster(distance_raster, 'results/distance_2000.tif', overwrite = TRUE)

# plot
plot(distance_raster)
