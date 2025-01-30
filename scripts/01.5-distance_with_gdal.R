
# since previous methods resulted in bad allocation errors in different forms
# we're calling gdal_proximity.py to perform the distance calculations
# because gdal somehow works out-of-core and it's super optimize for giant rasters

# first thing gdal will do is to calculate final file size and tell you if you have space

# WARNING: super heavy processes, don't run if it's not necessary

library(raster)
library(SpaDES)
library(doParallel)

r <- raster('data/Settlement_2000.tif')
r@data@names <- 'Settlement_2000'

nx=3
ny=2
buffer=c(10,10)
path='results/splitRaster/tiles_2x2'

splitRaster(r, nx=nx, ny=ny, buffer=buffer, path=path, cl = parallel::makeCluster(6))

# set python and libraries path
Sys.setenv(PYTHONHOME = "C:/Program Files/QGIS 3.28.13/apps/Python39")
Sys.setenv(PYTHONPATH = "C:/Program Files/QGIS 3.28.13/apps/Python39/Lib")

# list tiles
tiles <- list.files('results/splitRaster/tiles_3x2', pattern = '^Settlement_2000_tile.*\\.tif$', full.names = TRUE)

# loop to process each tile
for (i in seq_along(tiles)[2:6]) {
  # paths for the files involved in the process
  original <- tiles[i]
  processed <- sub("Settlement_2000_tile", "dists_2000_tile", original)  # intermediate distance raster
  final <- sub("Settlement_2000_tile", "dists_2000_tile_final", original)  # final raster
  
  cat(sprintf("Calculating distance of tile %d / %d...\n", i, length(tiles)))
  cat(sprintf("Original file: %s\n", original))
  
  # 1. calculate distances with gdal_proximity.py
  cmd_proximity <- paste(
    shQuote("C:/Program Files/QGIS 3.28.13/bin/python.exe"),
    shQuote("C:/Program Files/QGIS 3.28.13/apps/Python39/Scripts/gdal_proximity.py"),
    shQuote(original),
    shQuote(processed),
    "-distunits GEO",
    "-use_input_nodata YES",
    "-values 1",
    "-nodata -9999",
    "--config", "GDAL_NUM_THREADS", parallel::detectCores() - 1,
    "--config", "GDAL_CACHEMAX", "49152"
  )
  
  # run command
  result_proximity <- system(cmd_proximity)
  
  # verify if process is successful
  if (result_proximity == 0) {
    cat("Distance calculation completed successfully.\n")
  } else {
    cat(sprintf("Error in distance calculation %d. Command:\n%s\n", i, cmd_proximity))
    break
  }
  
  cat(sprintf("Restoring NAs in tile %d / %d...\n", i, length(tiles)))
  cat(sprintf("Original file: %s\n", original))
  
  # 2. restore NA values with gdal_calc.py
  cmd_fix_values <- paste(
    shQuote("C:/Program Files/QGIS 3.28.13/bin/python.exe"),
    shQuote("C:/Program Files/QGIS 3.28.13/apps/Python39/Scripts/gdal_calc.py"),
    "-A", shQuote(processed),  
    "-B", shQuote(original),   
    "--outfile", shQuote(final),  
    # formula: if a pixel in 'original' == 0 or 1, then extract value in 'processed' (dist)
    # if 'original' != 0 or 1, then extract value in 'original' (NA)
    "--calc", shQuote("(B == 0) * A + (B == 1) * A + ((B != 0) * (B != 1)) * B"),
    "--NoDataValue=-9999",
    "--co", "TILED=YES",
    "--co", "BLOCKXSIZE=512",
    "--co", "BLOCKYSIZE=512",
    "--co", "COMPRESS=LZW",
    "--co", "BIGTIFF=YES",  # needed because this function stops when reaching 4gb
    "--config", "GDAL_NUM_THREADS", parallel::detectCores() - 1,
    "--config", "GDAL_CACHEMAX", "49152"
  )
  
  result_restore <- system(cmd_fix_values)

  # verify if restoring was successful
  if (result_restore == 0) {
    cat("NA restore completed successfully.\n")
  } else {
    cat(sprintf("Error in NA restoring %d. Command:\n%s\n", i, cmd_restore_na))
    break
  }

  # 3. Erase intermediate files
  file.remove(processed)
  cat("Intermediate files removed.\n")
}

cat("Process completed.\n")

# now we need to get all the generated files
distance_tiles <- list.files('results/splitRaster/tiles_3x2',
                             pattern = 'final', full.names = TRUE)

output_raster <- "results/distance_2000.tif"

# and merge them with gdalwarp, getting minimum value when pixels overlap (buffer)
cmd <- paste(
  shQuote("C:/Program Files/QGIS 3.28.13/bin/python.exe"),  # Ruta a Python (GDAL)
  shQuote("C:/Program Files/QGIS 3.28.13/apps/Python39/Scripts/gdalwarp.py"),  # Ruta a gdalwarp.py
  paste(shQuote(distance_tiles), collapse = " "),  # Lista de tiles como entrada
  shQuote(output_raster),  # Raster de salida
  "-r min",  # Resolver solapamientos usando el valor mínimo
  "--config GDAL_NUM_THREADS ALL_CPUS",  # Usar todos los núcleos disponibles
  "--config GDAL_CACHEMAX 49152",  # Usar 48GB de caché (ajustable según tu sistema)
  "-co TILED=YES",  # Configurar el raster de salida como tiled
  "-co BLOCKXSIZE=512",  # Tamaño de bloque X
  "-co BLOCKYSIZE=512",  # Tamaño de bloque Y
  "-co COMPRESS=LZW"  # Comprimir usando LZW
)

# Ejecutar el comando en el sistema
system(cmd)

# and that's all
plot(rast('results/distance_2000.tif'))