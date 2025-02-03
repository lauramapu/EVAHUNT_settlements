
# since previous methods resulted in bad allocation errors in different forms
# we're calling gdal_proximity.py to perform the distance calculations
# because gdal somehow works out-of-core and it's super optimize for giant rasters

# first thing gdal will do is to calculate final file size and tell you if you have space

# WARNING: super heavy processes, don't run if it's not necessary

library(raster)

# set python and libraries path
Sys.setenv(PYTHONHOME = "C:/Program Files/QGIS 3.28.13/apps/Python39")
Sys.setenv(PYTHONPATH = "C:/Program Files/QGIS 3.28.13/apps/Python39/Lib")

files <- list.files('data', pattern='Settlements', full.names=T)

for (i in seq_along(files)) {
  
  raw <- files[i]
  downsampled <- file.path("results", gsub(".tif$", "_ds.tif", basename(raw)))
  distances <- file.path("results", gsub(".tif$", "_dists.tif", basename(raw)))
  final <- file.path("results", gsub(".tif$", "_final.tif", basename(raw)))
  
  # 1. downsample raster to 1x1km
  cmd_downsample <- paste(
    shQuote("C:/Program Files/QGIS 3.28.13/bin/gdalwarp.exe"),
    "-r max",  # resample by max value to keep all ones
    "-tr 0.00833333 0.00833333",  # target res (1x1km in degrees)
    "--config GDAL_NUM_THREADS ALL_CPUS", # use all cpus
    "--config GDAL_CACHEMAX 49152", # max cache 48gb
    "-co TILED=YES",  # store in tiles
    "-co COMPRESS=DEFLATE",  # efficient compression
    "-co BLOCKXSIZE=512",  # Tamaño de bloque horizontal
    "-co BLOCKYSIZE=512",  # Tamaño de bloque vertical
    shQuote('data/Settlement_2000.tif'),  # Raster de entrada
    shQuote('results/Settlement_2000_ds.tif')  # Raster de salida
  )
  system(cmd_downsample)

  # 2. calculate distances 
  cmd_proximity <- paste(
    shQuote("C:/Program Files/QGIS 3.28.13/bin/python.exe"),
    shQuote("C:/Program Files/QGIS 3.28.13/apps/Python39/Scripts/gdal_proximity.py"),
    shQuote(original),
    shQuote(processed),
    "-distunits GEO",
    "-use_input_nodata YES",
    "-values 1",
    "-nodata -9999",
    "--config GDAL_NUM_THREADS ALL_CPUS", # use all cpus
    "--config", "GDAL_CACHEMAX", "49152"
  )
  result_proximity <- system(cmd_proximity)
  
  # 3. restore NA values
  cmd_fix <- paste(
    shQuote("C:/Program Files/QGIS 3.28.13/bin/python.exe"),
    shQuote("C:/Program Files/QGIS 3.28.13/apps/Python39/Scripts/gdal_calc.py"),
    "-A", shQuote(processed),  
    "-B", shQuote(original),   
    "--outfile", shQuote(final),  
    # formula: if a pixel in 'original' == 0 or 1, then extract value in 'processed' (dist)
    # if 'original' != 0 or 1, then extract value in 'original' (NA)
    "--calc", shQuote("(B == 0) * A + (B == 1) * A + ((B != 0) * (B != 1)) * B"),
    # "--NoDataValue=-9999",
    "--co", "TILED=YES",
    "--co", "BLOCKXSIZE=512",
    "--co", "BLOCKYSIZE=512",
    "--co", "COMPRESS=LZW",
    "--co", "BIGTIFF=YES",  # needed because this function stops when reaching 4gb
    "--config GDAL_NUM_THREADS ALL_CPUS", # use all cpus
    "--config", "GDAL_CACHEMAX", "49152"
  )
  system(cmd_fix)
  
  # 4. remove intermediate files
  file.remove(downsampled, distances)
  cat(sprintf("Finished processing %s\n", raw))
}
