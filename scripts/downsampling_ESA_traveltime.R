
# downsampling traveltime, ESA and settlement files
# methosd: bilinear, ngb and max respectively

rm(list=ls())

library(purrr)  
library(glue)  

# Set Python and libraries path
set_env_vars <- function() {
  Sys.setenv(PYTHONHOME = "C:/Program Files/QGIS 3.28.13/apps/Python39")
  Sys.setenv(PYTHONPATH = "C:/Program Files/QGIS 3.28.13/apps/Python39/Lib")
}

# Generate output file paths
generate_paths <- function(raw) {
  base <- gsub(".tif$", "", basename(raw))
  list(
    downsampled = file.path("results", paste0(base, "_ds.tif"))
  )
}

# Execute system command
run_command <- function(cmd) {
  message("Executing: ", cmd)
  result <- tryCatch(system(cmd, intern = TRUE), error = function(e) e)
  if (inherits(result, "error")) message("Error: ", conditionMessage(result))
}

# Downsample raster to 1x1km

downsample_raster_max <- function(input, output) {
  cmd <- glue(
    '"C:/Program Files/QGIS 3.28.13/bin/gdalwarp.exe" ',
    '-r max ', # max because presence of a single settlement in a resampled pixel counts as presence
    '-tr 0.00833333300000000006 0.00833333300000000006 ',  
    '--config GDAL_NUM_THREADS ALL_CPUS ',  
    '--config GDAL_CACHEMAX 49152 ',  
    '-co TILED=YES ',  
    '-co COMPRESS=DEFLATE ',  
    '-co BLOCKXSIZE=512 ',  
    '-co BLOCKYSIZE=512 ',  
    '"{input}" "{output}"'
  )
  run_command(cmd)
}

downsample_raster_ngb <- function(input, reference, output) {
  
  # read reference raster (settlements)
  ref_rast <- rast(reference)
  extent_vals <- ext(ref_rast)  # extent (xmin, xmax, ymin, ymax)
  res_vals <- res(ref_rast)  # res (res_x, res_y)
  srs_proj <- crs(ref_rast, proj=TRUE)  # proj in WKT format (needed for gdal)
  
  # construct gdal command
  cmd <- glue(
    '"C:/Program Files/QGIS 3.28.13/bin/gdalwarp.exe" ',
    '-r near ',  # nearest neighbour
    '-te {extent_vals[1]} {extent_vals[3]} {extent_vals[2]} {extent_vals[4]} ',  # coords xmin, ymin, xmax, ymax
    '-tr {res_vals[1]} {res_vals[2]} ',  # pixel size (res_x, res_y)
    '-t_srs "{srs_proj}" ',  # proj
    '--config GDAL_NUM_THREADS ALL_CPUS ',  
    '--config GDAL_CACHEMAX 49152 ',  
    '-co TILED=YES ',  
    '-co COMPRESS=DEFLATE ',  
    '-co BLOCKXSIZE=512 ',  
    '-co BLOCKYSIZE=512 ',  
    '"{input}" "{output}"'
  )
  
  run_command(cmd)
  
  print(glue("Downsampling completed: {output}"))
}

downsample_raster_bil <- function(input, reference, output) {
  
  # read reference raster (settlements)
  ref_rast <- rast(reference)
  extent_vals <- ext(ref_rast)  # extent (xmin, xmax, ymin, ymax)
  res_vals <- res(ref_rast)  # res (res_x, res_y)
  srs_proj <- crs(ref_rast, proj=TRUE)  # proj in WKT format (needed for gdal)
  
  # construct gdal command
  cmd <- glue(
    '"C:/Program Files/QGIS 3.28.13/bin/gdalwarp.exe" ',
    '-r bilinear ',  
    '-te {extent_vals[1]} {extent_vals[3]} {extent_vals[2]} {extent_vals[4]} ',  # coords xmin, ymin, xmax, ymax
    '-tr {res_vals[1]} {res_vals[2]} ',  # pixel size (res_x, res_y)
    '-t_srs "{srs_proj}" ',  # proj
    '--config GDAL_NUM_THREADS ALL_CPUS ',  
    '--config GDAL_CACHEMAX 49152 ',  
    '-co TILED=YES ',  
    '-co COMPRESS=DEFLATE ',  
    '-co BLOCKXSIZE=512 ',  
    '-co BLOCKYSIZE=512 ',  
    '"{input}" "{output}"'
  )
  
  run_command(cmd)
  
  print(glue("Downsampling completed: {output}"))
}

process_raster_max <- function(input, output) {
  message("Processing: ", input)
  paths <- generate_paths(input)
  downsample_raster_max(input, paths$downsampled)
}
process_raster_ngb <- function(input, output) {
  message("Processing: ", input)
  paths <- generate_paths(input)
  downsample_raster_ngb(input, 'results/Settlement_1990_ds.tif', paths$downsampled)
}
process_raster_bil <- function(input, output) {
  message("Processing: ", input)
  paths <- generate_paths(input)
  downsample_raster_bil(input, 'results/Settlement_1990_ds.tif', paths$downsampled)
}

set_env_vars()
# settlements
files <- list.files('data', 'Settlement_', full.names=T)[2:3]
walk(files, process_raster_max)
# esa land cover
files <- list.files('data', pattern='ESACCI', full.names=T)
walk(files, process_raster_ngb)
# traveltime
files <- list.files('data', '\\.tif$', full.names=T, recursive=T)[1:2]
walk(files, process_raster_bil)


################################################
########## extracting urban and traveltime from each settlement layer
###################################################################################
#################################################################################################################

# we only have traveltime for 2000 and 2015 so we're using 2000 to filter settlements from 1990
# we only have ESACCI from 1992 so we're using that year to filter settlements from 1990
# for 2000 and 2015 everything is just fine

library(raster)
library(terra)

# load all needed files
files <- list.files('results', '\\.tif$', full.names=T)

acc_00 <- raster(files[1])
acc_15 <- raster(files[2])
esa_92 <- raster(files[3])
esa_00 <- raster(files[4])
esa_15 <- raster(files[5])
set_90 <- raster(files[7])
set_00 <- raster(files[10])
set_15 <- raster(files[11])

# binarize traveltime (1h and 5mins)
acc_00_1h <- calc(acc_00, function(x) {ifelse(x<=60, 1, 0)})
acc_00_5m <- calc(acc_00, function(x) {ifelse(x<=5, 1, 0)})
acc_15_1h <- calc(acc_15, function(x) {ifelse(x<=60, 1, 0)})
acc_15_5m <- calc(acc_15, function(x) {ifelse(x<=5, 1, 0)})

# binarize esa land uses (190=urban)
urban_92 <- calc(esa_92, function(x) {ifelse(x==190, 1, 0)})
urban_00 <- calc(esa_00, function(x) {ifelse(x==190, 1, 0)})
urban_15 <- calc(esa_15, function(x) {ifelse(x==190, 1, 0)})

# extract esa land uses from settlement layers
set_90_esa <- set_90 - urban_92
set_00_esa <- set_00 - urban_00
set_15_esa <- set_15 - urban_15

# extract traveltime
set_90_1h <- set_90_esa - acc_00_1h
set_90_5m <- set_90_esa - acc_00_5m
set_00_1h <- set_00_esa - acc_00_1h
set_00_5m <- set_00_esa - acc_00_5m
set_15_1h <- set_15_esa - acc_15_1h
set_15_5m <- set_15_esa - acc_15_5m

set_90_1h <- calc(set_90_1h, function(x) {ifelse(x==1, 1, 0)})
set_90_5m <- calc(set_90_5m, function(x) {ifelse(x==1, 1, 0)})
set_00_1h <- calc(set_00_1h, function(x) {ifelse(x==1, 1, 0)})
set_00_5m <- calc(set_00_5m, function(x) {ifelse(x==1, 1, 0)})
set_15_1h <- calc(set_15_1h, function(x) {ifelse(x==1, 1, 0)})
set_15_5m <- calc(set_15_5m, function(x) {ifelse(x==1, 1, 0)})

writeRaster(set_90_1h, 'results/settlements_1990_1h.tif', overwrite=T)
writeRaster(set_90_5m, 'results/settlements_1990_5m.tif', overwrite=T)
writeRaster(set_00_1h, 'results/settlements_2000_1h.tif', overwrite=T)
writeRaster(set_00_5m, 'results/settlements_2000_5m.tif', overwrite=T)
writeRaster(set_15_1h, 'results/settlements_2015_1h.tif', overwrite=T)
writeRaster(set_15_5m, 'results/settlements_2015_5m.tif', overwrite=T)

######################################
# calculating distances and restoring original NAs with the new filtered files
#################################################################################################
######################################################################################################################

rm(list=ls())

library(purrr)  
library(glue)  

# Set Python and libraries path
set_env_vars <- function() {
  Sys.setenv(PYTHONHOME = "C:/Program Files/QGIS 3.28.13/apps/Python39")
  Sys.setenv(PYTHONPATH = "C:/Program Files/QGIS 3.28.13/apps/Python39/Lib")
}

# Generate output file paths
generate_paths <- function(raw) {
  base <- gsub(".tif$", "", basename(raw))
  list(
    distances   = file.path("results", paste0(base, "_dists.tif")),
    final       = file.path("results", paste0(base, "_dists_final.tif"))
  )
}

# Execute system command
run_command <- function(cmd) {
  message("Executing: ", cmd)
  result <- tryCatch(system(cmd, intern = TRUE), error = function(e) e)
  if (inherits(result, "error")) message("Error: ", conditionMessage(result))
}

# Calculate distances 
calculate_distances <- function(input, output) {
  cmd <- glue(
    '"C:/Program Files/QGIS 3.28.13/bin/python.exe" ',
    '"C:/Program Files/QGIS 3.28.13/apps/Python39/Scripts/gdal_proximity.py" ',
    '"{input}" "{output}" ',
    '-distunits GEO ',  
    '-use_input_nodata YES ',  
    '-values 1 ',  
    '--config GDAL_NUM_THREADS ALL_CPUS ',  
    '--config GDAL_CACHEMAX 49152 '
  )
  run_command(cmd)
}

# Restore NA values
restore_na_values <- function(processed, original, final) {
  cmd <- glue(
    '"C:/Program Files/QGIS 3.28.13/bin/python.exe" ',
    '"C:/Program Files/QGIS 3.28.13/apps/Python39/Scripts/gdal_calc.py" ',
    '-A "{processed}" ',  
    '-B "{original}" ',  
    '--outfile "{final}" ',  
    '--calc "(B == 0) * A + (B == 1) * A + ((B != 0) * (B != 1)) * B" ',  
    '--co TILED=YES ',  
    '--co BLOCKXSIZE=512 ',  
    '--co BLOCKYSIZE=512 ',  
    '--co COMPRESS=DEFLATE ',  
    # if more compression is needed you can add --co PREDICTOR=2
    '--co BIGTIFF=YES ',  
    '--config GDAL_NUM_THREADS ALL_CPUS ',  
    '--config GDAL_CACHEMAX 49152 '
  )
  run_command(cmd)
}

# 4. Process raster and remove intermediate files
process_raster <- function(raw) {
  paths <- generate_paths(raw)
  
  message("Processing: ", raw)
  
  calculate_distances(raw, paths$distances)
  if (!file.exists(paths$distances)) {
    message("Error: Distance file not created. Skipping processing.")
    return()
  }
  
  restore_na_values(paths$distances, raw, paths$final)
  file.remove(paths$distances)
  
  message("Finished processing: ", raw, "\n")
}

# ---- EXECUTION ----
set_env_vars()
files <- c('results/settlements_1990_1h.tif', 'results/settlements_1990_5m.tif',
           'results/settlements_2000_1h.tif', 'results/settlements_2000_5m.tif',
           'results/settlements_2015_1h.tif', 'results/settlements_2015_5m.tif')
walk(files, process_raster)
