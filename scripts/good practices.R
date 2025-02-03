
# we're using GDAL because somehow works out-of-core and it's super optimized for giant rasters
# WARNING: super heavy processes, don't run if it's not necessary

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
    downsampled = file.path("results", paste0(base, "_ds.tif")),
    distances   = file.path("results", paste0(base, "_dists.tif")),
    final       = file.path("results", paste0(base, "_final.tif"))
  )
}

# Execute system command
run_command <- function(cmd) {
  message("Executing: ", cmd)
  result <- tryCatch(system(cmd, intern = TRUE), error = function(e) e)
  if (inherits(result, "error")) message("Error: ", conditionMessage(result))
}

# 1. Downsample raster to 1x1km
downsample_raster <- function(input, output) {
  cmd <- glue(
    '"C:/Program Files/QGIS 3.28.13/bin/gdalwarp.exe" ',
    '-r max ',  
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

# 2. Calculate distances 
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

# 3. Restore NA values
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
    '--co COMPRESS=LZW ',  
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
  
  downsample_raster(raw, paths$downsampled)
  if (!file.exists(paths$downsampled)) {
    message("Error: Downsampled file not created. Skipping processing.")
    return()
  }
  
  calculate_distances(paths$downsampled, paths$distances)
  if (!file.exists(paths$distances)) {
    message("Error: Distance file not created. Skipping processing.")
    return()
  }
  
  restore_na_values(paths$distances, paths$downsampled, paths$final)
  file.remove(paths$downsampled, paths$distances)
  
  message("Finished processing: ", raw, "\n")
}

# ---- EXECUTION ----
set_env_vars()
files <- list.files("data", pattern = "Settlement_", full.names = TRUE)
walk(files, process_raster)
