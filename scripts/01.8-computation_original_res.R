
# we're using GDAL because somehow works out-of-core and it's super optimized for giant rasters
# WARNING: super heavy processes, don't run if it's not necessary

# almost same process as 01.7 but reordering steps so that first we compute distances and finally we downsample
# so that distance calculation is done with the original resolution (30x30m)
# also we changed downsampling function to use average instead of max because now we have continuous values

# we also need to again split the original files in tiles with a buffer because the whole distance layer requires 1.96TB

rm(list=ls())

library(purrr)  
library(glue)  
library(terra)
library(SpaDES)
library(parallel)

split_rasters <- function(files, nx = 3, ny = 2, buffer = c(10, 10), output_dir = "results/tiles") {
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # Set up parallel processing
  cl <- makeCluster(nx*ny)
  
  # Loop through each raster file
  for (file in files) {
    cat("Processing:", file, "\n")
    
    # extract raster name
    file_base <- tools::file_path_sans_ext(basename(file))  # Get filename without extension

    # Read raster
    r <- rast(file)
    names(r) <- file_base
    
    # Split raster
    splitRaster(r, nx = nx, ny = ny, buffer = buffer, path = output_dir, cl = cl)
    
    cat("File", file, "split into tiles.\n")
  }
  
  # Stop parallel cluster
  stopCluster(cl)
  
  cat("All rasters have been split into tiles.\n")
}

files <- list.files("data", pattern = "Settlement_", full.names = TRUE)
split_rasters(files)

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
    nas = file.path("results", paste0(base, "_nas.tif")),
    final = file.path("results", paste0(base, "_final.tif"))
  )
}

# Execute system command
run_command <- function(cmd) {
  message("Executing: ", cmd)
  result <- system(cmd, intern = TRUE, ignore.stderr = FALSE)  # Capture stderr
  message("Output: ", paste(result, collapse = "\n"))
  return(result)
}

# 1. Calculate distances 
calculate_distances <- function(input, output) {
  cmd <- glue(
    '"C:/Program Files/QGIS 3.28.13/bin/python.exe" ',
    '"C:/Program Files/QGIS 3.28.13/apps/Python39/Scripts/gdal_proximity.py" ',
    '"{input}" "{output}" ',
    '-distunits GEO ',  
    '-use_input_nodata YES ',  
    '-values 1 ',  
    '--config GDAL_NUM_THREADS ALL_CPUS ',  
    '--config GDAL_CACHEMAX 49152 ',
    # this function won't admit --co so can't compress 
  )
  run_command(cmd)
}

# 2. Restore NA values
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
    '--co PREDICTOR=2 ', # more compression for continuous rasters
    '--co BIGTIFF=YES ',  
    '--config GDAL_NUM_THREADS ALL_CPUS ',  
    '--config GDAL_CACHEMAX 49152 '
  )
  run_command(cmd)
}

# 3. Downsample raster to 1x1km
downsample_raster <- function(input, output) {
  cmd <- glue(
    '"C:/Program Files/QGIS 3.28.13/bin/gdalwarp.exe" ',
    '-r average ',  
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

# 4. Process raster and remove intermediate files
process_raster <- function(raw) {
  paths <- generate_paths(raw)
  
  message("Processing: ", raw)
  
  calculate_distances(raw, paths$distances)
  if (!file.exists(paths$distances)) {
    message("Error: Distance file not created. Skipping processing.")
    return()
  }
  
  restore_na_values(paths$distances, raw, paths$nas)
  
  downsample_raster(paths$nas, paths$final)
  if (!file.exists(paths$downsampled)) {
    message("Error: Downsampled file not created. Skipping processing.")
    return()
  }
  
  file.remove(paths$distances, paths$nas)
  
  message("Finished processing: ", raw, "\n")
}

# ---- EXECUTION ----
set_env_vars()
files <- list.files("results/tiles", pattern = "Settlement_", full.names = TRUE, recursive=T)
walk(files, process_raster)
