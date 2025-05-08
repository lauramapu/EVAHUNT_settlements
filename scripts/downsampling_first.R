rm(list=ls())

library(purrr)
library(glue)
library(terra)

# Set environment variables
set_env_vars <- function() {
  Sys.setenv(PYTHONHOME = "C:/Program Files/QGIS 3.28.13/apps/Python39")
  Sys.setenv(PYTHONPATH = "C:/Program Files/QGIS 3.28.13/apps/Python39/Lib")
}

# Generate output file paths
generate_paths <- function(raw) {
  base <- gsub(".tif$", "", basename(raw))
  list(
    urban_binary     = file.path("results", paste0(base, "_urban_binary.tif")),
    urban_resamp     = file.path("results", paste0(base, "_urban_resamp.tif")),
    resta            = file.path("results", paste0(base, "_resta.tif")),
    traveltime_resamp = file.path("results", paste0(base, "_traveltime_resamp.tif")),
    traveltime_1h    = file.path("results", paste0(base, "_traveltime_1h.tif")),
    traveltime_5m    = file.path("results", paste0(base, "_traveltime_5m.tif")),
    settle_1h        = file.path("results", paste0(base, "_settle_1h.tif")),
    settle_5m        = file.path("results", paste0(base, "_settle_5m.tif")),
    final_1h         = file.path("results", paste0(base, "_final_1h.tif")),
    final_5m         = file.path("results", paste0(base, "_final_5m.tif"))
  )
}

# Execute system command with error logging
run_command <- function(cmd) {
  message("Executing: ", cmd)
  result <- tryCatch(
    system(cmd, intern = TRUE, ignore.stderr = FALSE),
    error = function(e) {
      message("Error: ", conditionMessage(e))
      return(e)
    }
  )
  if (!is.null(attr(result, "status"))) {
    message("Command failed with status: ", attr(result, "status"))
  }
  message(paste(result, collapse = "\n"))
}

resample_raster <- function(input, reference, output, method) {
  if (file.exists(output)) {
    print(glue::glue("Skipping resampling, {output} already exists."))
    return()
  }
  
  # Load rasters
  input_rast <- rast(input)
  ref_rast <- rast(reference)
  
  # Resample directly to disk
  writeRaster(resample(input_rast, ref_rast, method = method), 
              filename = output, 
              overwrite = TRUE)
  
  print(glue::glue("Resampling completed: {output}"))
}

raster_difference <- function(input1, input2, output) {
  if (file.exists(output)) {
    print(glue("Skipping raster difference, {output} already exists."))
    return()
  }
  
  cmd <- glue('"C:/Program Files/QGIS 3.28.13/bin/python.exe" ',
              '"C:/Program Files/QGIS 3.28.13/apps/Python39/Scripts/gdal_calc.py" ',
              '-A "{input1}" -B "{input2}" --outfile="{output}" ',
              '--calc="A - B"')
  run_command(cmd)
}

binary_threshold <- function(input, output, threshold) {
  if (file.exists(output)) {
    print(glue("Skipping binary threshold, {output} already exists."))
    return()
  }
  
  cmd <- glue('"C:/Program Files/QGIS 3.28.13/bin/python.exe" ',
              '"C:/Program Files/QGIS 3.28.13/apps/Python39/Scripts/gdal_calc.py" ',
              '-A "{input}" --outfile="{output}" ',
              '--calc="(A<={threshold})*1"')
  run_command(cmd)
}

binary_urban <- function(input, output) {
  if (file.exists(output)) {
    print(glue("Skipping binary urban, {output} already exists."))
    return()
  }
  
  cmd <- glue('"C:/Program Files/QGIS 3.28.13/bin/python.exe" ',
              '"C:/Program Files/QGIS 3.28.13/apps/Python39/Scripts/gdal_calc.py" ',
              '-A "{input}" --outfile="{output}" ',
              '--calc="(A==190)*1" --NoDataValue=0')
  run_command(cmd)
}

final_settlement_mask <- function(input, output) {
  if (file.exists(output)) {
    print(glue("Skipping final settlement mask, {output} already exists."))
    return()
  }
  
  cmd <- glue('"C:/Program Files/QGIS 3.28.13/bin/python.exe" ',
              '"C:/Program Files/QGIS 3.28.13/apps/Python39/Scripts/gdal_calc.py" ',
              '-A "{input}" --outfile="{output}" ',
              '--calc="(A==1)*1"')
  run_command(cmd)
}

process_raster <- function(raw) {
  paths <- generate_paths(raw)
  
  binary_urban("data/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2000-v2.0.7.tif", paths$urban_binary)
  resample_raster(paths$urban_binary, raw, paths$urban_resamp, "near")
  
  raster_difference(raw, paths$urban_resamp, paths$resta)
  
  resample_raster("data/access_50k/acc_50k.tif", raw, paths$traveltime_resamp, "bilinear")
  
  binary_threshold(paths$traveltime_resamp, paths$traveltime_1h, 60)
  binary_threshold(paths$traveltime_resamp, paths$traveltime_5m, 5)
  
  raster_difference(paths$resta, paths$traveltime_1h, paths$settle_1h)
  raster_difference(paths$resta, paths$traveltime_5m, paths$settle_5m)
  
  final_settlement_mask(paths$settle_1h, paths$final_1h)
  final_settlement_mask(paths$settle_5m, paths$final_5m)
}

# ---- EXECUTION ----
set_env_vars()
files <- list.files("data", pattern = "Settlement_", full.names = TRUE)
files <- files[1]
walk(files, process_raster)