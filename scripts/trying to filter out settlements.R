
# distance calculation only for vietnam, year 2000

library(raster)
library(terra)
library(sf)

settle <- raster('results/settlement2000_vietnam.tif')
traveltime <- raster('results/traveltime2000_vietnam.tif')
lc <- raster('results/lc2000_vietnam.tif')

traveltime_1h <- calc(traveltime, function(x) {ifelse(x<=60, 1, 0)})
traveltime_5m <- calc(traveltime, function(x) {ifelse(x<=5, 1, 0)})
par(mfrow=c(1,3))
plot(traveltime); plot(traveltime_1h); plot(traveltime_5m)

lc_urban <- calc(lc, function(x) {ifelse(x==190, 1, 0)})
par(mfrow=c(1,2))
plot(lc); plot(lc_urban)

lc_urban_resamp <- projectRaster(lc_urban, settle, method='ngb')

resta <- settle-lc_urban_resamp

traveltime_1h_resamp <- projectRaster(traveltime_1h, settle, method='ngb')
traveltime_5m_resamp <- projectRaster(traveltime_5m, settle, method='ngb')

settle_1h <- resta-traveltime_1h_resamp
settle_5m <- resta-traveltime_5m_resamp

mapview(settle)+mapview(resta)+mapview(settle_1h)+mapview(settle_5m)

settle_1h_final <- calc(settle_1h, function(x) {ifelse(x==1,1,0)})
settle_5m_final <- calc(settle_5m, function(x) {ifelse(x==1,1,0)})

mapview(settle_1h_final)+mapview(settle_5m_final)

writeRaster(settle_1h_final, 'results/settlements_vietnam_2000_1h.tif', overwrite=T)
writeRaster(settle_5m_final, 'results/settlements_vietnam_2000_5m.tif', overwrite=T)

# now we need to repeat the GDAL workflow for these rasters
# first we're trying to downsample to 1x1km and then calculating distances
# and second to first calculate distances with original resolution and downsampling at the end

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
files <- list.files("results", pattern = "settlements_vietnam_2000_", full.names = TRUE)
walk(files, process_raster)

#################################

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
    final = file.path("results", paste0(base, "_final_original.tif"))
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
  if (!file.exists(paths$final)) {
    message("Error: Downsampled file not created. Skipping processing.")
    return()
  }
  
  file.remove(paths$distances, paths$nas)
  
  message("Finished processing: ", raw, "\n")
}

# ---- EXECUTION ----
set_env_vars()
files <- list.files("results", pattern = "settlements_vietnam", full.names = TRUE, recursive=T)[4]
walk(files, process_raster)


# corr plot

files <- list.files('results', pattern='settlements_vietnam', full.names=T)
settle_1h_downsamp <- raster(files[2])
settle_1h_original <- raster(files[3])

stack <- stack(settle_1h_downsamp, settle_1h_original)
df <- na.omit(as.data.frame(stack))
colnames(df) <- c('downsampled','original')
#df <- data.frame(original=na.omit(settle_1h_original[]), na.omit(downsampled=settle_1h_downsamp[]))

library(ggplot2)
library(ggpubr)

# Create the scatter plot
p1 <- ggplot(df, aes(x = original, y = downsampled)) +
  geom_point(alpha = 0.6, color = "blue") +  # Scatter plot points
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add a linear regression line
  theme_minimal() +
  labs(title = "1h traveltime - all values",
       x = "Original Values",
       y = "Downsampled Values")

# Add the correlation coefficient
p1 + stat_cor(method = "pearson", label.x = min(df$original), label.y = max(df$downsampled))

# cortar ejes hasta 0.1 y calcular ahi la correlacion

# Filter the data for values where original <= 0.1
df_filtered <- df[df$original <= 0.02, ]

# Create the scatter plot with the filtered data
p2 <- ggplot(df_filtered, aes(x = original, y = downsampled)) +
  geom_point(alpha = 0.6, color = "blue") +  # Scatter plot points
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Linear regression line
  theme_minimal() +
  labs(title = "1h traveltime - values up to 2km",
       x = "Original Values",
       y = "Downsampled Values")

# Add the correlation coefficient
p2 + stat_cor(method = "pearson", label.x = min(df_filtered$original), label.y = max(df_filtered$downsampled))

cor(df_filtered$original, df_filtered$downsampled, method = "pearson")

# for 5 mins

settle_5m_downsamp <- raster(files[5])
settle_5m_original <- raster(files[6])

stack <- stack(settle_1h_downsamp, settle_1h_original)
df <- na.omit(as.data.frame(stack))
colnames(df) <- c('downsampled','original')
#df <- data.frame(original=na.omit(settle_1h_original[]), na.omit(downsampled=settle_1h_downsamp[]))

# Create the scatter plot
p3 <- ggplot(df, aes(x = original, y = downsampled)) +
  geom_point(alpha = 0.6, color = "blue") +  # Scatter plot points
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add a linear regression line
  theme_minimal() +
  labs(title = "5 mins traveltime - all values",
       x = "Original Values",
       y = "Downsampled Values")

# Add the correlation coefficient
p3 + stat_cor(method = "pearson", label.x = min(df$original), label.y = max(df$downsampled))

# cortar ejes hasta 0.1 y calcular ahi la correlacion

# Filter the data for values where original <= 0.1
df_filtered <- df[df$original <= 0.02, ]

# Create the scatter plot with the filtered data
p4 <- ggplot(df_filtered, aes(x = original, y = downsampled)) +
  geom_point(alpha = 0.6, color = "blue") +  # Scatter plot points
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Linear regression line
  theme_minimal() +
  labs(title = "5 mins traveltime - values up to 2km",
       x = "Original Values",
       y = "Downsampled Values")

# Add the correlation coefficient
p4 + stat_cor(method = "pearson", label.x = min(df_filtered$original), label.y = max(df_filtered$downsampled))

cor(df_filtered$original, df_filtered$downsampled, method = "pearson")

library(patchwork)

(p1 | p2) / (p3 | p4)
