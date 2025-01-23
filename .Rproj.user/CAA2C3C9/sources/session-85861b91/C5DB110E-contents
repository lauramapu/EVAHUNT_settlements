######################################
##### PRESENCE POINTS EXTRACTION #####
######################################

rm(list=ls())
source('scripts/utils.R') # all used libraries and my custom functions

# load pMazamaSpatialUtils# load pre-filtered data from 2019 National Census
iberica <- read.csv('data/iberica_coords.csv')
ortega <- read.csv('data/ortega_coords.csv')

# transform csv to coordinates
# i use Xpos and Ypos cause are the wgs84 coordinates
iberica$`Xpos_WGS84` <- as.numeric(iberica$`Xpos_WGS84`)
iberica$`Ypos_WGS84` <- as.numeric(iberica$`Ypos_WGS84`)

# Convert to sf object
iberica_sf <- st_as_sf(iberica, coords = c('Xpos_WGS84', 'Ypos_WGS84'), crs = 4326)

# save as R object
saveRDS(iberica_sf, 'objects/iberica_points.rds')

# same with ortega
ortega$`Xpos_WGS84` <- as.numeric(ortega$`Xpos_WGS84`)
ortega$`Ypos_WGS84` <- as.numeric(ortega$`Ypos_WGS84`)
ortega_sf <- st_as_sf(ortega, coords = c('Xpos_WGS84', 'Ypos_WGS84'), crs = 4326)
saveRDS(ortega_sf, 'objects/ortega_points.rds')

##################################################
##### PREPARATION OF ENVIRONMENTAL VARIABLES #####
##################################################

# spanish provinces borders
spain <- getpeninsularspain()

##### CLIMATE #####

# we have present and future variables
# we need to scale all together to keep ranges
# future variables are composed by 5 models

# MODELS: GFDL-ESM4, IPSL,CM6A-LR, MPI-ESM1-2-HR, MRI-ESM2-0, UKESM1-O-LL
# SCENARIOS: SSP126 AND SSP585

# we're creating a function to preprocess most of the needed files

processing <- function(mi_pattern, mi_calc){
  listadefiles <- list.files('spatial_data/climate/future',
                             pattern = mi_pattern,
                             recursive = TRUE,
                             full.names = TRUE)
  print(listadefiles)
  stackdefiles <- stack(listadefiles)
  maskk <- st_transform(spain, crs=crs(stackdefiles))
  cortado <- raster::crop(stackdefiles, extent(maskk))
  masqueado <- raster::mask(cortado, maskk)
  raster_final <- calc(masqueado, mi_calc)
  return(raster_final)
}

tas_gfdl_1 <- processing('^CHELSA_gfdl.*ssp126_tas_.*\\.tif$', mi_calc = mean) 
tas_ipsl_1 <- processing('^CHELSA_ipsl.*ssp126_tas_.*\\.tif$', mi_calc = mean) 
tas_mpi_1 <- processing('^CHELSA_mpi.*ssp126_tas_.*\\.tif$', mi_calc = mean) 
tas_mri_1 <- processing('^CHELSA_mri.*ssp126_tas_.*\\.tif$', mi_calc = mean) 
tas_uke_1 <- processing('^CHELSA_uke.*ssp126_tas_.*\\.tif$', mi_calc = mean) 

tas_1 <- calc(stack(tas_gfdl_1,tas_ipsl_1,tas_mpi_1,tas_mri_1,tas_uke_1), mean)

tas_gfdl_5 <- processing('^CHELSA_gfdl.*ssp585_tas_.*\\.tif$', mi_calc = mean) 
tas_ipsl_5 <- processing('^CHELSA_ipsl.*ssp585_tas_.*\\.tif$', mi_calc = mean) 
tas_mpi_5 <- processing('^CHELSA_mpi.*ssp585_tas_.*\\.tif$', mi_calc = mean) 
tas_mri_5 <- processing('^CHELSA_mri.*ssp585_tas_.*\\.tif$', mi_calc = mean) 
tas_uke_5 <- processing('^CHELSA_uke.*ssp585_tas_.*\\.tif$', mi_calc = mean) 

tas_5 <- calc(stack(tas_gfdl_5,tas_ipsl_5,tas_mpi_5,tas_mri_5,tas_uke_5), mean)

# temp seasonality = stddev * 100

tassd_gfdl_1 <- processing('^CHELSA_gfdl.*ssp126_tas_.*\\.tif$', mi_calc = sd) 
tassd_ipsl_1 <- processing('^CHELSA_ipsl.*ssp126_tas_.*\\.tif$', mi_calc = sd) 
tassd_mpi_1 <- processing('^CHELSA_mpi.*ssp126_tas_.*\\.tif$', mi_calc = sd) 
tassd_mri_1 <- processing('^CHELSA_mri.*ssp126_tas_.*\\.tif$', mi_calc = sd) 
tassd_uke_1 <- processing('^CHELSA_uke.*ssp126_tas_.*\\.tif$', mi_calc = sd) 

tasseas_1 <- calc(stack(tassd_gfdl_1,tassd_ipsl_1,tassd_mpi_1,tassd_mri_1,tassd_uke_1), mean) *100

tassd_gfdl_5 <- processing('^CHELSA_gfdl.*ssp585_tas_.*\\.tif$', mi_calc = sd) 
tassd_ipsl_5 <- processing('^CHELSA_ipsl.*ssp585_tas_.*\\.tif$', mi_calc = sd) 
tassd_mpi_5 <- processing('^CHELSA_mpi.*ssp585_tas_.*\\.tif$', mi_calc = sd) 
tassd_mri_5 <- processing('^CHELSA_mri.*ssp585_tas_.*\\.tif$', mi_calc = sd) 
tassd_uke_5 <- processing('^CHELSA_uke.*ssp585_tas_.*\\.tif$', mi_calc = sd) 

tasseas_5 <- calc(stack(tassd_gfdl_5,tassd_ipsl_5,tassd_mpi_5,tassd_mri_5,tassd_uke_5), mean) *100

tasmax_gfdl_1 <- processing('^CHELSA_gfdl.*ssp126_tasmax_.*\\.tif$', mi_calc = mean) 
tasmax_ipsl_1 <- processing('^CHELSA_ipsl.*ssp126_tasmax_.*\\.tif$', mi_calc = mean) 
tasmax_mpi_1 <- processing('^CHELSA_mpi.*ssp126_tasmax_.*\\.tif$', mi_calc = mean) 
tasmax_mri_1 <- processing('^CHELSA_mri.*ssp126_tasmax_.*\\.tif$', mi_calc = mean) 
tasmax_uke_1 <- processing('^CHELSA_uke.*ssp126_tasmax_.*\\.tif$', mi_calc = mean) 

tasmax_1 <- calc(stack(tasmax_gfdl_1,tasmax_ipsl_1,tasmax_mpi_1,tasmax_mri_1,tasmax_uke_1), mean)

tasmax_gfdl_5 <- processing('^CHELSA_gfdl.*ssp585_tasmax_.*\\.tif$', mi_calc = mean) 
tasmax_ipsl_5 <- processing('^CHELSA_ipsl.*ssp585_tasmax_.*\\.tif$', mi_calc = mean) 
tasmax_mpi_5 <- processing('^CHELSA_mpi.*ssp585_tasmax_.*\\.tif$', mi_calc = mean) 
tasmax_mri_5 <- processing('^CHELSA_mri.*ssp585_tasmax_.*\\.tif$', mi_calc = mean) 
tasmax_uke_5 <- processing('^CHELSA_uke.*ssp585_tasmax_.*\\.tif$', mi_calc = mean) 

tasmax_5 <- calc(stack(tasmax_gfdl_5,tasmax_ipsl_5,tasmax_mpi_5,tasmax_mri_5,tasmax_uke_5), mean)

tasmin_gfdl_1 <- processing('^CHELSA_gfdl.*ssp126_tasmin_.*\\.tif$', mi_calc = mean) 
tasmin_ipsl_1 <- processing('^CHELSA_ipsl.*ssp126_tasmin_.*\\.tif$', mi_calc = mean) 
tasmin_mpi_1 <- processing('^CHELSA_mpi.*ssp126_tasmin_.*\\.tif$', mi_calc = mean) 
tasmin_mri_1 <- processing('^CHELSA_mri.*ssp126_tasmin_.*\\.tif$', mi_calc = mean) 
tasmin_uke_1 <- processing('^CHELSA_uke.*ssp126_tasmin_.*\\.tif$', mi_calc = mean) 

tasmin_1 <- calc(stack(tasmin_gfdl_1,tasmin_ipsl_1,tasmin_mpi_1,tasmin_mri_1,tasmin_uke_1), mean)

tasmin_gfdl_5 <- processing('^CHELSA_gfdl.*ssp585_tasmin_.*\\.tif$', mi_calc = mean) 
tasmin_ipsl_5 <- processing('^CHELSA_ipsl.*ssp585_tasmin_.*\\.tif$', mi_calc = mean) 
tasmin_mpi_5 <- processing('^CHELSA_mpi.*ssp585_tasmin_.*\\.tif$', mi_calc = mean) 
tasmin_mri_5 <- processing('^CHELSA_mri.*ssp585_tasmin_.*\\.tif$', mi_calc = mean) 
tasmin_uke_5 <- processing('^CHELSA_uke.*ssp585_tasmin_.*\\.tif$', mi_calc = mean) 

tasmin_5 <- calc(stack(tasmin_gfdl_5,tasmin_ipsl_5,tasmin_mpi_5,tasmin_mri_5,tasmin_uke_5), mean)

pr_gfdl_1 <- processing('^CHELSA_gfdl.*ssp126_pr_.*\\.tif$', mi_calc = sum) 
pr_ipsl_1 <- processing('^CHELSA_ipsl.*ssp126_pr_.*\\.tif$', mi_calc = sum) 
pr_mpi_1 <- processing('^CHELSA_mpi.*ssp126_pr_.*\\.tif$', mi_calc = sum) 
pr_mri_1 <- processing('^CHELSA_mri.*ssp126_pr_.*\\.tif$', mi_calc = sum) 
pr_uke_1 <- processing('^CHELSA_uke.*ssp126_pr_.*\\.tif$', mi_calc = sum) 

pr_1 <- calc(stack(pr_gfdl_1,pr_ipsl_1,pr_mpi_1,pr_mri_1,pr_uke_1), mean)

pr_gfdl_5 <- processing('^CHELSA_gfdl.*ssp585_pr_.*\\.tif$', mi_calc = sum) 
pr_ipsl_5 <- processing('^CHELSA_ipsl.*ssp585_pr_.*\\.tif$', mi_calc = sum) 
pr_mpi_5 <- processing('^CHELSA_mpi.*ssp585_pr_.*\\.tif$', mi_calc = sum) 
pr_mri_5 <- processing('^CHELSA_mri.*ssp585_pr_.*\\.tif$', mi_calc = sum) 
pr_uke_5 <- processing('^CHELSA_uke.*ssp585_pr_.*\\.tif$', mi_calc = sum) 

pr_5 <- calc(stack(pr_gfdl_5,pr_ipsl_5,pr_mpi_5,pr_mri_5,pr_uke_5), mean)

# prec seasonality (cv = (stdv / (1 + mean)) * 100)

prsd_gfdl_1 <- processing('^CHELSA_gfdl.*ssp126_pr_.*\\.tif$', mi_calc = sd) 
prsd_ipsl_1 <- processing('^CHELSA_ipsl.*ssp126_pr_.*\\.tif$', mi_calc = sd) 
prsd_mpi_1 <- processing('^CHELSA_mpi.*ssp126_pr_.*\\.tif$', mi_calc = sd) 
prsd_mri_1 <- processing('^CHELSA_mri.*ssp126_pr_.*\\.tif$', mi_calc = sd) 
prsd_uke_1 <- processing('^CHELSA_uke.*ssp126_pr_.*\\.tif$', mi_calc = sd)

prsd_1 <- calc(stack(prsd_gfdl_1,prsd_ipsl_1,prsd_mpi_1,prsd_mri_1,prsd_uke_1), mean)

prmean_gfdl_1 <- processing('^CHELSA_gfdl.*ssp126_pr_.*\\.tif$', mi_calc = mean) 
prmean_ipsl_1 <- processing('^CHELSA_ipsl.*ssp126_pr_.*\\.tif$', mi_calc = mean) 
prmean_mpi_1 <- processing('^CHELSA_mpi.*ssp126_pr_.*\\.tif$', mi_calc = mean) 
prmean_mri_1 <- processing('^CHELSA_mri.*ssp126_pr_.*\\.tif$', mi_calc = mean) 
prmean_uke_1 <- processing('^CHELSA_uke.*ssp126_pr_.*\\.tif$', mi_calc = mean)

prmean_1 <- calc(stack(prmean_gfdl_1,prmean_ipsl_1,prmean_mpi_1,prmean_mri_1,prmean_uke_1), mean)

prseas_1 <- (prsd_1/(1-prmean_1))*100

prsd_gfdl_5 <- processing('^CHELSA_gfdl.*ssp585_pr_.*\\.tif$', mi_calc = sd) 
prsd_ipsl_5 <- processing('^CHELSA_ipsl.*ssp585_pr_.*\\.tif$', mi_calc = sd) 
prsd_mpi_5 <- processing('^CHELSA_mpi.*ssp585_pr_.*\\.tif$', mi_calc = sd) 
prsd_mri_5 <- processing('^CHELSA_mri.*ssp585_pr_.*\\.tif$', mi_calc = sd) 
prsd_uke_5 <- processing('^CHELSA_uke.*ssp585_pr_.*\\.tif$', mi_calc = sd)

prsd_5 <- (prsd_gfdl_5+prsd_ipsl_5+prsd_mpi_5+prsd_mri_5+prsd_uke_5)/5

prmean_gfdl_5 <- processing('^CHELSA_gfdl.*ssp585_pr_.*\\.tif$', mi_calc = mean) 
prmean_ipsl_5 <- processing('^CHELSA_ipsl.*ssp585_pr_.*\\.tif$', mi_calc = mean) 
prmean_mpi_5 <- processing('^CHELSA_mpi.*ssp585_pr_.*\\.tif$', mi_calc = mean) 
prmean_mri_5 <- processing('^CHELSA_mri.*ssp585_pr_.*\\.tif$', mi_calc = mean) 
prmean_uke_5 <- processing('^CHELSA_uke.*ssp585_pr_.*\\.tif$', mi_calc = mean)

prmean_5 <- (prmean_gfdl_5+prmean_ipsl_5+prmean_mpi_5+prmean_mri_5+prmean_uke_5)/5

prseas_5 <- (prsd_5/(1-prmean_5))*100

# now we need to scale the variables along with present variables
# we're doing it manually since we need the mean and std of both periods for each variable

processing_temp <- function(mi_pattern, mi_calc){
  listadefiles <- list.files('spatial_data/climate/present',
                             pattern = mi_pattern,
                             recursive = TRUE,
                             full.names = TRUE)
  print(listadefiles)
  stackdefiles <- stack(listadefiles)
  maskk <- st_transform(spain, crs=crs(stackdefiles))
  cortado <- raster::crop(stackdefiles, extent(maskk))
  masqueado <- raster::mask(cortado, maskk)
  raster_final <- calc(masqueado, mi_calc)
  return(raster_final)
}

mean_temp <- processing_temp('^CHELSA_tas_.*\\.tif$', mi_calc = mean)
mean_temp_seas <- processing_temp('^CHELSA_tas_.*\\.tif$', mi_calc = sd) * 100
max_temp <- processing_temp('^CHELSA_tasmax_.*\\.tif$', mi_calc = mean)
min_temp <- processing_temp('^CHELSA_tasmin_.*\\.tif$', mi_calc = mean)

processing_prec <- function(mi_pattern, mi_calc){
  listadefiles <- list.files('spatial_data/climate/present',
                             pattern = mi_pattern,
                             recursive = TRUE,
                             full.names = TRUE)
  print(listadefiles)
  stackdefiles <- stack(listadefiles)
  maskk <- st_transform(spain, crs=crs(stackdefiles))
  cortado <- raster::crop(stackdefiles, extent(maskk))
  masqueado <- raster::mask(cortado, maskk)
  raster_final <- calc(masqueado, mi_calc)
  return(raster_final)
}

prec <- processing_prec('^CHELSA_pr_.*\\.tif$', mi_calc = sum)
prec_seas <- (processing_prec('^CHELSA_pr_.*\\.tif$', mi_calc = sd) / (1 + processing_prec('^CHELSA_pr_.*\\.tif$', mi_calc = mean))) * 100

# z-score formula: (x-mu)/sigma
# we're doing it for 3 rasters: present, scenario 126 and scenario 585
manual_zscore <- function(raster1, raster2, raster3) {
  raster1v <- na.omit(as.vector(raster1))
  raster2v <- na.omit(as.vector(raster2))
  raster3v <- na.omit(as.vector(raster3))
  raster4 <- c(raster1v, raster2v, raster3v)
  assign(deparse(substitute(raster1)), (raster1-mean(raster4))/sd(raster4), envir = .GlobalEnv)
  assign(deparse(substitute(raster2)), (raster2-mean(raster4))/sd(raster4), envir = .GlobalEnv)
  assign(deparse(substitute(raster3)), (raster3-mean(raster4))/sd(raster4), envir = .GlobalEnv)
}

manual_zscore(mean_temp, tas_1, tas_5)
manual_zscore(mean_temp_seas, tasseas_1, tasseas_5)
manual_zscore(max_temp, tasmax_1, tasmax_5)
manual_zscore(min_temp, tasmin_1, tasmin_5)
manual_zscore(prec, pr_1, pr_5)
manual_zscore(prec_seas, prseas_1, prseas_5)

clim_variables_present <- c(mean_temp, mean_temp_seas, max_temp, min_temp, prec, prec_seas)
clim_variables_future <- c(tas_1, tas_5, tasseas_1, tasseas_5,
                           tasmin_1, tasmin_5, tasmax_1, tasmax_5,
                           pr_1, pr_5, prseas_1, prseas_5)

# save all the generated asciis

writeRaster(mean_temp, 'spatial_data/env_asciis/mean_temp.asc', format='ascii',overwrite=T)
writeRaster(mean_temp_seas, 'spatial_data/env_asciis/mean_temp_seas.asc', format='ascii',overwrite=T)
writeRaster(max_temp, 'spatial_data/env_asciis/max_temp.asc', format='ascii',overwrite=T)
writeRaster(min_temp, 'spatial_data/env_asciis/min_temp.asc', format='ascii',overwrite=T)
writeRaster(prec, 'spatial_data/env_asciis/prec.asc', format='ascii',overwrite=T)
writeRaster(prec_seas, 'spatial_data/env_asciis/prec_seas.asc', format='ascii',overwrite=T)

writeRaster(tas_1, 'spatial_data/env_asciis_fut/mean_temp_126_4170.asc',
            format='ascii',overwrite=T)
writeRaster(tas_5, 'spatial_data/env_asciis_fut/mean_temp_585_4170.asc',
            format='ascii',overwrite=T)
writeRaster(tasseas_1, 'spatial_data/env_asciis_fut/mean_temp_seas_126_4170.asc',
            format='ascii',overwrite=T)
writeRaster(tasseas_5, 'spatial_data/env_asciis_fut/mean_temp_seas_585_4170.asc',
            format='ascii',overwrite=T)
writeRaster(tasmax_1, 'spatial_data/env_asciis_fut/max_temp_126_4170.asc',
            format='ascii',overwrite=T)
writeRaster(tasmax_5, 'spatial_data/env_asciis_fut/max_temp_585_4170.asc',
            format='ascii',overwrite=T)
writeRaster(tasmin_1, 'spatial_data/env_asciis_fut/min_temp_126_4170.asc',
            format='ascii',overwrite=T)
writeRaster(tasmin_5, 'spatial_data/env_asciis_fut/min_temp_585_4170.asc',
            format='ascii',overwrite=T)
writeRaster(pr_1, 'spatial_data/env_asciis_fut/prec_126_4170.asc',
            format='ascii',overwrite=T)
writeRaster(pr_5, 'spatial_data/env_asciis_fut/prec_585_4170.asc',
            format='ascii',overwrite=T)
writeRaster(prseas_1, 'spatial_data/env_asciis_fut/prec_seas_126_4170.asc',
            format='ascii',overwrite=T)
writeRaster(prseas_5, 'spatial_data/env_asciis_fut/prec_seas_585_4170.asc',
            format='ascii',overwrite=T)

##### ELEVATION AND SLOPE ##### 

# crop+mask function for all variables
mask_raster <- function(raster_layer) {
  maskk <- st_transform(spain, st_crs(raster_layer))
  cropped <- raster::crop(raster_layer, extent(maskk))
  masked <- raster::mask(cropped, maskk)
  return(masked)
}

# load rasters
dem_list <- list.files('spatial_data/dem', pattern = '', full.names = TRUE)
# rasters have different extent (dont know why) so we have to merge instead of stack
dem_list <- lapply(dem_list, raster) # read as list of rasters (list.files only reads filepaths)
dem <- do.call(merge, dem_list) # do call to merge with dem_list

# calculate slope
slope <- raster::terrain(dem, opt = 'slope', unit = 'degrees', neighbors = 8)

# list topography variables and crop/mask by reprojected mask
topo_variables <- list(dem,slope)
topo_variables <- lapply(topo_variables, mask_raster) 

### LAND USES

raw_landuses <- raster('spatial_data/land_uses/EU_landSystem.tif')
# extract unique values from corine
unique_values <- unique(na.omit(values(raw_landuses)))

# loop through each value and extract binary raster
binary_rasters <- list()
for (value in unique_values) {
  binary_raster <- raw_landuses == value
  binary_rasters[[as.character(value)]] <- binary_raster
}

binary_rasters$'43' 
binary_rasters_stack <- stack(binary_rasters)

# now we sum up the following rasters:
# settlements: 21, 22, 23
# forest: 41, 42, 43
# water: 11, 12, 13
# basing on the known ecological preferences of steppe birds and for simplification purposes

elements_to_erase <- c('0','11', '12', '13', '21', '22', '23', '41', '42', '43')
binary_rasters_filtered <- binary_rasters[setdiff(names(binary_rasters), elements_to_erase)]
filtered_stack <- stack(binary_rasters_filtered)

settlements <- calc(stack(binary_rasters_stack$X21, binary_rasters_stack$X22, binary_rasters_stack$X23), sum)
forest <- calc(stack(binary_rasters_stack$X41, binary_rasters_stack$X42, binary_rasters_stack$X43), sum)
water <- calc(stack(binary_rasters_stack$X11, binary_rasters_stack$X12, binary_rasters_stack$X13), sum)

landuses <- stack(settlements, forest, water, filtered_stack)
# 1 = settlements, 2 = forest, 3 = water

# crop/mask landuse stack by reprojected mask

land_variables <- mask_raster(landuses)
# if the scipen error occurs do options(scipen = 99)
  
# ANTHROPIC PERTURBATION
# road and train length per cell (1x1km), calculated and rasterized in QGIS because of some computing problems 
# workflow in doc
# zeros are ones but it's ok because we need for log transformation

linear_infrastr <- raster('spatial_data/roads/road_length_OK.tif') +
  raster('spatial_data/roads/train_lines_length_OK.tif')
linear_infrastr <- calc(linear_infrastr, fun = function(x) log10(x))

linear_infrastr <- mask_raster(linear_infrastr)

# save objects (optional)

saveRDS(clim_variables_present, 'objects/clim_variables_present.rds')
saveRDS(clim_variables_future, 'objects/clim_variables_future.rds')
saveRDS(topo_variables, 'objects/topo_variables.rds')
saveRDS(land_variables, 'objects/land_variables.rds')
saveRDS(linear_infrastr, 'objects/linear_infrastr.rds')

### HOMOGENEIZATION OF VARIABLES

# variables already crop/masked, we just need to put them together, reproject and resample to mean_temp

# list all env variables except landuses (we need to resample by nearest neighbour)
# and climate variables (we resample to clim variables)

env_list <- c(topo_variables, linear_infrastr)

# reproject and resample to climate variables, also scale

resampling <- function(input_raster) {
  resampled <- scale(raster::projectRaster(input_raster, mean_temp, method='bilinear'))
  return(resampled)
}

env_list_resampled <- lapply(env_list, resampling)

# reproject and resample landuses with ngb
land_variables <- projectRaster(land_variables, mean_temp, method = 'ngb')

# join landuses to general list and generate a rasterstack

env_list_present <- c(env_list_resampled, land_variables)
env_stack <- stack(env_list_present)

# change name of all variables in the stack
new_names<-c('layer.1.1'='elev','layer.2.1'='linear_infrastr','layer.1.2'='settlements',
             'layer.2.2'='forest','layer.3'='water','X80'='shrub','X90'='bare',
             'X74'='for_shr_bare','X72'='for_shr_grass','X71'='for_shr_crop',
             'X75'='for_shr_agric','X731'='mosaic_low','X62'='crop_med',
             'X51'='grass_low','X63'='crop_high','X52'='grass_med','X53'='grass_high',
             'X732'='mosaic_med','X31'='ext_perm_crop','X61'='crop_low',
             'X32'='int_perm_crop','X733'='mosaic_high')
# # change name of only one layer
# names(land_variables)[names(land_variables) == 'layer.1'] <- 'settlements'
# loop this
for (change in names(new_names)) {
  old_name <- change
  new_name <- new_names[change]
  names(env_stack)[names(env_stack) == old_name] <- new_name
}

# write rasters in asc from the stack
# Function to write ASC files for a raster stack
write_raster_stack <- function(raster_stack) {
  for (i in 1:nlayers(raster_stack)) {
    output_file <- file.path(output_directory, paste0(names(raster_stack)[i], '.asc'))
    writeRaster(raster_stack[[i]], output_file, format = 'ascii', overwrite = TRUE)
  }
}

output_directory <- 'spatial_data/env_asciis'
write_raster_stack(env_stack)
