library(terra)

# Cargar raster gigante (ejemplo)
tile_files <- list.files('results/splitRaster', pattern = 'Settle', full.names = TRUE)
r <- raster(tile_files[[2]])

# Crear una máscara para celdas con valor 1
mask_1 <- r == 1

# Función para procesar distancias en bloques
process_block <- function(block) {
  distance(block == 1, mask = !is.na(block))
}

# Aplicar el cálculo por bloques
dist_raster <- app(r, process_block, cores = 4)  # Usar 4 núcleos paralelos (ajustar según tu equipo)

# Guardar el resultado
writeRaster(dist_raster, "distancias_raster.tif", overwrite = TRUE)
