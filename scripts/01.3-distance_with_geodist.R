library(terra)
library(raster)

# install.packages('geodist')
library(geodist)

r <- raster('data/Settlement_2000.tif')
r@data@names <- 'Settlement_2000'

tile_files <- list.files('results/splitRaster', pattern = 'Settle', full.names = TRUE)
r <- raster(tile_files[[2]])

# 2. Extraer coordenadas de las celdas con valores 0 y 1
coords_0 <- as.data.frame(xyFromCell(r, which(values(r) == 0)))
coords_1 <- as.data.frame(xyFromCell(r, which(values(r) == 1)))

# 3. Calcular distancias desde cada celda 0 a las celdas 1 más cercanas
dist_matrix <- geodist(coords_0, coords_1, paired = FALSE, measure = "geodesic")

# 4. Encontrar la distancia mínima para cada celda con valor 0
min_distances <- apply(dist_matrix, 1, min)

# 5. Asignar las distancias al raster
dist_raster <- r
dist_raster[which(values(r) == 0)] <- min_distances
dist_raster[which(values(r) != 0)] <- NA  # Opcional: NA para celdas no relevantes

# Visualizar el resultado
plot(dist_raster, main = "Distancia desde celdas 0 a las celdas 1 más cercanas")