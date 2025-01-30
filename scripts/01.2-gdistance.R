library(raster)
library(gdistance)

# Cargar el raster
r <- raster("data/Settlement_2000.tif")

# d <- as.data.frame(r, xy=T, na.rm=T)

# # Asegurarse de que el raster es binario
# r <- reclassify(r, cbind(c(-Inf, 0.5, 1.5, Inf), c(0, 1, NA)))

# Crear capas de inicio (origen: píxeles 0) y destino (objetivo: píxeles 1)
origin <- r
origin[origin != 0] <- NA  # Mantener solo píxeles con valor 0

destination <- r
destination[destination != 1] <- NA  # Mantener solo píxeles con valor 1

# Crear una transición basada en distancia euclidiana
trans <- transition(r, function(x) 1, directions = 8)
trans <- geoCorrection(trans, type = "c")  # Corregir para distancias geográficas

# Calcular la distancia desde píxeles 0 (origen) hacia píxeles 1 (destino)
distances <- accCost(trans, as.matrix(which(!is.na(destination), arr.ind = TRUE)))

# Guardar el resultado
writeRaster(distances, "results/distance_2000_gdistance.tif", overwrite = TRUE)
