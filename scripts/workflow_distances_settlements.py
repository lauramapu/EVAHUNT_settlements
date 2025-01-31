# Raster Processing Workflow in Python

import os
import glob
import rasterio
import numpy as np
from osgeo import gdal
from rasterio.windows import Window

def create_output_dir(path: str):
    """ensure output path exists"""
    os.makedirs(path, exist_ok=True)

def split_raster(input_raster: str, nx: int, ny: int, buffer: int, output_dir: str):
    """split into tiles with a buffer (to get minimum distances in overlapping pixels)"""
    create_output_dir(output_dir)
   
   # get width and height
    with rasterio.open(input_raster) as src:
        tile_width = src.width // nx
        tile_height = src.height // ny
       
       # iterate though x and y to split with buffer
        for i in range(nx):
            for j in range(ny):
                x_off = max(i * tile_width - buffer, 0)
                y_off = max(j * tile_height - buffer, 0)
                width = min(tile_width + 2 * buffer, src.width - x_off)
                height = min(tile_height + 2 * buffer, src.height - y_off)
               
                window = Window(x_off, y_off, width, height)
                tile = src.read(window=window)
                meta = src.meta.copy()
                meta.update({"width": width, "height": height, "transform": src.window_transform(window)})
               
                tile_filename = os.path.join(output_dir, f"Settlement_2000_tile_{i}_{j}.tif")
                with rasterio.open(tile_filename, "w", **meta) as dst:
                    dst.write(tile)

def compute_distance(input_tile: str, output_tile: str):
    """compute distance to nearest 1 using GDAL API"""
    ds = gdal.Open(input_tile)
    out_ds = gdal.GetDriverByName("GTiff").Create(
        output_tile, ds.RasterXSize, ds.RasterYSize, 1, gdal.GDT_Float32
    )
    out_ds.SetGeoTransform(ds.GetGeoTransform())
    out_ds.SetProjection(ds.GetProjection())
    gdal.ComputeProximity(ds.GetRasterBand(1), out_ds.GetRasterBand(1), options=["DISTUNITS=GEO", "VALUES=1", "NODATA=-9999"]) # target value = 1
    out_ds, ds = None, None  # close datasets

def restore_na_values(original_tile: str, processed_tile: str, output_tile: str):
    """restore NA values from the original tile"""
    ds_A = gdal.Open(processed_tile)
    ds_B = gdal.Open(original_tile)
    A, B = ds_A.GetRasterBand(1).ReadAsArray(), ds_B.GetRasterBand(1).ReadAsArray()
   
    result = np.where((B == 0) | (B == 1), A, B) # basing on zeros and ones because NAs are not read correctly 
    driver = gdal.GetDriverByName("GTiff")
    out_ds = driver.Create(output_tile, ds_A.RasterXSize, ds_A.RasterYSize, 1, gdal.GDT_Float32)
    out_ds.SetGeoTransform(ds_A.GetGeoTransform())
    out_ds.SetProjection(ds_A.GetProjection())
    out_ds.GetRasterBand(1).WriteArray(result)
   
    out_ds, ds_A, ds_B = None, None, None  # close datasets
    os.remove(processed_tile)  # remove intermediate file

def merge_tiles(tiles: list, output_raster: str):
    """merge final tiles using GDAL Warp"""
    gdal.Warp(
        output_raster,
        tiles,
        format="GTiff",
        resampleAlg=gdal.GRA_Min,
        options=[
            "NUM_THREADS=ALL_CPUS",
            "GDAL_CACHEMAX=49152",
            "TILED=YES",
            "COMPRESS=DEFLATE", # better compression method
            "PREDICTOR=2", # horizontal differencing also for compression
            "BLOCKXSIZE=512",
            "BLOCKYSIZE=512",
        ]
    )

def main():
    """main workflow"""
    input_raster = "data/Settlement_2000.tif"
    tiles_path = "results/splitRaster/tiles_3x2"
    output_raster = "results/distance_2000.tif"
   
    split_raster(input_raster, nx=3, ny=2, buffer=10, output_dir=tiles_path)
    tiles = glob.glob(os.path.join(tiles_path, "Settlement_2000_tile_*.tif"))
   
    for tile in tiles:
        processed_tile = tile.replace("Settlement_2000_tile", "dists_2000_tile")
        final_tile = tile.replace("Settlement_2000_tile", "dists_2000_tile_final")
       
        print(f"Processing {tile}...")
        compute_distance(tile, processed_tile)
        restore_na_values(tile, processed_tile, final_tile)
   
    distance_tiles = glob.glob(os.path.join(tiles_path, "*dists_2000_tile_final*.tif"))
    merge_tiles(distance_tiles, output_raster)
    print("Processing complete!")

if __name__ == "__main__":
    main()
