# shaded relief for Vegetation Object Model

library(sf)
library(rayshader)
library(RColorBrewer)
library(gdalUtilities)

# Vegetation height
# see https://experience.arcgis.com/experience/753ad2ebd3554fa696885b8c366c3049/page/LIDAR/?views=Vegetation-Object-Model for data
VOM_height  <- raster::raster('data/National-LIDAR-Programme-VOM-2018-SK53nw/P_10724/VOM_SK5035_P_10724_20180125_20180130.tif')
# have a look - it's a 5km tile
sp::plot(VOM_height)

# Wollaton Park boundary
WP_boundary <- read_sf("data/OSM/Wollaton Park.shp")

st_crs(WP_boundary) # EPSG 4326
st_crs(VOM_height) # EPSG 27700

# reproject to British National Grid
WP_boundary <- st_transform(WP_boundary, crs = st_crs(VOM_height))

# crop to boundary
VOM_height_cropped <- raster::crop(VOM_height, st_bbox(WP_boundary))
# have a look - should probably crop a bit less tightly but oh well
sp::plot(VOM_height_cropped)

# convert to matrix
height_matrix <- raster_to_matrix(VOM_height_cropped)

# have a look
height_matrix %>% 
  height_shade(texture = colorRampPalette(brewer.pal(9, "Greens"))(9)) %>% 
  plot_map()

# maybe blur the VOM_height_cropped first before ray_shade, otherwise the shadows have jagged edges

# get shadows
system.time(
  ray_mat_anglebreaks <-ray_shade(height_matrix, zscale = 1,
                                  anglebreaks = seq(35, 60, by = 1),
                                  sunangle = 270)
) # doesn't actually take too long

# have a look
ray_mat_anglebreaks %>% plot_map()

# ambient shade
ambient_mat <- ambient_shade(height_matrix, zscale = 1)

# have a look
ambient_mat %>% plot_map()

# combine both shaded layers
VOM_height_cropped %>% 
  raster_to_matrix() %>% 
  height_shade(texture = colorRampPalette(c("#FFFFFF", "#FFFFFF"))(2)) %>%
  add_shadow(ray_mat_anglebreaks, 0.7) %>%
  add_shadow(ambient_mat, .5) %>% 
  plot_map()

# function to save georeferenced TIFs from the rayshaded layers
#'@title Save TIF
#'
#'@description Writes the hillshaded map to raster.
#'
#'@param hillshade Array (or matrix) of hillshade to be written.
#'@param filename String with the filename.
#'@param source_raster Raster used to generate the hillshade.
save_tif <- function(
    hillshade, filename,
    source_raster
) {
  if(is.null(filename)) {
    stop("save_tif requires a filename")
  }
  ray_raster <- raster(hillshade)
  ray_raster_t <- t(ray_raster)
  temp1 <- tempfile("ray_raster_", fileext = ".tif")
  temp2 <- tempfile("ray_raster_gcps", fileext = ".tif")
  terra::writeRaster(ray_raster_t, temp1, overwrite=TRUE)
  # ground control points
  gcps <- matrix(c(ray_raster@nrows, ray_raster@ncols, 
                   extent(source_raster)@xmin, extent(source_raster)@ymin,  ## lower-left
                   ray_raster@nrows, 0, 
                   extent(source_raster)@xmin, extent(source_raster)@ymax,  ## upper-right
                   0, ray_raster@ncols, 
                   extent(source_raster)@xmax, extent(source_raster)@ymin,  ## lower-right
                   0, 0, extent(source_raster)@xmax, extent(source_raster)@ymax), ## upper-left
                 ncol = 4, byrow = TRUE)
  gdalUtilities::gdal_translate(src_dataset = temp1,
                                dst_dataset = temp2,
                                gcp = gcps,
                                of = "GTiff")
  source_raster_epsg <- st_crs(source_raster)$epsg
  gdalUtilities::gdalwarp(srcfile = temp2,
                          dstfile = filename,
                          tps = T,
                          r = "bilinear",
                          s_srs = paste0("EPSG:",source_raster_epsg), 
                          t_srs = paste0("EPSG:",source_raster_epsg), 
                          overwrite = T)
}

# save the shadow one
save_tif(hillshade = ray_mat_anglebreaks, 
         filename = "data/WP_ray_raster_anglebreaks.tif",
         source_raster = VOM_height_cropped)

# save the ambient shade one
save_tif(hillshade = ambient_mat, 
         filename = "data/WP_ambient_raster.tif",
         source_raster = VOM_height_cropped)
