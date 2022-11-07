library(sf)
library(rayshader)
library(leaflet)
library(terrainr)
library(terra)

# this is the centre of the map
menan_buttes <- data.frame(x = -111.97, y = 43.7732) %>% 
  st_as_sf(coords = c("x", "y"), crs = 4326) # into an sf object

# 8km squared box around that centre
menan_buttes_bbox <- set_bbox_side_length(menan_buttes, 8000)

# reproject to WGS84 to have a look at in leaflet
menan_buttes_bbox_wgs84 <- st_transform(menan_buttes_bbox, "+proj=longlat +datum=WGS84")

# neat interactive map (works best in an IDE like RStudio with a built-in web viewer)
menan_buttes_bbox_wgs84 %>% leaflet() %>% 
  addProviderTiles("Esri.WorldShadedRelief") %>%
  addPolygons(color = "#444444", weight = 1) %>% 
  addCircleMarkers(data = menan_buttes, radius = 2)
# good for fine-tuning the starting coordinates / bbox width

# reproject to EPSG:3857
menan_buttes_bbox_3857 <- st_transform(menan_buttes_bbox_wgs84, 3857)

# get elevation tiles with terrainr::get_tiles
resolution <- 2
output_tiles_2m <- get_tiles(menan_buttes_bbox_3857,
                             services = "elevation", # we just want elevation, not imagery
                             resolution = resolution, # pixel side length in meters
                             projected = T, bboxSR = 3857)

# convert to a raster I can use with rayshader
elevation <- output_tiles_2m[[1]] %>% 
  merge_rasters() %>% 
  raster_to_matrix()

# read in data for the Snake River
# originally I did this straight from OSM with the osmdata package,
# but it didn't handle doughnut geometries well so I got it via the QuickOSM QGIS plugin in the end
snake <- read_sf(dsn = "Snake.gpkg", 
                 layer = "Snake_poly")

# you can pipe all the following bits directly into one more concise code chunk,
# but it's nice to have the different layers separated out so you can tweak each one
# independently without having to do all the raytracing etc every time you 
# want to see how your tweak will look

# colours for the sphere_shade texture
texture = create_texture("#f5dfca","#63372c","#dfa283","#195f67","#c9e3c5",
                         cornercolors = c("#ffc500", "#387642", "#d27441","#296176"))
plot_map(texture) # nice way to have a look at them

# the actual raytracing bit to get the big beautiful shadows
system.time( # times how long it takes (it can be quite a while)
  raymat <- ray_shade(elevation, zscale = resolution/5, 
                      # silly low sun altitude for extra long shadows
                      anglebreaks = seq(13.5, 16.5, by = 0.02))
) # 2 hrs 17 mins to run on my laptop

# have a look at the layer it produces
raymat %>% plot_map()

# it's a bit dark in the flat areas for my taste, so I'll transform it:

raymat2 <- raymat # to get a matrix the right size
# there's probably an easier / more sensible way to transform the matrix than this
# but this worked and made sense to me
raymat2[] <- vapply(raymat, function(x) ((1.3*x-0.3)^3 - (1.3*x-0.5)^2), numeric(1))

# explicitly rescaling (so the values are between 0 and 1) is necessary here
# but it's done automatically later in add_shadow()
raymat2 %>% scales::rescale() %>% plot_map()

river <- generate_polygon_overlay(st_transform(snake, crs=crs(menan_buttes_bbox_3857)), 
                                  extent = raster::extent(st_bbox(menan_buttes_bbox_3857)), 
                                  heightmap = elevation, palette="#5f8cc7",
                                  linewidth = .4, linecolor = "#95B2CB")

elevation %>% 
  sphere_shade(texture, sunangle = 0, colorintensity = 5) %>%
  add_overlay(river, alphalayer = 0.7) %>% 
  add_shadow(raymat2, max_darken = 0.2) %>%
  plot_map(maxpixels = length(elevation))