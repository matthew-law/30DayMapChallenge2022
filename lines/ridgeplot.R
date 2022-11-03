# code adapted from https://dieghernan.github.io/202205_Unknown-pleasures-R/

# not really sure I need all of these
library(sf)
library(terra)
library(raster)
library(elevatr)
library(ggplot2)
library(dplyr)
library(ggridges)

# define the bounds of the map in degrees (can be decimals if you want more precision)
lon <-  c(56, 62)
lat <-  c(58, 63)
coords <- data.frame(lon, lat)

# make a polygon
poly <- cbind(
  coords$lon[c(1,2,2,1,1)], 
  coords$lat[c(1,1,2,2,1)]) %>% 
  list() %>% 
  st_polygon() %>% 
  st_sfc(crs = 4326) # pseudo-Mercator in degrees

# get elevation data for the area of the polygon
dem <- get_elev_raster(poly, z = 4, clip = "bbox") %>%
  rast() %>% # convert to terra
  terra::project("EPSG:3857") # pseudo-Mercator in metres

terra::plot(dem) # have a look

names(dem) <- "elev" # will reference this later in geom_ridgeline()

# plot_rows is (roughly) how many rows you want in the plot
plot_rows <- 90
dem_agg <- aggregate(dem, round(nrow(dem) / plot_rows))
dem_df <- as.data.frame(dem_agg, xy = TRUE, na.rm = FALSE)
dem_df$y %>% unique() %>% length() # ended up with 98 rows

poly_3857 <- poly %>% st_transform(3857) # to same crs as the raster

# plot!
ggplot() +
  geom_sf(data = poly_3857, color = NA, fill = NA) +
  geom_ridgeline(
    data = dem_df, aes(
      x = x, y = y,
      group = y,
      height = elev
    ),
    scale = 100, # change the elevation exaggeration
    fill = "black",
    color = "white",
    size = .3 # line thickness
  ) +
  theme_void() +
  theme(plot.background = element_rect(fill = "black"))

# save the plot
ggsave("Urals ridgeplot.png", height = 2000, width = 1200, units = "px")

# I cheated and added the text in PowerPoint but you could easily so do in R
# if you were so inclined