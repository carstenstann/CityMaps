library(tidyverse)
library(osmdata)
library(sf)
library(rgdal)

shape <- st_read("/Users/Carsten/Downloads/land-polygons-split-3857/land_polygons.shp")

available_tags("highway")
available_features()

getbb("Cieszyn Poland")

highway <- opq(bbox = c(-122.6, 37.5, -122.2, 37.9)) %>% 
  add_osm_feature(key = "highway",
                  value = c("motorway", 
                            "primary")) %>%
  osmdata_sf()

streets <- opq(bbox = c(-122.6, 37.5, -122.2, 37.9)) %>% 
  add_osm_feature(key = "highway",
                  value = c("secondary",
                            "tertiary")) %>%
  osmdata_sf()

small_streets <- opq(bbox = c(-122.6, 37.5, -122.2, 37.9)) %>% 
  add_osm_feature(key = "highway", 
                  value = c("residential", 
                            "living_street",
                            "unclassified"#"service"
                            )) %>%
  osmdata_sf()

runway <- opq(bbox = c(-122.6, 37.5, -122.2, 37.9)) %>% 
  add_osm_feature(key = "aeroway", 
                  value = c("runway")) %>%
  osmdata_sf()

taxiway <- opq(bbox = c(-122.6, 37.5, -122.2, 37.9)) %>% 
  add_osm_feature(key = "aeroway", 
                  value = c("taxiway")) %>%
  osmdata_sf()

airport <- opq(bbox = c(-122.6, 37.5, -122.2, 37.9)) %>% 
  add_osm_feature(key = "aeroway", 
                  value = c("aerodrome")) %>%
  osmdata_sf()

coastline <- opq(bbox = c(-122.6, 37.5, -122.2, 37.9)) %>%
  add_osm_feature(key = 'natural', value = 'coastline') %>% 
  osmdata_sf()

river <- opq(bbox = c(-122.6, 37.5, -122.2, 37.9)) %>% 
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()

water <- opq(bbox = c(-122.6, 37.5, -122.2, 37.9)) %>% 
  add_osm_feature(key = "natural",
                  value = c("water")) %>% 
  osmdata_sf()

bay <- opq(bbox = c(-122.6, 37.5, -122.2, 37.9)) %>% 
  add_osm_feature(key = "natural",
                  value = c("bay")) %>% 
  osmdata_sf()

ca <- map_data("state") %>% 
  filter(region == "california")


# plot map ------------------------------------------------------------------------------
ggplot() +
  geom_sf(data = highway$osm_lines,
          inherit.aes = FALSE,
          color = "white",
          size = .55,
          alpha = .8) +
  geom_sf(data = streets$osm_lines,
          inherit.aes = FALSE,
          color = "white",
          size = .3,
          alpha = .6) +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = "white",
          size = .15,
          alpha = .5) +
  geom_sf(data = runway$osm_lines,
          inherit.aes = FALSE,
          color = "white",
          size = .8,
          alpha = .8) +
  geom_sf(data = taxiway$osm_lines,
          inherit.aes = FALSE,
          color = "white",
          size = .1,
          alpha = .5) +
  geom_sf(data = coastline$osm_lines,
          inherit.aes = FALSE,
          color = "white",
          size = .4,
          alpha = .8) +
  geom_sf(data = water$osm_polygons,
          inherit.aes = FALSE,
          fill = "#7fc0ff",
          size = 0.01) +
  geom_sf(data = bay$osm_lines,
          inherit.aes = FALSE,
          color = "white",
          size = .4,
          alpha = .8) +
  coord_sf(xlim = c(-122.55, -122.223),
           ylim = c(37.52, 37.838),
           expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#282828")
  )




