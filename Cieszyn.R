library(tidyverse)
library(osmdata)
library(sf)

highway <- getbb("Cieszyn Poland") %>% 
  opq() %>% 
  add_osm_feature(key = "highway",
                  value = c("motorway", 
                            "trunk",
                            "primary",
                            "secondary",
                            "tertiary",
                            "motorway_link",
                            "trunk_link",
                            "primary_link",
                            "secondar_link",
                            "tertiary_link")) %>%
  osmdata_sf()

small_streets <- getbb("Cieszyn Poland")  %>% 
  opq() %>% 
  add_osm_feature(key = "highway", 
                  value = c("residential")) %>%
  osmdata_sf()

junctions <- getbb("Cieszyn Poland")  %>% 
  opq() %>% 
  add_osm_feature(key = "junction", 
                  value = c("roundabout")) %>%
  osmdata_sf()

mini_streets <- getbb("Cieszyn Poland")  %>% 
  opq() %>% 
  add_osm_feature(key = "highway", 
                  value = c("living_street",
                            "unclassified")) %>% 
  osmdata_sf()
                  
service_streets <- getbb("Cieszyn Poland")  %>% 
  opq() %>% 
  add_osm_feature(key = "highway", 
                  value = c("service",
                            "track",
                            "pedestrian",
                            "road")) %>% 
  osmdata_sf()




river <- getbb("Cieszyn Poland")  %>% 
  opq() %>% 
  add_osm_feature(key = "waterway", value = c("river",
                                              "riverbank")) %>%
  osmdata_sf()

stream <- getbb("Cieszyn Poland")  %>% 
  opq() %>% 
  add_osm_feature(key = "waterway", 
                  value = c("stream", "ditch", "drain")) %>%
  osmdata_sf()

water <- getbb("Cieszyn Poland") %>% 
  opq() %>% 
  add_osm_feature(key = "natural",
                  value = c("water")) %>% 
  osmdata_sf()

rail <- getbb("Cieszyn Poland") %>% 
  opq() %>% 
  add_osm_feature(key = "railway",
                  value = c("rail", 
                            "light_rail",
                            "disused")) %>% 
  osmdata_sf()

# plot map ------------------------------------------------------------------------------
cieszyn <- ggplot() +
  geom_sf(data = stream$osm_lines,
          inherit.aes = FALSE,
          color = "#7fc0ff",
          size = .7) + 
  geom_sf(data = filter(river$osm_lines, name %in% c("Puńcówka", "Bobrówka")),
          inherit.aes = FALSE,
          color = "#7fc0ff",
          size = 1) + 
  geom_sf(data = filter(river$osm_lines, !name %in% c("Puńcówka", "Bobrówka")),
          inherit.aes = FALSE,
          color = "#7fc0ff",
          size = 1) + 
  geom_sf(data = water$osm_polygons,
          inherit.aes = FALSE,
          fill = "#7fc0ff",
          size = 0.01) +
  geom_sf(data = rail$osm_lines,
          inherit.aes = FALSE,
          color = "#D13434",
          size = .75) + 
  geom_sf(data = service_streets$osm_lines,
          inherit.aes = FALSE,
          color = "#B6B6B6",
          size = .3, 
          alpha = 0.8) +
  geom_sf(data = mini_streets$osm_lines,
          inherit.aes = FALSE,
          color = "#B6B6B6",
          size = .4,
          alpha = 0.85) +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = "white",
          size = .55) +
  geom_sf(data = junctions$osm_polygons,
          inherit.aes = FALSE,
          fill = NA,
          color = "white",
          size = .5) +
  geom_sf(data = highway$osm_lines,
          inherit.aes = FALSE,
          color = "white",
          size = .85) +
  coord_sf(xlim = c(18.60, 18.70),
           ylim = c(49.715, 49.79),
           expand = FALSE) +
  theme(
    plot.background = element_rect(fill = "#282828"),
    panel.background = element_rect(fill = "#282828"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.margin = margin(25, 25.7, 251, 25.7)
    )
cieszyn

ggsave(plot = cieszyn, "./Cieszyn.pdf", width = 29.7, height = 42, unit = "cm", dpi = 320, limitsize = FALSE)
