library(tidyverse)
library(osmdata)
library(sf)

available_tags("highway")
available_features()


highway <- getbb("Berlin Germany") %>% 
  opq() %>% 
  add_osm_feature(key = "highway",
                  value = c("motorway", 
                            "primary",
                            "secondary",
                            "tertiary")) %>%
  osmdata_sf()

small_streets <- getbb("Berlin Germany")  %>% 
  opq() %>% 
  add_osm_feature(key = "highway", 
                  value = c("residential", 
                            "living_street",
                            "unclassified"#"service"
                  )) %>%
  osmdata_sf()

runway <- getbb("Berlin Germany")  %>% 
  opq() %>% 
  add_osm_feature(key = "aeroway", 
                  value = c("runway")) %>%
  osmdata_sf()

taxiway <- getbb("Berlin Germany")  %>% 
  opq() %>% 
  add_osm_feature(key = "aeroway", 
                  value = c("taxiway")) %>%
  osmdata_sf()

airport <- getbb("Berlin Germany")  %>% 
  opq() %>% 
  add_osm_feature(key = "aeroway", 
                  value = c("aerodrome")) %>%
  osmdata_sf()

river <- getbb("Berlin Germany")  %>% 
  opq() %>% 
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()

water <- getbb("Berlin Germany") %>% 
  opq() %>% 
  add_osm_feature(key = "natural",
                  value = c("water")) %>% 
  osmdata_sf()

rail <- getbb("Berlin Germany") %>% 
  opq() %>% 
  add_osm_feature(key = "railway",
                  value = c("rail", "light_rail")) %>% 
  osmdata_sf()

# plot map ------------------------------------------------------------------------------
ggplot() +
  geom_sf(data = highway$osm_lines,
          inherit.aes = FALSE,
          color = "white",
          size = .5,
          alpha = .8) +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = "white",
          size = .1,
          alpha = .4) +
  geom_sf(data = runway$osm_lines,
          inherit.aes = FALSE,
          color = "white",
          size = 1,
          alpha = .8) +
  geom_sf(data = taxiway$osm_lines,
          inherit.aes = FALSE,
          color = "white",
          size = .1,
          alpha = .4) +
  geom_sf(data = rail$osm_lines,
          inherit.aes = FALSE,
          color = "#F0D722",
          size = .2,
          alpha = 0.5) + 
  geom_sf(data = river$osm_lines,
          inherit.aes = FALSE,
          color = "#7fc0ff",
          size = .8,
          alpha = 0.8) + 
  geom_sf(data = water$osm_polygons,
          inherit.aes = FALSE,
          fill = "#7fc0ff",
          size = 0.01) +
  coord_sf(xlim = c(13.25, 13.55),
           ylim = c(52.305, 52.57),
           expand = FALSE) +
  geom_text(aes(label = "BERLIN", x = 13.4, y = 52.32),
            color = "white",
            family = "Source Sans Pro",
            size = 16,
            hjust = "outward") +
  geom_segment(inherit.aes = FALSE,
               aes(x = 13.33, 
                   y = 52.335, 
                   xend = 13.47, 
                   yend = 52.335), 
               size = 1,
               color = "white") +
  theme(
    plot.background = element_rect(fill = "#282828"),
    panel.background = element_rect(fill = "#282828"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    plot.margin = margin(15, 15, 0, 15)
  )
  
ggsave("./Berlin_2.png", width = 9, height = 16, dpi = 320)

