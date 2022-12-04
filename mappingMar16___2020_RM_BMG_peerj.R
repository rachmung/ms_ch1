
# Load packages -----------------------------------------------------------
library(ggspatial)
library(tidyverse)
library(viridis)

# Set default ggplot2 to black-and-white
theme_set(theme_bw())

# Load data ---------------------------------------------------------------

bahamas <- raster::getData("GADM", country = "BHS", level = 0)
coords  <- read.csv("rocksound_coords_peerj.csv")


# Plot Rock Sound map -----------------------------------------------------

rocksound <-
  ggplot() +
  layer_spatial(data = bahamas, colour = "black") +
  geom_spatial_point(data = coords, alpha = 0.7, size = 5.5, pch = 21, crs = 4326,
                    aes(long, lat, fill = total_cuke_den_sg)) +
  annotation_scale(location = "bl", style="bar", pad_x = unit(0.4,"cm"), width_hint = 0.2) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         height = unit(2, "cm"), width = unit(1.8, "cm"),
                         pad_x = unit(0.1,"cm"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-76.35, -76.16), ylim = c(24.78,24.9)) +
  labs(x = "Longitude", y = "Latitude") +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank()) +
  scale_colour_viridis(name = "Sea cucumber \ndensity (individuals/"~m^2~")") +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=10,face="bold"))

#look at part 1 of map
rocksound

# Plot Eleuthera map ------------------------------------------------------

eleuthera <-
  ggplot() +
  layer_spatial(data = bahamas, fill = "grey", colour = "black") +
  annotate("rect", xmin=-76.15, xmax=-76.36, ymin=24.77, ymax=24.9,
           alpha=0, color="red") +
  annotation_scale(location = "bl", style="bar", pad_x = unit(1.2,"cm")) +
  coord_sf(xlim = c(-76.9,-76.1), ylim = c(24.6,25.56)) +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank(),
              plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
              panel.border = element_rect(colour = "black")) +
  labs(x = "Longitude", y = "Latitude") 



#look at part 2 of map (inset)
eleuthera

# FIGURE in PEERJ Manuscript MAP FIGURE (MSc CH1 FIG 1) - Eleuthera map as inset of Rock Sound map ------------------------

combined <-
  rocksound +
  annotation_custom(
    grob = ggplotGrob(eleuthera),
    xmin = -76.40,
    xmax = -76.24,
    ymin = 24.8407,
    ymax = 24.907) +
  #annotate(geom="text", x=-76.258, y=24.895, label="Rock Sound",
           #color="black", size = 8) +
  annotate(geom="text", x=-76.322, y=24.8772, label="Eleuthera\nIsland",
           color="black", size = 5) +
  #annotate(geom="point", x=-76.335, y=24.8345, color="red", size = 3.5) +
  #annotate(geom="text", x=-76.325, y=24.836, label="CEI",
           #color="red", size = 6) +
  annotate(geom="text", x=-76.24, y=24.8, label="Rock Sound",
           color="black", size = 6) +
  theme( 
  axis.title.x = element_text(size=17),
  axis.title.y = element_text(size=17),
  axis.text.y = element_text(size=18),
  axis.text.x = element_text(size=18),
  legend.title = element_text(size = 16),
  legend.text = element_text(size = 16))

# look at final version of map with inset
combined



coords %>% 
  summarise(mean(ft_density_grass), mean(dd_density_grass))
