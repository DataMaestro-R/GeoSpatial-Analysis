# GEOSPATIAL ANALYSIS OF CHILD LABOR IN GHANA.

pacman::p_load(tidyverse, sf, mapdata, mapsf, readxl, rnaturalearth)

theme_set(theme_minimal())

label_scale <- theme(plot.subtitle = element_text(size = 11, face = "bold"), 
                     plot.caption = element_text(size = 10, face = "bold"), 
                     axis.title = element_text(size = 15, face = "bold"), 
                     axis.text.x = element_text(size = 15, face = "bold",
                                                colour = "black"),
                     axis.text.y = element_text(size = 15, face = "bold",
                                                colour = "black"),
                     plot.title = element_text(size = 20, face = "bold"),
                     legend.text = element_text(size = 10, face = "bold"), 
                     strip.text = element_text(face = "bold", size = 14), 
                     legend.title = element_text(size = 13, face = "bold"), 
                     legend.background = element_rect(fill = NA))

custom_colors <- c("#d73027", "#f46d43", "#fdae61", "#fee08b", "#d9ef8b",
                   "#a6d96a", "#66bd63", "#1a9850")

Ghana <- st_read(choose.files())
Ghana_1 <- st_read(choose.files())
Ghana_2 <- st_read(choose.files())



Preval_Child_Labour <- read_excel(choose.files())

Child_Labour <- Preval_Child_Labour %>% 
  mutate(Region = str_to_upper(Region)) %>% 
  rename(REGION = Region)


GhanaChildLAbour <- inner_join(Ghana_1, Child_Labour, by = "REGION")

GeoSpatData <- GhanaChildLAbour %>%
  mutate(Total_per_thousand = (TOTAL / 10000) %>% round(2))

Data_Centroids <- st_centroid(GeoSpatData)

map_point <- ggplot() +
  geom_sf(data = GeoSpatData, fill = "white", color = "black") +  
  geom_sf(data = Data_Centroids, aes(size = Total_per_thousand), color = "red") +  
  scale_size_continuous(name = "", 
                        range = c(1, 10)) + 
  theme(legend.position = "none")  +
  label_scale 
 


map_fill <- ggplot(GeoSpatData) +
  geom_sf(aes(fill = Total_per_thousand)) +
  geom_sf_label(aes(label = REGION), size = 4) +
  scale_fill_gradientn(name = "Frequency of \nChild Labour \nper 10,000", 
                       colors = custom_colors, trans = "reverse") +
  labs(, y = "", x = "") + label_scale


library(patchwork)

map_point + 
  map_fill +
  plot_annotation(title = "Prevalence of Child Labor in Ghana.", 
                  subtitle = "A Geospatial Analysis of Child Labor Across the 16 Regions of Ghana.",
                  theme = label_scale)
  
