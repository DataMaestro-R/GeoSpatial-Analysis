library(tidyverse)
library(sf)
library(mapdata)
library(mapsf)
library(readxl)
library(rnaturalearth)


Ghana <- st_read("G:/GeoSpatial Stat R/Maps and SHPfiles/Shapefiles/Ghana_Shapefile_(New)/Ghana_Shapefile_(New).shp")
Ghana

ggplot() + geom_sf(data = jd, aes(fill = TOTAL)) + 
  theme(legend.position = "none")


preval_child_labour <- read_excel("G:/Project_Paperclip/Kelvin Dzamashi/CHILD_LABOR.xlsx", 
                                  sheet = "preval_region")
child_labour <- preval_child_labour %>% 
  mutate(Region = str_to_upper(Region)) %>% 
  rename(REGION = Region)


jd <- inner_join(Ghana, child_labour, by = "REGION")



ghana_1 <- st_read("G:/GeoSpatial Stat R/Maps and SHPfiles/Ghana SHP files/New folder/49870dd9-693d-4511-9913-129c9c4ae2062020231-1-16cfip8.k3tx.shp")

ghana_2 <- st_read("G:/GeoSpatial Stat R/Maps and SHPfiles/Ghana SHP files/New folder/Ghana Shapefile (New)/Ghana_Shapefile_(New).shp")



j <- inner_join(ghana_1, child_labour, by = "REGION")

ggplot() + geom_sf(data = j, aes(fill = REGION, size = TOTAL))






data <- j %>%
  mutate(TOTAL_proportion = TOTAL / sum(TOTAL))


ggplot(data) +
  geom_sf(aes(fill = TOTAL_proportion)) +
  scale_fill_continuous(name = "Proportion of \n TOTAL", labels = scales::percent,
                        trans = "reverse") +
  theme_minimal() +
  labs(title = "Child labour") + 
  theme(legend.position = "top")


data_centroids <- st_centroid(data)

 ggplot() +
  geom_sf(data = data, fill = "white", color = "black") +  # Base map with region boundaries
  geom_sf(data = data_centroids, aes(size = TOTAL), color = "red") +  # Points representing TOTAL
  scale_size_continuous(name = "TOTAL", range = c(1, 10)) +  # Adjust the range as needed
  theme_minimal() +
  labs(title = "CHILD LABOR")


 ggplot(data) +
  geom_sf(aes(fill = TOTAL)) +
  geom_sf_label(aes(label = REGION), size = 2.5) +
  scale_fill_continuous(name = "TOTAL", trans = "reverse") +
  theme_minimal() +
  labs(title = "CHILD LABOR") + 
   theme(legend.position = "top")


# Define your custom color palette
custom_colors <- c("#d73027", "#f46d43", "#fdae61", "#fee08b", "#d9ef8b",
                   "#a6d96a", "#66bd63", "#1a9850")

# Plot the map with manually defined colors
 ggplot(data) +
  geom_sf(aes(fill = TOTAL)) +
  geom_sf_label(aes(label = REGION), size = 2.5) +
  scale_fill_gradientn(name = "TOTAL", colors = custom_colors, trans = "reverse") +
  theme_minimal() +
  labs(title = "CHILD LABOR", y = "", x = "")  
   


library(patchwork)


a1 + a3


########################################################



########################################################

theme_set(theme_minimal())

educational_background <- read_excel("G:/Project_Paperclip/Kelvin Dzamashi/CHILD_LABOR.xlsx", 
         sheet = "educational_background") %>% janitor::clean_names() %>% 
  mutate_if(is.character, factor)


edu_back <- educational_background %>% 
  pivot_longer(cols = c(never_attended, nursery, kindergarten, 
                        primary), names_to = "educational_background", 
               values_to = "frequency")



# viz 1

educational_background %>% 
  mutate(region = fct_reorder(region, total)) %>% 
  ggplot(
    aes(
      y = region, 
      x = total,
      fill = region
    )
  ) + geom_col(
    color = "black"
  ) +
  scale_x_continuous(labels = scales::number) + 
  theme(legend.position = "none")



# viz 2

edu_back %>% 
  select(-total) %>% 
  mutate(region = fct_reorder(region, frequency)) %>% 
  ggplot(
    aes(
      y = region, 
      x = frequency,
      fill = region
    )
  ) + geom_col(position = "dodge", color = "black")  + 
  facet_wrap(~educational_background, scales = "free")+
  scale_x_continuous(labels = scales::number)+ 
  theme(legend.position = "none")




sex <- read_excel("G:/Project_Paperclip/Kelvin Dzamashi/CHILD_LABOR.xlsx", 
                  sheet = "sex_region") %>% janitor::clean_names() %>% 
  mutate_if(is.character, factor)


















