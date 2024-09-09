library(tidyverse)
library(sf)
library(mapdata)
library(mapsf)
library(readxl)

Ghana_district <- st_read("G:/GeoSpatial Stat R/Maps and SHPfiles/Shapefiles/Districts 170/Ghana_Districts_170.shp")
Ghana_district


st_crs(Ghana_district) # WGS 84

Ashanti_region <- Ghana_district %>% 
  filter(REGION == "ASHANTI")



Ghana <- st_read("G:/GeoSpatial Stat R/Maps and SHPfiles/Shapefiles/Ghana_Shapefile_(New)/Ghana_Shapefile_(New).shp")
Ghana

st_crs(Ghana) # Accra / Ghana National Grid

ggplot() + geom_sf(data = Ghana, aes(fill = REGION)) + 
  scale_fill_viridis_d() +
  theme(legend.position = "none")


Ghana_District_capital <- st_read("G:/Project_Tacana/gh/GHA_District_Capitals.shp")
Ghana_District_capital


st_crs(Ghana_District_capital) # WGS 84


mp <- st_read("G:/Project_Tacana/gh/ff6d4f1a-07cd-46c4-b260-eb3356687ab6202045-1-jgmzb2.17p4.shp")
mp 


Towns <- c("Ayeduase", "Deduako", "South Danyame", "Danyame", "Atimatim",
                  "Apemso", "Buokrom Estate", "Kentikrono", "Ahinsan","Kotei",
                  "Kenyase","Atonsu", "Krofrom", "Patasi","Apemsu", "Antoa",
                  "Barekese", "Ahumasu",  "Aputuogya", "Emena", "Ejisu-Besase",
                  "Asokore-Mampong", "Abrepo","Bohay", "Appiedu", "Tikrom",
                  "Afrancho", "Boadi","Ataseman", "Apiedu", "Asokwa", "Santase",
                  "Asisiwa","Abidjan Nkwanta","Abuatem","Onwe", "Woarekese",
                  "Esereso", "Adagya", "Domeabra","Apromase") %>% as_tibble()



Tick_Infestation <- read_csv("G:/Project_KCCR/Project_2/Tick Infestations on Dogs/Clean Data ANALYSIS.csv")

tick_map <- Tick_Infestation %>%
  select(location, tick_infestation)




filter(Tick_Infestation, facility == "Bosomtwe District") %>% 
  group_by(location) %>% 
  summarise(town = sum(tick_infestation))

tick_map %>% 
  group_by(location) %>% 
  summarise(town = sum(tick_infestation))


p1 <- read_sf("G:/GeoSpatial Stat R/Maps and SHPfiles/Shapefiles/GH/gadm41_GHA_0.shp")
p2 <- read_sf("G:/GeoSpatial Stat R/Maps and SHPfiles/Shapefiles/GH/gadm41_GHA_1.shp")
p3 <- read_sf("G:/GeoSpatial Stat R/Maps and SHPfiles/Shapefiles/GH/gadm41_GHA_2.shp")


ggplot() + geom_sf(data = p3)

pm = filter(p3, NAME_1 == "Ashanti") %>% 
  select(NAME_2) %>% rename(Districts = NAME_2)

ggplot() + geom_sf(data = pm, aes(fill = Districts)) + 
  labs(title = "Distribution of Tick Infestation in the Ashanti Region.", 
       subtitle = "Few selected towns in the Ashanti region.", 
       caption = "Source:: DataMaestro")


dist_town <- read_excel("G:/Project_KCCR/Project_2/Tick Infestations on Dogs/Districts and Towns.xlsx")

dist_town <- dist_town %>% 
  mutate(percentage = round((Tick_Infestation/sum(Tick_Infestation)) * 100, 2)) 

am = pm %>% 
  inner_join(dist_town, by = "Districts") %>% 
  mutate_if(is.character, factor)

fm <- pm %>% 
  left_join(dist_town, by = "Districts")

TK1 <- ggplot() + geom_sf(data = pm) + geom_sf(data = am, aes(fill =  Districts)) +
  theme_light() + labs(
    title = "Geographical Presentation of the Prevalence of Tick Infestation of Selected Districts in the Ashanti Region.",
    subtitle = "Eight(8) Selected Districts Comprising of various Towns."
  ) + scale_fill_manual(values = c("red", "green", "purple", "blue", "orange", "pink", "brown"),
                          labels = c("Asokwa" = "Asokwa (5.16%)",
                                   "Atwima-Kwanwoma" = "Atwima-Kwanwoma (0.56%)",
                                   "Atwima-Nwabiagya North" = "Atwima-Nwabiagya North (0.35%)",
                                   "Bosomtwe" = "Bosomtwe (51.26%)",
                                   "Ejisu" = "Ejisu (4.67)",
                                   "Kumasi" = "Kumasi (2.86%)",
                                   "Oforikrom" = "Oforikrom (26.29%)")) + theme(legend.text = element_text(size = 20,
    face = "italic", family = "serif"), legend.title = element_text(size = 20,
    face = "bold.italic", family = "serif")) + 
  theme(plot.subtitle = element_text(family = "serif",
    size = 20, face = "italic"), axis.title = element_text(family = "serif",
    face = "italic"), plot.title = element_text(family = "serif",
    size = 20, face = "bold.italic"),
    panel.grid.major = element_line(colour = "black",
                                    linetype = "dashed",
                                    size = .5),
    panel.grid.minor = element_blank(),
    panel.ontop = TRUE, 
    panel.background = element_rect(fill = NA, colour = "black")) 

# TK

TK2 <- ggplot() + geom_sf(data = am, aes(fill =  Districts))  +
  geom_sf_label(data = am, aes(label = paste0(percentage, "%")), alpha = 0, 
                fontface = "bold", ) + theme_light() + labs(
    title = "Geographical Presentation of the Prevalence of Tick Infestation of Selected Districts in the Ashanti Region.",
    subtitle = "Eight(8) Selected Districts Comprising of various Towns.", 
    x = "Longitude", y = "Latitude"
  ) + scale_fill_manual(values = c("red", "green", "purple", "blue", "orange", "pink", "brown")) + 
  theme(legend.text = element_text(size = 20,
    face = "italic", family = "serif"), legend.title = element_text(size = 20,
    face = "bold.italic", family = "serif")) +
  theme(plot.subtitle = element_text(family = "serif",
    size = 20, face = "italic"), axis.title = element_text(family = "serif",
    face = "italic"), plot.title = element_text(family = "serif",
    size = 20, face = "bold.italic"),
    panel.grid.major = element_line(colour = "black",
                                    linetype = "dashed",
                                    size = .5),
    panel.grid.minor = element_blank(),
    panel.ontop = TRUE, 
    panel.background = element_rect(fill = NA, colour = "black")) 


ggsave("TK1.png", width = 60, height = 30, dpi = 1000, 
       units = "cm", limitsize = F, plot = TK1)

ggsave("TK2.png", width = 60, height = 30, dpi = 1000, 
       units = "cm", limitsize = F, plot = TK2)

