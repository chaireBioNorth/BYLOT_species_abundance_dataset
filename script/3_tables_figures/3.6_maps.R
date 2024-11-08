#title: Create a map of the study area with the different zones
#author: Louis Moisan
#date: October 4 2023

#------------------#
#### librairies ####
#------------------#
library(dplyr)
library(sf)
library(ggmap)
library(ggplot2)

# To extract background satellite map from google, one need to register to have a API key
#!!! Need a personal API key from google
# https://developers.google.com/maps
#You have to create a API key and then under "Key and identifiers" you should see your key with the possibility to click on "show key", copy paste the key in the function below
ggmap::register_google(key = "", write = TRUE)

# Define the bounding box (latitude and longitude) for your area of interest
bbox <- c(left = -80.19, bottom = 72.75, right = -78.5, top = 73.3)

#Define only zones of interest
zones <- c("camp 2", "goose point","malaview","qarlikturvik valley",  "camp 3", "black plateau", "south plateau", "camp 3 plateau", "dufour")
  
#import study area
study_area <- sf::st_read("data/raw/shapefiles/study_area.shp") %>% 
  dplyr::filter(zone %in% zones) %>% 
# Transform nc to EPSG 3857 (Pseudo-Mercator, what Google uses)
 st_transform(3857)


#layer of the goose colony
goose_colony <- sf::st_read("data/raw/shapefiles/study_area.shp") %>%
  dplyr::filter(year== 2017, zone== "goose_colony") %>% 
  sf::st_transform(3857)

#color palette
colors_palette <- c("#bd959d","#84a98c", "#94214b", "#463a49","#ad382b","#096377","#63141c", "#003049", "#d4af37", "white")

#Get the google map background satellite
map <- get_map(location = c(lon = -79.7, lat = 73), maptype = "satellite", source = "google", zoom= 8)

# Define a function to fix the bbox to be in EPSG:3857
ggmap_bbox <- function(map) {
  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector, 
  # and set the names to what sf::st_bbox expects:
  map_bbox <- setNames(unlist(attr(map, "bb")), 
                       c("ymin", "xmin", "ymax", "xmax"))
  
  # Coonvert the bbox to an sf polygon, transform it to 3857, 
  # and convert back to a bbox (convoluted, but it works)
  bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))
  
  # Overwrite the bbox of the ggmap object with the transformed coordinates 
  attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
  attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
  attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
  attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
  map
}

# Use the function:
map <- ggmap_bbox(map)

#Reorder factor level to define order in the map
study_area <- study_area %>% 
  dplyr::mutate(zone=factor(zone, levels= c("black plateau", "qarlikturvik valley","south plateau", "camp 3", "camp 3 plateau", "malaview", "camp 2" ,"goose point", "dufour")))

ggmap(map) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = study_area, aes(fill = zone, color= zone), lwd= 0.8,inherit.aes = FALSE)+
  scale_fill_manual(values= alpha(colors_palette,0.5))+
  scale_color_manual(values= alpha(colors_palette, 0.9),
                     labels=c("camp 2"=  expression("Camp 2 - 80 km"^2), "goose point"= expression("Goose point - 41 km"^2),"malaview"= expression("Malaview - 60 km"^2),"qarlikturvik valley"= expression("Qarlikturvik valley - 65 km"^2), "camp 3"= expression("Camp 3 - 33 km"^2), "black plateau"= expression("Black plateau - 34 km"^2), "south plateau"= expression("Southern plateau - 12 km"^2), "camp 3 plateau"= expression("Camp 3 plateau - 30 km"^2), "dufour"= expression("Pointe Dufour - 33 km"^2), "goose_colony"= expression("")))+
  geom_sf(data = goose_colony,fill= NA, aes(color=zone), lwd= 0.8, linetype = "twodash", inherit.aes = FALSE)+
  ggplot2::coord_sf(xlim = c(-8822069.645366931, -8926709.966712607), ylim = c(12028963.853099173, 12238691.808127215))+ # set the map lat/lon limits
  ggplot2::xlab("Longitude") + 
  ggplot2::ylab("Latitude")+
  labs(color= "Zones of the study area")+
  guides(fill= "none")+
  theme_bw()

#export the plots
ggsave("MetadataS1/figures/zones_study_area.pdf", width= 18, height= 25, units = c("cm"), dpi= 500)


#-------------------------------------------------------#
#### Map with transects, goose colony and study area ####
#-------------------------------------------------------#
#transects
transects <- sf::st_read("data/raw/shapefiles/sampling/vertebrate_count_transects_2010_2023.shp") %>% 
  dplyr::select(transect) %>% 
  unique() %>% 
  sf::st_transform(3857)

#extract number of transect per zone
nb_transect_zone <- study_area %>% 
  sf::st_intersection(transects) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::group_by(zone) %>% 
  dplyr::summarise(nb_transect= n(), .groups = "drop")

#add zone with no transect
nb_transect_zone <- nb_transect_zone %>% 
rbind(data.frame(zone= 
  study_area$zone[which((study_area$zone %in% nb_transect_zone$zone) == FALSE)], nb_transect= 0))

ggmap(map) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = study_area, aes(fill = zone, color= zone), lwd= 0.8,inherit.aes = FALSE)+
  scale_fill_manual(values= alpha(colors_palette,0.5))+
  scale_color_manual(values= alpha(colors_palette, 0.9),
                     labels=c("camp 2"=  expression("Camp 2 - 81 transects"), "goose point"= expression("Goose point - 55 transects"),"malaview"= expression("Malaview - 70 transects"),"qarlikturvik valley"= expression("Qarlikturvik valley - 32 transects"), "camp 3"= expression("Camp 3 - 58 transects"), "black plateau"= expression("Black plateau - 0 transect"), "south plateau"= expression("Southern plateau - 0 transects"), "camp 3 plateau"= expression("Camp 3 plateau - 0 transect"), "dufour"= expression("Pointe Dufour - 35 transects")))+
  geom_sf(data= transects, lwd= 0.5,inherit.aes = FALSE, col= "white")+
  ggplot2::coord_sf(xlim = c(-8822069.645366931, -8926709.966712607), ylim = c(12028963.853099173, 12238691.808127215))+ # set the map lat/lon limits
  ggplot2::xlab("Longitude") + 
  ggplot2::ylab("Latitude")+
  labs(color= "Zones of the study area")+
  guides(fill= "none")+
  theme_bw()
  

#export the plots
ggsave("MetadataS1/figures/transects.pdf", width= 18, height= 25, units = c("cm"), dpi= 500)


#----------------------------------#
#### Map of 4x2 and 2x1 km plot ####
#----------------------------------#
#import plots delimitation
qarlikturvik <- sf::st_read("data/raw/shapefiles/study_area.shp") %>% 
  dplyr::filter(zone %in% c("4x2km plot", "2x1km plot", "camp 1","qarlikturvik valley")) %>% 
  dplyr::mutate(zone=factor(zone, levels= c("qarlikturvik valley", "camp 1","4x2km plot","2x1km plot"))) %>% 
  # Transform nc to EPSG 3857 (Pseudo-Mercator, what Google uses)
  st_transform(3857)

map_qarlikturvik <- get_map(location = c(lon = -79.92, lat = 73.15), maptype = "satellite", source = "google", zoom= 11)

map_qarlikturvik <- ggmap_bbox(map_qarlikturvik)

ggmap(map_qarlikturvik) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = qarlikturvik%>% dplyr::filter(zone== "qarlikturvik valley"), aes(geometry= geometry, color= zone), fill= c(alpha("#84a98c", 0.45)), lwd= 0.8, inherit.aes = FALSE)+
  geom_sf(data = qarlikturvik%>% dplyr::filter(zone== "camp 1"), aes(geometry= geometry, color= zone), fill= c(alpha("#c9ada7", 0.45)), lwd= 0.8, inherit.aes = FALSE)+
  geom_sf(data = qarlikturvik %>% dplyr::filter(zone== "4x2km plot"), aes(geometry= geometry, color=zone), fill = alpha("#faedcd",0.3), lwd= 0.8, inherit.aes = FALSE)+
 geom_sf(data = qarlikturvik %>% dplyr::filter(zone== "2x1km plot"), aes(geometry= geometry, color=zone), fill = alpha("#bc6c25",0.3), lwd= 0.8, inherit.aes = FALSE)+
  scale_color_manual(values= c(alpha("#84a98c", 0.9),
                               alpha("#c9ada7",0.9),
                               alpha("#faedcd",0.9),
                               alpha("#bc6c25",0.9)),
                     labels= c("Qarlikturvik valley\n-Snowy owl\n-Rough-legged hawk\n-Peregrine falcon\n-Glaucous gull\n-Red-throated loon\n-Pacific loon\n-Cackling goose\n-Tundra swan","South Qarlikturvik\n-Long-tailed jaeger\n-Sandhill crane\n-Parasitic jaeger","8 km2 plot\n-Long-tailed duck\n-King eider\n-Rock ptarmigan","2 km2  plot\n-Lapland longspur\n-Baird's sandpiper"))+
  ggplot2::coord_sf(xlim = c(-8874000, -8921000), ylim = c(12167000, 12200000))+ # set the map lat/lon limits
  ggplot2::xlab("Longitude") + 
  ggplot2::ylab("Latitude")+
  labs(color= "Nest searching plots within\nthe Qarlikturvik valley")+
  theme_bw()+
  guides(color=guide_legend(
    keywidth=0.2,
    keyheight=0.75,
    default.unit="inch")
  )+
  theme(legend.text=element_text(size=10))

#export the plots
ggsave("MetadataS1/figures/qarlikturvik_valley.pdf", width= 20, height= 18, units = c("cm"), dpi= 500)

