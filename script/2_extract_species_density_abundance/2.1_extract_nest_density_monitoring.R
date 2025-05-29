#Title: Extract the density of species measure at different scales
#Date: May 29 2025
#Last update: NA

#-----------------#
#### Libraries ####
#-----------------#
#data manipulation
library(dplyr)
#spatial data manipulation
library(sf)
sf_use_s2(FALSE) #prevent geoprocessing errors
#Personnal functions to estimate species abundance
source("script/functions/FUNS.R")


#-----------------#
#### Read data ####
#-----------------#
#All nests found
nests_sf <- read_sf("data/raw/shapefiles/sampling/nests_2004-2023.shp")
nests_hawk_falcon <- read.csv("data/raw/sampling/hawk_falcon_nest_zone.csv")

#Species monitoring by zone
species_monitoring <- read.csv("data/metadata/systematic_nest_monitoring_clean.csv") %>% 
  dplyr::filter(sampled== "yes") %>% 
#remove Arctic fox and American ermine since we only have indirect indexes of density
  dplyr::filter(!c(species %in% c("american ermine", "arctic fox", "nearctic brown lemming", "nearctic collared lemming")))

#study area
study_area <- read_sf("data/raw/shapefiles/study_area.shp") %>% 
  dplyr::filter(zone != "goose_colony") %>% 
  dplyr::select(-year)


#---------------------------------------------------------#
#### Extract density of birds with coordinates of nest ####
#---------------------------------------------------------#
#Bird species for which we have nest coordinates
nests_with_coordinates <- species_monitoring %>% 
  dplyr::filter(!c(species %in% c("rough legged hawk", "peregrine falcon")))

#Calculate nest density
nest_density_with_coordinates <- get_nest_density(
  nest_data= nests_sf,
  zone_sf= study_area,
  species_list = unique(bird_monitoring_with_coordinates$species),
  zone_list= unique(bird_monitoring_with_coordinates$zone),
  year_list= min(bird_monitoring_with_coordinates$year): max(bird_monitoring_with_coordinates$year))

#Format data frame
nest_density_with_coordinates <- nest_density_with_coordinates %>% 
  dplyr::left_join(bird_monitoring_with_coordinates) %>% 
  na.omit() %>% 
  dplyr::select(-sampled) %>% 
  dplyr::left_join(study_area %>% 
                     sf::st_drop_geometry(), by= "zone") %>% 
  dplyr::select(species, year, zone, area_km2, monitoring, nb_nest, nest_density_km2) %>% 
  dplyr::rename(number_of_nests=nb_nest)


#------------------------------------------------------------#
#### Extract density of birds without coordinates of nest ####
#------------------------------------------------------------#
#Bird species for which we do not have nest coordinates
nests_without_coordinates <- species_monitoring %>% 
  dplyr::filter(species %in% c("rough legged hawk", "peregrine falcon"))

#Extract number of nests per zone
nests_density_without_coordinates <- nests_hawk_falcon %>% 
  dplyr::group_by(species, year, zone) %>% 
  dplyr::summarise(number_of_nests= n())

#Format data frame
nests_density_without_coordinates <- nests_without_coordinates %>% 
  dplyr::left_join(nests_density_without_coordinates)
  
  nests_density_without_coordinates %>% 
  
  na.omit() %>% 
  dplyr::left_join(study_area %>%
                     sf::st_drop_geometry(), by= "zone") %>% 
  dplyr::select(species, year, zone, area_km2, monitoring, number_of_nests) %>%
  dplyr::mutate(nest_density_km2= number_of_nests/area_km2)
