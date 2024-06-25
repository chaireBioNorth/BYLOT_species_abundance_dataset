#------------------#
#### Librairies ####
#------------------#
#general data manipulation
library(dplyr)
#spatial data manipulation
library(sf)
sf_use_s2(FALSE)

#Function
number_ind_transects <- function(
    transect_data,#Transect observation data
    zone_sf,#shapefile that contain the polygon of the zones of the study area
    species_list, #vector of species name for which mean number of individual on transect will be calculated
    zone_list, #list of the zones to calcul mean number of individual on transect
    status_list #status of individuals on transects that will be taken into account
    ){

#---Extract spatial features of the transect data
transects_coords <- transect_data %>% 
  dplyr::select(transect) %>% 
  unique()

#--- Filter transect data based on the selected status and species
transects <-  transect_data %>%
  #Remove spatial features to accelerate group_by() and summarise() functions
  sf::st_drop_geometry() %>% 
  #Keep only selected species and status
  dplyr::filter(species %in% species_list, status %in% status_list) %>%
  #Extract sum of individuals observed per transect
  dplyr::group_by(transect, year, species) %>% 
  dplyr::summarise(nb_ind= sum(nb_ind), .groups = "drop") %>% 
  #Assign coordinates to each transect
  dplyr::left_join(transects_coords, by= "transect") %>% # add spatial features
  sf::st_as_sf() %>% 
  #Assign a zone to each transect
  sf::st_intersection(zone_sf %>%  dplyr::filter(zone %in% zone_list))

#--- Calcul mean number of individual observed per transect
obs_transect <- transects  %>% 
  dplyr::group_by(species, zone, year) %>% 
  dplyr::summarise(mean_ind= mean(nb_ind), sd_ind= sd(nb_ind), .groups = "drop") %>% 
  dplyr::arrange(species, zone, year)%>% 
  #Remove spatial features
  sf::st_drop_geometry()

  return(obs_transect)
}