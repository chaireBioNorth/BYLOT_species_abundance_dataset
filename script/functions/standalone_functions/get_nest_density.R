#------------------#
#### Librairies ####
#------------------#
#general data manipulation
library(dplyr)
#spatial data manipulation
library(sf)
sf_use_s2(FALSE)

#-----------------------------------------#
#### Extract density of nests per zone ####
#-----------------------------------------#
get_nest_density <- function(
    nest_data, #Nest data (id, coordinates, species and year)
    zone_sf,#shapefile that contain the polygon of the zones of the study area
    species_list,#vector of species name for which nest density will be calculated
    zone_list,#list of the zones to calul nest density
    year_list){
  

nest_density <- nest_data %>% 
  #Keep only desired species and years
  dplyr::filter(year %in% year_list, species %in% species_list) %>% 
  #intersect with zone polygon to associate a zone to each nest
  sf::st_intersection(zone_sf) %>% 
  #Remove spatiale feature
  sf::st_drop_geometry() %>% 
  #Keep only desired zones
  dplyr::filter(zone %in% zone_list) %>% 
  #Group data by species, year and zone
  dplyr::group_by(species, zone, area_km2, year) %>% 
  #extract number of nest per year for the selected zone
  dplyr::summarise(nb_nest=n(), .groups = "drop") %>% 
  #Calculate density of nest and individuals (2*density of nests) per year
  dplyr::mutate(nest_density_km2= nb_nest/area_km2, ind_density_km2= nest_density_km2*2) %>% 
  #remove grouping 
  dplyr::ungroup() %>% 
  #Remove column area_km2
  dplyr::select(-area_km2) %>% 
  #Add all years-zone-species where no nest were found and assign 0
  full_join(expand.grid(year = year_list, species = species_list, zone = zone_list), by = c("species", "zone","year")) %>% 
  #Replace the NA with 0
  mutate_at(c("nest_density_km2", "nb_nest", "ind_density_km2"), ~ ifelse(is.na(.), 0, .))

  #return the complete data frame
  return(nest_density)
}