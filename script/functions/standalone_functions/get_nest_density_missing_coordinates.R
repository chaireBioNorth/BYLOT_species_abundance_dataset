# Extract nest density when nest coordinates are not available
#Date: November 28 2023

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
get_nest_density_missing_coordinates <- function(
    nest_data_csv, #Nest data for the concerned species
    zone_sf, #sf object with the polygon of the study area zone
    species_list, #list of species to estimate nest density
    zone, #zone to estimate nest density in
    year_list#list of year to calculate nest density
    ){
    
#Extract zone area
zone_area_km2 <- study_area[study_area$zone == zone,]$area_km2
  
#Calcul total number of nest per year for the selected species
nest_density <- nest_data_csv %>% 
  dplyr::filter(species %in% species_list) %>% 
  dplyr::group_by(species, year) %>% 
  dplyr::summarise(nb_nest= n(), .groups = "drop") %>% 
  dplyr::ungroup() %>% 
  #Assign zone and area of the zone and extract nest  and individual density
  dplyr::mutate(zone = zone, ind_density_km2= (2*nb_nest)/zone_area_km2) %>% 
  #Add all years-zone-species where no nest were found and assign 0
full_join(expand.grid(year = year_list, species = species_list, zone = zone), by = c("species", "zone","year")) %>% 
  #Replace the NA with 0
  mutate(ind_density_km2= ifelse(is.na(ind_density_km2), 0, ind_density_km2)) %>% 
  #Remove undesired columm
  dplyr::select(-nb_nest)
    
  #return the complete data frame
  return(nest_density)
}
