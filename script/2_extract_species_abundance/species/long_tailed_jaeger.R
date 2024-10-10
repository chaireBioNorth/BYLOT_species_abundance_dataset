#Title: Extract the abundance of Long-tailed jaeger on the Bylot Island study area 
#Date: October 11 2023
#Last update:  October 10 2024

#----------------------------------------------------------------------#
#### Extract nest density since 2005 when monitoring was systematic in qarlikturvik valley ####
#----------------------------------------------------------------------#
#long-tailed jaeger density in qarlikturvik valley since 2005
ltja_density <- get_nest_density(
  nest_data= nest_data,
  zone_sf= study_area,
  species_list = "long tailed jaeger",
  zone_list= "camp 1",
  #list of year for which nest density was measured
  year_list= sampling_year[sampling_year$species == "long tailed jaeger" & sampling_year$zone == "camp 1" & sampling_year$sampled=="yes",]$year) 

#Correct density estimate to account for proportion of mesic habitat
ltja_density <- ltja_density %>% 
  #add area and proportion of wet ana mesic habitat in each zone
  dplyr::left_join(study_area %>% sf::st_drop_geometry(), by= "zone") %>% 
  #adjust density to consider only mesic habitat
  dplyr::mutate(area_km2_mesic= area_km2*prop_mesic, ind_density_km2= (nb_nest*2)/area_km2_mesic) %>% 
  dplyr::rename(year= year.x) %>% 
  dplyr::select(species, year, ind_density_km2)

#Calculate area of mesic habitat in the study area
mesic_study_area <- study_area %>% 
  dplyr::filter(zone %in% c("camp 2", "goose point","malaview","qarlikturvik valley",  "camp 3", "dufour")) %>% 
  dplyr::mutate(area_km2_mesic= area_km2*prop_mesic) 
#Total sum of mesic area across study area
mesic_area_study_area <- sum(mesic_study_area$area_km2_mesic)

#Extrapolate density at the scale of the study area
long_tailed_jaeger <- ltja_density %>% 
  dplyr::mutate(zone= "study area",
                ind_density_km2= (ind_density_km2*mesic_area_study_area)/study_area[study_area$zone=="study area",]$area_km2, breeding_status= "breeding", method= "nest sampling (extrapolation habitat)") %>% 
  dplyr::select(species, zone, year, ind_density_km2, breeding_status, method)