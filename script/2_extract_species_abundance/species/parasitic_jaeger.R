#Title: Extract the abundance of Parasitic jaegers on the Bylot Island study area 
#Date: October 11 2023
#Last update:  October 10 2024

#---Estimate abundance based on the maximal number of individuals banded in a single year (17 individuals)
parasitic_jaeger <- data.frame(
  species= "parasitic jaeger",
  zone= "study area",
  ind_density_km2= round(17/ study_area[study_area$zone == "study area", ]$area_km2, digits = 3)) %>% 
  dplyr::mutate(breeding_status= "undetermined", method= "banding", year= NA) %>% 
  dplyr::select(species, zone, year, ind_density_km2, breeding_status, method)

#--- use total number of nest found as an alternative approach
parasitic_jaeger_alternative <- get_nest_density(
  nest_data= nest_data,
  zone_sf= study_area,
  species_list = "parasitic jaeger",
  zone_list= "study area",
  #years for which nest of parasitic jaegers where sampled
  year_list= sampling_year[sampling_year$species=="parasitic jaeger" & sampling_year$sampled== "yes" & sampling_year$zone== "study area",]$year) %>% 
  dplyr::select(-nb_nest, -nest_density_km2) %>% 
  dplyr::group_by(species, zone) %>% 
  dplyr::summarise(ind_density_km2= mean(ind_density_km2)) %>% 
  dplyr::mutate(breeding_status= "breeding", method= "nest sampling", year= NA) %>% 
  dplyr::select(species, zone, year, ind_density_km2, breeding_status, method)

#----------------------------#
#### Combined data frames ####
#----------------------------#
parasitic_jaeger <- parasitic_jaeger %>% 
  rbind(parasitic_jaeger_alternative)