#Title: Extract the abundance of Common-ringed plover on the Bylot Island study area 
#Date: October 11 2023
#Last update:  October 10 2024

#------------------------------------------------------------------------------------#
#### Extract nest density between 2015-2017 when monitoring was close to systematic ####
#------------------------------------------------------------------------------------#
common_ringed_plover <- get_nest_density(
  nest_data= nest_data,
  zone_sf= study_area,
  species_list = "common ringed plover",
  zone_list= "study area",
  year_list=  sampling_year[sampling_year$species== "common ringed plover" & sampling_year$zone== "study area" & sampling_year$sampled=="yes",]$year)%>% 
  dplyr::mutate(breeding_status= "breeding", method= "nest sampling") %>% 
  dplyr::select(species, zone, year, ind_density_km2, breeding_status, method)