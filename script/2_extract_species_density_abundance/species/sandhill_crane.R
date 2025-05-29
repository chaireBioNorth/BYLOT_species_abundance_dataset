#Title: Extract the abundance of Sandhill cranes on the Bylot Island study area 
#Date: October 11 2023
#Last update:  October 10 2024

#---------------------------------------------------------------------#
#### Extract regression nest density and individuals per transects ####
#---------------------------------------------------------------------#
sandhill_crane <- get_nest_density(nest_data= nest_data,
                 zone_sf= study_area,
                 species_list= "sandhill crane",
                 zone_list= "qarlikturvik valley",
                 year_list= sampling_year[sampling_year$species == "sandhill crane" & sampling_year$zone == "camp 1" & sampling_year$sampled=="yes",]$year) %>% 
  dplyr::group_by(species) %>% 
  dplyr::summarise(mean_ind_density_km2= mean(ind_density_km2)) %>% 
  #rename columns mean to simply ind_density_km2
  dplyr::rename(ind_density_km2= mean_ind_density_km2) %>% 
  #Add other column
  dplyr::mutate(zone= "study area", year= NA, breeding_status= "breeding", method= "nest sampling (extrapolation uniform)", spatial_extrapolation= "yes") %>% 
  #reorder columns
  dplyr::select(species, zone, year, ind_density_km2, breeding_status, method, spatial_extrapolation)
