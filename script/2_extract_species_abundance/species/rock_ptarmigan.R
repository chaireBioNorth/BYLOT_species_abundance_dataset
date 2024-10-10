#Title: Extract the abundance of Rock ptarmigans on the Bylot Island study area 
#Date: October 11 2023
#Last update:  October 10 2024

#--------------------------------------------------#
#### Extract mean nest density in the 8km2 plot ####
#--------------------------------------------------#
rock_ptarmigan_nests <- get_nest_density(
  nest_data= nest_data,
  zone_sf= study_area,
  species_list = "rock ptarmigan",
  zone_list= "4x2km plot",
  year_list= sampling_year[sampling_year$species == "rock ptarmigan" & sampling_year$zone == "4x2km plot" & sampling_year$sampled=="yes",]$year)

#Calculate mean density in the qarlikturvik valley
rock_ptarmigan <- rock_ptarmigan_nests %>% 
  dplyr::group_by(zone) %>% 
  dplyr::summarise(species="rock ptarmigan", year= NA, ind_density_km2= mean(ind_density_km2)) %>% 
  dplyr::mutate(zone="study area", breeding_status= "breeding", method= "nest sampling (extrapolation habitat)") %>% 
  dplyr::select(species, zone, year, ind_density_km2, breeding_status, method)