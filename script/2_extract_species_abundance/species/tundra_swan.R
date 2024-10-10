#Title: Extract the abundance of tundra swan on the Bylot Island study area 
#Date: October 11 2023
#Last update:  October 10 2024

#---------------------------------------------------------------------------------#
#### Extract nest density between 2017 and 2023 when monitoring was systematic ####
#---------------------------------------------------------------------------------#
tundra_swan <- get_nest_density(
  nest_data= nest_data,
  zone_sf= study_area,
  species_list = c("tundra swan"),
  zone_list= "study area",
  year_list=unique(sampling_year[sampling_year$monitoring =="systematic" & sampling_year$species == "tundra swan" & sampling_year$sampled == "yes" & sampling_year$zone == "study area",]$year))%>% 
  dplyr::mutate(breeding_status= "breeding", method= "nest sampling") %>% 
  dplyr::select(species, zone, year, ind_density_km2, breeding_status, method)