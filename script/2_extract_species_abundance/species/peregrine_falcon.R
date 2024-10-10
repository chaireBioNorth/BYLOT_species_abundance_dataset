#Title: Extract the abundance of peregrine falcons on the Bylot Island study area 
#Date: October 11 2023
#Last update: October 10 2024

#---------------------------------------------------------------------------------#
#### Extract nest density between 2013-2019, 2022 when monitoring was systematic ####
#---------------------------------------------------------------------------------#
#--- Extract nest density of rough legged hawk for each year and each zones
#We could not used the same function as other species because the coordinates are not provided since they are sensitive.
peregrine_falcon <- get_nest_density_missing_coordinates(
  nest_data_csv= nest_data_csv, 
  zone_sf= study_area, 
  species_list= c("peregrine falcon"), 
  zone= "study area", 
  #list of year for which species were sampled systematically at the scale of the study area
  year_list= sampling_year[sampling_year$species == "peregrine falcon" & sampling_year$zone == "study area" & sampling_year$sampled=="yes",]$year) %>% 
  dplyr::filter(year %in% sampling_year[sampling_year$species == "peregrine falcon" & sampling_year$zone == "study area" & sampling_year$sampled=="yes",]$year) %>% 
  dplyr::mutate(breeding_status= "breeding", method= "nest sampling") %>% 
  dplyr::select(species, zone, year, ind_density_km2, breeding_status, method)
