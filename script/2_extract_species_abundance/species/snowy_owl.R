#Title: Extract the abundance of Snowy owls on the Bylot Island study area 
#Date: October 11 2023
#Last update:  October 10 2024

#--------------------------------------------------------#
#### Extract nest density between  2012-2019, 2022-2023 when monitoring was systematic ####
#-------------------------------------------------------#
#--- Extract nest density of snowy owls for each year and each zones
snowy_owl <- get_nest_density(
  nest_data= nest_data,
  zone_sf= study_area,
  species_list = "snowy owl",
  zone_list= "study area",
  #keep only year systematically sampled at the study area
  year_list= sampling_year[sampling_year$species== "snowy owl" & sampling_year$zone== "study area" & sampling_year$sampled=="yes",]$year) %>%
  dplyr::mutate(breeding_status= "breeding", method= "nest sampling") %>% 
  dplyr::select(species, zone, year, ind_density_km2, breeding_status, method)

#--------------------------------------------#
#### Extend series snowy owl  (1993-2011) ####
#--------------------------------------------# 
# From 1993 and 2011, monitoring was only performed in the valley of Qarlikturvik valley and nearby plateaus
snowy_owl_extend <- extend_temporal_series_from_single_zone_nest(
  nest_data= nest_data,
  zone_sf=study_area,
  sp= "snowy owl",
  #Extract complete list of year for which monitoring was done in qarlikturvik valley
  year_list= sampling_year[sampling_year$species == "snowy owl" & sampling_year$zone == "qarlikturvik valley" & sampling_year$sampled=="yes",]$year,
  #Extract list of year for which sampling was done at the scale of study area and in the qarlikturvik valley
  year_compare= sampling_year[sampling_year$species == "snowy owl" & sampling_year$zone == "study area" & sampling_year$sampled=="yes",]$year,
  zone_ref= c("camp 1", "black plateau", "south plateau")) %>% 
  dplyr::mutate(breeding_status= "breeding", method= "nest sampling (extrapolation nests)") %>% 
  dplyr::select(species, zone, year, ind_density_km2, breeding_status, method)

#----------------------------#
#### Combined data frames ####
#----------------------------#
snowy_owl <- snowy_owl %>% 
  rbind(snowy_owl_extend)
