#Title: Extract the abundance of Glaucous gulls on the Bylot Island study area 
#Date: October 11 2023
#Last update:  October 10 2024

#---------------------------------------------------------------------------------#
#### Extract nest density between 2017 and 2023 when monitoring was systematic ####
#---------------------------------------------------------------------------------#
glaucous_gull <- get_nest_density(
  nest_data= nest_data,
  zone_sf= study_area,
  species_list = c("glaucous gull"),
  zone_list= "study area",
  year_list= unique(sampling_year[sampling_year$monitoring =="systematic" & sampling_year$species == "glaucous gull" & sampling_year$sampled == "yes" & sampling_year$zone == "study area",]$year))%>% 
  dplyr::mutate(breeding_status= "breeding", method= "nest sampling") %>% 
  dplyr::select(species, zone, year, ind_density_km2, breeding_status, method)

#------------------------------------------------#
#### Extend series glaucous gull  (2004-2016) ####
#------------------------------------------------#
#--- Extrapolate density of glaucous gull based on density between 2004 - 2016 measured in the qarlikturvik valley
glaucous_gull_extend <- extend_temporal_series_from_single_zone_nest(
  nest_data= nest_data,
  zone_sf=study_area,
  sp= "glaucous gull",
  #Extract complete list of year for which monitoring was done in qarlikturvik valley
  year_list= sampling_year[sampling_year$species == "glaucous gull" & sampling_year$zone == "qarlikturvik valley" & sampling_year$sampled=="yes",]$year,
  #Extract list of year for which sampling was done at the scale of study area and in the qarlikturvik valley
  year_compare= sampling_year[sampling_year$species == "glaucous gull" & sampling_year$zone == "study area" & sampling_year$sampled=="yes",]$year,
  zone_ref= "qarlikturvik valley") %>% 
  dplyr::mutate(breeding_status= "breeding", method= "nest sampling (extrapolation nests)") %>% 
  #Remove undesired columns
  dplyr::select(species, zone, year, ind_density_km2, breeding_status, method)


#----------------------------#
#### Combined data frames ####
#----------------------------#
glaucous_gull <- glaucous_gull %>% 
  rbind(glaucous_gull_extend)