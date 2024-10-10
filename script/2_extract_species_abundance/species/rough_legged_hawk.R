#Title: Extract the abundance of Rough-legged hawks on the Bylot Island study area 
#Date: October 11 2023
#Last update:  October 10 2024

#---------------------------------------------------------------------------------#
#### Extract nest density between 2013-2019, 2022 when monitoring was systematic ####
#---------------------------------------------------------------------------------#
#--- Extract nest density of rough legged hawk for each year and each zones
#We could not used the same function as other species because the coordinates are not provided since they are sensitive.
rough_legged_hawk <- get_nest_density_missing_coordinates(
  nest_data_csv= nest_data_csv, 
  zone_sf= study_area, 
  species_list= c("rough legged hawk"), 
  zone= "study area", 
  #list of year for which species were sampled systematically at the scale of the study area
  year_list= sampling_year[sampling_year$species == "rough legged hawk" & sampling_year$zone == "study area" & sampling_year$sampled=="yes",]$year) %>% 
  dplyr::filter(year %in% sampling_year[sampling_year$species == "rough legged hawk" & sampling_year$zone == "study area" & sampling_year$sampled=="yes",]$year) %>% 
  dplyr::mutate(breeding_status= "breeding", method= "nest sampling") %>% 
  dplyr::select(species, zone, year, ind_density_km2, breeding_status, method)

#--------------------------------------------#
#### Extend series snowy owl  (2007-2012) ####
#--------------------------------------------# 
#Estimate the density of rough-legged hawks between 2007 and 2012 based on correlation with nest density in the Qarlikturvik valley and nearby plateaus and density at the scale of the study area
rough_legged_hawk_extend <- extend_temporal_series_from_single_zone_nest_missing_coords(
  nest_data=read.csv("data/raw/sampling/hawk_falcon_nest_zone.csv") %>%
    dplyr::select(-X),
  zone_sf= study_area,
  sp= "rough legged hawk",
  #Extract complete list of year for which monitoring was done in qarlikturvik valley
  year_list= sampling_year[sampling_year$species == "rough legged hawk" & sampling_year$zone == "qarlikturvik valley" & sampling_year$sampled=="yes",]$year,
  #Extract list of year for which sampling was done at the scale of study area and in the qarlikturvik valley
  year_compare= sampling_year[sampling_year$species == "rough legged hawk" & sampling_year$zone == "study area" & sampling_year$sampled=="yes",]$year,
  zone_ref= c("qarlikturvik valley", "black plateau", "south plateau")) %>% 
  dplyr::mutate(breeding_status= "breeding", method= "nest sampling (extrapolation nests)") %>% 
  dplyr::select(species, zone, year, ind_density_km2, breeding_status, method)

#----------------------------#
#### Combined data frames ####
#----------------------------#
rough_legged_hawk <- rough_legged_hawk %>% 
  rbind(rough_legged_hawk_extend)