#Title: Extract the abundance of American golden-plover on the Bylot Island study area 
#Date: October 11 2023
#Last update:  October 10 2024

#-------------------------------------------------------------------------------#
#### Extract density of breeding individual abundance with distance sampling ####
#-------------------------------------------------------------------------------#
american_golden_plover <- extract_density_distance_sampling(
  transect_data= transect_data , 
  distance_data= read.csv("data/raw/sampling/american_golden_plover_distance.csv"), 
  sp= "american golden plover", 
  zone_list= zones, 
  zone_sf= study_area, 
  year_list= c(2014:2019, 2022:2023), 
  status_list= c("repro", "non repro", "inconnu"), 
  key= "hn", 
  adjustment= "cos",
  nb_adjust_term=1,
  transect_width=150,
  null_density_wetlands=TRUE,
  relative_abundance_upland= relative_abundance_upland) %>% 
  dplyr::mutate(breeding_status= "breeding", method= "distance sampling") %>% 
  dplyr::select(species, zone, year, ind_density_km2, breeding_status,method)