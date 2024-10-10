#Title: Extract the abundance of black-bellied plover on the Bylot Island study area 
#Date: October 11 2023
#Last update:  October 10 2024

#!!! The density of American golden plover need to extract before running the following code !!!
# source("script/2_extract_species_abundance/species/american_golden_plover.R")

#-------------------------------------------------------------------------------#
#### Extract density based on the relative proportion of observations of each species on transects ####
#-------------------------------------------------------------------------------#
black_bellied_plover <- estimate_density_from_reference_species(
  reference_species_density= american_golden_plover,
  transect_data= transect_data%>% 
    #Remove transects with more than 4 individuals pointing towards group of non-breeding individuals
    dplyr::filter(nb_ind<=4),
  status_list= c("repro", "non repro", "inconnu"),
  status_list_ref_sp= c("repro", "non repro", "inconnu"),
  species_list= c("american golden plover", "black bellied plover"),
  zone_list= zones,
  zone_sf= study_area,
  relative_abundance_upland= relative_abundance_upland,
  sp_null_density_wetlands=NULL)%>% 
  dplyr::filter(species== "black bellied plover") %>% 
  dplyr::mutate(breeding_status= "breeding", method= "transects (relative abundance)", year= NA) %>% 
  dplyr::select(species, zone, year, ind_density_km2, breeding_status, method)

#-------------------------------------------------------------------#
#### Alternative approach based on Gauhtier et al., 2023 indexes ####
#-------------------------------------------------------------------#
black_bellied_plover_alternative <-relative_abundance_gauthier %>% 
  dplyr::filter(species == "black bellied plover") %>% 
  #Add mean density of american golden plover as reference
  dplyr::left_join(
    american_golden_plover %>% 
      dplyr::filter(zone=="study area") %>% 
      dplyr::group_by(species, zone) %>% 
      dplyr::summarise(ref_ind_density_km2= mean(ind_density_km2))
    , by= c("ref_species"="species")) %>% 
  #Use cross-multiplication to estimate black bellied plover absolute abundance from relative abundance
  dplyr::mutate(ind_density_km2= ref_ind_density_km2*(index_sp/index_ref_sp), breeding_status= "breeding", method= "incidental observations (relative abundance)", year= NA) %>% 
  dplyr::select(species, zone, year, ind_density_km2, breeding_status, method)


#----------------------------#
#### Combined data frames ####
#----------------------------#
black_bellied_plover <- black_bellied_plover %>% 
  rbind(black_bellied_plover_alternative)