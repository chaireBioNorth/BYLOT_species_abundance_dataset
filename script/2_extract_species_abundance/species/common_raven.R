#Title: Extract the abundance of Common ravens on the Bylot Island study area 
#Date: October 11 2023
#Last update:  October 10 2024

#--------------------------------------------------------------------------#
#### Estimate density of ravens based on the abundance of glaucous gull ####
#--------------------------------------------------------------------------#
#glaucous gull nest density as reference
glgu <-get_nest_density(
  nest_data= nest_data,
  zone_sf= study_area,
  species_list =  "glaucous gull",
  zone_list= zones,
  year_list= c(2017:2023))

#--- Estimate common raven from glaucous gull abundance using mean number of individuals observed per transect as index of relative abundance
common_raven <- estimate_density_from_reference_species(
  reference_species_density= glgu,
  transect_data= transect_data,
  status_list= c("repro", "inconnu", "non repro"),
  status_list_ref_sp= c("repro", "inconnu", "non repro"),
  species_list= "common raven",
  zone_list= zones,
  zone_sf= study_area,
  relative_abundance_upland= relative_abundance_upland,
  sp_null_density_wetlands=NULL) %>% 
  dplyr::filter(species== "common raven", zone== "study area") %>% 
  dplyr::mutate(breeding_status= "undetermined", method= "transects (relative abundance)", year= NA) %>% 
  dplyr::select(species, zone, year, ind_density_km2, breeding_status, method)


#---------------------------------------------
#### Alternative approach based on Gauhtier et al., 2023 indexes of relative abundance #### 
#---------------------------------------------
common_raven_alternative <-relative_abundance_gauthier %>% 
  dplyr::filter(species== "common raven") %>% 
  #Add abundance of glaucous gull as reference
  dplyr::left_join(
    glgu %>%
      dplyr::filter(species== "glaucous gull") %>% 
      dplyr::group_by(species, zone) %>% 
      dplyr::summarise(ref_ind_density_km2= mean(ind_density_km2))
    , by= c("ref_species"="species")) %>% 
  dplyr::mutate(ind_density_km2= ref_ind_density_km2*(index_sp/index_ref_sp)) %>% 
  dplyr::mutate(breeding_status= "undetermined", method= "incidental observations (relative abundance)", year= NA) %>% 
  dplyr::select(species, zone, year, ind_density_km2, breeding_status, method)

#----------------------------#
#### Combined data frames ####
#----------------------------#
common_raven <- common_raven %>% 
  rbind(common_raven_alternative)