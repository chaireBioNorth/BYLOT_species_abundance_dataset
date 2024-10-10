#Title: Extract the abundance of horned larks, snow buntings and American pipits on the Bylot Island study area 
#Date: October 11 2023
#Last update:  October 10 2024

#!!! The density of Lapland longspur need to extract before running the following code !!!
# source("script/2_extract_species_abundance/species/lapland_longspur.R")

#-----------------------------------------------
#### Estimate abundance of each other passerines species based on the relative abundance of each species in the transect observation and the abundance of lapland longspurs ####
#-----------------------------------------------
passerines <- estimate_density_from_reference_species(
  reference_species_density= lapland_longspur,
  transect_data= transect_data,
  status_list= c("repro", "inconnu", "non repro"),
  status_list_ref_sp= c("repro", "inconnu", "non repro"),
  species_list= c("horned lark", "american pipit", "snow bunting"),
  zone_list= zones,
  zone_sf= study_area,
  relative_abundance_upland= relative_abundance_upland,
  sp_null_density_wetlands=NULL)%>% 
  dplyr::mutate(breeding_status= "breeding", method= "transects (relative abundance)", year= NA) %>% 
  dplyr::filter(species != "lapland longspur") %>% 
  dplyr::select(species, zone, year, ind_density_km2, breeding_status, method)

#----------------------------------------------
#### Alternative approach based on Gauhtier et al., 2023 indexes of relative abundance ####
#----------------------------------------------
passerines_alternative <-relative_abundance_gauthier %>% 
  dplyr::filter(species %in% c("lapland longspur","horned lark", "american pipit", "snow bunting")) %>% 
  #Add mean density of lapland longspur as reference
  dplyr::left_join(
    lapland_longspur %>% 
      dplyr::filter(zone=="study area") %>% 
      dplyr::group_by(species, zone) %>% 
      dplyr::summarise(ref_ind_density_km2= mean(ind_density_km2))
    , by= c("ref_species"="species")) %>% 
  dplyr::mutate(ind_density_km2= ref_ind_density_km2*(index_sp/index_ref_sp),
                breeding_status= "breeding", method= "incidental observations (relative abundance)", year= NA) %>% 
  dplyr::select(species, zone, year, ind_density_km2, breeding_status, method)

#----------------------------#
#### Combined data frames ####
#----------------------------#
passerines <- passerines %>% 
  rbind(passerines_alternative)