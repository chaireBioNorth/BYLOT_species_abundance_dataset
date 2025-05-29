#Title: Extract the abundance of white-rumped sandpiper, pectoral sandpiper, buff-breasted sandpiper, red knot, ruddy turnstone, red phalarope on the Bylot Island study area 
#Date: October 11 2023
#Last update:  October 10 2024

#!!! The density of Baird's sandpiper need to extract before running the following code !!!
# source("script/2_extract_species_abundance/species/bairds_sandpiper.R")

#-----------------------------------------------
#### Estimate abundance of each other sandpiper species based on the relative abundance of each species in the transect observation and the abundance of Bairds sandpiper ####
#-----------------------------------------------
sandpipers <- estimate_density_from_reference_species(
  reference_species_density= bairds_sandpiper,
  transect_data= transect_data,
  status_list= c("repro", "inconnu"),
  status_list_ref_sp= c("repro", "inconnu"),
  species_list= c("white rumped sandpiper", "pectoral sandpiper", "buff breasted sandpiper", "red knot", "red phalarope", "ruddy turnstone"),
  zone_list= zones,
  zone_sf= study_area,
  relative_abundance_upland= relative_abundance_upland,
  sp_null_density_wetlands=NULL)%>% 
  dplyr::mutate(breeding_status= "breeding", method= "transects (relative abundance)", year= NA, spatial_extrapolation= "yes") %>% 
  dplyr::filter(species != "bairds sandpiper") %>% 
  dplyr::select(species, zone, year, ind_density_km2, breeding_status, method, spatial_extrapolation) 


#----------------------------------------------
#### Alternative approach based on Gauhtier et al., 2023 indexes of relative abundance ####
#----------------------------------------------
sandpipers_alternative <-relative_abundance_gauthier %>% 
  dplyr::filter(species %in% c("red knot", "ruddy turnstone", "white rumped sandpiper", "pectoral sandpiper", "red phalarope", "buff breasted sandpiper")) %>% 
  #Add abundance of Baird's sanpiper as reference
  dplyr::left_join(
    bairds_sandpiper %>% 
      dplyr::filter(zone=="study area") %>% 
      dplyr::group_by(species, zone) %>% 
      dplyr::summarise(ref_ind_density_km2= mean(ind_density_km2))
    , by= c("ref_species"="species"))  %>% 
  dplyr::mutate(ind_density_km2= ref_ind_density_km2*(index_sp/index_ref_sp),
                breeding_status= "breeding", method= "incidental observations (relative abundance)", year= NA, spatial_extrapolation= "yes") %>% 
  dplyr::select(species, zone, year, ind_density_km2, breeding_status, method, spatial_extrapolation)

#----------------------------#
#### Combined data frames ####
#----------------------------#
sandpipers <- sandpipers %>% 
  rbind(sandpipers_alternative)