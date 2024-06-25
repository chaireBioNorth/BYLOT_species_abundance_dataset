# Extract species density by doing a rule of three with the relative proportion of observation among the selected species in each zone and the known species density of one of the species

#------------------#
#### Librairies ####
#------------------#
#data manipulation
library(dplyr)
library(tidyr)
#spatial data manipulation
library(sf)
sf_use_s2(FALSE)

#Dependencies on function created in other scripts
source("script/functions/standalone_functions/number_ind_transects.R")
source("script/functions/standalone_functions/extract_density_distance_sampling.R")
source("script/functions/nested_functions/spatial_extrapolation_species_density.R")


estimate_density_from_reference_species <- function(
    reference_species_density, #Object with the density estimate of the reference species
    transect_data, #Transect observation data
    status_list, #status of individuals on transects that will be taken into account
    status_list_ref_sp, #status of individuals of the reference species on transects that will be taken into account
    species_list, #List of species to estimate density from reference species density
    zone_list, #List of zone to estimate density in
    zone_sf, #Sf object of the zones of the study area to estimate density in
    relative_abundance_upland, #Data frame with the ratio of relative abundance in upland vs lowland habitats
    sp_null_density_wetlands # List of species for which to remove wetlands to the area to extrapolate in each zone
    ){

#----------------------------------------------#
#### Extract relative proportion obs by species for each zone ####
#----------------------------------------------#
# NOTE: we perform manipulation on transect data without considering spatial feature so it is faster to run and them we join back the spatial features of the transect (less straight away, but faster)
#Extract sf object associate to each transect
transects_coords <- transect_data %>% 
  dplyr::select(transect, geometry) %>% 
  unique()

#--- Transect data for species to estimate density
transects_sp <-  transect_data %>% 
  dplyr::filter(species %in% species_list,
                status %in% status_list)

#--- Transects reference species
transects_ref_sp <- transect_data %>% 
  dplyr::filter(species == unique(reference_species_density$species),
                status %in% status_list_ref_sp) 

#--- Combine transect obs from reference and other species
transects <- rbind(transects_sp, transects_ref_sp) %>% 
  sf::st_drop_geometry()%>%
  dplyr::group_by(transect, year, species) %>% 
  dplyr::summarise(nb_ind= sum(nb_ind), .groups = "drop") %>% 
  dplyr::left_join(transects_coords, by= "transect") %>% 
  sf::st_as_sf()

#Extract relative proportion of each species number of individual per transect
species_obs <- transects %>% 
  #Extract observation by zone
  sf::st_intersection(zone_sf) %>%
  #Remove spatial feature (no more needed)
  sf::st_drop_geometry() %>% 
  #Keep only zone of interest
  dplyr::filter(zone %in% zone_list) %>% 
  #Extract total number of individual observed per transect-zone-species
  dplyr::group_by(species, zone) %>% 
  dplyr::summarise(nb_ind= sum(nb_ind))
  
#Extract proportion observation by species in the species list per zone
total_obs_zone <- species_obs %>%
    dplyr::group_by(zone) %>% 
    dplyr::summarise(total_obs=sum(nb_ind), .groups = "drop")
  
#Add total number of obs of the selected species per zone 
species_obs <- species_obs %>% 
  dplyr::left_join(total_obs_zone) %>% 
  #calculate proportion of observation per species
    dplyr::mutate(prop_obs= nb_ind/total_obs)


#----------------------------------------#
#### Estimate density of each species ####
#----------------------------------------#
#extract individual density of each species based on proportion of observation and lapland longspur nest density per zone
ref_nest_density <- reference_species_density %>% 
  dplyr::group_by(species, zone) %>% 
  dplyr::summarise(ind_density_km2= mean(ind_density_km2), .groups = "drop") %>% 
  dplyr::left_join(species_obs %>% dplyr::select(species, zone,prop_obs)) %>% 
  dplyr::mutate(prop_obs= ifelse(is.na(prop_obs),0, prop_obs)) %>% 
  dplyr::rename(ref_ind_density_km2= ind_density_km2, ref_prop_obs= prop_obs) %>% 
  dplyr::select(zone, ref_ind_density_km2, ref_prop_obs)
  
#Add the reference density estimated by zone to the obs data frame
species_obs <- species_obs %>% 
    dplyr::left_join(ref_nest_density, by= "zone") %>% 
    dplyr::mutate(ind_density_km2= (prop_obs/ref_prop_obs)*ref_ind_density_km2) %>% 
    dplyr::mutate(ind_density_km2= ifelse(is.nan(ind_density_km2),0, ind_density_km2))
  
#----------------------------------------------#
#### Adjust density to account for wetlands ####
#----------------------------------------------#
#--- if species name appear in sp_null_density_wetlands then reduce density in mesic zone with a factor
if(is.null(sp_null_density_wetlands)==FALSE){
for (sp in sp_null_density_wetlands){
  for (z in c("qarlikturvik valley", "camp 2", "camp 3", "malaview", "goose point", "dufour")){
    #Adjust abundance
    species_obs[species_obs$zone== z & species_obs$species== sp,]$ind_density_km2 <- species_obs[species_obs$zone== z & species_obs$species== sp,]$ind_density_km2*
      study_area[study_area$zone == z,]$prop_mesic
    }
  }
}

#keep only desired columns
species_obs <- species_obs %>% 
  dplyr::select(species, zone, ind_density_km2)

#--------------------------------------#
#### Assign density to upland zones ####
#--------------------------------------#
density_lowland <- species_obs %>% 
  dplyr::left_join(study_area) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::mutate(abundance= prop_mesic*area_km2*ind_density_km2) %>% 
  dplyr::filter(zone!="study area") %>% 
  dplyr::group_by(species) %>% 
  dplyr::summarise(ind_density_km2= sum(abundance)/sum(area_km2))

#--- Assign density to higher elevation zones without transects
for (sp in unique(species_obs$species)){
  density_upland <-  data.frame(
  species= rep(sp, times= 3),
  zone= c("black plateau", "south plateau","camp 3 plateau"),
  ind_density_km2= density_lowland[density_lowland$species==sp,]$ind_density_km2 *
    relative_abundance_upland[relative_abundance_upland$species == sp,]$ratio_difference)
  
species_obs <- species_obs %>% 
  dplyr::select(species, zone, ind_density_km2) %>% 
  rbind(density_upland)
}


#-----------------------------#
#### Density on study area ####
#-----------------------------#  
study_area_density <- species_obs %>% 
  #remove study area density
  dplyr::filter(zone != "study area") %>% 
  # Add area of each zone
  dplyr::left_join(zone_sf %>% sf::st_drop_geometry() %>% dplyr::select(zone, area_km2)) %>% 
  # Estimate abundance per zone
  dplyr::mutate(nb_ind= ind_density_km2* area_km2) %>% 
  #Extract total abundance per species for all zones
  dplyr::group_by(species) %>% 
  dplyr::summarise(nb_ind= sum(nb_ind), .groups = "drop") %>% 
  #Transform abundance into density
  dplyr::mutate(zone= "study area", ind_density_km2= nb_ind/ zone_sf[zone_sf$zone == "study area", ]$area_km2) %>% 
  #keep only columns of interest
  dplyr::select(species, zone, ind_density_km2)

#Replace original density on study area
species_obs <- species_obs %>% 
  #remove original study area density
  dplyr::filter(zone != "study area") %>% 
  #add new rows
  rbind(study_area_density) %>% 
  #reorder columns
  dplyr::select(species, zone, ind_density_km2) %>% 
  #Add column NA for standard deviation
  dplyr::mutate(sd_ind_density_km2= NA) %>% 
  #arrange row by species and zone
  dplyr::arrange(species, zone)
  
  return(species_obs)
}

