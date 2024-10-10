#Title: Extract the abundance of Arctic hares on the Bylot Island study area 
#Date: October 11 2023
#Last update:  October 10 2024

#!!! The density of Arctic fox need to extract before running the following code !!!
# source("script/2_extract_species_abundance/species/arctic_fox.R")


#Comparaison of relative abundance with arctic fox based on incidental obs
source("script/2_extract_species_abundance/species/arctic_fox.R") 
fox_density <- arctic_fox %>% 
  dplyr::filter(zone== "study area")
# Gauthier et al., 2023 -> 0.9 hare/100h observation
# Gauthier et al., 2023 -> 9.0 fox/100h observation

#Estimate general density in lowland (where most incidental obs are done)
arha_lowland <- data.frame(
  zone= c("camp 2", "goose point","malaview","qarlikturvik valley",  "camp 3", "dufour"),
  ind_density_km2= mean(fox_density$ind_density_km2)*(0.9/9.0))

#Adjust density in plateau based index of relative abundance in incidental obs
arha <- arha_lowland %>% 
  rbind(data.frame(zone= c("black plateau", "south plateau", "camp 3 plateau"),
                   ind_density_km2= unique(arha_lowland$ind_density_km2) *  relative_abundance_upland[relative_abundance_upland$species== "arctic hare",]$ratio_difference) %>% 
          dplyr::select(zone, ind_density_km2))

#Recalculate density at the study area by combining density in low and upland
arctic_hare<- arha %>% 
  dplyr::left_join(study_area %>%
                     dplyr::select(zone, area_km2) %>%
                     sf::st_drop_geometry()) %>%
  dplyr::mutate(species= "arctic hare") %>% 
  dplyr::group_by(species) %>% 
  dplyr::summarise(ind_density_km2= sum(ind_density_km2* area_km2) /study_area[study_area$zone=="study area",]$area_km2) %>% 
  dplyr::mutate(zone= "study area", year= NA, breeding_status= "undetermined", method= "incidental observations (relative abundance)") %>% 
  dplyr::select(species, zone, year, ind_density_km2, breeding_status, method)