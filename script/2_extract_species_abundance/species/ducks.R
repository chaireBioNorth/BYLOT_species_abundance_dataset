#Title: Extract the abundance of King eiders and Long-tailed ducks on the Bylot Island study area 
#Date: October 11 2023
#Last update:  October 10 2024

#!!! The density of American golden plover need to extract before running the following code !!!
# source("script/2_extract_species_abundance/species/loons.R")

#---------------------------------------------------#
#### Extract mean nest density in the 8 km2 plot ####
#---------------------------------------------------#
#Estimate density from the 4x2 km plot in qarlikturvik valley
kiei_ltdu_nest <- get_nest_density(
  nest_data= nest_data,
  zone_sf= study_area,
  species_list = c("long tailed duck", "king eider"),
  zone_list= "4x2km plot",
  year_list= sampling_year[sampling_year$species == "long tailed duck" & sampling_year$zone == "4x2km plot" & sampling_year$sampled=="yes",]$year)

#Calculate mean density in the 8km2 plot
kiei_ltdu_density <- kiei_ltdu_nest %>% 
  dplyr::group_by(species, zone) %>% 
  dplyr::summarise(ind_density_km2= mean(ind_density_km2),
                   prop_wet_plot= study_area[study_area$zone=="4x2km plot",]$prop_wet)

#--- Extrapolate the mean nest density to the wetlands of the study area
ducks <- kiei_ltdu_density %>% 
  #Add proportion of wetland study area
  cbind(prop_wet_study_area=study_area[study_area$zone=="study area",]$prop_wet) %>%
  #Calculate density at the study area by adjusting density by proportion wetlands qarlikturvik valley
  dplyr::mutate(zone= "study area", year= NA,
                ind_density_km2= ind_density_km2*prop_wet_study_area/prop_wet_plot,
                breeding_status= "breeding",
                method= "nest sampling (extrapolation habitat)") %>% 
  dplyr::select(species, zone, year, ind_density_km2, breeding_status, method)


#---Alternative approach based on Gauhtier et al., 2023 indexes
ducks_alternative <-relative_abundance_gauthier %>% 
  dplyr::filter(species %in% c("long tailed duck", "king eider")) %>% 
  #Add density of red throated loons as reference
  dplyr::left_join(
    loons %>% 
      dplyr::filter(zone=="study area", species== "red throated loon") %>% 
      dplyr::group_by(species, zone) %>% 
      dplyr::summarise(ref_ind_density_km2= mean(ind_density_km2))
    , by= c("ref_species"="species")) %>% 
  #Cross-multiplication to determine absolute abundance from relative abundance 
  dplyr::mutate(year= NA,
                ind_density_km2= ref_ind_density_km2*(index_sp/index_ref_sp),
                breeding_status= "undetermined",
                method= "incidental observations (relative abundance)") %>% 
  dplyr::select(species, zone, year, ind_density_km2, breeding_status, method)

#----------------------------#
#### Combined data frames ####
#----------------------------#
ducks <- ducks %>% 
  rbind(ducks_alternative)