#Title: Extract the abundance of Sandhill cranes on the Bylot Island study area 
#Date: October 11 2023
#Last update:  October 10 2024

#---------------------------------------------------------------------#
#### Extract regression nest density and individuals per transects ####
#---------------------------------------------------------------------#
#Define location to save figure regression nest density and transect
pdf("MetadataS1/figures/sandhill_crane_nest_transects.pdf")

#Estimate a mean annual abundance of sandhill cranes based on the mean number of individuals observed per transect in each zone.
sandhill_crane<- spatial_extrapolation_species_density(
  nest_data= nest_data,
  transect_data= transect_data,
  transect_metric= "mean number of individual per transect",
  zone_sf= study_area,
  zone_reference_nest= "4x2km plot",
  zone_reference_transect= "4x2km plot",
  #list of years that nest density was measured in the 4x2 km plot
  years_reference= sampling_year[sampling_year$species == "sandhill crane" & sampling_year$zone == "4x2km plot" & sampling_year$sampled=="yes",]$year,
  zone_list= zones,
  sp="sandhill crane",
  status_list= c("repro", "inconnu", "non repro"),
  relative_abundance_upland= relative_abundance_upland,
  null_density_wetlands= FALSE) %>% 
  dplyr::filter(zone =="study area") %>% 
  dplyr::mutate(year= NA, breeding_status= "breeding", method= "nest sampling (extrapolation transects)") %>% 
  dplyr::select(species, zone, year, ind_density_km2, breeding_status, method)

#Save figure
dev.off()  