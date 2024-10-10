#Title: Extract the abundance of Baird's sandpiper on the Bylot Island study area 
#Date: October 11 2023
#Last update:  October 10 2024

#---------------------------------------------------------------------#
#### Extract regression nest density and individuals per transects ####
#---------------------------------------------------------------------#
#Define location to save figure regression nest density and transect
pdf("MetadataS1/figures/bairds_sandpiper_nest_transect.pdf")

#Correlation of Baird's sandpiper local nest density and proportion of transects with at least one individual observed
bairds_sandpiper<- spatial_extrapolation_species_density(
  nest_data= nest_data,
  transect_data= transect_data,
  transect_metric= "proportion of transect with individual",
  zone_sf= study_area,
  zone_reference_nest= "2x1km plot",
  zone_reference_transect= "4x2km plot",
  #list of years to use to extract mean values
  years_reference= sampling_year[sampling_year$species == "bairds sandpiper" & sampling_year$zone == "2x1km plot" & sampling_year$sampled=="yes",]$year,
  zone_list= zones,
  sp="bairds sandpiper",
  status_list= c("repro", "inconnu", "non repro"),
  relative_abundance_upland= relative_abundance_upland,
  null_density_wetlands= FALSE)%>% 
  dplyr::mutate(breeding_status= "breeding", method= "nest sampling (extrapolation transects)", year= NA) %>% 
  dplyr::select(species, zone, year, ind_density_km2, breeding_status, method)

#Save the figure
dev.off()