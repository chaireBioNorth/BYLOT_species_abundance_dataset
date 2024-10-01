#----List of all functions used in the project----#

#-------------------------------------------------------------------------#
#### Extract metrics from raw data that can be used in other functions ####
#-------------------------------------------------------------------------#
#Extract nest density for a list of species, zones and years
source("script/functions/standalone_functions/get_nest_density.R")

#Extract nest density for a list of species and years in a given zone when coordinates are missing
source("script/functions/standalone_functions/get_nest_density_missing_coordinates.R")

#Extract the annual mean number of individual observed per transect of the defined status for each species in the selected zones
source("script/functions/standalone_functions/number_ind_transects.R")



#---------------------------------------------------------------#
#### Estimate species density in each zone of the study area ####
#---------------------------------------------------------------#
#Spatially extrapolate species density monitored on a local plot to other zones of the study area using a cross multiplication based on a reference species with known abundance. Possibility to assign correction for wetlands and plateauous zones.
source("script/functions/nested_functions/spatial_extrapolation_species_density.R")

# Extract species density by doing a rule of three with the relative proportion of observation among the selected species in each zone and the known species density of one of the species
source("script/functions/nested_functions/estimate_density_from_reference_species.R")

#Extract species density in selected zones and years based on the selected parameter of the detection function (see script/1_data_cleaning/09_extract_american_golden_plover_distance & 10_extract_detection_function_american_golden_plover)
source("script/functions/standalone_functions/extract_density_distance_sampling.R")

#---------------------------------------------------------#
#### Extend the temporal abundance series of a species ####
#---------------------------------------------------------#
# Function to extend temporal series when abundance in a zone is tighly correlated to abundance at the study area using nest data
source("script/functions/nested_functions/extend_temporal_series_from_single_zone_nest.R")

#
source("script/functions/nested_functions/extend_temporal_series_from_single_zone_missing_coordinates.R")
