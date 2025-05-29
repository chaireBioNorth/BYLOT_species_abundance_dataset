#Title: Extract the abundance of every vertebrate species from Bylot Island
#Date: October 11 2023
#Last update:  October 10 2024

#-----------------#
#### Libraries ####
#-----------------#
#data manipulation
library(dplyr)
#spatial data manipulation
library(sf)
sf_use_s2(FALSE) #prevent geoprocessing errors
#Personnal functions to estimate species abundance
source("script/functions/FUNS.R")


#-----------------#
#### Read data ####
#-----------------#
#study area
study_area <- sf::st_read("data/raw/shapefiles/study_area.shp")

#years each species was monitored
sampling_year <- read.csv("data/metadata/systematic_nest_monitoring_clean.csv")

#nests
nest_data <-  sf::st_read("data/raw/shapefiles/sampling/nests_2004-2023.shp") 
nest_data_csv <- read.csv("data/raw/sampling/nests_2004-2023.csv")


#transects
transect_data <- sf::st_read("data/raw/shapefiles/sampling/vertebrate_count_transects_2010_2023.shp") %>% 
  #remove groups of shorebirds (>4 individuals) on transects
  dplyr::filter(!c(nb_ind> 4 & species %in% c("white rumped sandpiper", "bairds sandpiper", "pectoral sandpiper", "buff breasted sandpiper", "red knot", "red phalarope", "ruddy turnstone","black bellied plover", "american golden plover") & status == "inconnu"))

#incidental observation
incidental_obs <- read.csv("data/raw/sampling/incidental_obs_2007_2019.csv") %>%
  dplyr::select(-X)

#Index of relative abundance in upland zones
relative_abundance_upland <- read.csv("data/raw/other/incidental_obs_relative_abundance_upland.csv")

#Index of relative abundance from Gauthier et al., 2024
relative_abundance_gauthier <- read.csv("data/metadata/relative_abundance_Gauthier_et_al_2023.csv")

#define zones to estimate abundance
zones <- c("camp 2", "goose point","malaview","qarlikturvik valley",  "camp 3", "black plateau", "south plateau", "camp 3 plateau", "dufour", "study area")

#Import the list of Bylot Island species
sp_taxonomy <- read.csv("DataS1/BYLOT-species_taxonomy.csv") %>% 
  dplyr::mutate(species= gsub("'","", gsub("-"," ",tolower(species_en))))


#-------------------------------------#
#### Pacific and red-throated loon ####
#-------------------------------------#
source("script/2_extract_species_abundance/species/loons.R")

#------------------#
#### Tundra swan ####
#------------------#
source("script/2_extract_species_abundance/species/tundra_swan.R")

#----------------------#
#### Cackling goose ####
#----------------------#
source("script/2_extract_species_abundance/species/cackling_goose.R")

#---------------------#
#### Glaucous gull ####
#---------------------#
source("script/2_extract_species_abundance/species/glaucous_gull.R")
 
#------------------#
#### Snow goose ####
#------------------#
source("script/2_extract_species_abundance/species/snow_goose.R")

#---------------------------------------#
#### King eider and long-tailed duck ####
#---------------------------------------#
source("script/2_extract_species_abundance/species/ducks.R")

#-----------------#
#### Snowy owl ####
#-----------------#
source("script/2_extract_species_abundance/species/snowy_owl.R")

#-------------------------#
#### Rough-legged hawk ####
#-------------------------#
source("script/2_extract_species_abundance/species/rough_legged_hawk.R")

#------------------------#
#### Peregrine falcon ####
#------------------------#
source("script/2_extract_species_abundance/species/peregrine_falcon.R")

#----------------------#
#### Rock ptarmigan ####
#----------------------#
source("script/2_extract_species_abundance/species/rock_ptarmigan.R")

#----------------------#
#### Sandhill crane ####
#----------------------#
source("script/2_extract_species_abundance/species/sandhill_crane.R")

#------------------------------#
#### American-golden plover ####
#------------------------------#
source("script/2_extract_species_abundance/species/american_golden_plover.R")

#----------------------------#
#### Black-bellied plover ####
#----------------------------#
source("script/2_extract_species_abundance/species/black_bellied_plover.R")

#----------------------------#
#### Common-ringed plover ####
#----------------------------#
source("script/2_extract_species_abundance/species/common_ringed_plover.R")

#------------------------#
#### Lapland longpsur ####
#------------------------#
source("script/2_extract_species_abundance/species/lapland_longspur.R")

#-------------------------#
#### Baird's sandpiper ####
#-------------------------#
source("script/2_extract_species_abundance/species/bairds_sandpiper.R")

#-------------------------------------------------#
#### Horned lark, American pipit, snow bunting ####
#-------------------------------------------------#
source("script/2_extract_species_abundance/species/other_passerine.R")

#-------------------------------------#
#### White-rumped sandpiper, pectoral sandpiper, buff-breasted sandpiper, red knot, ruddy turnstone, red phalarope ####
#------------------------------------#
source("script/2_extract_species_abundance/species/other_sandpiper.R")

#---------------------------#
#### Long-tailed jaeger  ####
#---------------------------#
source("script/2_extract_species_abundance/species/long_tailed_jaeger.R")

#------------------------#
#### Parasitic jaeger ####
#------------------------#
source("script/2_extract_species_abundance/species/parasitic_jaeger.R")

#--------------------#
#### Common raven ####
#--------------------#
source("script/2_extract_species_abundance/species/common_raven.R")

#--------------------------------------------#
#### Nearctic Brown and collared lemmings ####
#--------------------------------------------#
source("script/2_extract_species_abundance/species/lemmings.R")

#-------------------#
####  Arctic fox ####
#-------------------#
source("script/2_extract_species_abundance/species/arctic_fox.R") 
#Minimum adult fox individual on around 520 km2 (Royer-Boutin et al., 2015)
# 17 ind -> 0.0327
# 67 ind -> 0.129

#-------------------#
#### Arctic hare ####
#-------------------#
source("script/2_extract_species_abundance/species/arctic_hare.R") 

#-----------------------#
#### American ermine ####
#-----------------------#
source("script/2_extract_species_abundance/species/american_ermine.R") 


#----------------------------------#
#### Combined all data together ####
#----------------------------------#
species_abundances <- loons %>% 
  rbind(ducks,
        cackling_goose,
        snow_goose,
        tundra_swan,
        rough_legged_hawk,
        peregrine_falcon,
        snowy_owl,
        rock_ptarmigan,
        sandhill_crane,
        american_golden_plover,
        black_bellied_plover,
        common_ringed_plover,
        sandpipers,
        bairds_sandpiper,
        glaucous_gull,
        long_tailed_jaeger,
        parasitic_jaeger,
        common_raven,
        passerines,
        lapland_longspur,
        lemming,
        arctic_hare,
        american_ermine,
        arctic_fox) %>%  
  dplyr::filter(zone == "study area")%>%
  #Format data frame
  dplyr::mutate(abundance= round(ind_density_km2* study_area[study_area$zone== "study area",]$area_km2, digits = 0)) %>% 
  dplyr::select(-ind_density_km2) %>%
  dplyr::arrange(species, year)


#--------------------------------------------#
#### Export clean version for the dataset ####
#--------------------------------------------#
abundances_dataset <- species_abundances %>% 
  dplyr::select(-zone) %>% 
  dplyr::arrange(year) 

#Import description of the method for each species
method_summary <- read.csv("data/metadata/summary_methods.csv")

#Add method definition and quality to estimate of abundance
abundances_dataset <- abundances_dataset %>% 
  dplyr::left_join(method_summary, by= join_by("species", "method")) %>% 
  dplyr::left_join(sp_taxonomy) %>% 
  dplyr::select(species_en, year, breeding_status, abundance, method_description, method_quality, spatial_extrapolation)


write.csv(abundances_dataset,"DataS1/BYLOT-species_abundance.csv",row.names = FALSE)
