# This script extract mean and standard deviation of annual nest density monitored in systematic plot
#Date: April 16 2024

#------------------#
#### Librairies ####
#------------------#
#data manipulation
library(dplyr)
#spatial data manipulation
library(sf)
sf_use_s2(FALSE)
source("script/functions/FUNS.R")
library(xtable)

#-------------------#
#### Import data ####
#-------------------#
#--- import study area
study_area <- sf::st_read("data/raw/shapefiles/study_area.shp") %>% 
  dplyr::filter(zone != "goose_colony")

#--- import data frame that specify each species that have been monitored in each zone for which years
sampling_plot <- read.csv("data/metadata/species_systematic_nest_monitoring_clean.csv") %>% 
  dplyr::select(-X)%>%
  dplyr::filter(sampled== "yes", species %in% c("lapland longspur", "bairds sandpiper", "sandhill crane", "long tailed duck", "king eider", "long tailed jaeger", "rock ptarmigan")) 

sampling_study_area <- read.csv("data/metadata/species_systematic_nest_monitoring_clean.csv") %>%
  dplyr::select(-X) %>% 
  dplyr::filter(sampled== "yes", species %in% c("cackling goose", "glaucous gull", "red throated loon", "pacific loon", "snowy owl", "tundra swan", "common raven", "common ringed plover", "parasitic jaeger"), zone=="study area") 

#--- nest data
nest_data <-  sf::st_read("data/raw/shapefiles/sampling/nests_2004-2023.shp")
nest_data_csv <- read.csv("data/raw/sampling/nests_2004-2023.csv")

#------------------------------#
#### Calculate nest density ####
#------------------------------#
#Extract nest density for each year (0 are assigned for year not sampled -> those years will be exclude below)
nest_density <- get_nest_density(
  nest_data= nest_data,
  zone_sf= study_area,
  species_list = c(sampling_study_area$species, sampling_plot$species),
  zone_list= study_area$zone,
  year_list= c(1990:2023)) %>% 
  unique() %>% 
  dplyr::arrange(species, zone, year)

#peregrine falcon and rough legged hawk
pefa_rlha <- get_nest_density_missing_coordinates(
  nest_data_csv= nest_data_csv, 
  zone_sf= study_area, 
  species_list= c("rough legged hawk", "peregrine falcon"), 
  zone= "study area", 
  year_list= c(2013:2019, 2022)) %>%
  dplyr::mutate(nest_density_km2= ind_density_km2*2) %>% 
  dplyr::filter(year %in% c(2013:2019, 2022)) %>% 
  dplyr::select(-ind_density_km2)

#filter to keep only year_zone systematically sampled
nest_density_study_area <- nest_density %>% 
  dplyr::semi_join(sampling_study_area, by= c("species", "year", "zone")) %>% 
  dplyr::select(species, zone, year, nest_density_km2)

nest_density_plot <- nest_density %>% 
  dplyr::semi_join(sampling_plot, by= c("species", "year", "zone"))%>% 
  dplyr::select(species, zone, year, nest_density_km2)

#Combine monitoring in plot and at the scale of the study area
nest_density_table <- nest_density_study_area %>% 
  rbind(nest_density_plot) %>% 
  rbind(pefa_rlha) %>% 
  dplyr::group_by(species, zone) %>% 
  dplyr::summarise(mean_nest_density_km2= format(round(mean(nest_density_km2), digits=3),nsmall = 3), sd_nest_density_km2= format(round(sd(nest_density_km2), digits=3), nsmall=3), nb_years= n(), .groups = "drop")


#-------------------------------------------#
#### Format data frame to export as .tex ####
#-------------------------------------------#
table <- nest_density_table %>% 
  dplyr::mutate(mean_nest_density_km2= paste(mean_nest_density_km2, "±", sd_nest_density_km2)) %>% 
  dplyr:::select(-sd_nest_density_km2) %>% 
  dplyr::rename(Species= species, Zone= zone, `Mean ± SD`= mean_nest_density_km2, `Number of years`= nb_years) %>% 
  dplyr::mutate(Zone= dplyr::recode(Zone, "camp 1"= "qarlikturvik valley", "2x1km plot" = "qarlikturvik (2x1 km plot)", "4x2km plot"= "qarlikturvik (4x2 km plot)", "study area"= "whole study area")) %>% 
  dplyr::arrange(Zone, `Mean ± SD`, Species)

#reorder names based on defined species list
sp_metadata <- read.csv("data/metadata/species_metadata.csv")
# Find the indices of species in df2 in df1
indices <- match(sp_metadata$species, table$Species)
# Reorder df1 based on the indices
table <- table[indices, , drop = FALSE] %>% 
  na.omit()

#update species names
table <- table %>% 
  dplyr::mutate(Species= dplyr::recode(Species,"bairds sandpiper"="Baird's sandpiper", "long tailed duck"= "long-tailed duck", "long tailed jaeger"= "long-tailed jaeger", "red throated loon"= "red-throated loon", "pacific loon"= "Pacific loon","rough legged hawk"= "rough-legged hawk", "lapland longspur"= "Lapland longspur")) %>% 
  dplyr::arrange(Zone)


#as tex
table_latex <- xtable(table, caption = "Annual nest density (nests/km2) of selected avian species estimated on different zones of Bylot Island.",
                      label= "table:interannual_nest_density_variation")
print(table_latex,
      include.rownames = FALSE,
      tabular.environment = "tabularx",
      width="0.9\\textwidth",
      align="rXXXXXX",
      size="\\fontsize{10pt}{10pt}\\selectfont",
      file = "manuscript/tables/interannual_nest_density_variation.tex",
      caption.placement = "top")



#---------------------------------------#
#### Export as .csv for the data set ####
#---------------------------------------#
table_dataset <- table %>% 
  dplyr::rename(species= Species,
                zone= Zone,
                mean= `Mean ± SD`,
                n=`Number of years`) %>% 
  dplyr::mutate(mean= as.numeric(gsub(" ±.*","",table$`Mean ± SD`)),
                sd= as.numeric(gsub(".* ±","",table$`Mean ± SD`))) %>% 
  dplyr::select(species, zone, mean, sd, n)


write.csv(table_dataset,"dataset/BYLOT-interannual_variation_nest_density.csv",row.names = FALSE)
  