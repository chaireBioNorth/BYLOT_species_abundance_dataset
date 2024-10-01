#data manipulation
library(dplyr)
library(tidyr)
#spatial data manipulation
library(sf)
#export latex table
library(xtable)

#--- Import study area 
#list of zones for which we want to obtain a density
zone_list <- c("camp 2", "goose point","malaview","qarlikturvik valley",  "camp 3", "black plateau", "south plateau", "camp 3 plateau", "dufour", "study area")
#shapefile of the area
study_area <- sf::st_read("data/raw/shapefiles/study_area.shp") %>% 
  sf::st_drop_geometry() %>% 
  #keep only non-nested zones
  dplyr::filter(zone %in% zone_list)

#--- Filter nest data to keep only nest found during years of systematic survey
sp_monitoring <- read.csv("data/metadata/systematic_nest_monitoring_raw.csv")
#reorder names based on defined species list
sp_taxonomy <- read.csv("DataS1/BYLOT-species_taxonomy.csv") %>% 
  dplyr::mutate(species= gsub("'","", gsub("-"," ",tolower(species_en))))
# Reorder df1 based on the species meta data order
sp_monitoring <- sp_taxonomy %>% 
  dplyr::left_join(sp_monitoring) %>% 
  na.omit()

#--- Extract standardize and extract a tex table
table_year_monitoring <- sp_monitoring %>% 
  dplyr::mutate(zone= dplyr::recode(zone, "camp 1"= "qarlikturvik valley","trapping grids"= "qarlikturvik (trapping grids)", "2x1km plot"= "qarlikturvik (2x1 km plot)", "4x2km plot"= "qarlikturvik (4x2 km plot)","black plateau"= "black plateau", "south plateau"= "south plateau", "study area"= "whole study area")) %>% 
  dplyr::mutate(number_years= paste("(", number_years, ")", sep="")) %>% 
  dplyr::rename(Species= species_en, Zone= zone, Years= year, `Number of years`= number_years, Monitoring= monitoring) %>% 
  dplyr::select(Species, Zone, Years, `Number of years`, Monitoring) %>% 
  dplyr::arrange(Zone) 

# Convert the zone variable to a factor with the custom order
table_year_monitoring$Zone <- as.character(factor(table_year_monitoring$Zone, levels = c("qarlikturvik (trapping grids)","qarlikturvik (2x1 km plot)","qarlikturvik (4x2 km plot)","qarlikturvik valley","camp 2","whole study area")))

#Change qarlikturvik snowy owl for qarlikturvik, black and south plateaus
table_year_monitoring[table_year_monitoring$Species == "Snowy owl" & table_year_monitoring$Zone == "qarlikturvik valley",]$Zone <- "qarlikt., black & south plat."
#same for rough-legged hawk and rough legged hawk
table_year_monitoring[table_year_monitoring$Species == "Peregrine falcon" & table_year_monitoring$Zone == "qarlikturvik valley",]$Zone <- "qarlikt., black & south plat."
table_year_monitoring[table_year_monitoring$Species == "Rough-legged hawk" & table_year_monitoring$Zone == "qarlikturvik valley",]$Zone <- "qarlikt., black & south plat."

# Reorder rows based on the zone variable
table_year_monitoring <- table_year_monitoring[order(table_year_monitoring$Zone), ]

#as tex
table_year_monitoring_latex <- xtable(table_year_monitoring, caption = "Summary of vertebrate species monitoring in the Bylot Island study area. In this paper, we excluded certain years for specific species due to reduced sampling efforts. As a result, duration of times series presented here may differ slightly from those in Gauthier et al. (2024b).",
                                      label= "table:table_species_year_monitoring")

print(table_year_monitoring_latex,
      include.rownames = FALSE,
      tabular.environment = "tabularx",
      width="\\textwidth",
      align="rXXXXXX",
      size="\\fontsize{8pt}{10pt}\\selectfont",
      file = "MetadataS1/tables/table_species_year_monitoring.tex",
      caption.placement = "top")

