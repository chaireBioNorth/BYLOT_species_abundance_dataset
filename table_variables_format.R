#Title: Create a table of all variables and their associated type,code definition and range of values
#Author: Louis Moisan
#Date: September 25 2024

#-----------------#
#### Libraries ####
#-----------------#
#general data manipulation
library(dplyr)
#export .tex
library(xtable)

#Import table structure .csv format
table <- read.csv("MetadataS1/tables/table_data_format.csv") 

#Import data set
species_taxonomy <- read.csv("dataset/BYLOT-species_taxonomy.csv")
species_abundance <- read.csv("dataset/BYLOT-species_abundance.csv")
species_body_mass <- read.csv("dataset/BYLOT-species_body_mass.csv")
variation_nest_density <- read.csv("dataset/BYLOT-interannual_variation_nest_density.csv")

#Assign range values
table[table$Variable.identity == "year",]$Range <- paste0(min(as.numeric(species_abundance$year), na.rm = T), "-", max(as.numeric(species_abundance$year), na.rm = T))
table[table$Variable.identity == "abundance",]$Range <- paste0(min(species_abundance$abundance), "-", max(species_abundance$abundance))
table[table$Variable.identity == "body_mass_g",]$Range <- paste0(min(species_body_mass$body_mass_g), " - ", max(species_body_mass$body_mass_g))
table[table$Variable.identity == "mean_nest_density_km2",]$Range <- paste0(min(variation_nest_density$mean_nest_density_km2), "-", max(variation_nest_density$mean_nest_density_km2))
table[table$Variable.identity == "sd_nest_density_km2",]$Range <- paste0(min(variation_nest_density$sd_nest_density_km2), "-", max(variation_nest_density$sd_nest_density_km2))
table[table$Variable.identity == "sample_size_nest_density_km2",]$Range <-paste0(min(variation_nest_density$sample_size_nest_density_km2), "-", max(variation_nest_density$sample_size_nest_density_km2))

#number of digits
table[table$Storage.type=="numeric",]$Number.digits <- 3
table[table$Storage.type=="integer",]$Number.digits <- 0


unique(species_taxonomy$annual_cycle_strategy)
unique(species_abundance$status)
unique(species_abundance$method)
unique(species_body_mass$source)
unique(variation_nest_density$zone)
