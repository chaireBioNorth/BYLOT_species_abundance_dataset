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
table <- read.csv("data/metadata/variables_format.csv") 

#Import data set
species_taxonomy <- read.csv("DataS1/BYLOT-species_taxonomy.csv")
species_abundance <- read.csv("DataS1/BYLOT-species_abundance.csv")
species_body_mass <- read.csv("DataS1/BYLOT-species_body_mass.csv")
variation_nest_density <- read.csv("DataS1/BYLOT-interannual_variation_nest_density.csv")

#Sum of columns
sum(species_abundance$abundance, na.rm = T)
sum(species_body_mass$mean_body_mass_g, na.rm = T)
sum(species_body_mass$sample_size, na.rm = T)
sum(variation_nest_density$mean_nest_density_km2)
sum(variation_nest_density$sd_nest_density_km2)
sum(variation_nest_density$number_years)




#Assign range values
table[table$variable.identity == "year",]$range <- paste0(min(as.numeric(species_abundance$year), na.rm = T), "-", max(as.numeric(species_abundance$year), na.rm = T))

table[table$variable.identity == "abundance",]$range <- paste0(min(species_abundance$abundance), "-", max(species_abundance$abundance))

table[table$variable.identity == "mean_body_mass_g",]$range <- paste0(min(species_body_mass$mean_body_mass_g), " - ", max(species_body_mass$mean_body_mass_g))

table[table$variable.identity == "sample_size",]$range <- paste0(min(species_body_mass$sample_size, na.rm = T), " - ", max(species_body_mass$sample_size, na.rm= T))

table[table$variable.identity == "mean_nest_density_km2",]$range <- paste0(min(variation_nest_density$mean_nest_density_km2), "-", max(variation_nest_density$mean_nest_density_km2))

table[table$variable.identity == "sd_nest_density_km2",]$range <- paste0(min(variation_nest_density$sd_nest_density_km2), "-", max(variation_nest_density$sd_nest_density_km2))

table[table$variable.identity == "number_years",]$range <-paste0(min(variation_nest_density$number_years), "-", max(variation_nest_density$number_years))

#number of digits
table[table$storage.type=="numeric",]$number.digits <- 3
table[table$storage.type=="integer",]$number.digits <- 0

#Rename columns
table <- table %>% 
  dplyr::rename(`Data file`= data.file, `Variable identity`= variable.identity, `Storage type`= storage.type, `Definition variable codes`= definition.variable.codes, `Range`= range, `Number digits`= number.digits)

table[is.na(table)] <- "NA"

#as tex
table_latex <- xtable(table, caption = "Summary of variable storage type, code definition, range and number of digit.",
                      label= "table:table_variables_format",
                      align= c("p{0.00\\textwidth}", 
                               "|p{0.04\\textwidth}|", 
                               "p{0.21\\textwidth}|", 
                               "p{0.07\\textwidth}|",
                               "p{0.40\\textwidth}|", 
                               "p{0.05\\textwidth}|",
                               "p{0.07\\textwidth}|")
                      ) 
#function to bold column header
bold <-  function(x) {paste('{\\textbf{',x,'}}', sep ='')}


print(table_latex,
      include.rownames = FALSE,
      tabular.environment = "longtable",
      hline.after= c(-1,0:nrow(table)),
      size="\\fontsize{8pt}{10pt}\\selectfont",
      file = "MetadataS1/tables/table_variables_format.tex",
      sanitize.colnames.function=bold,
      caption.placement = "top")
