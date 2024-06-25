#Title: Create a summary table of all data and method use for each species
#Author: Louis Moisan
#Date: June 3 2024

#-----------------#
#### Libraries ####
#-----------------#
#general data manipulation
library(dplyr)
#export .tex
library(xtable)


#-----------------------#
#### Import metadata ####
#-----------------------#
table <- read.csv("data/metadata/summary_data_sources.csv") %>% 
  dplyr::rename(`Sampling method`=Density.sampling.method,
               `Reference species`=Reference.species,
               `Sampling scale`=Sampling.scale,
               `Spatial extrapolation`=Spatial.extrapolation)

#as tex
table_latex <- xtable(table, caption = "Summary of the methods and spatial scales used to estimate the density of each vertebrate species on Bylot Island, and how abundance was extrapoated at the landscape scale.",
                      label= "table:summary_methods")
print(table_latex,
      include.rownames = FALSE,
      tabular.environment = "tabularx",
      table.placement = "H",
      width="\\textwidth",
      align="rXXXXXX",
      size="\\fontsize{8pt}{10pt}\\selectfont",
      file = "manuscript/tables/table_summary_methods.tex",
      caption.placement = "top")

