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
  dplyr::rename(`Sampling method`=Sampling.method,
               `Reference species`=Reference.species,
               `Sampling scale`=Sampling.scale,
               `Spatial extrapolation`=Spatial.extrapolation)

#as tex
table_latex <- xtable(table, caption = "Summary of the methods and spatial scales used to estimate the density of each vertebrate species on Bylot Island, and how abundance was extrapolated at the landscape scale. When sampling consist in relative abundance, we assumed that the ratios between relative and actual abundance are the same between the targeted species and a given reference species (i.e., similar detection probability). We therefore derived the absolute abundance of the targeted species from its relative abundance using the ratio between relative and absolute abundances of the reference species. Additionally, here the area of each sampling scale: two 11 ha grids (0.22 km2), 2x1km plot (2 km2) 4x2km plot (8 km2), south qarlikturvik (33 km2), camp 2 (80 km2), lowland (313 km2) and whole study area (389 km2).",
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

