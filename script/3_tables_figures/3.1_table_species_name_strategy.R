# Libraries
library(dplyr)
library(xtable)

  table_name <- read.csv("DataS1/BYLOT-species_taxonomy.csv") %>% 
  dplyr::select(functional_group, species_scientific, species_en, migratory_status) %>% 
  dplyr::rename(`Functional group`=functional_group,`Scientific name`= species_scientific, `English name`= species_en, `Migratory status`= migratory_status)

#as tex
sp_name_latex <- xtable(table_name, caption = "Species composition of the vertebrate community of Bylot Island with the corresponding migratory status (i.e., resident, partial migrant or migrant).", label= "table:table_species_name_strategy")
print(sp_name_latex,
      include.rownames = FALSE,
      tabular.environment = "tabularx",
      width="0.95\\textwidth",
      align="rXXXXXX",
      size="\\fontsize{10pt}{10pt}\\selectfont",
      file = "MetadataS1/tables/table_species_name_strategy.tex",
      caption.placement = "top")
