# Libraries
library(dplyr)
library(xtable)

table_name <- read.csv("data/metadata/species_metadata.csv") %>% 
  dplyr::select(functional_group, scientific, species, annual_strategy) %>% 
  dplyr::rename(`Functional group`=functional_group,`Scientific name`= scientific, Name= species, `Annual cycle strategy`= annual_strategy)

#update species names
table_name <- table_name %>% 
  dplyr::mutate(Name= dplyr::recode(Name,"bairds sandpiper"="Baird's sandpiper", "long tailed duck"= "long-tailed duck", "long tailed jaeger"= "long-tailed jaeger", "red throated loon"= "red-throated loon", "pacific loon"= "Pacific loon","rough legged hawk"= "rough-legged hawk", "arctic hare"= "Arctic hare", "black bellied plover"= "black-bellied plover", "buff breasted sandpiper"= "buff-breasted sandpiper", "american pipit"= "American pipit", "arctic fox"="Arctic fox", "common ringed plover"= "common-ringed plover", "american golden plover"= "American golden-plover", "white rumped sandpiper"= "white-rumped sandpiper","lapland longspur"= "Lapland longspur"))



#as tex
sp_name_latex <- xtable(table_name, caption = "Species composition of the vertebrate community of Bylot Island with the corresponding annual cycle strategy (i.e., resident, partial migrant, migrant).", label= "table:species_name_strategy")
print(sp_name_latex,
      include.rownames = FALSE,
      tabular.environment = "tabularx",
      width="0.95\\textwidth",
      align="rXXXXXX",
      size="\\fontsize{10pt}{10pt}\\selectfont",
      file = "manuscript/tables/species_name_strategy.tex",
      caption.placement = "top")


#---------------------------------------#
#### Export as .csv for the data set ####
#---------------------------------------#
table_name %>% 
  dplyr::rename(functional_group= `Functional group`,
                scientific_name= `Scientific name`,
                species= Name,
                annual_cycle_strategy=`Annual cycle strategy`) %>% 
  write.csv("dataset/BYLOT-community_composition.csv", row.names = FALSE)

