#librairies
library(dplyr)
#export latex table
library(xtable)

#import as .csv
relat_abund_compared <- read.csv("data/raw/other/incidental_obs_relative_abundance_upland.csv")

#----------------------#
#### Export as .tex ####
#----------------------#
table <- relat_abund_compared %>% 
  dplyr::mutate(nb_ind_hour_uplands= as.character(round(nb_ind_hour_upland, digits = 3)), 
                nb_ind_hour_lowland= as.character(round(nb_ind_hour_lowland, digits = 3)),
                ratio_difference= as.character(ratio_difference)) %>% 
  dplyr::rename(Uplands=nb_ind_hour_upland,
                Lowlands=nb_ind_hour_lowland,
                Ratio= ratio_difference
  )


#reorder names based on defined species list
sp_metadata <- read.csv("data/metadata/species_metadata.csv")
# Find the indices of species in df2 in df1
indices <- match(sp_metadata$species, table$species)
# Reorder df1 based on the indices
table <- table[indices, , drop = FALSE] 

#update species names
table <- table %>% 
  dplyr::mutate(species= dplyr::recode(species,"bairds sandpiper"="Baird's sandpiper", "long tailed duck"= "long-tailed duck", "long tailed jaeger"= "long-tailed jaeger", "red throated loon"= "red-throated loon", "pacific loon"= "Pacific loon","rough legged hawk"= "rough-legged hawk", "arctic hare"= "Arctic hare", "black bellied plover"= "black-bellied plover", "buff breasted sandpiper"= "buff-breasted sandpiper", "american pipit"= "American pipit", "arctic fox"="Arctic fox", "common ringed plover"= "common-ringed plover", "american golden plover"= "American golden-plover", "white rumped sandpiper"= "white-rumped sandpiper", "lapland longspur"= "Lapland longspur"))


#Filter some species
table <- table %>% 
  dplyr::filter(species %in% c("American golden-plover",
                               "American pipit",
                               "Arctic hare",
                               "Baird's sandpiper",
                               "black-bellied plover",
                               "buff-breasted sandpiper",
                               "horned lark",
                               "lapland longspur",
                               "pectoral sandpiper",
                               "red knot",
                               "red phalarope",
                               "rock ptarmigan",
                               "ruddy turnstone",
                               "sandhill crane",
                               "snow bunting",
                               "white-rumped sandpiper")) %>% 
  dplyr::left_join(sp_metadata %>% dplyr::select(species, functional_group)) %>% 
  dplyr::select(species, Uplands,Lowlands, Ratio) %>% 
  dplyr::rename(Species=species)

custom_row <- c("\\hline", "Custom Header", "\\hline")

#as tex
table_latex <- xtable(table, caption = "Index of relative abundance (i.e., number of individuals observed per hour) derived from incidental daily observations for selected vertebrate species in lowland (i.e., Qarlikturvik valley, Camp 3, Malaview, Camp 2, Goose point and Pointe Dufour) and upland (i.e., Black plateau, Southern plateau and Camp 3 plateau) zones of the Bylot Island study area. The ratio compares relative abundance indexes between these two types of zones, calculated by dividing the upland by the lowland index of relative abundance.",label= "table:species_relative_abundance_upland")


addtorow <- list()
addtorow$pos <- list(0, 0)
addtorow$command <- c("& \\multicolumn{2}{c}{Individuals/hour} \\\\\n",
                      "Species & Upland & Lowland & Ratio \\\\\n")

print(table_latex,
      hline.after = c(-1, 0, nrow(table)),
      include.rownames = FALSE,
      include.colnames = FALSE,
      add.to.row = addtorow,
      tabular.environment = "tabularx",
      width="0.55\\textwidth",
      align="rXXXXXX",
      size="\\fontsize{10pt}{10pt}\\selectfont",
      file = "MetadataS1/tables/species_relative_abundance_upland.tex",
      caption.placement = "top")
