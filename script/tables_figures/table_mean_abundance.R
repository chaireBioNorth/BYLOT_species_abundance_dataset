# Extract Bylot Island temporal series of species abundance and biomass
#Date: November 27 2023

#------------------#
#### Librairies ####
#------------------#
#data manipulation
library(dplyr)
library(tidyr)
#export latex table
library(xtable)



#-------------------#
#### Import data ####
#-------------------#
#metadata
metadata <- read.csv("data/metadata/species_method_selected.csv", sep = "\t")
#reorder names based on defined species list
sp_metadata <- read.csv("data/metadata/species_metadata.csv")
# Find the indices of species in df2 in df1
indices <- match(sp_metadata$species, metadata$species)
# Reorder df1 based on the indices
metadata <- metadata[indices, , drop = FALSE] 

#update species names
metadata <- metadata %>% 
  dplyr::mutate(species= dplyr::recode(species,"bairds sandpiper"="Baird's sandpiper", "long tailed duck"= "long-tailed duck", "long tailed jaeger"= "long-tailed jaeger", "red throated loon"= "red-throated loon", "pacific loon"= "Pacific loon","rough legged hawk"= "rough-legged hawk", "arctic hare"= "Arctic hare", "black bellied plover"= "black-bellied plover", "buff breasted sandpiper"= "buff-breasted sandpiper", "american pipit"= "American pipit", "arctic fox"="Arctic fox", "common ringed plover"= "common-ringed plover", "american golden plover"= "American golden-plover", "white rumped sandpiper"= "white-rumped sandpiper", "lapland longspur"= "Lapland longspur"))


#Read species final densities
#Abundance data
abundance <- read.csv("dataset/BYLOT-species_abundance.csv") %>%
  dplyr::mutate(method= ifelse(species== "cackling goose" & year<= 2016, "nest sampling (model)", method)) %>% 
  dplyr::mutate(year= ifelse(is.na(year), "mean", year),
                method= dplyr::recode(method, "nest sampling (extrapolation nests)"= "nest sampling")) %>% 
  dplyr::group_by(species, method) %>% 
  dplyr::summarise(mean= as.character(round(mean(abundance), digits=0)), sd= as.character(round(sd(abundance), digits= 0)), n= n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(n= ifelse(n==1, NA, n))


#--------------------------#
#### Generate the table ####
#--------------------------#
#Join data frames together
table <- metadata %>% 
  dplyr::left_join(abundance, by= c("species", "method")) %>% 
  dplyr::filter(is.na(mean)== FALSE)

#format data frame
table <- table %>% 
  dplyr::rename(Method= method, Species=species, Mean= mean)


#as tex
table_latex <- xtable(table, caption = "Selected methods to represent the mean (± standard deviation) of annual abundance of vertebrate species for the whole Bylot Island study area (389 km2).",
                                     label= "table:species_mean_abudance")
print(table_latex,
      include.rownames = FALSE,
      tabular.environment = "tabularx",
      width="0.85\\textwidth",
      align="rXXXXXX",
      size="\\fontsize{8pt}{10pt}\\selectfont",
      file = "manuscript/tables/species_mean_abudance.tex",
      caption.placement = "top")



#---------------------------------------#
#### Export as .csv for the data set ####
#---------------------------------------#
table_abundance <- table %>% 
  dplyr::rename(species= Species, method= Method, mean_abundance= Mean, sd_abundance= sd, sample_size_abundance= n)

write.csv(table_abundance,"dataset/BYLOT-mean_species_abundance.csv",row.names = FALSE)

