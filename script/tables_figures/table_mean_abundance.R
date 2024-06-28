# Extract Bylot Island temporal series of species abundance and biomass
#Date: November 27 2023

#------------------#
#### Librairies ####
#------------------#
#data manipulation
library(dplyr)
library(tidyr)
library(sf)
#export latex table
library(xtable)



#-------------------#
#### Import data ####
#-------------------#
#Read species final densities
abundances <- read.csv("data/clean/species_abundance.csv")

#Combined all data of snow goose under "combined methods
abundances[abundances$species == "snow goose",]$data <- "combined methods"

#Read study area
study_area <- sf::st_read("data/raw/shapefiles/study_area.shp") %>% 
  sf::st_drop_geometry()

#------------------------------------#
#### Table species mean abundance ####
#------------------------------------#
#--- Species with temporal series
abundance_series <- 
  abundances %>% 
  dplyr::filter(is.na(year)== FALSE) %>% 
  dplyr::mutate(data= gsub(" \\(.*", "", data)) %>%
  dplyr::group_by(species, data) %>% 
 dplyr::summarise(mean_abundance= round(mean(abundance),
                                        digits = 0),
                  sd_abundance= round(sd(abundance), digits=0),
                   n= n(),
                  min_abundance= min(abundance),
                  max_abundance= max(abundance)) 

#Species with only mean
mean_abundance <- 
  abundances %>% 
  dplyr::filter(is.na(year)) %>% 
  dplyr::select(-year, -status,-zone) %>% 
  dplyr::rename(mean_abundance= abundance) %>% 
  dplyr::mutate(sd_abundance= NA, n= NA, min_abundance= NA, max_abundance= NA)

#--- Combined together
table <- abundance_series %>% 
  rbind(mean_abundance) %>% 
  dplyr::mutate(mean_abundance= as.character(mean_abundance), sd_abundance= as.character(sd_abundance)) %>% 
  dplyr::mutate(`Mean ± sd`= ifelse(is.na(sd_abundance),  mean_abundance, paste(mean_abundance, "±", sd_abundance))) %>% 
  dplyr::rename(Minimum= min_abundance, Maximum= max_abundance, Data=data, Species= species) %>% 
  dplyr::select(Species, Data, `Mean ± sd`, n, Minimum, Maximum)


#reorder names based on defined species list
sp_metadata <- read.csv("data/metadata/species_metadata.csv") %>% 
  dplyr::mutate(indice=1:nrow(.)) %>% 
  dplyr::select(species, indice)

# Reorder df1 based on the indices
table <- table %>% 
  dplyr::left_join(sp_metadata, by= c("Species"= "species")) %>% 
  dplyr::arrange(indice) %>% 
  dplyr::select(-indice)

#update species names
table <- table %>% 
  dplyr::mutate(Species= dplyr::recode(Species,"bairds sandpiper"="Baird's sandpiper", "long tailed duck"= "long-tailed duck", "long tailed jaeger"= "long-tailed jaeger", "red throated loon"= "red-throated loon", "pacific loon"= "Pacific loon","rough legged hawk"= "rough-legged hawk", "arctic hare"= "Arctic hare", "black bellied plover"= "black-bellied plover", "buff breasted sandpiper"= "buff-breasted sandpiper", "american pipit"= "American pipit", "arctic fox"="Arctic fox", "common ringed plover"= "common-ringed plover", "american golden plover"= "American golden-plover", "white rumped sandpiper"= "white-rumped sandpiper", "lapland longspur"= "Lapland longspur")) %>% 
  dplyr::rename(Method= Data)

#as tex
table_mean_abundance_latex <- xtable(table, caption = "Mean (± standard deviation) and minimum and maximum (when available) annual abundance of vertebrate species for the whole Bylot Island study area (389 km2). In some cases, several independent approaches have been used to estimate the abundance of a species as a proxy for uncertainty.",
                                     label= "table:species_mean_abudance")
print(table_mean_abundance_latex,
      include.rownames = FALSE,
      tabular.environment = "tabularx",
      width="\\textwidth",
      align="rXXXXXX",
      size="\\fontsize{8pt}{10pt}\\selectfont",
      file = "manuscript/tables/species_mean_abudance.tex",
      caption.placement = "top")



#---------------------------------------#
#### Export as .csv for the data set ####
#---------------------------------------#
abundance_dataset <- abundance_series %>% 
rbind(mean_abundance)%>% 
  dplyr::mutate(species= dplyr::recode(species,"bairds sandpiper"="Baird's sandpiper", "long tailed duck"= "long-tailed duck", "long tailed jaeger"= "long-tailed jaeger", "red throated loon"= "red-throated loon", "pacific loon"= "Pacific loon","rough legged hawk"= "rough-legged hawk", "arctic hare"= "Arctic hare", "black bellied plover"= "black-bellied plover", "buff breasted sandpiper"= "buff-breasted sandpiper", "american pipit"= "American pipit", "arctic fox"="Arctic fox", "common ringed plover"= "common-ringed plover", "american golden plover"= "American golden-plover", "white rumped sandpiper"= "white-rumped sandpiper", "lapland longspur"= "Lapland longspur"))

write.csv(abundance_dataset,"dataset/BYLOT-mean_species_abundance.csv",row.names = FALSE)

