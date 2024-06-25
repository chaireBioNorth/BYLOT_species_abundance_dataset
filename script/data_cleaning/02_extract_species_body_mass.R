#Title: Extract mean species body mass measured directly on Bylot Island or from the literature
#Author: Louis Moisan
#Date: NA
#Last update: June 11 2024

#-----------------#
#### Libraries ####
#-----------------#
#general data manipulation
library(dplyr)



#--- Import the list of Bylot Island species
bylot_sp <- read.csv("data/metadata/species_metadata.csv")


#--------------------------------#
#### Measures from literature ####
#--------------------------------#
# Wilman, H., Belmaker, J., Simpson, J., de la Rosa, C., Rivadeneira, M. M., & Jetz, W. (2014). EltonTraits 1.0: Species‐level foraging attributes of the world's birds and mammals: Ecological Archives E095‐178. Ecology, 95(7), 2027-2027.

#--- BIRDS
# Set the download link
birds_bm_link <- "https://www.esapubs.org/archive/ecol/E095/178/BirdFuncDat.txt"
#Download the raw data
download.file(birds_bm_link, "data/raw/body_mass/birds_body_mass.txt")
#Extract data frame
birds_bm <-  read.table(file = "data/raw/body_mass/birds_body_mass.txt", header = TRUE, dec = ".", sep="\t", fill = TRUE, quote = "") %>% 
  #select only column species and body mass
  dplyr::select(Scientific, BodyMass.Value, BodyMass.Source) %>% 
  dplyr::rename(scientific= Scientific,body_mass_g= BodyMass.Value, reference= BodyMass.Source)

#filter to keep species from Bylot
birds_bylot <- birds_bm %>% 
  dplyr::filter(scientific %in% bylot_sp$scientific)


#--- MAMMALS
# Set the download link
mammals_bm_link <- "https://www.esapubs.org/archive/ecol/E095/178/MamFuncDat.txt"
#Download the raw data
download.file(mammals_bm_link, "data/raw/body_mass/mammals_body_mass.txt")
#Extract data frame
mammals_bm <-  read.table(file = "data/raw/body_mass/mammals_body_mass.txt", header = TRUE, dec = ".", sep="\t", fill = TRUE, quote = "") %>% 
  #select only column species and body mass
  dplyr::select(Scientific, BodyMass.Value, BodyMass.Source) %>% 
  dplyr::rename(scientific= Scientific,body_mass_g= BodyMass.Value, reference= BodyMass.Source)

#filter species of bylot
mammals_bylot <- mammals_bm %>% 
  dplyr::filter(scientific %in% bylot_sp$scientific)

#--- Combined birds and mammals
wilman_2014 <- rbind(birds_bylot, mammals_bylot)

#Which species are not included
bylot_sp$scientific[which(bylot_sp$scientific %in%wilman_2014$scientific == FALSE)]
#create a vector with the different species names
sp_add_scientific <- c("Chen caerulescens","Grus canadensis","Bubo scandiaca","Tryngites subruficollis", "Mustela erminea")

#Add species to the data frame
wilman_2014 <- wilman_2014 %>% 
  rbind(birds_bm %>% dplyr::filter(scientific %in% sp_add_scientific)) %>% 
  rbind(mammals_bm %>% dplyr::filter(scientific %in% sp_add_scientific)) %>% 
  #change back to original scientif name
  dplyr::mutate(scientific= dplyr::recode(scientific, "Chen caerulescens"= "Anser caerulescens", "Grus canadensis"= "Antigone canadensis", "Tryngites subruficollis"="Calidris subruficollis", "Bubo scandiaca"="Bubo scandiacus", "Mustela erminea"= "Mustela richardsonii")) %>% 
#Add english names
  dplyr::left_join(bylot_sp) %>% 
  dplyr::select(functional_group,species, scientific, body_mass_g) %>% 
  dplyr::rename(`Functional group`= functional_group, `Species name`= species, `Scientific name`= scientific, `Body mass g (literature)`= body_mass_g)


#----------------------------------#
#### Measured directly on Bylot ####
#----------------------------------#
#Mean of Arctic fox and ermine extracted from supplementary material of Legagneux, P., Gauthier, G., Berteaux, D., Bêty, J., Cadieux, M. C., Bilodeau, F., ... & Krebs, C. J. (2012). Disentangling trophic relationships in a High Arctic tundra ecosystem through food web modeling. Ecology, 93(7), 1707-1716.
arfo_ermine <- tibble(`Species name`= c("Arctic fox", "ermine"),
                          `Body mass g (study area)`= c(3300, 134),
                          n= NA)

#Extract for other species
bylot_sp_body_mass <- read.csv("data/raw/body_mass/individual_body_mass_bylot.csv") %>% 
  dplyr::rename(`Species name`=species) %>% 
  dplyr::group_by(`Species name`) %>% 
  dplyr::summarise(`Body mass g (study area)`= mean(body_mass_g), n= n())

#Combined bylot body mass measurements
bylot_body_mass <- bylot_sp_body_mass %>% 
  rbind(arfo_ermine)

# Left-join the 2 datasets
df_body_mass <- left_join(wilman_2014, bylot_body_mass, by = "Species name") %>% 
  dplyr::select(`Scientific name`, `Species name`, `Body mass g (study area)`,n, `Body mass g (literature)`)

#--- Export as .csv
write.csv(df_body_mass, "data/clean/species_body_mass.csv")
