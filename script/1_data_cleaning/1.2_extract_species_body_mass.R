#Title: Extract mean species body mass measured directly on Bylot Island or from the literature
#Author: Louis Moisan
#Date: summer 2024
#Last update: October 10 2024

#-----------------#
#### Libraries ####
#-----------------#
#general data manipulation
library(dplyr)

#--- Import the list of Bylot Island species
sp_taxonomy <- read.csv("DataS1/BYLOT-species_taxonomy.csv") %>% 
  dplyr::mutate(species= gsub("'","", gsub("-"," ",tolower(species_en))))


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
  dplyr::rename(species_scientific= Scientific, mean_body_mass_g= BodyMass.Value, reference= BodyMass.Source)

#filter to keep only species from Bylot
birds_bylot <- birds_bm %>% 
  dplyr::filter(species_scientific %in% sp_taxonomy$species_scientific)


#--- MAMMALS
# Set the download link
mammals_bm_link <- "https://www.esapubs.org/archive/ecol/E095/178/MamFuncDat.txt"
#Download the raw data
download.file(mammals_bm_link, "data/raw/body_mass/mammals_body_mass.txt")
#Extract data frame
mammals_bm <-  read.table(file = "data/raw/body_mass/mammals_body_mass.txt", header = TRUE, dec = ".", sep="\t", fill = TRUE, quote = "") %>% 
  #select only column species and body mass
  dplyr::select(Scientific, BodyMass.Value, BodyMass.Source) %>% 
  dplyr::rename(species_scientific= Scientific, mean_body_mass_g= BodyMass.Value, reference= BodyMass.Source)

#filter to keep only species from Bylot
mammals_bylot <- mammals_bm %>% 
  dplyr::filter(species_scientific %in% sp_taxonomy$species_scientific)

#--- Combined birds and mammals
wilman_2014 <- rbind(birds_bylot, mammals_bylot)

#Identify which species are not included
sp_taxonomy$species_scientific[which(sp_taxonomy$species_scientific %in%wilman_2014$species_scientific == FALSE)]

#Create a vector with the species names that present a different taxonomy
sp_add_scientific <- c("Chen caerulescens","Bubo scandiaca", "Grus canadensis","Tryngites subruficollis", "Mustela erminea")

#Add species to the data frame
wilman_2014 <- wilman_2014 %>% 
  rbind(birds_bm %>% dplyr::filter(species_scientific %in% sp_add_scientific)) %>% 
  rbind(mammals_bm %>% dplyr::filter(species_scientific %in% sp_add_scientific)) %>% 
  #change back to original scientif name
  dplyr::mutate(species_scientific= dplyr::recode(species_scientific, "Chen caerulescens"= "Anser caerulescens", "Grus canadensis"= "Antigone canadensis", "Tryngites subruficollis"="Calidris subruficollis", "Bubo scandiaca"="Bubo scandiacus", "Mustela erminea"= "Mustela richardsonii"),
                site= "undetermined",
                reference= "Wilman et al., 2014",
                sample_size= NA) %>% 
  dplyr::select(species_scientific, site, mean_body_mass_g, sample_size, reference)


#----------------------------------#
#### Measured directly on Bylot ####
#----------------------------------#
#Mean of Arctic fox and ermine extracted from supplementary material of Legagneux, P., Gauthier, G., Berteaux, D., Bêty, J., Cadieux, M. C., Bilodeau, F., ... & Krebs, C. J. (2012). Disentangling trophic relationships in a High Arctic tundra ecosystem through food web modeling. Ecology, 93(7), 1707-1716.
arfo_ermine <- tibble(species_scientific= c("Vulpes lagopus", "Mustela richardsonii"),
                      reference= "Legagneux et al., 2012",
                      mean_body_mass_g= c(3300, 134),
                      sample_size= NA)

#Extract for other species
bylot_sp_body_mass <- read.csv("data/raw/body_mass/individual_body_mass_bylot.csv") %>%
  dplyr::left_join(sp_taxonomy, by="species") %>% 
  dplyr::group_by(species_scientific, reference) %>% 
  dplyr::summarise(mean_body_mass_g= mean(body_mass_g), sample_size= n()) %>% 
  dplyr::ungroup()

#Combined bylot body mass measurements
bylot_body_mass <- bylot_sp_body_mass %>% 
  rbind(arfo_ermine) %>% 
  dplyr::mutate(site= "bylot") %>% 
  dplyr::select(species_scientific, site,  mean_body_mass_g, sample_size, reference)


#---------------------------------#
#### Combined both data frames ####
#---------------------------------#
df_body_mass <- bylot_body_mass %>% 
  rbind(wilman_2014) %>% 
  dplyr::arrange(species_scientific) %>% 
  dplyr::mutate(mean_body_mass_g= round(mean_body_mass_g, digits=0)) %>% 
  dplyr::left_join(sp_taxonomy) %>% 
  dplyr::select(species_en, site,  mean_body_mass_g, sample_size, reference)

#--- Export as .csv
write.csv(df_body_mass, "DataS1/BYLOT-species_body_mass.csv",row.names = FALSE)

