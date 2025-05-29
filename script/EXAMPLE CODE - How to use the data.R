#Title: Code to present a simple example on how to use the data
#Date: May 28 2025
#Last update: NA

#-----------------#
#### Libraries ####
#-----------------#
#data manipulation
library(dplyr)

#------------------------------#
#### Mean species abundance ####
#------------------------------#
#--- Extract mean species abundance
abundance <- read.csv("DataS1/BYLOT-species_abundance.csv", check.names = FALSE) %>% 
  #Change method quality to transform as a semi-quantitative variable
  dplyr::mutate(method_quality=
                  factor(method_quality,
                         levels = c("very low", "low", "moderate", "high"),  ordered = TRUE)) %>%
  #Select the estimate derived from the method with the highest quality for each species
  dplyr::group_by(species_en) %>%
  dplyr::slice_max(order_by = method_quality) %>% 
  #Caculate mean abundance for each species
  dplyr::summarise(mean_abundance= round(mean(abundance), digits = 0)) %>% 
  ungroup()

#------------------------------#
#### Mean species body mass ####
#------------------------------#
body_mass <- read.csv("DataS1/BYLOT-species_body_mass.csv", check.names = FALSE)  %>% 
  #mutate site to transform as a semi-quantitative variable
  dplyr::mutate(site=
                  factor(site,
                         levels = c("undetermined","baffin", "bylot"),  ordered = TRUE)) %>%
  #Retain in priority measurements from Bylot Island and then from the literature
  dplyr::group_by(species_en) %>%
  dplyr::slice_max(order_by = site) %>% 
  ungroup() %>% 
  #Keep only the species name and the mean body mass column
  dplyr::select(species_en, mean_body_mass_g)

#-----------------------#
#### Species biomass ####
#-----------------------#
#Mean abundance (ind.) x Mean body mass = Biomass
abundance_biomass <- abundance %>% 
  #Add mean body mass value of each species to abundance data frame
  dplyr::left_join(body_mass) %>% 
  dplyr::mutate(mean_biomass_kg= (mean_abundance*mean_body_mass_g)/1000)


#----------------------------------#
#### Plot Abundance and Biomass ####
#----------------------------------#
plot(mean_abundance~mean_biomass_kg, data= abundance_biomass,
     log = "xy",
     xlab= "Biomass (kg)",
  ylab="Number of individuals",
  cex=1.2,
  col= "steelblue4",
  pch=16)