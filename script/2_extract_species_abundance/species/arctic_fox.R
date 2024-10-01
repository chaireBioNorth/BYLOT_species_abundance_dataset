#Title: Estimate density of arctic foxes on the southern plain of Bylot Island from foxes home range size presented in Dulude-de-Broin 2023
#Original author: Frédéric Dulude-de-Broin
#Original script: https://datadryad.org/stash/dataset/doi:10.5061/dryad.f1vhhmh30
#Adapted by: Louis Moisan


#------------------#
#### Librairies ####
#------------------#
#data manipulation
library(dplyr)
#Generalized linear model
library(lme4)
library(AICcmodavg)
#spatial data manipulation
library(sf)

#---------------------------------------------------------------#
#### Code from Dulude et al., 2023 to predict fox home range ####
#---------------------------------------------------------------#
# import homerange and covariates
    dataHR_all <- read.csv(file = "data/raw/sampling/arctic_fox_home_range.csv")
    
    # scale all homerange based on the relationships between simulated Argos and GPS data
    dataHR_all$area <-  0.91570 + 0.72824*dataHR_all$area_argos
  
    # compute log home range
    dataHR_all$l_area.gps <- log(dataHR_all$area)
    
    # format column
    dataHR <- dataHR_all %>% 
                mutate(sex = factor(sex, levels = c("F", "M")),
                       rep_status = factor(rep_status, levels= c("Non-breeder", "Breeder")),
                       goose_category = factor(goose_category, levels = c("outside colony", "outside with overlap", "inside colony")),
                       centro_in_colony = factor(centro_in_colony, levels = c(0, 1)),
                      ) %>% 
                na.omit()
  
  
  #.... fit model to test P1: Effect of primary prey availability on HR size ####
    mod1 <- lmer(l_area.gps ~ goose_category + sex + rep_status + (1|id_merged_pair) + (1|year), weights = 1/VAR.log, data=dataHR)

  #.... predict values ####
    # dumy dataset
    new.data <- unique(dataHR[, c("goose_category", "sex", "rep_status")])
    
    # predictions
      pred <- AICcmodavg::predictSE(mod1, newdata=new.data, se.fit=T, re.form=NA) 
    
    # on response scale
    new.data$pred.fit <- exp(pred$fit)
    new.data$pred.se <- pred$se.fit
    new.data$upperCI <- exp(pred$fit+2*pred$se.fit)
    new.data$lowerCI <- exp(pred$fit-2*pred$se.fit)
    new.data <- new.data %>% filter(sex=="F", rep_status == "Breeder", goose_category != "outside with overlap")

    
#--------------------------------------------------------#  
#### Extract artic fox individual density for each zone of the study area ####
#-------------------------------------------------------#
#--- Import the study area shapefile
study_area <- sf::st_read("data/raw/shapefiles/study_area.shp")
    
# Calcul the average snow goose colony area size
in_colony <- mean(study_area[study_area$zone== "goose_colony", ]$area_km2)
    
#calcul the area size outside of the colony in the study area
out_colony <- study_area[study_area$zone == "study area",]$area_km2 - in_colony
    
#import proportion of overlap of goose colony and the different zones
overlap_colony <- read.csv("data/raw/other/proportion_goose_colony_zone.csv") %>% 
      # averaging proportion of overlap with the colony by zone
      dplyr::group_by(zone) %>% 
      dplyr::summarise(prop_overlap_colony= round(mean(prop_overlap_colony), digits= 2), .groups = "drop")
    

#--- Prepare arctic fox data
fox_density <- new.data %>% 
  dplyr::select(goose_category, pred.fit, pred.se) %>% 
  dplyr::rename(goose_colony=goose_category, home_range_km2= pred.fit, se=pred.se) %>% 
  #transform into individual density with the formula 2/0.82* predict home range. 0.82 is the measured proportion of overlap between adjacent home range
  dplyr::mutate(ind_density_km2= 2/(home_range_km2*0.82)) %>% 
  dplyr::select(-se, -home_range_km2)


    #Add area size in and out colony in fox data frame
    #create empty area size variable
fox_density$area_km2= NA
fox_density[fox_density$goose_colony == "outside colony", ]$area_km2 <- out_colony
fox_density[fox_density$goose_colony == "inside colony", ]$area_km2 <- in_colony
    
    #Create a data frame to store the density of foxes for each zone
    fox_density_df <- overlap_colony %>%
      #create a empty column to store individual density
      dplyr::mutate(ind_density_km2= NA)
    
    # Extract the density of foxes for each zone by taking into account the proportion of the goose colony in each zone
    for (i in 1:nrow(fox_density_df)){
      #area
      zone= fox_density_df$zone[i]
      #Extract abundance of fox for the portion with goose colony in the zone
      nb_fox_in= fox_density[fox_density$goose_colony == 
                       "inside colony", ]$ind_density_km2 *
        overlap_colony[overlap_colony$zone == zone, ]$prop_overlap_colony *
        study_area[study_area$zone == zone, ]$area_km2
      #Extract abundance of fox for the portion without goose colony in the zone
      nb_fox_out= fox_density[fox_density$goose_colony == 
                        "outside colony", ]$ind_density_km2 *
        (1- overlap_colony[overlap_colony$zone == zone, ]$prop_overlap_colony) *
        study_area[study_area$zone == zone, ]$area_km2
      
      #calcul individual density for the zone
      ind_density_km2= (nb_fox_in + nb_fox_out) /  study_area[study_area$zone == zone, ]$area_km2
      
      #store in data frame
      fox_density_df$ind_density_km2[i] <- ind_density_km2
    }
    
    #removed columns that are no more necessary
    arfo <- fox_density_df %>% 
      dplyr::select(-prop_overlap_colony) %>% 
      #add a column for species
      dplyr::mutate(species= "arctic fox", year= NA) %>% 
      #reorder columns
      dplyr::select(species, zone, year, ind_density_km2) %>% 
      #arrange row by year and zone
      dplyr::arrange(year, zone) %>% 
      #add empty column  for standard deviation since it is based on predicted model values
      dplyr::mutate(sd_ind_density_km2= NA)
    
    