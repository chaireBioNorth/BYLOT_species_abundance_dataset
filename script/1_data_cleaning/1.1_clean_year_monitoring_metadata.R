#Title: Transform the table of year sampled for nest monitoring of each species into a usable data frame in R
#Author: Louis Moisan
#Date: 29 september 2023
#Last update: June 11 2024


#------------------#
#### Librairies ####
#------------------#
#general data manipulation
library(dplyr)
library(tidyr)
#spatial data manipulation
library(sf)

#--- Extract the list of the zones of the study area
zone_list <- sf::st_read("data/raw/shapefiles/study_area.shp") %>% 
  dplyr::filter(zone != "goose_colony") %>% # remove the goose colony area
  sf::st_drop_geometry() %>% #remove associate spatial coordinates
  dplyr::select(zone) %>%  # keep only column zone
  unlist() %>% # extract as a vector
  as.vector()

#--- Import raw table of species year monitoring
sp_monitoring <- read.csv("data/metadata/systematic_nest_monitoring_raw.csv")

#--- Extract a list sampling year for each species
year_sampling <- stringr::str_split(sp_monitoring$year, ",")
#Divide into multiple parts if species have been sampled non-continuously
year_sampling <- lapply(year_sampling, function(x) stringr::str_split(x, "-"))
#Assign species name to each list
names(year_sampling) <- sp_monitoring$species
#transform the year format from character to numeric
for (sp in names(year_sampling)){
  print(sp)
  year_sampling[[sp]] <-   lapply(year_sampling[[sp]], function(x) x %>% unlist() %>% as.numeric())
}

#create an empty data frame to store the output
year_sampling_df <- data.frame()

#For each list with more than one year extend the interval of years by integer
for (list in 1:length(year_sampling)){
  for (sublist in 1:length(year_sampling[[list]])){
    if(length(year_sampling[[list]][[sublist]]) > 1){
      year_sampling[[list]][[sublist]] <- year_sampling[[list]][[sublist]][[1]]: year_sampling[[list]][[sublist]][[2]]}
    
    #if list contain only a single value just transform to numeric
    if(length(year_sampling[[list]][[sublist]]) == 1){
      year_sampling[[list]][[sublist]] <- as.numeric(year_sampling[[list]][[sublist]])}
    
    #transform into a data frame
    years <- unlist(year_sampling[[list]][[sublist]])
    year_sampling_df <- year_sampling_df %>% 
      rbind(data.frame(species= rep(sp_monitoring$species[list], times= length(years)),zone= rep(sp_monitoring$zone[list], times= length(years)), year= years))
  }
}

#--- if sampled throughout the study area, consider that zones have been sampled for given year
for (i in 1:nrow(year_sampling_df)){
  if(year_sampling_df$zone[i]== "study area"){
divide_study_area <-  data.frame(species= year_sampling_df$species[i], zone= zone_list, year= year_sampling_df$year[i])
 #add rows to the data frame
year_sampling_df <- rbind(year_sampling_df, divide_study_area)                }
}

#--- Add a columns that specified if the combination of species-year-zone have been studied
year_sampling_df$sampled <- "yes"

#Add combination of species-year-zone that have not been studied
combination <- data.frame(species= rep(unique(year_sampling_df$species),
                          each=length(unique(year_sampling_df$zone))* length(unique(year_sampling_df$year))),
                          zone=rep(unique(year_sampling_df$zone), each= length(unique(year_sampling_df$year)), times= length(unique(year_sampling_df$species))),
                          year= rep(unique(year_sampling_df$year), times= length(unique(year_sampling_df$zone))* length(unique(year_sampling_df$species)))) %>% 
  dplyr::arrange(species, zone, year)
                  
#Add years where combination of species-year-zone have not been sampled
year_sampling_df <- year_sampling_df %>% 
  dplyr::full_join(combination) %>% 
  unique()%>% 
  dplyr::arrange(species, zone, year) %>% 
  dplyr::mutate(sampled= replace_na(sampled, "no"))

#Remove snow goose since data on nest density in random plots is more representative
year_sampling_df <- year_sampling_df %>% 
  dplyr::filter(species != "snow goose")

#Add a column if the monitoring was systematic or not
year_sampling_df <- year_sampling_df %>% 
  dplyr::left_join(sp_monitoring %>%  dplyr::select(species, monitoring), relationship ="many-to-many") %>% 
  unique()

#--- Export as .csv
write.csv(year_sampling_df, "data/metadata/systematic_nest_monitoring_clean.csv", row.names = FALSE)

