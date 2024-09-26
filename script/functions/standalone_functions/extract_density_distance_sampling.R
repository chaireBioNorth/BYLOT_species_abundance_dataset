#Title: Extract abundance estimate of american golden plover based on distance sampling
#Date: October 12 2023
#Author: Louis Moisan

#------------------#
#### Librairies ####
#------------------#
#data manipulation
library(dplyr)
# spatial data manipulation
library(sf)
#Distance sampling (calcul detection function and estimate abundance)
library(Distance)
#data visualization
library(ggplot2)

extract_density_distance_sampling <- function(
    transect_data, #Transect observation data
    distance_data, #.csv with a column distance that contain perpendicular distance between observer and the transect in meters
    sp, #Species name in lower case without dashed or underscore (use space)
    zone_list, #vector of the zone to estimate density in
    zone_sf, #shapefile that contain the polygon of the zones of the study area
    year_list, #vector of the year for which every zones specified above have been covered adequately by transects
    status_list, # vector of the status of individuals on transects that will be taken into account
    key, # Key parameter determine based on lowest AIC model (see extract_detection_function_american_golden_plover.R)
    adjustment, # see extract_detection_function_american_golden_plover.R
    nb_adjust_term,# ssee extract_detection_function_american_golden_plover.R
    transect_width, #150 meters on each side
    null_density_wetlands, #Remove wetlands to the area to extrapolate in each zone
    relative_abundance_upland #Data frame with the comprising the ratio of relative abundance in upland vs lowland zones
    ){

#---------------------#
#### Prepare  data ####
#---------------------#
#Create sf object with only the zones transects
transect_area <- zone_sf %>% 
  dplyr::filter(zone %in% c("goose point","malaview","qarlikturvik valley",  "camp 3", "dufour", "camp 2")) 

#Extract transects sampled in each year and zone
transects_sampled <- transect_data %>%
  dplyr::filter(species ==sp) %>% 
  sf::st_intersection(transect_area) %>% 
  sf::st_drop_geometry() %>% 
    dplyr::select(transect, year, zone) %>% 
  unique()

#Extract sf object associate to each transect
transects_coords <- transect_data %>% 
  dplyr::select(transect, geometry) %>% 
  unique()

#Filter transect data based on the selected status,species and year
transects = transect_data %>% 
  sf::st_drop_geometry() %>% 
  dplyr::filter(species == sp, status %in% status_list, year %in% year_list) %>%
  dplyr::group_by(transect, year, species) %>% 
  #Combine number of individual of the selected status
  dplyr::summarise(nb_ind= sum(nb_ind), .groups = "drop") %>% 
  ungroup() %>% 
  #Filter transect to keep only transect sampled with detection of individual  
  dplyr::filter(nb_ind > 0) %>% 
  dplyr::left_join(transects_coords, by= "transect") %>% 
  sf::st_as_sf() %>% 
#Assign zones of the study area to each observation made on transect
  sf::st_intersection(transect_area %>% dplyr::select(-year))

#Extract list of all zones and years that have been sampled
sampled_year_zone <- data.frame(zone= rep(transect_area$zone, times= length(year_list)), year= rep(year_list, each= length(transect_area$zone)))


#-------------------------------------------------#
####  Estimate abundance based on distance sampling for each year and zone ####
#------------------------------------------------#
#Create an empty data frame to store output of plover abundance
sp_abundance <- data.frame()

#for each year and zone combination (e.g., camp 2 in 2017)
i= 1
for (i in 1:nrow(sampled_year_zone)){
  #Extract the year of interest as an object
  year_select= sampled_year_zone$year[i]
  #Extract the zone of interest as an object
  zone_select= sampled_year_zone$zone[i]

#--- Extract region table containing each transect id, zone and zone area
region_table <- data.frame(Region.Label= 
  transect_area[transect_area$zone == zone_select,]$zone,
  #transform km2 to m2 since we used detection distance in meters
  Area = transect_area[transect_area$zone == zone_select,]$area_km2*1000000)

#--- Extract observation table containing each detection on transect
obs <- transects %>%
  #keep only detection on transect for the specified year and zone
  dplyr::filter(year== year_select, zone== zone_select)

#--- If presence of observation estimate density
if (nrow(obs) >0){
  #Create a object id containing a unique id for each observation
  #repeat the row of transects with multiple individuals observed the number of times corresponding to the number of individuals. Since each row correpond to a given individual detected.
obs_table <- obs[rep(1:nrow(obs), times = obs$nb_ind), ] %>% 
  dplyr::mutate(Sample.Label= paste(transect, year), object= 1:length(year)) %>% 
  dplyr::rename(Region.Label= zone) %>% 
  sf::st_drop_geometry() %>% 
  #keep only columns of interest
  dplyr::select(object, Region.Label, Sample.Label)

#--- Extract sample table containing each transect sampled
sample_table <- transects_sampled %>%
  dplyr::filter(year== year_select, zone== zone_select) %>% 
  dplyr::mutate(Sample.Label= paste(transect, year), Effort= 500) %>% 
  dplyr::rename(Region.Label= zone) %>% 
  dplyr::select(Sample.Label, Region.Label, Effort) %>% 
  unique()
  
#extract sample size (number of transect sampled per zone-year)
sample_size= length(sample_table$Sample.Label)

#--- Estimate abudance using the function ds from the package Distance
dist_sampling <- Distance::ds(
                  data=distance_data,
                  truncation= transect_width,
                  transect = "line",
                  key=key, #see above 
                  nadj=nb_adjust_term , #see above
                  adjustment=adjustment, #see above
                  scale= "width", #default
                  dht_group=TRUE, #consider each detection as 1 individual
                  convert_units =1, #units are already correct m
                  region_table = region_table,
                  sample_table = sample_table,
                  obs_table = obs_table)
#save the abundance estimate into data frame
sp_abundance <- rbind(sp_abundance,
  data.frame(zone= zone_select,
             year= year_select,
             abundance=dist_sampling$dht$individuals$N$Estimate,
             sd_abundance= dist_sampling$dht$individuals$N$se* sqrt(sample_size)))}

#If no observation were made, but the zone was sampled create a row with 0
if (nrow(obs)== 0){
  sp_abundance <- rbind(sp_abundance,
   data.frame(zone= zone_select,
    year= year_select,
    abundance=0,
    sd_abundance=NA))}
}

#-------------------------------------------------------------#
#### Correct density to account for proportion of wetlands ####
#-------------------------------------------------------------#
#--- if null_density wetlands is true remove proportion of wetlands, rivers and lakes 
if (null_density_wetlands == TRUE){
  for (z in c("qarlikturvik valley", "camp 2", "camp 3", "malaview", "goose point", "dufour")){
    #Adjust abundance
    sp_abundance[sp_abundance$zone== z,]$abundance <- sp_abundance[sp_abundance$zone== z,]$abundance*
    study_area[study_area$zone == z,]$prop_mesic
    
    #Adjust standard deviation of abundance
    sp_abundance[sp_abundance$zone== z,]$sd_abundance <- sp_abundance[sp_abundance$zone== z,]$sd_abundance*
      study_area[study_area$zone == z,]$prop_mesic
    }
  }


#----------------------------------#
#### Densities in upland zones ####
#---------------------------------#
#Calculate density in all zones combined except plateau
abundance_lowland <- sp_abundance %>% 
  dplyr::left_join(study_area %>% dplyr::select(-year), by ="zone") %>% 
  sf::st_drop_geometry() %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarise(ind_density_km2= sum(abundance)/sum(area_km2))  

#--- Assign density to higher elevation zones without transects
abundance_upland <-  data.frame(
  species= rep(sp, times= 3* nrow(abundance_lowland)),
  year= rep(abundance_lowland$year, each= 3),
  zone= rep(c("black plateau", "south plateau","camp 3 plateau"), times= nrow(abundance_lowland)),
  ind_density_km2= rep(abundance_lowland$ind_density_km2, each= 3) *
    relative_abundance_upland[relative_abundance_upland$species == sp,]$ratio_difference) %>% 
  dplyr::left_join(study_area %>% dplyr::select(-year) %>% sf::st_drop_geometry()) %>% 
  dplyr::mutate(abundance= round(ind_density_km2*area_km2, digits = 0), sd_abundance=NA) %>% 
  dplyr::select(zone, year, abundance, sd_abundance)

#Add to the general data frame
sp_abundance <- sp_abundance %>% 
  rbind(abundance_upland)


#-----------------------------#
#### Density on study area ####
#-----------------------------#
#--- calculate abundance for the study area
study_area_abund <-  sp_abundance %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarise(abundance= sum(abundance), sd_abundance= sum(sd_abundance), .groups = "drop") %>% 
  dplyr::mutate(zone= "study area") %>% 
  dplyr::select(zone, year, abundance, sd_abundance)
#Add to general data frame
sp_abundance <- rbind(sp_abundance, study_area_abund)

#--- Transform abundance into density
sp_density <- sp_abundance %>% 
  #Add area of each zone
  dplyr::left_join(zone_sf %>%sf::st_drop_geometry() %>% dplyr::select(zone, area_km2) , by= "zone") %>% 
  #Calcul density of individuals
  dplyr::mutate(ind_density_km2= abundance/ area_km2, sd_ind_density_km2 = sd_abundance/ area_km2, species =sp) %>%
  #replace NA with 0 if abundance was of 0
  dplyr::mutate(ind_density_km2= ifelse(is.na(ind_density_km2), 0, ind_density_km2)) %>% 
  #keep only columns of interest
  dplyr::select(species, zone, year, ind_density_km2, sd_ind_density_km2) %>% 
  dplyr::arrange(zone) 

return(sp_density)
}

