# Spatially extrapolate species density monitored on a local plot to other zones of the study area using a cross multiplication based on the mean number of individual per transect


#------------------#
#### Librairies ####
#------------------#
#general data manipulation
library(dplyr)
#spatial data manipulation
library(sf)
sf_use_s2(FALSE)
#adjust colors
library(scales)

#Dependencies on function created in other scripts
source("script/functions/standalone_functions/number_ind_transects.R")
source("script/functions/standalone_functions/get_nest_density.R")


spatial_extrapolation_species_density <- function(
    nest_data,#Nest data (id, coordinates, species and year)
    transect_data,#Transect observation data
    transect_metric, #"mean number of individual per transect" OR
    #"proportion of transect with individual"
    zone_sf,#shapefile that contain the polygon of the zones of the study area
    zone_reference_nest, #List of zones to use as reference for nest density
    zone_reference_transect, #List of zones to use as reference for transects relative abundance
    years_reference, #List of year to used to define the relation between transect and nest density
    sampling_year, # data frame with the species, year and zone that have been sampled
    zone_list,#list of the zones to estimate individual density
    sp, #species of interest
    status_list,#status of individuals that will be taken into account
    relative_abundance_upland, # Ratio of relative abundance between plateau and non-plateau areas
    null_density_wetlands# TRUE/FALSE, should the area of wetlands be remove from the area used to perform spatial extrapolation
    ) {
 
#--------------------------------------------------------------#
#### Extract local individual density based on nest density ####
#--------------------------------------------------------------#
#If the reference zone is the 2x1 km plot, adjust the area by removing area of lakes (0.26km2) in the zone
if (zone_reference_nest == "2x1km plot"){
 zone_sf[zone_sf$zone== "2x1km plot",]$area_km2 <- 1.74
}
  
sp_density_local <- get_nest_density(
  nest_data= nest_data,
  zone_sf= zone_sf,
  species_list = sp,
  zone_list= zone_reference_nest,
  year_list= years_reference) %>% 
    dplyr::arrange(year)
  
#-------------------------------#
#### Extract  transect  data ####
#-------------------------------#
#--- Assign a zone to each transect and sum obs by transect (combined status)
transect_obs <- transect_data %>%
  #Keep only observation for selected species and status
  dplyr::filter(species %in% sp, status %in% status_list) %>%
  #Assign zone to each transect
  sf::st_intersection(zone_sf %>%
                        dplyr::filter(zone != "goose_colony") %>% 
                        dplyr::select(zone, area_km2)) %>% 
  #remove spatial features (lighter to run)
  st_drop_geometry() %>% 
  #keep only zones of interest
  dplyr::filter(zone %in% zone_sf$zone) %>% 
  #Sum all observation of the selected status by transect
  dplyr::group_by(species, zone, year, transect) %>% 
    dplyr::summarise(nb_ind= sum(nb_ind), .groups = "drop")


#-----------------------------------------------#
##### Mean number of individual per transect ####
#-----------------------------------------------#
if (transect_metric == "mean number of individual per transect"){
#Extract by year for the reference zone
transect_obs_ref <- transect_obs %>% 
  dplyr::filter(zone == zone_reference_transect) %>% 
  #Extract mean number of individual per transect per year
  dplyr::group_by(species, zone, year) %>% 
  dplyr::summarise(value= mean(nb_ind), .groups = "drop") %>% 
  dplyr::arrange(year)

#Extract the mean for all zone
transect_obs <- transect_obs %>% 
  #Extract mean number of individual per transect per year
  dplyr::group_by(species, zone) %>% 
  dplyr::summarise(value= mean(nb_ind), .groups = "drop")
}  

  
#-----------------------------------------------------------#
##### Proportion of transect with at least an individual ####
#-----------------------------------------------------------#
if (transect_metric == "proportion of transect with individual"){
#Extract by year for the reference zone
transect_obs_ref <- transect_obs %>% 
  dplyr::filter(zone == zone_reference_transect) %>% 
  #Create a column if individual present or absent
  dplyr::mutate(presence= ifelse(nb_ind == 0, 0,1)) %>% 
  #Extract proportion of transect with obs
  dplyr::group_by(species, zone, year) %>% 
  dplyr::summarise(value= sum(presence)/n(), .groups = "drop")%>% 
  dplyr::arrange(year)

#Extract mean for all zone
transect_obs <- transect_obs %>%
  #Create a column if individual present or absent
  dplyr::mutate(presence= ifelse(nb_ind == 0, 0,1)) %>% 
  #Extract mean number of individual per transect per year
  dplyr::group_by(species, zone) %>% 
  dplyr::summarise(value= sum(presence)/n(), .groups = "drop")
}

  
#---------------------------------------------------------------#
#### Regression nest density - number individual on transect #### 
#-------------------------------------------------------------
#Extract points with nest density and transects
reg_df <- transect_obs_ref %>% 
  dplyr::mutate(year= as.integer(year)) %>% 
  dplyr::left_join(sp_density_local, by= "year") %>% 
  dplyr::select(year, value, nest_density_km2)

#Build the linear model
model <- lm(nest_density_km2~value -1 , data= reg_df)
summary(model)
print(paste("nest density (nest/km2)", "=", as.character(round(model$coefficients, digits=4)),"x", transect_metric))


#----------------------------#
#### Visualize regression ####
#----------------------------#
#Predict values based on the regression model
predictions <- predict(model, newdata =data.frame(value= reg_df$value))

#plot
plot(reg_df$nest_density_km2~ reg_df$value, col= "steelblue", pch= 16, cex=2.5,
     ylim = c(0,  max(reg_df$nest_density_km2)),
     xlim=c(0,max(reg_df$value)),
     xlab= transect_metric,
     ylab= "nest density (nest/km²)",
    main= paste0(toupper(substring(sp, 1, 1)), substring(sp, 2)),
     cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2)+ 
  abline(model, col= "red", lwd= 6)


#---------------------------------------#
#### Estimate density in each zone ###
#---------------------------------------#
#Estimate density of in each zone with regression
sp_density <- transect_obs %>% 
    dplyr::mutate(ind_density_km2=
      (2*(value * as.numeric(model$coefficients)))) %>%
  #Replace NaN and Inf values with 0 when no observation on transects were made
  dplyr::mutate(ind_density_km2= ifelse(is.na(ind_density_km2) | is.infinite(ind_density_km2), 0, ind_density_km2)) %>% 
   #keep only columns of interest
    dplyr::select(species, zone, ind_density_km2) %>% 
  #keep only selected zones
    dplyr::filter(zone %in% zone_list)

#---------------------------------------------------#
#### Adjust density based on wetlands proportion ####
#---------------------------------------------------#
#--- if species name appear in sp_null_density_wetlands then reduce density in mesic zone with a factor
if(null_density_wetlands==TRUE){
    for (z in unique(sp_density$zone)){
      #Adjust abundance
      sp_density[sp_density$zone== z,]$ind_density_km2 <- sp_density[sp_density$zone== z,]$ind_density_km2*
        study_area[study_area$zone == z,]$prop_mesic
  }
}

#---------------------------------#
#### Densities in upland zones ####
#---------------------------------#
#Calculate density in all zones combined except plateaus
density_lowland <- sp_density %>% 
  dplyr::left_join(study_area) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::mutate(abundance= prop_mesic*area_km2*ind_density_km2) %>% 
  dplyr::filter(zone!="study area")

density_lowland <- sum(density_lowland$abundance)/sum(density_lowland$area_km2)

#--- Assign density to higher elevation zones without transects
density_upland <-  data.frame(
    species= rep(sp, times= 3),
    zone= c("black plateau", "south plateau","camp 3 plateau"),
    ind_density_km2= density_lowland *
      relative_abundance_upland[relative_abundance_upland$species == sp,]$ratio_difference)

#Add to the general data frame
sp_density <- sp_density %>% 
  rbind(density_upland)


#-----------------------------#
#### Density on study area ####
#-----------------------------#  
study_area_density <- sp_density %>% 
  #remove study area density
  dplyr::filter(zone != "study area") %>% 
  # Add area of each zone
  dplyr::left_join(zone_sf %>%
                     sf::st_drop_geometry() %>%
                     dplyr::select(zone, area_km2)) %>% 
  # Estimate abundance per zone
  dplyr::mutate(nb_ind= ind_density_km2* area_km2) %>% 
  #Extract total abundance per species for all zones
  dplyr::group_by(species) %>% 
  dplyr::summarise(nb_ind= sum(nb_ind), .groups = "drop") %>% 
  #Transform abundance into density
  dplyr::mutate(zone= "study area", ind_density_km2= nb_ind/ zone_sf[zone_sf$zone == "study area", ]$area_km2) %>% 
  #keep only columns of interest
  dplyr::select(species, zone, ind_density_km2)

#Replace original density on study area
sp_density <- sp_density %>% 
  #remove original study area density
  dplyr::filter(zone != "study area") %>% 
  #add new rows
  rbind(study_area_density) %>% 
  #arrange row by species and zone
  dplyr::arrange(species, zone)
  
return(sp_density)
}
