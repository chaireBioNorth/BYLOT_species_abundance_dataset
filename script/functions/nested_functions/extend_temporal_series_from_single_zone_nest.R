# Function to extend temporal series when abundance in a zone is tighly correlated to abundance at the study area
#Author: Louis Moisan
#Date: December 5 2023

#------------------#
#### Librairies ####
#------------------#
#data manipulation
library(dplyr)
#spatial data manipulation
library(sf)
sf_use_s2(FALSE)

source("script/functions/standalone_functions/get_nest_density.R")

extend_temporal_series_from_single_zone_nest <- function(
    nest_data, # data frame of nest location for each species
    zone_sf, #Sf object of the zones of the study area to estimate density in
    sp, # species name (in lower case, no dashed, no underscore)
    year_list, #List of year to estimate density
    year_compare, #List of year to based the defined the regression between the density in the reference zone and the density at the study area
    zone_ref #List of zone(s) to used as reference
    ){
  
#--- Extract density from nest found in each zone
sp_density <- get_nest_density(
    nest_data= nest_data,
    zone_sf= study_area,
    species_list = sp,
    zone_list= c(zone_ref, "study area"),
    year_list= year_list)
  
#Species density at the scale of the study area
sp_density_study_area <- sp_density %>% 
  dplyr::filter(zone== "study area", year %in% year_compare)%>% 
  dplyr::arrange(year) 

#Species density in the reference zone
if(length(zone_ref)>1){
sp_density_ref <- sp_density %>% 
  dplyr::filter(zone %in% zone_ref) %>% 
  dplyr::left_join(study_area %>%  sf::st_drop_geometry(), by= "zone") %>% 
  dplyr::group_by(species, year.x) %>% 
  dplyr::summarise(sum_nb_nest= sum(nb_nest), sum_area_km2= sum(area_km2)) %>% 
  dplyr::mutate(species= sp, year= year.x, zone= "zone reference", ind_density_km2= sum_nb_nest*2/sum_area_km2) %>% 
  dplyr::select(species, zone, year, ind_density_km2)
}else {
  sp_density_ref <- sp_density %>% 
    dplyr::filter(zone == zone_ref) %>% 
    dplyr::arrange(year)
}
#Extract a data frame of the density in the reference zone with the same year as in the study area data frame
sp_density_ref_filter <- sp_density_ref %>% 
  dplyr::filter(year %in% sp_density_study_area$year) %>% 
  dplyr::arrange(year)
  
#Extract correlation coefficient and p value (excluding 0,0 values)
#Identify which years do not present 0,0 values
index_non_zero <- which(sp_density_study_area$ind_density_km2>0 | sp_density_ref_filter$ind_density_km2 >0)
#Extract correlation coefficient and p value (without 0,0 values)
cor <- cor.test(sp_density_study_area$ind_density_km2[index_non_zero], sp_density_ref_filter$ind_density_km2[index_non_zero])

print(plot(sp_density_study_area$ind_density_km2~ sp_density_ref_filter$ind_density_km2,
     xlab= "Density reference zone",
     ylab= "Density study area"))

print(paste("Correlation for", sp, "between", unique(sp_density_ref_filter$zone), "and study area =", round(as.numeric(cor$estimate^2), digits = 2),",","p=", round(as.numeric(cor$p.value), digits=3),",","n=", length(index_non_zero), sep =" "))

#Build linear model regression model
model <- lm(sp_density_study_area$ind_density_km2[index_non_zero] ~sp_density_ref_filter$ind_density_km2[index_non_zero])

print(paste("density at study area=", as.numeric(round(model$coefficients[2], digits= 5)), "x density at", unique(sp_density_ref_filter$zone), "+", as.numeric(round(model$coefficients[1], digits= 5)) ,sep=" "))

#Extend temporal series using this correlation
extended_series <- sp_density_ref %>% 
  dplyr::filter(! c(year %in% sp_density_study_area$year)) %>% 
    dplyr::mutate(zone= "study area", ind_density_km2= ind_density_km2*as.numeric(model$coefficients[2]) + as.numeric(model$coefficients[1])) %>% 
    dplyr::mutate(ind_density_km2 = if_else(ind_density_km2 < 0, 0, ind_density_km2)) %>%
    dplyr::arrange(year)  
  
return(extended_series)
}
