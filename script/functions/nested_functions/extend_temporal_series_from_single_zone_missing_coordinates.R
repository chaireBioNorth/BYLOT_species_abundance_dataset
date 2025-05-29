# Function to extend temporal series when abundance in a zone is tighly correlated to abundance at the study area (when coordinates of nests are not accessible, but zone is)
#Author: Louis Moisan
#Date: June 11 2024

#------------------#
#### Librairies ####
#------------------#
#data manipulation
library(dplyr)
#spatial data manipulation
library(sf)
sf_use_s2(FALSE)

extend_temporal_series_from_single_zone_nest_missing_coords <- function(
    nest_data,
    zone_sf,
    sp,
    year_list,
    year_compare,
    zone_ref){
  
#--- Extract density from nest found in each zone
sp_density <- nest_data %>% 
  dplyr::filter(species ==sp, year%in% year_list) %>% 
  group_by(species, zone, year) %>% 
  dplyr::summarise(nb_nest= n()) %>% 
  dplyr::left_join(study_area %>% dplyr::select(zone, area_km2) %>% sf::st_drop_geometry()) %>% 
  dplyr::mutate(nest_density_km2= nb_nest/area_km2, ind_density_km2= nest_density_km2*2) %>% 
  #Add all years-zone-species where no nest were found and assign 0
  full_join(expand.grid(year = c(year_list,year_compare), species = sp, zone = unique(zone_sf$zone), by = c("species", "zone","year"))) %>% 
  #Replace the NA with 0
  mutate(nb_nest= ifelse(is.na(nb_nest), 0, nb_nest),
    ind_density_km2= ifelse(is.na(ind_density_km2), 0, ind_density_km2)) %>% 
  dplyr::select(species, zone, year, nb_nest,ind_density_km2) %>% 
  unique()
  
#Species density at the scale of the study area
sp_density_study_area <- sp_density %>% 
  dplyr::filter(zone== "study area", year %in% year_compare)%>% 
  dplyr::arrange(year) 

#Species density in the reference zone
if(length(zone_ref)>1){
sp_density_ref <- sp_density %>% 
  dplyr::filter(zone %in% zone_ref) %>% 
  dplyr::left_join(study_area %>% dplyr::select(zone, area_km2) %>%  sf::st_drop_geometry(), by= "zone") %>% 
  dplyr::group_by(species, year) %>% 
  dplyr::summarise(sum_nb_nest= sum(nb_nest), sum_area_km2= sum(area_km2)) %>% 
  dplyr::mutate(zone= "zone reference", ind_density_km2= sum_nb_nest*2/sum_area_km2) %>% 
  dplyr::select(species, zone, year, ind_density_km2)
}else {
  sp_density_ref <- sp_density %>% 
    dplyr::filter(zone == zone_ref) %>% 
    dplyr::arrange(year)
}
#Extract a data frame of the density in the reference zone with the same year as in the study area data frame
sp_density_ref_filter <- sp_density_ref %>% 
  dplyr::filter(year %in% unique(sp_density_study_area$year)) %>% 
  dplyr::arrange(year)
  
#Extract correlation coefficient and p value (excluding 0,0 values)
#Identify which years do not present 0,0 values
index_non_zero <- which(sp_density_study_area$ind_density_km2 >0 | sp_density_ref_filter$ind_density_km2 >0)
#Extract correlation coefficient and p value (without 0,0 values)
cor <- cor.test(sp_density_study_area$ind_density_km2[index_non_zero], sp_density_ref_filter$ind_density_km2[index_non_zero])


print(plot(sp_density_study_area$ind_density_km2~ sp_density_ref_filter$ind_density_km2,
     xlab= "Density reference zone",
     ylab= "Density study area"))

print(paste("Correlation for", sp, "between", unique(sp_density_ref_filter$zone), "and study area =", round(as.numeric(cor$estimate^2), digits = 2),",","p=", round(as.numeric(cor$p.value), digits=3),",","n=", length(index_non_zero),sep =" "))

#Build linear model regression model
model <- lm(sp_density_study_area$ind_density_km2[index_non_zero]~sp_density_ref_filter$ind_density_km2[index_non_zero])

print(paste("density at study area=", as.numeric(round(model$coefficients[2], digits= 5)), "x density at", unique(sp_density_ref_filter$zone), "+", as.numeric(round(model$coefficients[1], digits= 5)) ,sep=" "))

#Extend temporal series using this correlation
extended_series <- sp_density_ref %>% 
  dplyr::filter(! c(year %in% sp_density_study_area$year)) %>% 
    dplyr::mutate(zone= "study area", ind_density_km2= ind_density_km2*as.numeric(model$coefficients[2]) + as.numeric(model$coefficients[1])) %>% 
    dplyr::mutate(ind_density_km2 = if_else(ind_density_km2 < 0, 0, ind_density_km2)) %>%
    dplyr::arrange(year)  
  
return(extended_series)
}
