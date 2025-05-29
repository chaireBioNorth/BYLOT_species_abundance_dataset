#Title: Extract the abundance of Cackling goose on the Bylot Island study area 
#Date: October 11 2023
#Last update:  October 10 2024

#---------------------------------------------------------------------------------#
#### Extract nest density between 2017 and 2023 when monitoring was systematic ####
#---------------------------------------------------------------------------------#
cackling_goose <- get_nest_density(
  nest_data= nest_data,
  zone_sf= study_area,
  species_list = c("cackling goose"),
  zone_list= "study area",
  year_list= unique(sampling_year[sampling_year$monitoring =="systematic" & sampling_year$species == "cackling goose" & sampling_year$sampled == "yes" & sampling_year$zone == "study area",]$year))

#------------------------------------------------------------#
#### Extend series cackling goose  (1996-2016, 2020,2021) ####
#------------------------------------------------------------#
#Add first nest found in 1996 
cackling_goose_nest_model <- cackling_goose %>% 
  dplyr::select(year, nb_nest) %>% 
  rbind(data.frame(year=1996, nb_nest=1))

#Define the model
cackling_model <- lm(log(nb_nest)~year, data= cackling_goose_nest_model)
cackling_model_summary <-   summary(cackling_model)

#!!! The following code allow to extend the time series of cackling goose by estimating abundance from 1996 to 2019 and 2020 and 2021 using the exponential equation presented in Figure 5.

#Predict cackling goose abundance between 1996 and 2016 from the model
cackling_goose_extend <- data.frame(
  species = "cackling goose",
  zone="study area",
  year= c(1996:2016,2020,2021),
  nb_nest= exp(predict(cackling_model, list(year=c(1996:2016, 2020,2021))))
) %>% 
  #Transform number of nest into density of individuals
  dplyr::mutate(ind_density_km2=nb_nest*2/study_area[study_area$zone=="study area",]$area_km2, breeding_status= "breeding", method= "nest sampling (extrapolation nests)", spatial_extrapolation= "yes") %>% 
  dplyr::select(species, zone, year, ind_density_km2, breeding_status, method, spatial_extrapolation)

#-------------------------------#
#### Combined both data sets ####
#-------------------------------#
cackling_goose <- cackling_goose %>% 
  #Add breeding status and method
  dplyr::mutate(breeding_status= "breeding", method= "nest sampling", spatial_extrapolation= "no") %>% 
  dplyr::select(species, zone, year, ind_density_km2, breeding_status, method, spatial_extrapolation)

# !!! Use the below command to consider the extended time series of cackling goose
#cackling_goose <- cackling_goose %>% 
#  rbind(cackling_goose_extend)

#----------------------------------------------------------------#
#### Extract figure nest cackling goose and model predictions ####
#----------------------------------------------------------------#
cackling_goose_all_nests <- get_nest_density(
  nest_data= nest_data,
  zone_sf= study_area,
  species_list = "cackling goose",
  zone_list= "study area",
  year_list= c(1990:2019, 2022,2023))

pdf(file= "MetadataS1/figures/cackling_goose_nest_exponential.pdf")
plot(cackling_goose_all_nests$year, cackling_goose_all_nests$nb_nest, 
     xlab= "Years",
     ylab= "Number of nests",
     main= "Cackling goose",
     cex= 1.5, pch= 16, col= "steelblue4", cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)+
  lines(c(1996: 2023), exp(predict(cackling_model, list(year=c(1996:2023)))), col = "red", lwd= 3)
dev.off()