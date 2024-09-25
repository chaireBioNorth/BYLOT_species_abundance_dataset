#Title: Extract the abundance of every vertebrate species from Bylot Island
#Date: October 11 2023
#Last update:  June 17 2024



#-----------------#
#### Libraries ####
#-----------------#
#data manipulation
library(dplyr)
#spatial data manipulation
library(sf)
sf_use_s2(FALSE) #prevent geoprocessing errors
#Personnal functions to estimate species abundance
source("script/functions/FUNS.R")


#-------------------#
#### Import data ####
#-------------------#
#study area
study_area <- sf::st_read("data/raw/shapefiles/study_area.shp")

#years each species was monitored
sampling_year <- read.csv("data/metadata/species_systematic_nest_monitoring_clean.csv")

#nests
nest_data <-  sf::st_read("data/raw/shapefiles/sampling/nests_2004-2023.shp")
nest_data_csv <- read.csv("data/raw/sampling/nests_2004-2023.csv")

#transects
transect_data <- sf::st_read("data/raw/shapefiles/sampling/vertebrate_count_transects_2010_2023.shp")

#incidental observation
incidental_obs <- read.csv("data/raw/sampling/incidental_obs_2007_2019.csv") %>%
  dplyr::select(-X)

#Index of relative abundance in upland zones
relative_abundance_upland <- read.csv("data/raw/other/incidental_obs_relative_abundance_upland.csv")

#Index of relative abundance from Gauthier et al., 2024
relative_abundance_gauthier <- read.csv("data/metadata/relative_abundance_Gauthier_et_al_2023.csv")

#define zones to estimate abundance
zones <- c("camp 2", "goose point","malaview","qarlikturvik valley",  "camp 3", "black plateau", "south plateau", "camp 3 plateau", "dufour", "study area")



#--------------------------------------------------#
#### 3.1 Pacific loon, red-throated loon, cackling goose, tundra swan, glaucous gull ####
#-------------------------------------------------#
#--- Extract nest density for each year and each zones
wetlands_sp <- get_nest_density(
  nest_data= nest_data,
  zone_sf= study_area,
  species_list = c("cackling goose", "tundra swan","red throated loon", "pacific loon", "glaucous gull"),
  zone_list= "study area",
  year_list= c(2017:2023))

#--- Filter to keep only year systematically sampled at the study area
wetlands_sp <- wetlands_sp %>% 
  dplyr::semi_join(
    #Extract combination of year and zone systematically sampled for each species
    sampling_year %>%
    dplyr::filter(monitoring == "systematic" & sampled == "yes" & zone == "study area") %>%
    dplyr::select(species, year)
    )

#--- Estimate cackling goose abundance between 1996 and 2016 based on a model of exponential increase of the abundance, which presents a better fit than the linear model.

#Extract cackling goose nest 1996, 2017-2019, 2022-2023
cack_nests <- wetlands_sp %>% 
  dplyr::filter(species== "cackling goose", zone== "study area") %>% 
  dplyr::select(year, nb_nest) %>% 
  #Add first nest found in 1996
  rbind(data.frame(year=1996, nb_nest=1))

#Define the model
cack_model <- lm(log(nb_nest)~year, data= cack_nests)
cack_model_summary <-   summary(cack_model)

#Predict cackling goose abundance between 1996 and 2016 from the model
cack_extend <- data.frame(
      species = "cackling goose",
      zone="study area",
      year= c(1996:2016,2020,2021),
      nb_nest= exp(predict(cack_model, list(year=c(1996:2016, 2020,2021))))
    ) %>% 
  #Transform number of nest into density of individuals
  dplyr::mutate(ind_density_km2=nb_nest*2/study_area[study_area$zone=="study area",]$area_km2, status= "breeding", data= "nest sampling (extrapolation nests)") %>% 
  dplyr::select(species, zone, year, ind_density_km2, status, data)

#--- extract figure nest cackling goose and fit with model
all_cack_nests <- get_nest_density(
  nest_data= nest_data,
  zone_sf= study_area,
  species_list = "cackling goose",
  zone_list= "study area",
  year_list= c(1990:2019, 2022,2023))

pdf(file= "manuscript/figures/cackling_goose_nest_exponential.pdf")
plot(all_cack_nests$year, all_cack_nests$nb_nest, 
     xlab= "Years",
     ylab= "Number of nests",
     main= "cackling goose",
     cex= 1.5, pch= 16, col= "steelblue4", cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)+
  lines(c(1996: 2023), exp(predict(cack_model, list(year=c(1996:2023)))), col = "red", lwd= 3)
dev.off()


#--- Extrapolate density of glaucous gull based on density between 2004 - 2016 measured in the qarlikturvik valley
glgu_extend <- extend_temporal_series_from_single_zone_nest(
      nest_data= nest_data,
      zone_sf=study_area,
      sp= "glaucous gull",
      #Extract complete list of year for which monitoring was done in qarlikturvik valley
      year_list= sampling_year[sampling_year$species == "glaucous gull" & sampling_year$zone == "qarlikturvik valley" & sampling_year$sampled=="yes",]$year,
      #Extract list of year for which sampling was done at the scale of study area and in the qarlikturvik valley
      year_compare= sampling_year[sampling_year$species == "glaucous gull" & sampling_year$zone == "study area" & sampling_year$sampled=="yes",]$year,
  zone_ref= "qarlikturvik valley") %>% 
  dplyr::mutate(status= "breeding", data= "nest sampling (extrapolation nests)") %>% 
  #Remove undesired columns
  dplyr::select(species, zone, year, ind_density_km2, status, data) 
  



#----------------------#
#### 3.2 snow goose ####
#----------------------#
#--- Estimate snow goose breeding abundance between 2010 and 2023
#Read the snow goose file
source("script/extract_species_abundance/species/snow_goose.R")

#Standardise format
sngo <- sngo %>% 
  dplyr::mutate(species= "snow goose", zone= "study area", status= "breeding",data= "nest sampling") %>% 
  dplyr::select(species, zone, year, abundance, status, data)
sngo[sngo$year %in% c(2010:2019, 2022, 2023),]$data <- "combined methods"
sngo[sngo$year %in% c(1999:2009),]$data <- "nest sampling (extrapolation colony)"


#-------------------------------------------#
#### 3.3 King eider and long-tailed duck ####
#-------------------------------------------#
#Estimate density from the 4x2 km plot in qarlikturvik valley
kiei_ltdu_nest <- get_nest_density(
  nest_data= nest_data,
  zone_sf= study_area,
  species_list = c("long tailed duck", "king eider"),
  zone_list= "4x2km plot",
  year_list= sampling_year[sampling_year$species == "long tailed duck" & sampling_year$zone == "4x2km plot" & sampling_year$sampled=="yes",]$year)

#Calculate mean density in the 4x2 plot (8km2 plot)
kiei_ltdu_density <- kiei_ltdu_nest %>% 
  dplyr::group_by(species, zone) %>% 
  dplyr::summarise(ind_density_km2= mean(ind_density_km2),
                   prop_wet_plot= study_area[study_area$zone=="4x2km plot",]$prop_wet)

#--- Extrapolate the mean nest density to the wetlands of the study area
kiei_ltdu <- kiei_ltdu_density %>% 
  #Add proportion of wetland study area
  cbind(prop_wet_study_area=study_area[study_area$zone=="study area",]$prop_wet) %>%
  #Calculate density at the study area by adjusting density by proportion wetlands qarlikturvik valley
  dplyr::mutate(zone= "study area", year= NA,
    ind_density_km2= ind_density_km2*prop_wet_study_area/prop_wet_plot,
    status= "breeding",
    data= "nest sampling (extrapolation habitat)") %>% 
  dplyr::select(species, zone, year, ind_density_km2, status, data)

#---Alternative approach based on Gauhtier et al., 2023 indexes
kiei_ltdu_alt <-relative_abundance_gauthier %>% 
  dplyr::filter(species %in% c("long tailed duck", "king eider")) %>% 
  #Add density of red throated loons as reference
  dplyr::left_join(
    wetlands_sp %>% 
      dplyr::filter(zone=="study area", species== "red throated loon") %>% 
      dplyr::group_by(species, zone) %>% 
      dplyr::summarise(ref_ind_density_km2= mean(ind_density_km2))
    , by= c("ref_species"="species")) %>% 
  #Cross-multiplication to determine absolute abundance from relative abundance 
  dplyr::mutate(year= NA,
    ind_density_km2= ref_ind_density_km2*(index_sp/index_ref_sp),
    status= "undetermined",
    data= "incidental observations (relative abundance)") %>% 
  dplyr::select(species, zone, year, ind_density_km2, status, data)



#-------------------------------------------------------------#
#### 3.4 Snowy owl, rough-legged hawk and peregrine falcon ####
#-------------------------------------------------------------#
#--- Extract nest density of snowy owls for each year and each zones
snow <- get_nest_density(
  nest_data= nest_data,
  zone_sf= study_area,
  species_list = "snowy owl",
  zone_list= "study area",
  #keep only year systematically sampled at the study area
  year_list= sampling_year[sampling_year$species== "snowy owl" & sampling_year$zone== "study area" & sampling_year$sampled=="yes",]$year) %>%
  dplyr::mutate(status= "breeding", data= "nest sampling") %>% 
  dplyr::select(species, zone, year, ind_density_km2, status, data)

#--- Estimate snowy owl nest density between 1993 and 2011, when monitoring was only performed in the valley of Qarlikturvik valley and nearby plateaus
snow_extend <- extend_temporal_series_from_single_zone_nest(
  nest_data= nest_data,
  zone_sf=study_area,
  sp= "snowy owl",
  #Extract complete list of year for which monitoring was done in qarlikturvik valley
  year_list= sampling_year[sampling_year$species == "snowy owl" & sampling_year$zone == "camp 1" & sampling_year$sampled=="yes",]$year,
  #Extract list of year for which sampling was done at the scale of study area and in the qarlikturvik valley
  year_compare= sampling_year[sampling_year$species == "snowy owl" & sampling_year$zone == "study area" & sampling_year$sampled=="yes",]$year,
  zone_ref= c("camp 1", "black plateau", "south plateau")) %>% 
  dplyr::mutate(status= "breeding", data= "nest sampling (extrapolation nests)") %>% 
  dplyr::select(species, zone, year, ind_density_km2, status, data)

#---Calcul the nest density of peregrine falcons and rough legged hawks. We could not used the same function as below for those species because the coordinates are not provided since they are sensitive.
pefa_rlha <- get_nest_density_missing_coordinates(
  nest_data_csv= nest_data_csv, 
  zone_sf= study_area, 
  species_list= c("rough legged hawk", "peregrine falcon"), 
  zone= "study area", 
  #list of year for which species were sampled systematically at the scale of the study area
  year_list= sampling_year[sampling_year$species == "peregrine falcon" & sampling_year$zone == "study area" & sampling_year$sampled=="yes",]$year) %>% 
  dplyr::filter(year %in% sampling_year[sampling_year$species == "peregrine falcon" & sampling_year$zone == "study area" & sampling_year$sampled=="yes",]$year) %>% 
  dplyr::mutate(status= "breeding", data= "nest sampling") %>% 
  dplyr::select(species, zone, year, ind_density_km2, status, data)

#--- Estimate the density of rough-legged hawks between 2004 and 2012 based on correlation with nest density in the Qarlikturvik valley and nearby plateaus and density at the scale of the study area
rlha_extend <- extend_temporal_series_from_single_zone_nest_missing_coords(
  nest_data=read.csv("data/raw/sampling/hawk_falcon_nest_zone.csv") %>%
    dplyr::select(-X),
  zone_sf= study_area,
  sp= "rough legged hawk",
  #Extract complete list of year for which monitoring was done in qarlikturvik valley
  year_list= sampling_year[sampling_year$species == "rough legged hawk" & sampling_year$zone == "qarlikturvik valley" & sampling_year$sampled=="yes",]$year,
  #Extract list of year for which sampling was done at the scale of study area and in the qarlikturvik valley
  year_compare= sampling_year[sampling_year$species == "rough legged hawk" & sampling_year$zone == "study area" & sampling_year$sampled=="yes",]$year,
  zone_ref= c("qarlikturvik valley", "black plateau", "south plateau")) %>% 
  dplyr::mutate(status= "breeding", data= "nest sampling (extrapolation nests)") %>% 
  dplyr::select(species, zone, year, ind_density_km2, status, data)



#--------------------------#
#### 3.5 Rock ptarmigan ####
#--------------------------#
#Nest density measured in the qarlikturvik valley
ropt_density <- get_nest_density(
  nest_data= nest_data,
  zone_sf= study_area,
  species_list = "rock ptarmigan",
  zone_list= "4x2km plot",
  year_list= sampling_year[sampling_year$species == "rock ptarmigan" & sampling_year$zone == "4x2km plot" & sampling_year$sampled=="yes",]$year)

#Calculate mean density in the qarlikturvik valley
ropt <- ropt_density %>% 
  dplyr::group_by(zone) %>% 
  dplyr::summarise(species="rock ptarmigan", year= NA, ind_density_km2= mean(ind_density_km2)) %>% 
  dplyr::mutate(zone="study area", status= "breeding", data= "nest sampling (extrapolation habitat)") %>% 
  dplyr::select(species, zone, year, ind_density_km2, status, data)



#--------------------------#
#### 3.6 Sandhill crane ####
#--------------------------#
#Define location to save figure regression nest density and transect
pdf("manuscript/figures/sandhill_crane_nest_transects.pdf")

#Estimate a mean annual abundance of sandhill cranes based on the mean number of individuals observed per transect in each zone.
sacr<- spatial_extrapolation_species_density(
  nest_data= nest_data,
  transect_data= transect_data,
  transect_metric= "mean number of individual per transect",
  zone_sf= study_area,
  zone_reference_nest= "4x2km plot",
  zone_reference_transect= "4x2km plot",
  #list of years that nest density was measured in the 4x2 km plot
  years_reference= sampling_year[sampling_year$species == "sandhill crane" & sampling_year$zone == "4x2km plot" & sampling_year$sampled=="yes",]$year,
  zone_list= zones,
  sp="sandhill crane",
  status_list= c("repro", "inconnu", "non repro"),
 relative_abundance_upland= relative_abundance_upland,
  null_density_wetlands= FALSE) %>% 
  dplyr::filter(zone =="study area") %>% 
  dplyr::mutate(year= NA, status= "breeding", data= "nest sampling (extrapolation transects)") %>% 
  dplyr::select(species, zone, year, ind_density_km2, status, data)

#Save figure
dev.off()  



#-----------------------------------------------------#
#### 3.7 American-golden  and black-bellied plover ####
#-----------------------------------------------------#
#--- Extract density of breeding individual abundance with distance sampling
amgp <- extract_density_distance_sampling(
    transect_data= transect_data %>% 
      #Remove transects with more than 4 individuals pointing towards group of non-breeding individuals
      dplyr::filter(nb_ind<=4), 
    distance_data= read.csv("data/raw/sampling/american_golden_plover_distance.csv"), 
    sp= "american golden plover", 
    zone_list= zones, 
    zone_sf= study_area, 
    year_list= c(2014:2019, 2022:2023), 
    status_list= c("repro", "non repro", "inconnu"), 
    key= "hn", 
    adjustment= "cos",
    nb_adjust_term=1,
    transect_width=150,
    null_density_wetlands=TRUE,
    relative_abundance_upland= relative_abundance_upland) %>% 
  dplyr::mutate(status= "breeding", data= "distance sampling") %>% 
  dplyr::select(species, zone, year, ind_density_km2, status,data)

#--- Estimate density of breeding black-bellied plover based on the relative proportion of observations of each species on transects
bbpl <- estimate_density_from_reference_species(
  reference_species_density= amgp,
  transect_data= transect_data%>% 
    #Remove transects with more than 4 individuals pointing towards group of non-breeding individuals
    dplyr::filter(nb_ind<=4),
  status_list= c("repro", "non repro", "inconnu"),
  status_list_ref_sp= c("repro", "non repro", "inconnu"),
  species_list= c("american golden plover", "black bellied plover"),
  zone_list= zones,
  zone_sf= study_area,
  relative_abundance_upland= relative_abundance_upland,
  sp_null_density_wetlands=NULL)%>% 
  dplyr::filter(species== "black bellied plover") %>% 
  dplyr::mutate(status= "breeding", data= "transects (relative abundance)", year= NA) %>% 
  dplyr::select(species, zone, year, ind_density_km2, status, data)

#---Alternative approach based on Gauhtier et al., 2023 indexes
bbpl_alt <-relative_abundance_gauthier %>% 
  dplyr::filter(species == "black bellied plover") %>% 
  #Add mean density of american golden plover as reference
  dplyr::left_join(
    amgp %>% 
      dplyr::filter(zone=="study area") %>% 
      dplyr::group_by(species, zone) %>% 
      dplyr::summarise(ref_ind_density_km2= mean(ind_density_km2))
    , by= c("ref_species"="species")) %>% 
  #Use cross-multiplication to estimate black bellied plover absolute abundance from relative abundance
  dplyr::mutate(ind_density_km2= ref_ind_density_km2*(index_sp/index_ref_sp), status= "breeding", data= "incidental observations (relative abundance)", year= NA) %>% 
  dplyr::select(species, zone, year, ind_density_km2, status, data)



#--------------------------------#
#### 3.8 Common-ringed plover ####
#--------------------------------#
crpl <- get_nest_density(
  nest_data= nest_data,
  zone_sf= study_area,
  species_list = "common ringed plover",
  zone_list= "study area",
  year_list=  sampling_year[sampling_year$species== "common ringed plover" & sampling_year$zone== "study area" & sampling_year$sampled=="yes",]$year)%>% 
  dplyr::mutate(status= "breeding", data= "nest sampling") %>% 
  dplyr::select(species, zone, year, ind_density_km2, status, data)



#----------------------------#
#### 3.9 Lapland longpsur ####
#----------------------------#
#Define location to save figure regression nest density and transect
pdf("manuscript/figures/lapland_longspur_nest_transect.pdf")

#Correlation of lapland longpsur local nest density and mean number of individuals observed per transect
lalo<- spatial_extrapolation_species_density(
  nest_data= nest_data,
  transect_data= transect_data,
  transect_metric= "mean number of individual per transect",
  zone_sf= study_area,
  zone_reference_nest= "2x1km plot",
  zone_reference_transect= "4x2km plot",
  #List of year to use to extract mean values
  years_reference= sampling_year[sampling_year$species == "lapland longspur" & sampling_year$zone == "2x1km plot" & sampling_year$sampled=="yes",]$year,
  zone_list= zones,
  sp="lapland longspur",
  status_list= c("repro", "inconnu", "non repro"),
  relative_abundance_upland= relative_abundance_upland,
  null_density_wetlands= FALSE)%>% 
  dplyr::mutate(status= "breeding", data= "nest sampling (extrapolation transects)", year= NA) %>% 
  dplyr::select(species, zone, year, ind_density_km2, status, data)

#Save figure
dev.off()



#-----------------------------#
#### 3.9 Baird's sandpiper ####
#-----------------------------#
#Define location to save figure regression nest density and transect
pdf("manuscript/figures/bairds_sandpiper_nest_transect.pdf")

#Correlation of Baird's sandpiper local nest density and proportion of transects with at least one individual observed
basa<- spatial_extrapolation_species_density(
  nest_data= nest_data,
  transect_data= transect_data,
  transect_metric= "proportion of transect with individual",
  zone_sf= study_area,
  zone_reference_nest= "2x1km plot",
  zone_reference_transect= "4x2km plot",
  #list of years to use to extract mean values
  years_reference= sampling_year[sampling_year$species == "bairds sandpiper" & sampling_year$zone == "2x1km plot" & sampling_year$sampled=="yes",]$year,
  zone_list= zones,
  sp="bairds sandpiper",
  status_list= c("repro", "inconnu", "non repro"),
  relative_abundance_upland= relative_abundance_upland,
  null_density_wetlands= FALSE)%>% 
  dplyr::mutate(status= "breeding", data= "nest sampling (extrapolation transects)", year= NA) %>% 
  dplyr::select(species, zone, year, ind_density_km2, status, data)

#Save the figure
dev.off()



#------------------------------------------------------#
#### 3.10 Horned lark, American pipit, snow bunting ####
#------------------------------------------------------#
#--- Estimate abundance of each other passerines species based on the relative abundance of each species in the transect observation and the abundance of lapland longspurs
passerines <- estimate_density_from_reference_species(
    reference_species_density= lalo,
    transect_data= transect_data,
    status_list= c("repro", "inconnu", "non repro"),
    status_list_ref_sp= c("repro", "inconnu", "non repro"),
    species_list= c("horned lark", "american pipit", "snow bunting"),
    zone_list= zones,
    zone_sf= study_area,
    relative_abundance_upland= relative_abundance_upland,
    sp_null_density_wetlands=NULL)%>% 
  dplyr::mutate(status= "breeding", data= "transects (relative abundance)", year= NA) %>% 
  dplyr::filter(species != "lapland longspur") %>% 
  dplyr::select(species, zone, year, ind_density_km2, status, data)

#---Alternative approach based on Gauhtier et al., 2023 indexes of relative abundance
passerines_alt <-relative_abundance_gauthier %>% 
  dplyr::filter(species %in% c("lapland longspur","horned lark", "american pipit", "snow bunting")) %>% 
  #Add mean density of lapland longspur as reference
  dplyr::left_join(
    lalo %>% 
      dplyr::filter(zone=="study area") %>% 
      dplyr::group_by(species, zone) %>% 
      dplyr::summarise(ref_ind_density_km2= mean(ind_density_km2))
    , by= c("ref_species"="species")) %>% 
  dplyr::mutate(ind_density_km2= ref_ind_density_km2*(index_sp/index_ref_sp),
                status= "breeding", data= "incidental observations (relative abundance)", year= NA) %>% 
  dplyr::select(species, zone, year, ind_density_km2, status, data)



#-------------------------------------#
#### 3.10 white-rumped sandpiper, pectoral sandpiper, buff-breasted sandpiper, red knot, ruddy turnstone, red phalarope ####
#------------------------------------#
#Estimate abundance of each sandpipers species based on the relative proportion of transects with at least one individual observed and the estimated abundance of Baird's sandpiper abundance.
sandpipers <- estimate_density_from_reference_species(
  reference_species_density= basa,
  transect_data= transect_data,
  status_list= c("repro", "inconnu"),
  status_list_ref_sp= c("repro", "inconnu"),
  species_list= c("white rumped sandpiper", "pectoral sandpiper", "buff breasted sandpiper", "red knot", "red phalarope", "ruddy turnstone"),
  zone_list= zones,
  zone_sf= study_area,
  relative_abundance_upland= relative_abundance_upland,
  sp_null_density_wetlands=NULL)%>% 
  dplyr::mutate(status= "breeding", data= "transects (relative abundance)", year= NA) %>% 
  dplyr::filter(species != "bairds sandpiper") %>% 
  dplyr::select(species, zone, year, ind_density_km2, status, data) 

#---Alternative approach based on Gauhtier et al., 2023 indexes of relative abundance
sandpipers_alt <-relative_abundance_gauthier %>% 
  dplyr::filter(species %in% c("red knot", "ruddy turnstone", "white rumped sandpiper", "pectoral sandpiper", "red phalarope", "buff breasted sandpiper")) %>% 
  #Add abundance of Baird's sanpiper as reference
  dplyr::left_join(
    basa %>% 
      dplyr::filter(zone=="study area") %>% 
      dplyr::group_by(species, zone) %>% 
      dplyr::summarise(ref_ind_density_km2= mean(ind_density_km2))
    , by= c("ref_species"="species"))  %>% 
  dplyr::mutate(ind_density_km2= ref_ind_density_km2*(index_sp/index_ref_sp),
                status= "breeding", data= "incidental observations (relative abundance)", year= NA) %>% 
  dplyr::select(species, zone, year, ind_density_km2, status, data)



#--------------------------------#
#### 3.11 Long-tailed jaeger  ####
#--------------------------------#
#long-tailed jaeger density in qarlikturvik valley since 2005
ltja_density <- get_nest_density(
  nest_data= nest_data,
  zone_sf= study_area,
  species_list = "long tailed jaeger",
  zone_list= "camp 1",
  #list of year for which nest density was measured
  year_list= sampling_year[sampling_year$species == "long tailed jaeger" & sampling_year$zone == "camp 1" & sampling_year$sampled=="yes",]$year) 

#Correct density estimate to account for proportion of mesic habitat
ltja_density <- ltja_density %>% 
  #add area and proportion of wet ana mesic habitat in each zone
  dplyr::left_join(study_area %>% sf::st_drop_geometry(), by= "zone") %>% 
  #adjust density to consider only mesic habitat
  dplyr::mutate(area_km2_mesic= area_km2*prop_mesic, ind_density_km2= (nb_nest*2)/area_km2_mesic) %>% 
  dplyr::rename(year= year.x) %>% 
  dplyr::select(species, year, ind_density_km2)

#Calculate area of mesic habitat in the study area
mesic_study_area <- study_area %>% 
  dplyr::filter(zone %in% c("camp 2", "goose point","malaview","qarlikturvik valley",  "camp 3", "dufour")) %>% 
  dplyr::mutate(area_km2_mesic= area_km2*prop_mesic) 
  #Total sum of mesic area across study area
mesic_area_study_area <- sum(mesic_study_area$area_km2_mesic)

#Extrapolate density at the scale of the study area
ltja <- ltja_density %>% 
  dplyr::mutate(zone= "study area",
                ind_density_km2= (ind_density_km2*mesic_area_study_area)/study_area[study_area$zone=="study area",]$area_km2, status= "breeding", data= "nest sampling (extrapolation habitat)") %>% 
  dplyr::select(species, zone, year, ind_density_km2, status, data)



#----------------------------#
#### 3.12 Parasitic jaeger ####
#----------------------------#
#---Estimate abundance based on the maximal number of individuals banded in a single year (17 individuals)
paja <- data.frame(
  species= "parasitic jaeger",
  zone= "study area",
  ind_density_km2= round(17/ study_area[study_area$zone == "study area", ]$area_km2, digits = 3)) %>% 
  dplyr::mutate(status= "undetermined", data= "banding", year= NA) %>% 
  dplyr::select(species, zone, year, ind_density_km2, status, data)

#--- use total number of nest found as an alternative approach
paja_alt <- get_nest_density(
  nest_data= nest_data,
  zone_sf= study_area,
  species_list = "parasitic jaeger",
  zone_list= "study area",
  #years for which nest of parasitic jaegers where sampled
  year_list= sampling_year[sampling_year$species=="parasitic jaeger" & sampling_year$sampled== "yes" & sampling_year$zone== "study area",]$year) %>% 
  dplyr::select(-nb_nest, -nest_density_km2) %>% 
  dplyr::group_by(species, zone) %>% 
  dplyr::summarise(ind_density_km2= mean(ind_density_km2)) %>% 
  dplyr::mutate(status= "breeding", data= "nest sampling", year= NA) %>% 
  dplyr::select(species, zone, year, ind_density_km2, status, data)



#-------------------------#
#### 3.13 Common raven ####
#-------------------------#
#glaucous gull nest density as reference
glgu <-get_nest_density(
  nest_data= nest_data,
  zone_sf= study_area,
  species_list =  "glaucous gull",
  zone_list= zones,
  year_list= c(2017:2023))
  
#--- Estimate common raven from glaucous gull abundance using mean number of individuals observed per transect as index of relative abundance
cora <- estimate_density_from_reference_species(
  reference_species_density= glgu,
  transect_data= transect_data,
  status_list= c("repro", "inconnu", "non repro"),
  status_list_ref_sp= c("repro", "inconnu", "non repro"),
  species_list= "common raven",
  zone_list= zones,
  zone_sf= study_area,
  relative_abundance_upland= relative_abundance_upland,
  sp_null_density_wetlands=NULL) %>% 
  dplyr::filter(species== "common raven", zone== "study area") %>% 
  dplyr::mutate(status= "undetermined", data= "transects (relative abundance)", year= NA) %>% 
  dplyr::select(species, zone, year, ind_density_km2, status, data)
  
#---Alternative approach based on Gauhtier et al., 2023 indexes of relative abundance
cora_alt <-relative_abundance_gauthier %>% 
  dplyr::filter(species== "common raven") %>% 
  #Add abundance of glaucous gull as reference
  dplyr::left_join(
  wetlands_sp %>%
  dplyr::filter(species== "glaucous gull") %>% 
  dplyr::group_by(species, zone) %>% 
  dplyr::summarise(ref_ind_density_km2= mean(ind_density_km2))
  , by= c("ref_species"="species")) %>% 
  dplyr::mutate(ind_density_km2= ref_ind_density_km2*(index_sp/index_ref_sp)) %>% 
  dplyr::mutate(status= "undetermined", data= "incidental observations (relative abundance)", year= NA) %>% 
  dplyr::select(species, zone, year, ind_density_km2, status, data)



#----------------------------------------#
#### 3.14 brown and collared lemmings ####
#----------------------------------------#
source("script/extract_species_abundance/species/lemmings.R")
#return "lemming" object with estimated density at the scale of the study area
lemming <- lemming %>% 
  dplyr::filter(zone =="study area") %>% 
  dplyr::mutate(status= "undetermined", data= "trapping") %>% 
  dplyr::select(-sd_ind_density_km2) 


#------------------------#
#### 3.15 Arctic hare ####
#------------------------#
#Comparaison of relative abundance with arctic fox based on incidental obs
source("script/extract_species_abundance/species/arctic_fox.R") 
fox_density <- arfo %>% 
  dplyr::filter(zone== "study area")
# Gauthier et al., 2023 -> 0.9 hare/100h observation
# Gauthier et al., 2023 -> 9.0 fox/100h observation

#Estimate general density in lowland (where most incidental obs are done)
arha_lowland <- data.frame(
  zone= c("camp 2", "goose point","malaview","qarlikturvik valley",  "camp 3", "dufour"),
  ind_density_km2= mean(fox_density$ind_density_km2)*(0.9/9.0))

#Adjust density in plateau based index of relative abundance in incidental obs
arha <- arha_lowland %>% 
 rbind(data.frame(zone= c("black plateau", "south plateau", "camp 3 plateau"),
    ind_density_km2= unique(arha_lowland$ind_density_km2) *  relative_abundance_upland[relative_abundance_upland$species== "arctic hare",]$ratio_difference) %>% 
  dplyr::select(zone, ind_density_km2))

#Recalculate density at the study area by combining density in low and upland
arha<- arha %>% 
  dplyr::left_join(study_area %>%
                     dplyr::select(zone, area_km2) %>%
                     sf::st_drop_geometry()) %>%
  dplyr::mutate(species= "arctic hare") %>% 
  dplyr::group_by(species) %>% 
  dplyr::summarise(ind_density_km2= sum(ind_density_km2* area_km2) /study_area[study_area$zone=="study area",]$area_km2) %>% 
  dplyr::mutate(zone= "study area", year= NA, status= "undetermined", data= "incidental observations (relative abundance)") %>% 
  dplyr::select(species, zone, year, ind_density_km2, status, data)



#-------------------#
#### 3.16 ermine ####
#-------------------#
#--- Import dataframe with indices of ermines' abundances from 
# https://www.sciencedirect.com/science/article/pii/S1439179122000895
df_ermine = read.csv('data/raw/sampling/ermine_index.csv', header= TRUE)

# Minimum and maximum ermine abundances
#https://esajournals.onlinelibrary.wiley.com/doi/abs/10.1890/11-1973.1?casa_token=5nWeE-CMA9gAAAAA%3AcCaOHXXyyZHXxmtxgyzr01ieSct6FZwrzkwQ4SDjkFD-mo1U6uj0oa7_pcyJ-EHBYXy1fPyTXdxOo8NEPg
# in ind.·km-2
ermin = 0.02
ermax = 0.4

#Associate the minimum and maximum scores of relative abundance with the minimum and maximum density of individuals, respectively
df_ermine$Ermine_estimated_abundance = ermin + (ermax-ermin)*df_ermine$Ermine / max(df_ermine$Ermine)

# number of lines
n = length(df_ermine$Year)

# create df to store the abundances
df_data_final <- data.frame(
  species = rep('ermine', n),
  zone = rep('qarlikturvik valley', n),
  year = df_ermine$Year,
  ind_density_km2 = df_ermine$Ermine_estimated_abundance,
  sd_ind_density_km2 = rep(NA, n)
)

#Assume the same density at the scale of the study area
ermine <- df_data_final %>% 
  rbind(df_data_final %>%  dplyr::mutate(zone= "study area"))

#return "ermine" object with estimated density at the scale of the study area
ermine <- ermine %>% 
  dplyr::filter(zone == "study area") %>% 
  dplyr::mutate(status= "undetermined", data= "testimonials") %>% 
  dplyr::select(-sd_ind_density_km2)



#-----------------------#
#### 3.17 Arctic fox ####
#-----------------------#
source("script/extract_species_abundance/species/arctic_fox.R") 
#return "arfo" object with estimated density at the scale of the study area
arfo <- arfo %>% 
  dplyr::filter(zone== "study area") %>% 
  dplyr::mutate(status= "undetermined", data= "individual home range") %>% 
  dplyr::select(-sd_ind_density_km2)

#Minimum adult fox individual on around 520 km2 (Royer-Boutin et al., 2015)
# 17 ind -> 0.0327
# 67 ind -> 0.129



#----------------------------------#
#### Combined all data together ####
#----------------------------------#
wetlands_sp <- wetlands_sp %>% 
  dplyr::mutate(status= "breeding", data= "nest sampling") %>% 
  dplyr::select(species, zone, year, ind_density_km2, status, data)

other_sp <- lalo %>% 
  rbind(basa,
        amgp, 
        bbpl,
        passerines,
        passerines_alt,
        sandpipers,
        sandpipers_alt) %>% 
  dplyr::filter(zone == "study area")


#--- Standardise data format
abundances <- wetlands_sp %>% 
  rbind(cack_extend,
        glgu_extend,
        kiei_ltdu,
        kiei_ltdu_alt,
        snow,
        snow_extend,
        rlha_extend,
        pefa_rlha,
        ropt,
        sacr,
        other_sp,
        bbpl_alt,
        crpl,
        ltja,
        paja,
        paja_alt,
        cora,
        cora_alt,
        lemming,
        arha,
        ermine,
        arfo) %>% 
  #Format data frame
  dplyr::mutate(abundance= round(ind_density_km2* study_area[study_area$zone== "study area",]$area_km2, digits = 0)) %>% 
  dplyr::select(-ind_density_km2) %>%
  rbind(sngo) %>% 
  dplyr::arrange(species, year)

#Export as .csv
write.csv(abundances,"data/clean/species_abundance.csv", row.names = FALSE)


#--------------------------------------------#
#### Export clean version for the dataset ####
#--------------------------------------------#
abundances_dataset <- abundances %>% 
  dplyr::select(-zone) %>% 
  dplyr::rename(method= data) %>% 
  dplyr::arrange(year) %>% 
  dplyr::mutate(species= dplyr::recode(species,"bairds sandpiper"="Baird's sandpiper", "long tailed duck"= "long-tailed duck", "long tailed jaeger"= "long-tailed jaeger", "red throated loon"= "red-throated loon", "pacific loon"= "Pacific loon","rough legged hawk"= "rough-legged hawk", "arctic hare"= "Arctic hare", "black bellied plover"= "black-bellied plover", "buff breasted sandpiper"= "buff-breasted sandpiper", "american pipit"= "American pipit", "arctic fox"="Arctic fox", "common ringed plover"= "common-ringed plover", "american golden plover"= "American golden-plover", "white rumped sandpiper"= "white-rumped sandpiper", "lapland longspur"= "Lapland longspur"))

write.csv(abundances_dataset,"dataset/BYLOT-species_abundance.csv",row.names = FALSE)
