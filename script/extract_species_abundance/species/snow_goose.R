#Title: Estimate snow goose abundance at the scale of the study area
#Author: Louis Moisan
#Date: NA
#Last update: 31 May 2024

#------------------#
#### Librairies ####
#------------------#
#General data manipulation
library(dplyr) 
library(tidyr)
#Spatial data manipulation
library(sf)
sf::sf_use_s2(FALSE)
#Visualization
library(ggplot2)
library(ggpubr)


#-------------------#
#### Import data ####
#-------------------#
#Study area
study_area <- sf::st_read("data/raw/shapefiles/study_area.shp") 

#Goose colony outline 2010-2023
goose_colony_2010_2023 <- study_area %>% 
  sf::st_drop_geometry() %>% 
  dplyr::filter(zone == "goose_colony") %>% 
  dplyr::select(-prop_up)
#Goose colony 1999-2009 (2020-2021)
goose_colony_1999_2009 <- goose_colony_2010_2023 %>% 
  dplyr::summarise(area_km2= mean(area_km2), prop_wet= mean(prop_wet), prop_mesic= mean(prop_mesic),zone= "goose_colony") 
# Replicate the row for each year and add a "year" column
goose_colony_1999_2009 <- goose_colony_1999_2009[rep(1, length(c(1999:2009, 2020, 2021))), ] %>% 
dplyr::mutate(year = c(1999:2009, 2020, 2021))
#combine to extract a single data frame of the goose colony
goose_colony <- goose_colony_2010_2023 %>% 
  rbind(goose_colony_1999_2009)

#Systematically monitored wetland plot (peksek)
peksek <- sf::st_read("data/raw/shapefiles/sampling/snow_goose_systematic_wetland_plot.shp") %>%
  sf::st_transform(26717) %>% 
  dplyr::mutate(area_km2= round(as.numeric(sf::st_area(geometry)/ 1000000), digits = 4)) %>% 
  dplyr::select(area_km2, geometry)
#Goose nests within the peksek systematic plot
peksek_nest <- sf::st_read("data/raw/shapefiles/sampling/nests_2004-2023.shp") %>%
  dplyr::filter(species == "snow goose") %>% 
  sf::st_intersection(peksek)

#Random plots
#Number of nest found in random plot
random_plot <- read.csv("data/raw/sampling/snow_goose_nests_random_plot.csv") %>%
  dplyr::rename(year= Year, habitat= Habitat) %>% 
  dplyr::group_by(year, habitat) %>% 
  dplyr::summarise(nb_nest= sum(nb_nest), area_km2= sum(area_km2), .groups = "drop")

#Transects
#Transects 2010 to 2013 where status of geese was not defined
transects_2010_2013_sngo <- sf::st_read("data/raw/shapefiles/sampling/vertebrate_count_transects_2010_2023.shp") %>%
  dplyr::filter(year<=2013) %>% 
  dplyr::group_by(transect, year, habitat) %>%
  #Extract sum of individual observed on transect
  dplyr::summarise(nb_ind= sum(nb_ind), .groups = "drop") %>%
  dplyr::mutate(area_km2= (300*500)/1000000, year= as.integer(year))
#Transects 2014 to 2023 where status was defined
transects_2014_2023_sngo <-  sf::st_read("data/raw/shapefiles/sampling/vertebrate_count_transects_2010_2023.shp") %>%
  dplyr::filter(year>=2014, status== "repro") %>% 
  dplyr::group_by(transect, year, habitat) %>%
  #Extract sum of individual observed on transect
  dplyr::summarise(nb_ind= sum(nb_ind), .groups = "drop") %>%
  dplyr::mutate(area_km2= (300*500)/1000000, year= as.integer(year))
#Combined together
transects_sngo <- transects_2010_2013_sngo %>% 
  rbind(transects_2014_2023_sngo)
#Identify transects that are located in wetland habitat
transects_sngo_wet <- transects_sngo %>% 
  dplyr::filter(habitat== "wetland")
  
#Point counts
sngo_point <- sf::st_read("data/raw/shapefiles/sampling/snow_goose_point_counts_2010_2023.shp")
#Identify point counts that are located in wet habitat
points_sngo_wet <- sngo_point %>% 
  dplyr::filter(habitat== "wetland")


#------------------------------------------#
##### Extract goose density in wetlands ####
#------------------------------------------#
#Extract number of nest per year found in the systematically monitored wetland plot (peksek)
peksek_nest_grouped <- peksek_nest %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarise(nb_nest= n(), area_km2= peksek$area_km2, .groups = "drop") %>% 
  sf::st_drop_geometry() %>% 
  #keep only year for which we also have random plots (to combine with random plot)
  dplyr::filter(year %in%random_plot$year)

#Extract density found in wetland random plots
random_plot_nest_wet <- random_plot %>% 
  dplyr::filter(habitat== "Wetland", year %in% peksek_nest$year) %>% 
  dplyr::select(year, nb_nest, area_km2) 

#Combined nest found in peksek and random plots to obtain general density
peksek_plot_wet_density <- peksek_nest_grouped %>% 
  rbind(random_plot_nest_wet) %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarise(data= "random plot and peksek", habitat= "wet",ind_density_km2= (sum(nb_nest)*2)/ sum(area_km2), .groups = "drop")

#Extract density using only random plots in wet habitats
plot_wet_density <- random_plot_nest_wet %>% 
  dplyr::mutate(ind_density_km2= (nb_nest*2)/area_km2, data= "random plot", habitat= "wet") %>% 
  dplyr::select(year, data, habitat, ind_density_km2)

#Combined random plot + peksek and only peksek
wet_density <- peksek_plot_wet_density %>% 
  rbind(plot_wet_density)%>% 
  dplyr::mutate(data= dplyr::recode(data, "random plot"="Nest sampling plot" ,  "random plot and peksek"="Nest sampling plot and peksek"))

#visualize
ggplot(data= wet_density, aes(x= year, y= ind_density_km2, col= data))+
  ggtitle("Wet habitat")+
  labs(x="Years", y= "Density of breeding individuals (ind./km2)")+
  scale_color_manual(values= c("black","darkorange"))+
  geom_point()+
  geom_line(linewidth=1)+
  scale_x_continuous(breaks = seq(1999, 2023, by = 2))+
  theme_classic()


#---------------------------------------#
##### Extract goose density in mesic ####
#---------------------------------------#
#--- Transects
mesic_density_transects <- 
  #Remove transects in wet habitat
  transects_sngo[which((transects_sngo$transect %in% transects_sngo_wet$transect)==FALSE),] %>% 
  #Assign within the goose range
  sf::st_intersection(study_area %>% dplyr::filter(zone=="goose_colony")) %>%
  dplyr::filter(year ==year.1, nb_ind>0) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(year, transect, nb_ind, area_km2) %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarise(data= "transect", habitat= "mesic",ind_density_km2= mean(nb_ind/area_km2), .groups="drop")

#--- Point counts
mesic_density_points<- 
  #Remove transects in wet habitat
  sngo_point[which((sngo_point$point %in% points_sngo_wet$point)==FALSE),] %>% 
  #Assign within the goose range
  sf::st_intersection(study_area %>% dplyr::filter(zone=="goose_colony")) %>%
  dplyr::filter(year ==year.1, n_oie_125>0) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(year, point, n_oie_125, area_km2) %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarise(data= "point count",habitat="mesic",ind_density_km2= mean((n_oie_125*2)/((pi * 125^2)/1000000)), .groups="drop")

#--- Random plots
mesic_density_plot <- random_plot %>% 
  dplyr::filter(habitat== "Mesic") %>% 
  dplyr::select(year,nb_nest, area_km2) %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarise(data= "random plot", habitat= "mesic",ind_density_km2= sum(nb_nest*2)/sum(area_km2), .groups = "drop")

#--- Combined
mesic_density <- mesic_density_transects %>% 
  rbind(mesic_density_points) %>% 
  rbind(mesic_density_plot) %>% 
  dplyr::mutate(data= dplyr::recode(data, "random plot"="Nest sampling plot" ,  "point count"="Snow goose point count",  "transect"="Vertebrate count transect"))

#visualize
ggplot(data= mesic_density, aes(x= year, y= ind_density_km2, col= data))+
  ggtitle("Mesic habitat")+
  labs(x="Years", y= "Density of breeding individuals (ind./km2)")+
  scale_color_manual(values= c("black", "steelblue", "red"), name= "Methods")+
  geom_point(size=0.75)+
  geom_line(linewidth=0.75)+
  scale_x_continuous(breaks = seq(1999, 2023, by = 3))+
  theme_classic()
ggsave(units= "cm",width = 18,height = 10,dpi= 500, "manuscript/figures/snow_goose_mesic_density.pdf")


#-------------------------------#
#### Extract goose abundance ####
#-------------------------------#
#Extract abundance in wet habitat
goose_abundance_wet <- goose_colony %>%
  dplyr::mutate(area_km2_wet= area_km2*prop_wet) %>% 
  dplyr::left_join(wet_density) %>% 
  dplyr::mutate(abundance= round(area_km2_wet*ind_density_km2, digits=0)) %>%
  dplyr::select(year, data, habitat, abundance)%>% 
  #Remove 2020-2021 since we do not have estimate of density
  na.omit()

#Extract abundance in mesic habitat
goose_abundance_mesic <- goose_colony %>%
  dplyr::mutate(area_km2_mesic= area_km2*prop_mesic) %>% 
  dplyr::left_join(mesic_density) %>% 
  dplyr::mutate(abundance= round(area_km2_mesic*ind_density_km2, digits=0)) %>%
  dplyr::select(year, data, habitat, abundance)%>% 
  #Remove 2020-2021 since we do not have estimate of density
  na.omit()

#Combined together
goose_abundance <- goose_abundance_wet %>% 
  rbind(goose_abundance_mesic)

#Extract an estimate of abundance for each possible combination
goose_abundance_all <- data.frame()

for (y in unique(goose_abundance$year)){
  #Select all data for a given year
  df_year <- goose_abundance %>% 
    dplyr::filter(year==y)
  
for (wet in unique(df_year[df_year$habitat== "wet",]$data)) {
  for (mesic in unique(df_year[df_year$habitat== "mesic",]$data)) {
    df <- data.frame(
      year= y,
      wet_data= wet,
      mesic_data= mesic,
      abundance= sum(df_year[df_year$habitat== "wet" & df_year$data == wet,]$abundance,
                     df_year[df_year$habitat== "mesic" & df_year$data == mesic,]$abundance)
    )
    #Add output to data frame
    goose_abundance_all <- rbind(goose_abundance_all, df)
    }
  }
}


#-------------------------------------------------#
#### Extract final serie of abundance in mesic ####
#-------------------------------------------------#
#For all estimate use density in wet based on random plot and systematically sampled wetland

#Between 2010 and 2023 extract mean of the abundance of each mesic density estimate
goose_abundance_2010_2023 <- goose_abundance_all %>% 
  dplyr::filter(wet_data== "Nest sampling plot and peksek", year>=2010) %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarise(abundance= round(mean(abundance), digits=0))

#Between 1999 and 2009 use density of random plot in mesic
goose_abundance_1999_2009 <- goose_abundance_all %>% 
  dplyr::filter(wet_data== "Nest sampling plot and peksek", year<2010) %>% 
  dplyr::select(year, abundance)

#Combined together
sngo <- goose_abundance_2010_2023 %>% 
  rbind(goose_abundance_1999_2009)

#Canadian Wildlife service survey on Bylot
# 1993: 55 000 breeding adults and 0.89 nesting success = 61 798 ind
# 1998: 37 575 breeding adults and 0.79 nesting success = 47 563 ind
# 2003: 36 885 breeding adults and 0.82 nesting success = 44 981 ind
# 2008: 29 822 breeding adults and 0.74 nesting success = 40 300 ind



#-----------------------------------------#
#### Maps of annual goose distribution ####
#-----------------------------------------#
study_area_boundaries <- study_area %>% sf::st_union()

map <-  
  ggplot()+
  geom_sf(data=study_area_boundaries, aes(geometry=geometry), fill= "#FFFFFF", col= "gray30")+
  geom_sf(data= study_area %>%
            dplyr::filter(zone=="goose_colony") %>% 
            dplyr::filter(zone== "goose_colony", year== 2017), aes(geometry= geometry), alpha=1, col= "black", fill= alpha("#a3b18a", alpha= 0.2))+
  geom_sf(data= sngo_point %>% 
            dplyr::filter(year== 2017, n_oie_125 > 0), aes(geometry= geometry), alpha=1, fill= "#283618", col= "#283618")+
  geom_sf(data= sngo_point %>% 
            dplyr::filter(year== 2017, n_oie_125 == 0), aes(geometry= geometry), alpha=1, fill= "#d00000",col= "#d00000")+
  theme_void()

ggsave(plot= map, units = "cm", height= 18, width=10,"manuscript/figures/map_geese_colony_2017.pdf")
