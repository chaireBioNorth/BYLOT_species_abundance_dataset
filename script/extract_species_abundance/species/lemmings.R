#title: Create dataframe with the time-series for collared and brown lemmings (1995-2022)
#author: Azenor Bideault
#date: november 8 2023

#Libraries
library(dplyr)
library(sf)


#-------------------#
#### IMPORT DATA ####
#-------------------#
#-- For densities from 1995 to 2003
df_lemming = read.csv('data/raw/sampling/lemming_density_1993-2019.csv', header = TRUE)
#-- For densities from 2004 to 2022
df_lemming2 = read.csv('data/raw/sampling/lemming_density_2004-2022.csv', header = TRUE)


study_area_df <- sf::st_read("data/raw/shapefiles/study_area.shp") %>% 
  sf::st_drop_geometry() %>% 
  dplyr::filter(zone=="study area")


#-----------------------------------------#
#### DENSITIES FOR THE YEARS 2004-2022 ####
#-----------------------------------------#
#--- Time-series is computed for the valley of camp 1 only
# The period P2 (july) and P3 (august) are used
# Filter the data
filtered_data <- df_lemming2 %>% 
  filter(site == 'Goose camp', period %in% c("P2", "P3"))

#--- Compute the densities according to the good period to consider for each year
# 2004 to 2007 -> period 3 is used
# 2008 to 2018 and 2020 to 2022 -> mean between periods P2 and P3 is used
# 2019 -> period P2 only is used
# see document Document_interne_trappage_mortel_version (tables 5 and 6 for details)

# Filter the data for the specified periods and years
dens_lemming <- df_lemming2 %>% 
  filter(
    (year >= 2004 & year <= 2007 & period == "P3") |
      (year >= 2008 & year <= 2018 & period %in% c("P2", "P3")) |
      (year == 2019 & period == "P2") |
      (year >= 2020 & year <= 2022 & period %in% c("P2", "P3"))
  ) %>% 
  group_by(species, habitat, grid, year) %>% 
  summarize(mean_density = mean(density, na.rm = TRUE))

#-- Compute densities for the whole study area according to the percentages of wetland and mesic habitats :
dens_lemming_area <- dens_lemming %>% 
  filter((grid == 'LG2' | grid == 'LG1'))  %>% 
  group_by(species, year) %>% 
  summarize(
    density = sum(mean_density * ifelse(habitat == "mesic", study_area_df$prop_mesic, study_area_df$prop_wet))
  )

#-- Densities are in ind/ha, we convert in ind/km2
dens_lemming_area$density_km2 = dens_lemming_area$density*100

#-- Modify the dataset to have the common structure
dens_lemming_area_modified <- dens_lemming_area %>% 
  mutate(
    species = ifelse(species == "brown", "brown lemming", "collared lemming"),
    zone = "qarlikturvik valley",
    ind_density_km2 = density_km2,
    sd_ind_density_km2 = NA
  ) %>% 
  select(species, zone, year, ind_density_km2, sd_ind_density_km2)


#-----------------------------------------#
#### DENSITIES FOR THE YEARS 1993-2003 ####
#-----------------------------------------#
#-- Filter the df to keep only the years 1995 to 2003
#-- And compute the densities for the whole area according to the percentage of wet and mesic habitats
dens_lemming2 <- df_lemming %>%
  filter(Year >= 1995 & Year <= 2003) %>%
  mutate(
    Brown_density = (Brown.Mesic.habitat * study_area_df$prop_mesic + Brown.Wet.habitat * study_area_df$prop_wet)*100,
    Coll_density = (Coll.Mesic.habitat * study_area_df$prop_mesic + Coll.Wet.habitat * study_area_df$prop_wet)*100
  ) %>%
  select(Year, Brown_density, Coll_density)


# Create dataframes with the common structure
dens_bl = data.frame(year = dens_lemming2$Year, zone = 'qarlikturvik valley', species = 'brown lemming', ind_density_km2 = dens_lemming2$Brown_density, sd_ind_density_km2 = NA)
dens_cl = data.frame(year = dens_lemming2$Year, zone = 'qarlikturvik valley', species = 'collared lemming', ind_density_km2 = dens_lemming2$Coll_density, sd_ind_density_km2 = NA)

#---------------------------------------------------------------#
#### CONCATENATE TO GET DENSITIES FOR THE YEARS 1993-2022 IN ONE DF ####
#--------------------------------------------------------------#
#--- Concatenate the 3 df
final_dens = rbind(dens_lemming_area_modified, dens_bl, dens_cl)
#-- Order the df
final_dens = final_dens[order(final_dens$species, final_dens$year),]


#Assume the same density at the scale of the study area
lemming <- final_dens %>% 
  rbind(final_dens %>%  dplyr::mutate(zone= "study area"))

#Remove row with NA as estimated density
lemming <- lemming[complete.cases(lemming[, "ind_density_km2"]), ]
