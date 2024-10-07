# Extract temporal series of abundance for each species
#Date: January 16 2024

#------------------#
#### Librairies ####
#------------------#
library(dplyr)
library(scales)
library(ggplot2)

#-------------------------#
#### Data manipulation ####
#-------------------------#
#Read species final densities
sp_abundance <- read.csv("DataS1/BYLOT-species_abundance.csv") %>% 
  dplyr::filter(is.na(year)== FALSE)

#defined order of graphs
sp_order <- c("Cackling goose","Snow goose","Rough-legged hawk","Peregrine falcon", "Snowy owl","American golden-plover",
  "Glaucous gull","Long-tailed jaeger","Nearctic brown lemming", "Nearctic collared lemming", "American ermine")

letters <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K")

#reorder species in data frame
sp_abundance$species_en <- factor(sp_abundance$species_en, levels = sp_order)

# Order the data frame based on the 'Name' column
sp_abundance <- sp_abundance[order(sp_abundance$species_en), ]

#Read study area
study_area <- sf::st_read("data/raw/shapefiles/study_area.shp") %>% 
  sf::st_drop_geometry() %>% 
  dplyr::filter(zone != "goose_colony")


#-------------------------------------#
#### Extract plot for each species ####
#-------------------------------------#
for (i in 1:length(unique(sp_abundance$species_en))){
sp=unique(sp_abundance$species_en)[i]

#Filter data frame for a single species
df <- sp_abundance %>% 
    dplyr::filter(species_en ==sp)%>% 
  mutate(year_diff = c(0, diff(year))) %>% 
  mutate(group = cumsum(year_diff != 1),
         year= as.integer(year))
#Plot
plot <-
  ggplot(df, aes(x= year, y= abundance, group=group))+
  ggtitle(paste0(letters[i], ". ", sp))+
  geom_point(col= "black", size= 2)+
  geom_line(col= "black")+
  labs(x= "Years", y= "Number of individuals")+
  scale_x_continuous(n.breaks = 6)+
  scale_y_continuous(n.breaks = 6, labels = scales::number_format())+
  theme_classic()+
  guides(color= "none", shape= "none")+
  theme(axis.text = element_text(size = 9, color = "black"),
        axis.title = element_text(size = 9, color = "black"),
        plot.title = element_text(face = "bold")) +
  expand_limits(y = 0)

ggsave(plot= plot, filename = paste("MetadataS1/figures/species_temporal_series/",gsub(" ","_",sp),".pdf", sep=""), units = "cm", height = 8, width = 10)
}
