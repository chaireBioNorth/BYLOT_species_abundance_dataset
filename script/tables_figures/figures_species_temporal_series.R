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
sp_abundance <- read.csv("data/clean/species_abundance.csv") %>% 
  dplyr::filter(is.na(year)== FALSE) %>% 
  dplyr::mutate(data= dplyr::recode(data, "nest sampling (extrapolation nests)"="extrapolation from nest sampling", "nest sampling (extrapolation habitat)"= "extrapolation from nest sampling", "combined methods"= "transects, points counts and nest sampling")) %>% 
  dplyr::mutate(species= dplyr::recode(species, "long tailed jaeger"= "long-tailed jaeger", "red throated loon"= "red-throated loon", "pacific loon"= "Pacific loon","rough legged hawk"= "rough-legged hawk", "american golden plover"= "American golden-plover"))

sp_abundance[sp_abundance$species== "snow goose" & sp_abundance$year %in% c(1999:2009),]$data <- "extrapolation from nest sampling"

#Create a data frame to assign colors to each data type
data_color <- data.frame(
  data= c(unique(sp_abundance$data)),
  color= c("#40916c", "#6c757d", "#d00000","#000000","#ca6702","#5a189a")
)

#defined order of graphs
sp_order <- c("brown lemming", "collared lemming", "ermine", "snowy owl", "rough-legged hawk", "peregrine falcon", "glaucous gull", "long-tailed jaeger", "snow goose", "cackling goose", "American golden-plover")

#reorder species in data frame
sp_abundance$species <- factor(sp_abundance$species, levels = sp_order)

# Order the data frame based on the 'Name' column
sp_abundance <- sp_abundance[order(sp_abundance$species), ]

#Read study area
study_area <- sf::st_read("data/raw/shapefiles/study_area.shp") %>% 
  sf::st_drop_geometry() %>% 
  dplyr::filter(zone != "goose_colony")


#-------------------------------------#
#### Extract plot for each species ####
#-------------------------------------#
for (i in 1:length(unique(sp_abundance$species))){
sp=unique(sp_abundance$species)[i]

#Filter data frame for a single species
df <- sp_abundance %>% 
    dplyr::filter(species ==sp)%>% 
  mutate(year_diff = c(0, diff(year))) %>% 
  mutate(group = cumsum(year_diff != 1),
         year= as.integer(year),
         data= as.character(data))

#colors
colors <- data_color[data_color$data %in% unique(df$data),]$color

#Plot
plot <-
  ggplot(df, aes(x= year, y= abundance, group=group))+
  ggtitle(paste0(i, ". ", sp))+
  geom_point(aes(col= data), size= 3)+
  geom_line(col= "black")+
  scale_color_manual(values = colors, name= "Method")+
  labs(x= "Years", y= "Number of individuals")+
  scale_x_continuous(n.breaks = 6)+
  scale_y_continuous(n.breaks = 6, labels = scales::number_format())+
  theme_classic()+
  guides(color= "none", shape= "none")+
  theme(axis.text = element_text(size = 9, color = "black"),
        axis.title = element_text(size = 9, color = "black"),
        plot.title = element_text(face = "bold"))

ggsave(plot= plot, filename = paste("MetadataS1/figures/species_temporal_series/",gsub(" ","_",sp),".pdf", sep=""), units = "cm", height = 8, width = 10)
}


#-------------------------------------#
##### Create a plot for the legend ####
#-------------------------------------#
legend_df <- data.frame(x= 0,
                        y= 1:nrow(data_color),
                        data= c("trapping", "testimonials", "extrapolation from\nnest sampling", "nest sampling", "transects, point counts\nand nest sampling (average)", "distance sampling"),
                        color= c("#6c757d","#ca6702","#d00000","#000000","#5a189a","#40916c"))

legend <- ggplot(data= legend_df, aes(x= x, y=y, label= rev(data)))+
  geom_point(size=3, aes(col= as.factor(rev(y))))+
  scale_color_manual(values= legend_df$color)+
  guides(color= 'none')+
  ggtitle("Methods")+
 geom_text(hjust = -0.2, color= "black", size=5)+
  theme_classic() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.line= element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 18, face = "bold"))+
  coord_cartesian(clip = "off")+
  ylim(1,6)+
  xlim(-0.25, 7)

ggsave(plot= legend, filename = "MetadataS1/figures/species_temporal_series/legend.pdf", units = "cm", height = 8, width = 10)
