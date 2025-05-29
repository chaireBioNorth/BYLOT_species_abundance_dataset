#Title: Use the estimated abundance in script 2.1 to format a matrix of species (rows) and year (columns) by retaining only the estimate of abundance with the highest quality (when multiple estimates are available)
#Date: May 28 2025
#Last update: NA

#-----------------#
#### Libraries ####
#-----------------#
#data manipulation
library(dplyr)
library(tidyr)
#data visualization
library(ggplot2)

#--------------------------#
#### Data manipulatuion ####
#--------------------------#
#Import annual or mean species abundance (see script 2.1)
abundance_df <- read.csv("DataS1/BYLOT-species_abundance.csv", check.names = FALSE) %>% 
  #Change method quality to transform as a semi-quantitative variable
  dplyr::mutate(method_quality=
                  factor(method_quality,
                         levels = c("very low", "low", "moderate", "high"),  ordered = TRUE)) %>%
  #Select the estimate derived from the method with the highest quality for each species
  dplyr::group_by(species_en, year) %>%
  dplyr::slice_max(order_by = method_quality) %>% 
  #Extract mean abundance if presence of two estimates of the same quality
  dplyr::group_by(species_en, year, method_quality) %>% 
  dplyr::summarise(abundance= mean(abundance), .groups = "drop") %>% 
  #Change value of NA in column year for "mean"
  dplyr::mutate(year= ifelse(is.na(year), "mean", year)) %>% 
  dplyr::arrange(year)

#Transform into a wide format
abundance_wide <- abundance_df %>% 
  dplyr::select(species_en, year, abundance) %>% 
  tidyr::pivot_wider(names_from = year, values_from = abundance, values_fill = NA)

#Export as .csv
write.csv(abundance_wide,"DataS1/BYLOT-community_composition.csv", row.names = FALSE)


#--------------------------------------------------------#
#### Plot time line of avaialable abundance estimates ####
#--------------------------------------------------------#
#retrieve order of species for the plot
species_taxonomy <- read.csv("DataS1/BYLOT-species_taxonomy.csv") 

#reorder species and year as factor levels for the plot
abundance_df <- abundance_df %>% 
  dplyr::mutate(species_en= factor(species_en, levels= rev(species_taxonomy$species_en)),
                year= factor(year, levels= c(c(1993:2023), "mean")),
                highlight= ifelse(year == "mean", TRUE, FALSE))

#plot timeline
time_line <- ggplot(abundance_df, aes(x = year, y = species_en, fill= highlight)) +
  geom_tile(color = NA) +
  scale_fill_manual(values = c("black", "#9d0208"))+
  scale_x_discrete(position = "top", drop = FALSE)+
  labs(x = "Year", y = "Species") +
  guides(fill= "none")+
  theme_classic() +
  theme(axis.text = element_text(color = "black"),
        axis.text.x = element_text(angle = 45, hjust = -0.1, size=7.5, color= c(rep("black", times= length(1993:2023)), "#9d0208")),
        plot.margin = margin(t = 5, r = 10, b = 5, l = 5))

ggsave(plot= time_line, units= "mm" ,width = 180, height = 140,dpi= 500, "MetadataS1/figures/species_temporal_series/Time_line_available_estimates.pdf")
length(1993:2023)
