#general data manipulation
library(dplyr)
library(tidyr)
#export latex table
library(xtable)


#--- Import as .csv
df_body_mass <- read.csv("data/clean/species_body_mass.csv") %>% 
  dplyr::select(-X, -Scientific.name) %>% 
  dplyr::rename(Species= Species.name, `Body mass g (study area)`= Body.mass.g..study.area., `Body mass g (literature)`= Body.mass.g..literature.)


#reorder names based on defined species list
sp_metadata <- read.csv("data/metadata/species_metadata.csv")
# Find the indices of species in df2 in df1
indices <- match(sp_metadata$species, df_body_mass$Species)
# Reorder df1 based on the indices
df_body_mass <- df_body_mass[indices, , drop = FALSE] 

#update species names
df_body_mass <- df_body_mass %>% 
  dplyr::mutate(Species= dplyr::recode(Species,"bairds sandpiper"="Baird's sandpiper", "long tailed duck"= "long-tailed duck", "long tailed jaeger"= "long-tailed jaeger", "red throated loon"= "red-throated loon", "pacific loon"= "Pacific loon","rough legged hawk"= "rough-legged hawk", "arctic hare"= "Arctic hare", "black bellied plover"= "black-bellied plover", "buff breasted sandpiper"= "buff-breasted sandpiper", "american pipit"= "American pipit", "arctic fox"="Arctic fox", "common ringed plover"= "common-ringed plover", "american golden plover"= "American golden-plover", "white rumped sandpiper"= "white-rumped sandpiper", "lapland longspur"= "Lapland longspur"))


#Remove decimals
df_body_mass <- df_body_mass %>% 
  dplyr::mutate(`Body mass g (study area)`= round(`Body mass g (study area)`, digits = 0),
                `Body mass g (literature)`= round(`Body mass g (literature)`, digits=0),
                n= ifelse(is.na(n), NA, paste0("(",n,")")))
# set NA for ermine and Arctic fox
df_body_mass[df_body_mass$Species %in% c("ermine", "Arctic fox"),]$n <- "(NA)"



df_body_mass_latex <- xtable(df_body_mass, caption= "Average adult body mass (g) of the vertebrate species from Bylot Island measured directly at the study area or extracted from the literature (Wilman et al., 2014).", label= "table:body_mass",digits = 0)


addtorow <- list()
addtorow$pos <- list(0, 0)
addtorow$command <- c("& \\multicolumn{3}{c}{Body mass (g)} \\\\\n",
                      "Species & Study area & (n) & Literature \\\\\n")

# Save the table as .tex to directly insert it in mat_met.tex
print(df_body_mass_latex,
      include.rownames = FALSE,
      include.colnames = FALSE,
      add.to.row = addtorow,
      tabular.environment = "tabularx",
      width="0.6\\textwidth",
      align="rXXXXXX",
      size="\\fontsize{10pt}{10pt}\\selectfont",
      file = "manuscript/tables/body_mass.tex",
      caption.placement = "top")



#---------------------------------------#
#### Export as .csv for the data set ####
#---------------------------------------#
df_body_mass_dataset <- df_body_mass %>% 
  dplyr::select(-n) %>% 
  dplyr::rename(species= Species, study_area=`Body mass g (study area)`, literature= `Body mass g (literature)`) %>% 
  tidyr::pivot_longer(cols = c(study_area, literature),
                      names_to = "data",
                      values_to = "body_mass_g") %>% 
  na.omit()


write.csv(df_body_mass_dataset, "dataset/BYLOT-species_mean_body_mass.csv", row.names = FALSE)
