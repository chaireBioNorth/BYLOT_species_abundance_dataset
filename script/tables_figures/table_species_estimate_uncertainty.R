#Transform .csv of uncertainty on species estimate to a .tex table
library(dplyr)
library(xtable)


esti_uncert <- read.csv("data/metadata/estimate_uncertainty.csv") %>% 
  dplyr::rename(`abundance estimate`= abundance.estimate)

#reorder names based on defined species list
esti_uncert <- read.csv("data/metadata/species_metadata.csv") %>% 
  dplyr::select(species) %>% 
  dplyr::left_join(esti_uncert)

#update species names
esti_uncert <- esti_uncert %>% 
  dplyr::mutate(species= dplyr::recode(species,"bairds sandpiper"="Baird's sandpiper", "long tailed duck"= "long-tailed duck", "long tailed jaeger"= "long-tailed jaeger", "red throated loon"= "red-throated loon", "pacific loon"= "Pacific loon","rough legged hawk"= "rough-legged hawk", "arctic hare"= "Arctic hare", "black bellied plover"= "black-bellied plover", "buff breasted sandpiper"= "buff-breasted sandpiper", "american pipit"= "American pipit", "arctic fox"="Arctic fox", "common ringed plover"= "common-ringed plover", "american golden plover"= "American golden-plover", "white rumped sandpiper"= "white-rumped sandpiper","lapland longspur"= "Lapland longspur"))

#Rename columns 
esti_uncert <- esti_uncert %>% 
  dplyr::rename(Species= species, Estimate= `abundance estimate`, Confidence= confidence, Reason= reason)

# test (add \newline if string if more than 55 characters)
Reason_updated <- c()

for(i in esti_uncert$Reason){
  #space positions
space_positions <-gregexpr(" ", i)[[1]] %>% as.vector()
 if (max(space_positions)>= 53){
#Select first space position that over 53
   select_space <- min(space_positions[which(space_positions>=53)])
#Insert newline at this location
 string <- paste0("\\makecell[tl]{",substr(i, 1, select_space-1), "\\\\",substr(i, select_space +1, nchar(i)), "}")
#Replace original string
 Reason_updated <- c(Reason_updated, string)
 }else{
   Reason_updated <- c(Reason_updated, i)
 }
}

esti_uncert$Reason <- Reason_updated


#as tex
esti_uncert_latex <- xtable(esti_uncert, caption = "Confidence level on the various estimates of annual or mean abundance of vertebrate species on Bylot Island.", label= "table:confidence_estimate_abundance")



print(esti_uncert_latex,
      include.rownames = FALSE,
      tabular.environment = "longtable",
     # width="\\textwidth",
      align= "tl",
      size="\\fontsize{8pt}{10pt}\\selectfont",
      file = "manuscript/tables/confidence_estimate_abundance.tex",
      caption.placement = "top",
      type='latex',
      floating = FALSE,
      sanitize.text.function=identity)

