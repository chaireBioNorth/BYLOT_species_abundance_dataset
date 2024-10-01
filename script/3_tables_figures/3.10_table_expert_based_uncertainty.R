#Title: Create a table of the expert based uncertainty described by guess estimate
#Author: Louis Moisan
#Date: September 30 2024

#-----------------#
#### Libraries ####
#-----------------#
#general data manipulation
library(dplyr)
#export .tex
library(xtable)


#--- Import the list of Bylot Island species
sp_taxonomy <- read.csv("DataS1/BYLOT-species_taxonomy.csv") %>% 
  dplyr::mutate(species= gsub("'","", gsub("-"," ",tolower(species_en))))

#--- Import estimates made by experts
table <- read.csv("data/metadata/expert_based_uncertainty.csv") %>% 
  dplyr::left_join(sp_taxonomy) %>% 
  dplyr::rename(Species= species_en, Period= period, `Lowest abundance`= lowest_abundance, `Highest abundance`= highest_abundance, `Mean abundance`= mean_abundance) %>% 
  dplyr::select(Species, Period, `Lowest abundance`,`Highest abundance`,`Mean abundance`) %>% 
  dplyr::mutate(Period= ifelse(is.na(Period), "Mean abundance",Period))

#Add above header row
addtorow <- list()
addtorow$pos <- list(0, 0)
addtorow$command <- c("& &\\multicolumn{3}{c}{Annual abundance (ind.)} \\\\\n",
                      "Species & Period & Lowest & Highest & Mean \\\\\n")
#as tex
table_latex <- xtable(table, caption = "Considering the absence of confidence intervals in our abundance estimates, we present below uncertainty intervals on estimated abundance values derived from field expert impressions. The intervals presented represent the minimum and maximum values between which the experts believe the actual abundance values should lie. When annual abundance has been estimated for several years (time series) we present intervals over the minimum and maximum abundance values encountered during the given time series. For other species, the uncertainty interval is estimated on the mean abundance.")
print(table_latex,
      include.rownames = FALSE,
      tabular.environment = "tabularx",
      width="\\textwidth",
      align="rXXXXXX",
      size="\\fontsize{10pt}{8pt}\\selectfont",
      file = "MetadataS1/tables/table_expert_based_uncertainty.tex",
      caption.placement = "top",
      add.to.row = addtorow,
      include.colnames = FALSE)

