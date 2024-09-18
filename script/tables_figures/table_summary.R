#Title: Create a summary table of all data and method use for each species
#Author: Louis Moisan
#Date: June 3 2024

#-----------------#
#### Libraries ####
#-----------------#
#general data manipulation
library(dplyr)
#export .tex
library(xtable)


#-------------------#
#### Import data ####
#-------------------#
#Metadata
metadata <- read.csv("data/metadata/summary_table.csv", sep = "\t")

#Abundance data
abundance <- read.csv("dataset/BYLOT-species_abundance.csv") %>% 
  dplyr::mutate(year= ifelse(is.na(year), "mean", year)) %>% 
  dplyr::group_by(species, method) %>% 
  dplyr::summarise(min= min(abundance), max= max(abundance), mean= as.character(round(mean(abundance), digits=0)), sd= as.character(round(sd(abundance), digits= 0)), n= n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(n= ifelse(n==1, NA, n), min= ifelse(min==max, NA, min), max= ifelse(max==min, NA, max))


#--------------------------#
#### Generate the table ####
#--------------------------#
#Join data frames together
table <- metadata %>% 
  dplyr::left_join(abundance, by= c("species", "method"))

#format table
table <- table %>% 
  dplyr::select(species, Estimate, Method.description, Method.quality, Justification, min, max, mean, sd, n, Expert.based.confidence.interval) %>% 
  dplyr::rename(Species= species,  Method= Method.description,  `Method quality`= Method.quality, Min= min, Max= max, Mean= mean,  `Expert based confidence interval`=Expert.based.confidence.interval)


#as tex
table_latex <- xtable(table, caption = "Summary of the mean (± standard deviation) and minimum and maximum (when available) annual abundance of vertebrate species for the whole Bylot Island study area (389 km2).  In some cases, several independent approaches have been used to estimate the abundance of a species as a proxy for uncertainty. We did not quantitatively assess uncertainty on each estimate of species abundance. However, we provide a qualitative measure of method quality based on the quality and quantity of data available, method used for extrapolation (if necessary), and in some cases, from the fit of statistical models to estimate density. We also provided coarse confidence intervals derived from consultation with field experts at the study site.",
                      label= "table:summary_methods",
                      align= c("p{0.00\\textwidth}", 
                               "|p{0.11\\textwidth}|", #species
                                "p{0.07\\textwidth}|", #years
                                "p{0.21\\textwidth}|", #method
                                "p{0.06\\textwidth}|", #method quality
                                "p{0.18\\textwidth}|", #justification
                                "p{0.035\\textwidth}|", 
                               "p{0.037\\textwidth}|", #min
                               "p{0.037\\textwidth}|", #mean
                               "p{0.035\\textwidth}|", #sd
                               "p{0.017\\textwidth}|",#n
                               "p{0.14\\textwidth}|" ) #CI
                      ) 
#function to bold column header
bold <-  function(x) {paste('{\\textbf{',x,'}}', sep ='')}


print(table_latex,
      include.rownames = FALSE,
      tabular.environment = "longtable",
      hline.after= c(-1,0:nrow(table)),
      size="\\fontsize{8pt}{10pt}\\selectfont",
      file = "manuscript/tables/table_summary_methods.tex",
      sanitize.colnames.function=bold,
      caption.placement = "top")

