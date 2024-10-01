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
metadata <- read.csv("data/metadata/summary_methods.csv")

#Import the list of Bylot Island species
sp_taxonomy <- read.csv("DataS1/BYLOT-species_taxonomy.csv") %>% 
  dplyr::mutate(species= gsub("'","", gsub("-"," ",tolower(species_en))))

#Abundance data
abundance <- read.csv("DataS1/BYLOT-species_abundance.csv") %>% 
  dplyr::mutate(year= ifelse(is.na(year), "mean", year),
                species= gsub("'","", gsub("-"," ",tolower(species_en)))) %>%
  dplyr::group_by(species, method_description) %>% 
  dplyr::summarise(min= min(abundance), max= max(abundance), mean= as.character(round(mean(abundance), digits=0)), sd= as.character(round(sd(abundance), digits= 0)), n= n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(n= ifelse(n==1, NA, n), min= ifelse(min==max, NA, min), max= ifelse(max==min, NA, max))



#--------------------------#
#### Generate the table ####
#--------------------------#
#Join data frames together
table <- metadata %>% 
  dplyr::left_join(abundance, by= c("species", "method_description")) %>% 
  dplyr::left_join(sp_taxonomy)

#--- reoreder row based on species taxonomy
#Extract beginning year of each time series
table <- table %>% 
  dplyr::mutate(year= as.numeric(substr(table$period, 1, 4)),
                species= factor(species, levels= sp_taxonomy$species)) %>% 
  dplyr::arrange(species, year)

#format table
table <- table %>% 
  dplyr::select(species_en, period, method_description, method_quality, justification, min, max, mean, sd, n) %>% 
  #rename columns
  dplyr::rename(Species= species_en,  Method= method_description,  `Method quality`= method_quality, Justification=justification, `Lowest abundance`= min, `Highest abundance`= max, `Mean abundance`= mean) %>% 
  #Set mean to NA in column estimate
  dplyr::mutate(period= ifelse(period == "mean", NA,period)) %>% 
  #Add years to column n (sample size)
  dplyr::mutate(n= paste0(n, " (",period,")")) %>% 
  dplyr::mutate(n= ifelse(n== "NA (NA)", NA, n)) %>% 
  #Remove column Estimate
  dplyr::select(-period)


#as tex
table_latex <- xtable(table, caption = "Summary of the lowest, highest, mean and standard deviation of the estimated abundance of each vertebrate species in the vertebrate community of the southern plain of Bylot Island (389 km2). In some cases, two independent approaches have been used to estimate the abundance of the same species as a proxy for uncertainty. We provide a qualitative measure of the method quality based on data available, method used for extrapolation (if necessary), and in some cases, from the fit of statistical models to estimate density.",
                      label= "table:table_summary_methods_and_abundance",
                      align= c("p{0.00\\textwidth}", 
                               "|p{0.10\\textwidth}|", #species
                                "p{0.27\\textwidth}|", #method
                                "p{0.06\\textwidth}|", #method quality
                                "p{0.23\\textwidth}|", #justification
                                "p{0.09\\textwidth}|", #lowest abundance
                                "p{0.09\\textwidth}|", #highest abundance
                               "p{0.085\\textwidth}|", #Mean abundance
                               "p{0.04\\textwidth}|", #sd
                               "p{0.10\\textwidth}|") #n
                      ) 
#function to bold column header
bold <-  function(x) {paste('{\\textbf{',x,'}}', sep ='')}


print(table_latex,
      include.rownames = FALSE,
      tabular.environment = "longtable",
      hline.after= c(-1,0:nrow(table)),
      size="\\fontsize{8pt}{10pt}\\selectfont",
      file = "MetadataS1/tables/table_summary_methods_and_abundance.tex",
      sanitize.colnames.function=bold,
      caption.placement = "top")

