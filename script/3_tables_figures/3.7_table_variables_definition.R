#Title: Create a summary table of all variables in the data set
#Author: Louis Moisan
#Date: September 23 2024

#-----------------#
#### Libraries ####
#-----------------#
#general data manipulation
library(dplyr)
#export .tex
library(xtable)

#Import .csv format
table <- read.csv("data/metadata/variables_definition.csv") %>% 
  dplyr::rename(`Data file`= data.file, `Variable identity`= variable.identity, `Variable definition`= variable.definition, Units= units)
  
table[is.na(table)] <- "NA"

#as tex
table_latex <- xtable(table, caption = "Summary of variable definition and unit of measurement.",
                      label= "table:table_variables_definition",
                      align= c("p{0.00\\textwidth}", 
                               "|p{0.08\\textwidth}|", 
                               "p{0.21\\textwidth}|", 
                               "p{0.50\\textwidth}|", 
                               "p{0.08\\textwidth}|" 
                               ) 
) 
#function to bold column header
bold <-  function(x) {paste('{\\textbf{',x,'}}', sep ='')}


print(table_latex,
      include.rownames = FALSE,
      tabular.environment = "longtable",
      hline.after= c(-1,0:nrow(table)),
      size="\\fontsize{8pt}{10pt}\\selectfont",
      file = "MetadataS1/tables/table_variables_definition.tex",
      sanitize.colnames.function=bold,
      caption.placement = "top")
