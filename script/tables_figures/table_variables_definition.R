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
table <- read.csv("MetadataS1/tables/table_variables_definition.csv") %>% 
  dplyr::rename(`Data file`= Data.file, `Variable identity`= Variable.identity, `Variable definition`= Variable.definition)
  


#as tex
table_latex <- xtable(table, caption = "Summary of variable definitions, units, and data types for each data set file.
",
                      label= "table:summary_variables",
                      align= c("p{0.00\\textwidth}", 
                               "|p{0.08\\textwidth}|", #Data set file
                               "p{0.27\\textwidth}|", #Variable identity
                               "p{0.40\\textwidth}|", #Variable definition
                               "p{0.08\\textwidth}|", #Units
                               "p{0.06\\textwidth}|" #Storage type
                               ) #CI
) 
#function to bold column header
bold <-  function(x) {paste('{\\textbf{',x,'}}', sep ='')}


print(table_latex,
      include.rownames = FALSE,
      tabular.environment = "longtable",
      hline.after= c(-1,0:nrow(table)),
      size="\\fontsize{8pt}{10pt}\\selectfont",
      file = "MetadataS1/tables/table_summary_variables.tex",
      sanitize.colnames.function=bold,
      caption.placement = "top")
