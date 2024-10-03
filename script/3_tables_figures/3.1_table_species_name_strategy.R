# Libraries
library(dplyr)
library(xtable)

  table_name <- read.csv("DataS1/BYLOT-species_taxonomy.csv") %>% 
  dplyr::select(functional_group, species_scientific, species_en, migratory_status) %>% 
  dplyr::rename(`Functional group`=functional_group,`Scientific name`= species_scientific, `English name`= species_en, `Migratory status`= migratory_status)

  
#function to bold column header
bold <-  function(x) {paste('{\\textbf{',x,'}}', sep ='')}
#function to italic text
italic <- function(x){
  paste0('{\\emph{ ', x, '}}')
}

#as tex
sp_name_latex <- xtable(table_name, caption = "Species of the vertebrate community of Bylot Island and their corresponding migratory status (i.e., resident, partial migrant or migrant).", label= "table:table_species_name_strategy")
print(sp_name_latex,
      include.rownames = FALSE,
      tabular.environment = "tabularx",
      sanitize.colnames.function=bold,
      sanitize.text.function = function(x) {
        ifelse(x %in% table_name$`Scientific name`, italic(x), x)
      },
      width="0.97\\textwidth",
      align="rXXXXXX",
      size="\\fontsize{10pt}{10pt}\\selectfont",
      file = "MetadataS1/tables/table_species_name_strategy.tex",
      caption.placement = "top")
