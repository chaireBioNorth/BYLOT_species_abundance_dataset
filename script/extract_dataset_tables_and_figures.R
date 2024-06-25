#---------------------#
#### Data cleaning ####
#---------------------#
source("script/data_cleaning/01_clean_year_monitoring_metadata.R")
source("script/data_cleaning/02_extract_species_body_mass.R")
#Run if wanted to look at the AIC model selection for the american golden-plover distance sampling method
#source("script/data_cleaning/extract_detection function_american_golden_plover.R")


#------------------------------------------#
#### Estimate abundance of each species ####
#------------------------------------------#
source("script/extract_species_abundance/extract_species_abundance.R")

#---------------------------------#
#### Export tables and figures ####
#---------------------------------#
source("script/tables_figures/table_species_name_strategy.R")
source("script/tables_figures/table_species_year_monitoring.R")
source("script/tables_figures/table_interannual_variation_nest_density.R")
source("script/tables_figures/table_species_relative_abundance_upland.R")
source("script/tables_figures/table_body_mass.R")
source("script/tables_figures/table_summary_methods_data.R")
source("script/tables_figures/table_species_estimate_uncertainty.R")
source("script/tables_figures/table_mean_abundance.R")
source("script/tables_figures/figures_species_temporal_series.R")
source("script/tables_figures/maps.R")
