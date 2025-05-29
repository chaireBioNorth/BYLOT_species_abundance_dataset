#Title: Extract the abundance of American ermine on the Bylot Island study area 
#Date: October 11 2023
#Last update:  October 10 2024

#--- Import dataframe with indices of ermines' abundances from 
# https://www.sciencedirect.com/science/article/pii/S1439179122000895
df_ermine = read.csv('data/raw/sampling/ermine_index.csv', header= TRUE)

# Minimum and maximum ermine abundances
#https://esajournals.onlinelibrary.wiley.com/doi/abs/10.1890/11-1973.1?casa_token=5nWeE-CMA9gAAAAA%3AcCaOHXXyyZHXxmtxgyzr01ieSct6FZwrzkwQ4SDjkFD-mo1U6uj0oa7_pcyJ-EHBYXy1fPyTXdxOo8NEPg
# in ind.·km-2
ermin = 0.02
ermax = 0.4

#Associate the minimum and maximum scores of relative abundance with the minimum and maximum density of individuals, respectively
df_ermine$Ermine_estimated_abundance = ermin + (ermax-ermin)*df_ermine$Ermine / max(df_ermine$Ermine)

# number of lines
n = length(df_ermine$Year)

# create df to store the abundances
df_data_final <- data.frame(
  species = rep('american ermine', n),
  zone = rep('qarlikturvik valley', n),
  year = df_ermine$Year,
  ind_density_km2 = df_ermine$Ermine_estimated_abundance,
  sd_ind_density_km2 = rep(NA, n)
)

#Assume the same density at the scale of the study area
ermine <- df_data_final %>% 
  rbind(df_data_final %>%  dplyr::mutate(zone= "study area"))

#return "ermine" object with estimated density at the scale of the study area
american_ermine <- ermine %>% 
  dplyr::filter(zone == "study area") %>% 
  dplyr::mutate(breeding_status= "undetermined", method= "testimonials", spatial_extrapolation= "yes") %>% 
  dplyr::select(-sd_ind_density_km2)