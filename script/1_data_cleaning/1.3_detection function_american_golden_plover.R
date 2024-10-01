#Title: Define detection function parameters of american golden plover based on distance sampling
#Date: November 1 2023
#Author: Louis Moisan

#------------------#
#### Librairies ####
#------------------#
#data manipulation
library(dplyr)
# spatial data manipulation
library(sf)
#Distance sampling (calcul detection function and estimate abundance)
library(Distance)
#data visualization
library(ggplot2)

#---import distance between plovers and transect
distance_table <- read.csv("data/raw/sampling/american_golden_plover_distance.csv") %>% 
  dplyr::select(-X)

#--- The code below have been run to determine the best detection function model based on AIC
 aic_model <- data.frame()
 model_plot <- list()

#list of key term
 key_list= c("hn","hr", "unif")
#list of ajdustement term
 adjust_list= c(NULL, "cos", "herm", "poly")

 for (k in key_list) {
  for (adjust in adjust_list) {
   print(paste(k, adjust))
    model <- ds(data=distance_table, truncation = 150,key=k, adjustment=adjust) 
    summary <-   summary(model)
    aic <- summary$ds$aic
    if (is.null(adjust)){
      adjust= NA
   }
    aic_model <- rbind(aic_model, data.frame(key= k, adjustment= adjust,aic= aic))

   plot(model, which =2, showpoints= F, xlab= "Distance (m)")
   model_plot[[paste(k, adjust)]] <- recordPlot()
  }
}

#Dectection function based on combination of parameter that lead to the lowest AIC
aic_model <- aic_model %>% 
 dplyr::mutate(diff_aic= aic- min(aic)) %>% 
  dplyr::arrange(aic)
#Look at the match between predicted and observed value that seem to represent well the detection function. Those with the minimum AIC seem to overfit the function.
model_plot$`hn cos`
#Retained paramater
best_key= "hn"
best_adjustment <- "cos"

#look at the lowest aic based on number of ajdustement term
 ds_function <-  ds(data=distance_table, truncation = 150, key=best_key, adjustment=best_adjustment)
#best number adjustment term
best_nadj=1

 ds_function <-  ds(data=distance_table, truncation = 150, key=best_key, adjustment=best_adjustment, nadj=best_nadj)
 ds_function$ddf
#Look at the fit of the model (Good)
gof_ds(ds_function)

#--- Export plot of the fit with empirical observations
#Look at the fit with empirical observation
 plot(ds_function, which =2, showpoints= F, xlab= "Distance (m)")