"
########################################################
## PLOT-1 ASH DIEBACK SAPLING RATIO
########################################################
##
## AUTHOR: Haoran Wu (haoran.wu@wolfson.ox.ac.uk)
## Ecosystem Lab | Environmental Change Institute
## School of Geography and the Environment @ Wolfson College
## University of Oxford
##
########################################################
## INPUT DATA:
##   '../0_data/DBH-080623.csv' - DBH data
##
########################################################
"
rm(list = ls())
library(ggplot2)
library(dplyr)

#functions-------------
##average DBH
mean_dbh <- function(df){
  df$dbh_mm_mean <- with(df, ifelse(is.na(dbh_mm_2021) & is.na(dbh_mm_2022), NA, 
                                    ifelse(is.na(dbh_mm_2021), dbh_mm_2022,
                                           ifelse(is.na(dbh_mm_2022), dbh_mm_2021, (dbh_mm_2021 + dbh_mm_2022)/2 ))))
  df
}

aggregate_ash_sapling_ratio <- function(df, thr = 100){
  ash <- df[df$species=="ash",]
  
  aggregate(ash$dbh_mm_mean,
            list(plot_location = ash$plot_location, plot = ash$plot),
            function(x){
              x <- x[!is.na(x)]
              sum(x<thr)/length(x)
            }) %>% rename(sapling_ratio = colnames(.)[3])
}


#procedure------------------
read.csv("../0_data/DBH-080623.csv") %>% mean_dbh() %>% aggregate_ash_sapling_ratio()
