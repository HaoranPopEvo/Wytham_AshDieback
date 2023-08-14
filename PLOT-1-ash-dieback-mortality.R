"
########################################################
## PLOT-1 ASH DIEBACK MORTALITY
########################################################
##
## AUTHOR: Haoran Wu (haoran.wu@wolfson.ox.ac.uk)
## Ecosystem Lab | Environmental Change Institute
## School of Geography and the Environment @ Wolfson College
## University of Oxford
##
########################################################
## INPUT DATA:
##   '../0_data/Mortality-data-121422.csv' - mortality data
##
########################################################
"
rm(list = ls())
library(ggplot2)
library(dplyr)

aggregate_ash_death <- function(df){
  ash <- with(df, df[species=="Ash" | species=="ash", ])

  ash_death_2021 <- with(df, df[(grepl("dead", death_2021) |
                                   grepl("Dead", death_2021) |
                                   grepl("Dying", death_2021) |
                                   grepl("dear", death_2021)) &
                                  (species=="Ash" | species=="ash"), ])

  ash_death_2022 <- with(df, df[(grepl("dead", death_2022) |
                                   grepl("Dead", death_2022) |
                                   grepl("Dying", death_2022) |
                                   grepl("dear", death_2022)) &
                                  (species=="Ash" | species=="ash"), ])

  ash_sum <- aggregate(ash$species,
                       by = list(plot_location = ash$plot_location, plot = ash$plot),
                       FUN = length) %>% rename(total = colnames(.)[3])

  ash_death_2021_sum <- aggregate(ash_death_2021$species,
                                  by = list(plot_location = ash_death_2021$plot_location, plot = ash_death_2021$plot),
                                  FUN = length) %>% rename(death_2021 = colnames(.)[3])

  ash_death_2022_sum <- aggregate(ash_death_2022$species,
                                  by = list(plot_location = ash_death_2022$plot_location, plot = ash_death_2022$plot),
                                  FUN = length) %>% rename(death_2022 = colnames(.)[3])

  res <- do.call(rbind, lapply(1:nrow(ash_sum), function(i){
    death_2021_val <- ash_death_2021_sum$death_2021[ash_death_2021_sum$plot_location == ash_sum$plot_location[i] &
                                                      ash_death_2021_sum$plot == ash_sum$plot[i]]
    death_2022_val <- ash_death_2022_sum$death_2022[ash_death_2022_sum$plot_location == ash_sum$plot_location[i] &
                                                      ash_death_2022_sum$plot == ash_sum$plot[i]]
    data.frame(
      plot_location = ash_sum$plot_location[i],
      plot = ash_sum$plot[i],
      total = ash_sum$total[i],
      death_2021 = ifelse(length(death_2021_val)==0, 0, death_2021_val),
      death_2022 = ifelse(length(death_2022_val)==0, 0, death_2022_val)
    )
  }))

  res$plot <- gsub("A", "a", res$plot)
  res$plot <- gsub("B", "b", res$plot)
  res$plot <- gsub("C", "c", res$plot)
  res
}


#import data------------------
death <- read.csv("../0_data/Mortality-data-121422.csv") %>% aggregate_ash_death()

#render plot------------------
theme1 <- theme_bw()+
  theme(axis.text.x=element_text(size=16,angle=0,colour="black"),
        axis.text.y=element_text(size=16,angle=0,colour="black"),
        axis.title=element_text(size=18),
        axis.line=element_line(linetype=1,color="black",size=0.1),
        axis.ticks = element_line(colour="black"),
        panel.grid.major = element_blank(), #change the major and minor grid lines,
        panel.grid.minor = element_blank(), #if want to change, check this parameters, I think it's easier to dao that
        #strip.background = element_rect(colour = "black",size = 0.8),
        #panel.background = element_rect(colour="black", fill="white"),
        #panel.border = element_blank(),
        panel.border = element_rect(colour = "black",fill=NA,size = 1.2),
        plot.title=element_text(size=14,angle=0,colour="black", face = "italic"),
        plot.tag=element_text(size=14,angle=0,colour="black", face = "bold"),
        plot.caption=element_text(size=14,angle=0,colour="black",face = "italic"),
        axis.title.y=element_text(vjust=1.9),
        axis.title.x=element_text(vjust=0.5),
        legend.text=element_text(colour="black",size=14),
        legend.background= element_rect(fill = "transparent", color = NA),
        #legend.position = "bottom",
        legend.title = element_text(colour="black", size=14,angle=0))

##number of ash trees--------------------
ggplot(death, aes(x = plot_location, y = total, fill = plot)) +
  geom_bar(stat = "identity", position = "dodge", alpha  = 0.8, width = 0.8, color = "black", linewidth = 0.5) +
  scale_fill_manual(values = c("a" = "#507359", "b" = "#a3ccb5", "c" = "#cdd2da")) +
  labs(x = "Site", y = "Number of Ash Trees", fill = "Plot") +
  theme_minimal() + theme1 + ylim(0, 101)

##deaths 2021-------------------
ggplot(death, aes(x = plot_location, y = death_2021, fill = plot)) +
  geom_bar(stat = "identity", position = "dodge", alpha  = 0.8, width = 0.8, color = "black", linewidth = 0.5) +
  scale_fill_manual(values = c("a" = "#507359", "b" = "#a3ccb5", "c" = "#cdd2da")) +
  labs(x = "Site", y = "Deaths 2021", fill = "Plot") +
  theme_minimal() + theme1 + ylim(0, 101)

##deaths 2022-------------------
ggplot(death, aes(x = plot_location, y = death_2022, fill = plot)) +
  geom_bar(stat = "identity", position = "dodge", alpha  = 0.8, width = 0.8, color = "black", linewidth = 0.5) +
  scale_fill_manual(values = c("a" = "#507359", "b" = "#a3ccb5", "c" = "#cdd2da")) +
  labs(x = "Site", y = "Deaths 2022", fill = "Plot") +
  theme_minimal() + theme1 + ylim(0, 101)

#Canvas 4.77 x 3.65
