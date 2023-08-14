"
########################################################
## PLOT-1 ASH DIEBACK REGENERATION
########################################################
##
## AUTHOR: Haoran Wu (haoran.wu@wolfson.ox.ac.uk)
## Ecosystem Lab | Environmental Change Institute
## School of Geography and the Environment @ Wolfson College
## University of Oxford
##
########################################################
## INPUT DATA:
##   '../0_data/Seed-data_Data-for-Haoran-011723.csv' - seed data
##
########################################################
"
rm(list = ls())
library(ggplot2)
library(dplyr)



#import data------------------
dat <- read.csv("../0_data/Seed-data_Data-for-Haoran-011723.csv")

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
  labs(x = "Site", y = "Number of Ash Stands", fill = "Plot") +
  theme_minimal() + theme1
