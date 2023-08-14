"
########################################################
## CLAC-2-2 MODEL PREDICTION
########################################################
##
## AUTHOR: Haoran Wu (haoran.wu@wolfson.ox.ac.uk)
## Ecosystem Lab | Environmental Change Institute
## School of Geography and the Environment @ Wolfson College
## University of Oxford
##
########################################################
## INPUT DATA:
##   None
##
########################################################
"

require(devtools)
library(ggplot2)
library(stringr)

setwd("../ecode/")
load_all()

# Import Model------------------------
load("../1_code/fitted-model.RData")
rm(list = ls()[ls()!="x_sa"])
print.eode(x_sa)

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
        strip.text = element_text(size = 14),
        panel.border = element_rect(colour = "black",fill=NA,size = 1.2),
        plot.title=element_text(size=14,angle=0,colour="black", face = "italic"),
        plot.tag=element_text(size=14,angle=0,colour="black", face = "bold"),
        plot.caption=element_text(size=14,angle=0,colour="black",face = "italic"),
        axis.title.y=element_text(vjust=1.9),
        axis.title.x=element_text(vjust=0.5),
        legend.text=element_text(colour="black",size=14),
        legend.background= element_rect(fill = "transparent", color = NA),
        #legend.position = "bottom",
        legend.title = element_blank())

# Projection--------------------------
plot.pc(pc_calculator(x_sa, eode_proj(x_sa, value0 = pp(list(X_C = 4, Y_C = 0, X_A = 30, Y_A = 1)),
                                      N = 1000, step = 0.01),
                      formula = "disease_death = (rho_C * Y_C[-delta] + rho_A * Y_A[-delta]) dt[0-t]"),
        model_var_label = list(disease_death = "Deaths", X_A = "Susceptible Adults", X_C = "Susceptible Saplings",
                               Y_A = "Infected Adults", Y_C = "Infected Saplings"),
        model_var_color = list(disease_death = "#F53239", X_A = "#765916", X_C = "#C7B790",
                               Y_A = "#3157EE", Y_C = "#81A2DB"),
        size = 0.9) + theme1 + xlab("Year") + ylab("Number") #10-year dynamics, Canvas: 6.71*3.94
   #t = 10yr, a total individual of 24.34408 => reduction of 28.39976%

#Initial Conditions--------------------
sens_init <- eode_sensitivity_proj(x_sa, valueSpace = list(X_C = seq(20,100,40), Y_C = 0,
                                                           X_A = seq(20,100,40), Y_A = 1), N = 1000, step = 0.01)
for(i in 1:length(sens_init)){
  sens_init[[i]]$pc <- pc_calculator(
    x_sa, sens_init[[i]]$pc,
    formula = "disease_death = (rho_C * Y_C[-delta] + rho_A * Y_A[-delta]) dt[0-t]"
  )
}
plot.pcfamily(
  sens_init,
  model_var_label = list(disease_death = "Deaths", X_A = "Susceptible Adults", X_C = "Susceptible Saplings",
                         Y_A = "Infected Adults", Y_C = "Infected Saplings"),
  model_var_color = list(disease_death = "#F53239", X_A = "#765916", X_C = "#C7B790",
                         Y_A = "#3157EE", Y_C = "#81A2DB"),
  facet_grid_label = list(X_A = "Initial Adult", X_C = "Initial Sapling"),
  size = 0.9
) + theme1 + xlab("Year") + ylab("Number") +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) #Canvas: 9.86*7.05

#Parameters----------------------------
para_init <- eode_sensitivity_proj(x_sa, valueSpace = list(X_C = 16, Y_C = 0, X_A = 72, Y_A = 1,
                                                           fai = seq(0.1, 1, 0.1) ), N = 1000, step = 0.01)

  #OPTIONS
  ## beta = seq(0.01, 0.1, 0.01)
  ## rho_A = seq(0.3, 3, 0.3)
  ## rho_C = seq(0.03, 0.3, 0.03)
  ## mu = seq(0.02, 0.2, 0.02)
  ## nu = seq(0.02, 0.2, 0.02)
  ## q = seq(0.1, 1, 0.1)
  ## fai = seq(0.1, 1, 0.1)

plot.pcfamily(
  para_init,
  model_var_label = list(X_A = "Susceptible Adults", X_C = "Susceptible Saplings",
                         Y_A = "Infected Adults", Y_C = "Infected Saplings"),
  model_var_color = list(disease_death = "#F53239", X_A = "#765916", X_C = "#C7B790",
                         Y_A = "#3157EE", Y_C = "#81A2DB"),
  facet_paras = FALSE,
  size = 0.9
) + theme1 + xlab("Year") + ylab("Number") + theme(legend.title = element_text(size = 14)) +
  scale_x_continuous(breaks = seq(0, 10, by = 2)) #Canvas: 10.22*2.96

