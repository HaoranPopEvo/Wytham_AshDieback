"
########################################################
## CLAC-2-3 RESISTANT TREES
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

rm(list = ls())
require(devtools)
library(ggplot2)
library(stringr)

setwd("../ecode/")
load_all()

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

# Model------------------------
## Initial value of 'beta' = 0.15
dX_Cdt <- function(X_C, Y_C, X_A, Y_A, Y_AR, Y_CR,
                   nu = 0.5345907, q = 0.5676651, beta = 0.08681674, mu = 0.4836877, g = 0.04)
  nu * X_A + nu * (1 - q) * Y_A - beta * X_C * (Y_C + Y_A + Y_CR + Y_AR) - (mu + g) * X_C

dY_Cdt <- function(X_C, Y_C, Y_A, Y_CR, Y_AR,
                   beta = 0.08681674, mu = 0.4836877, fai = 0.1593713 , g = 0.04, rho_C = 0.1560502, delta = 4)
  beta * X_C * (Y_C + Y_A + Y_CR + Y_AR) - (mu + g * (1 - fai)) * Y_C - rho_C * Y_C[-delta]

dX_Adt <- function(X_C, Y_C, X_A, Y_A, Y_CR, Y_AR, beta = 0.08681674, g = 0.04)
  g * X_C - beta * X_A * (Y_C + Y_A + Y_CR + Y_AR)

dY_Adt <- function(X_A, Y_C, Y_A, Y_CR, Y_AR,
                   beta = 0.08681674, g = 0.04, fai = 0.1593713, rho_A = 0.09070803, delta = 4)
  beta * X_A * (Y_C + Y_A + Y_CR + Y_AR) + g * (1 - fai) * Y_C - rho_A * Y_A[-delta]

dX_CRdt <- function(Y_C, Y_A, X_CR, X_AR, Y_CR, Y_AR,
                    nu = 0.5345907, beta = 0.08681674, mu = 0.4836877, g = 0.04)
  nu * (X_AR + Y_AR) - beta * X_CR * (Y_C + Y_A + Y_CR + Y_AR) - (mu + g) * X_CR

dY_CRdt <- function(Y_C, Y_A, Y_CR, Y_AR, beta = 0.08681674, mu = 0.4836877, g = 0.04)
  beta * Y_CR * (Y_C + Y_A + Y_CR + Y_AR) - (mu + g) * Y_CR

dX_ARdt <- function(Y_C, Y_A, X_CR, X_AR, Y_CR, Y_AR, beta = 0.08681674, g = 0.04)
  g * X_CR - beta * X_AR * (Y_C + Y_A + Y_CR + Y_AR)

dY_ARdt <- function(Y_C, Y_A, X_AR, Y_CR, Y_AR, beta = 0.08681674, g = 0.04)
  beta * X_AR * (Y_C + Y_A + Y_CR + Y_AR) + g * Y_CR


x <- eode(dX_Cdt = dX_Cdt, dY_Cdt = dY_Cdt, dX_Adt = dX_Adt, dY_Adt = dY_Adt,
          dX_CRdt = dX_CRdt, dY_CRdt = dY_CRdt, dX_ARdt = dX_ARdt, dY_ARdt = dY_ARdt,
          constraint = c("X_C>=0","Y_C>=0","X_A>=0","Y_A>=0","X_CR>=0","Y_CR>=0","X_AR>=0","Y_AR>=0"))
print(x)

# Simulation------------------
resist_pc <- pc_calculator(eode_proj(x, value0 = pp(list(X_C = 16, Y_C = 0, X_A = 50, Y_A = 1,
                                                            X_CR = 0, Y_CR= 0, X_AR= 22,  Y_AR= 0)), N = 10000, step = 0.01),
                           formula = c("X_CT = X_C + X_CR", "X_AT = X_A + X_AR",
                                       "Y_CT = Y_C + Y_CR", "Y_AT = Y_A + Y_AR"))
  #0% resistant trees: X_A = 72, X_AR = 0
  #5% resistant trees: X_A = 68, X_AR = 4
  #10% resistant trees: X_A =63, X_AR = 9
  #30% resistant trees: X_A =50, X_AR = 22
resist_pc <- resist_pc[!(names(resist_pc) %in% x$variables)]
class(resist_pc) <- "pc"
plot(
  resist_pc,
  model_var_label = list(X_AT = "Susceptible Adults", X_CT = "Susceptible Saplings",
                         Y_AT = "Infected Adults", Y_CT = "Infected Saplings"),
  model_var_color = list(X_AT = "#765916", X_CT = "#C7B790",
                         Y_AT = "#3157EE", Y_CT = "#81A2DB")
) + theme1 + xlab("Year") + ylab("Number")

# Deer Browsing---------------
deers <- eode_sensitivity_proj(x, valueSpace = list(X_C = 16, Y_C = 0, X_A = 68, Y_A = 1,
                                                    X_CR = 0, Y_CR= 0, X_AR= 4,  Y_AR= 0,
                                                    mu = seq(0.2, 2, 0.2)), N = 10000, step = 0.01)

deers_calc <- deers
for(i in 1:length(deers_calc)){
  deers_calc[[i]]$pc <- pc_calculator(
    x, deers_calc[[i]]$pc,
    formula = c("X_CT = X_C + X_CR", "X_AT = X_A + X_AR", "Y_CT = Y_C + Y_CR", "Y_AT = Y_A + Y_AR")
  )
  deers_calc[[i]]$pc <- deers_calc[[i]]$pc[!(names(deers_calc[[i]]$pc) %in% x$variables)]
}

plot.pcfamily(
  deers_calc,
  model_var_label = list(X_AT = "Susceptible Adults", X_CT = "Susceptible Saplings",
                         Y_AT = "Infected Adults", Y_CT = "Infected Saplings"),
  model_var_color = list(X_AT = "#765916", X_CT = "#C7B790",
                         Y_AT = "#3157EE", Y_CT = "#81A2DB"),
  facet_paras = FALSE,
  size = 0.9
) + theme1 + xlab("Year") + ylab("Number") + theme(legend.title = element_text(size = 14)) +
  scale_x_continuous(breaks = seq(0, 100, by = 20)) #Canvas: 10.22*2.96
  #facet_grid(df$Varible ~ ., switch = "y")
save.image("../1_code/calc-2-3-result.R.RData")

