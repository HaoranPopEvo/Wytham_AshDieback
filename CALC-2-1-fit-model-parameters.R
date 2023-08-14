"
########################################################
## CLAC-2-1 FIT MODEL PARAMETERS
########################################################
##
## AUTHOR: Haoran Wu (haoran.wu@wolfson.ox.ac.uk)
## Ecosystem Lab | Environmental Change Institute
## School of Geography and the Environment @ Wolfson College
## University of Oxford
##
########################################################
## INPUT DATA:
##    None
##
########################################################
"

require(devtools)
library(ggplot2)

setwd("../ecode/")
load_all()


# Model------------------------
## Initial value of 'beta' = 0.15
dX_Cdt <- function(X_C, Y_C, X_A, Y_A, nu = 0.712, q = 0.35, beta = 0.1, mu = 0.643, g = 1/25)
  nu * X_A + nu * (1 - q) * Y_A - beta * X_C * (Y_C + Y_A) - (mu + g) * X_C

dY_Cdt <- function(X_C, Y_C, Y_A, beta = 0.1, mu = 0.643, fai = 0.26 , g = 1/25, rho_C = 3.111, delta = 4)
  beta * X_C * (Y_C + Y_A) - (mu + g * (1 - fai)) * Y_C - rho_C * Y_C[-delta]

dX_Adt <- function(X_C, Y_C, X_A, Y_A, beta = 0.1, g = 1/25)
  g * X_C - beta * X_A * (Y_C + Y_A)

dY_Adt <- function(X_A, Y_C, Y_A, beta = 0.1, g = 1/25, fai = 0.26, rho_A = 2.656, delta = 4)
  beta * X_A * (Y_C + Y_A) + g * (1 - fai) * Y_C - rho_A * Y_A[-delta]


x <- eode(dX_Cdt = dX_Cdt, dY_Cdt = dY_Cdt, dX_Adt = dX_Adt, dY_Adt = dY_Adt,
          constraint = c("X_C>=0","Y_C>=0","X_A>=0","Y_A>=0"))
print.eode(x)

# Try Parameters--------------------------------

#1. Original settings
x #Problem: mortality much larger than the reality
  #Solution: reduce 'rho_A' and 'rho_C'

plot.pc(pc_calculator(x, eode_proj(x, value0 = pp(list(X_C = 16, Y_C = 0, X_A = 72, Y_A = 1)),
                                   N = 1000, step = 0.01),
                      formula = "disease_death = (rho_C * Y_C[-delta] + rho_A * Y_A[-delta]) dt[0-t]"))


#2. Reduced mortality
x1 <- set_parameter(x, list(rho_A=2.656*0.05,rho_C=3.111*0.05))


plot.pc(pc_calculator(x1, eode_proj(x1, value0 = pp(list(X_C = 16, Y_C = 0, X_A = 72, Y_A = 1)),
                                   N = 1000, step = 0.01),
                      formula = "disease_death = (rho_C * Y_C[-delta] + rho_A * Y_A[-delta]) dt[0-t]"))

# Data-----------------------
training_data <- pdata(x, init = data.frame(X_C = c(3, 6, 1, 4, 16,0, 4, 8, 1, 0),
                                            Y_C = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                                            X_A = c(24,44,31,64,73,30,18,6, 5, 14),
                                            Y_A = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)),
                       t = c(4, 4, 4, 4, 4, 4, 4, 4, 4, 4),
                       lambda = data.frame(death_2021 = c(1, 2, 1, 1, 7, 0, 1, 6, 0, 0),
                                           death_2022 = c(0, 3, 1, 1, 4, 0, 1, 5, 0, 0)),
                       formula = c("death_2021 = (rho_C * Y_C + rho_A * Y_A + mu * X_C + mu * Y_C) dt[t-(t+1)]",
                                   "death_2022 = (rho_C * Y_C + rho_A * Y_A + mu * X_C + mu * Y_C) dt[(t+1)-(t+2)]"))
print.pdata(training_data)

# Training--------------------
## Grid search method
res1 <- eode_gridSearch(x, training_data, space = list(beta = seq(0.01, 0.1, 0.01),
                                                       rho_A = seq(0.06, 0.26,0.04),
                                                       rho_C = seq(0.06, 0.26,0.04),
                                                       delta = 3:4))
##Optimal Parameters: beta = 0.1, rhoA = 0.06, rhoC = 0.06, delta = 3
##Loss Function: 2933.027
## no significance in values of loss function found
## just satrt simulated annealing

## Simulated Annealing                                                                     #loss_f
sa <- eode_simuAnnealing(x1, pdat, paras = c("nu","q","beta","mu","fai","rho_C","rho_A"))  #87.40011
sa2 <- eode_simuAnnealing(x1, pdat, paras = c("nu","q","beta","mu","fai","rho_C","rho_A")) #87.58261
sa3 <- eode_simuAnnealing(x1, pdat, paras = c("nu","q","beta","mu","fai","rho_C","rho_A")) #87.82557
sa4 <- eode_simuAnnealing(x1, pdat, paras = c("nu","q","beta","mu","fai","rho_C","rho_A")) #87.40220
sa5 <- eode_simuAnnealing(x1, pdat, paras = c("nu","q","beta","mu","fai","rho_C","rho_A")) #87.40004 <- choose
sa6 <- eode_simuAnnealing(x1, pdat, paras = c("nu","q","beta","mu","fai","rho_C","rho_A")) #87.44438
sa7 <- eode_simuAnnealing(x1, pdat, paras = c("nu","q","beta","mu","fai","rho_C","rho_A")) #87.40206
sa8 <- eode_simuAnnealing(x1, pdat, paras = c("nu","q","beta","mu","fai","rho_C","rho_A")) #87.40176
sa9 <- eode_simuAnnealing(x1, pdat, paras = c("nu","q","beta","mu","fai","rho_C","rho_A")) #87.56930
sa10 <- eode_simuAnnealing(x1, pdat, paras = c("nu","q","beta","mu","fai","rho_C","rho_A"))#87.43033


# Show Results--------------------------------
x_sa <- set_parameter(x, as.list(sa5[length(sa5),]))
print(
  plot.pc(pc_calculator(x_sa, eode_proj(x_sa, value0 = pp(list(X_C = 16, Y_C = 0, X_A = 72, Y_A = 1)),
                                        N = 1000, step = 0.01),
                        formula = "disease_death = (rho_C * Y_C[-delta] + rho_A * Y_A[-delta]) dt[0-t]"))
)

print.eode(x_sa)
save.image("~/1_ecode/1_code/fitted-model.RData")

