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

library(ecode)
library(stringr)
library(ggplot2)

rm(list = ls())

#model-------------------
dS_Ssapdt <- function(S_Ssap, S_Sadu, I_Ssap, I_Sadu,  #equation 1
                       S_IRadu, I_IRsap, I_IRadu, 
                       S_Radu, I_Rsap, I_Radu, 
                       v=0.712, q=0.35, g=0.04, mu=0.643, beta=0.0174,
                       pS_S=0.73, pIR_S = 0.64, pR_S = 0.6)
  v * (pS_S * S_Sadu + pIR_S * (S_IRadu + I_IRadu) + pR_S * (S_Radu + I_Radu)) + 
  v * (1 - q) * pS_S * I_Sadu - (mu + g) * S_Ssap -
  beta * S_Ssap * (I_Ssap + I_Sadu + I_IRsap + I_IRadu + I_Rsap + I_Radu)

dI_Ssapdt <- function(S_Ssap, I_Ssap, I_Sadu,  #equation 2
                       I_IRsap, I_IRadu, 
                       I_Rsap, I_Radu,
                       g=0.04, mu=0.643, beta=0.0174, fai_S = 0.26, delta = 4, rho_Ssap = 0.0877)
  beta * S_Ssap * (I_Ssap + I_Sadu + I_IRsap + I_IRadu + I_Rsap + I_Radu) -
  (mu + g * (1 - fai_S)) * I_Ssap - rho_Ssap * I_Ssap[-delta]

dS_Sadudt <- function(S_Ssap, S_Sadu, I_Ssap, I_Sadu,  #equation 3
                       I_IRsap, I_IRadu, 
                       I_Rsap, I_Radu,
                       g=0.04, beta=0.0174)
  g * S_Ssap - beta * S_Sadu * (I_Ssap + I_Sadu + I_IRsap + I_IRadu + I_Rsap + I_Radu)

dI_Sadudt <- function(S_Sadu, I_Ssap, I_Sadu,  #equation 4
                       I_IRsap, I_IRadu, 
                       I_Rsap, I_Radu,
                       g=0.04, beta=0.0174, delta=4, fai_S=0.26,rho_Sadu=0.0495, remove_rate = 0.05)
  beta * S_Sadu * (I_Ssap + I_Sadu + I_IRsap + I_IRadu + I_Rsap + I_Radu) +
  g * (1 - fai_S) * I_Ssap - rho_Sadu * I_Sadu[-delta] - remove_rate * I_Sadu
  
  
dS_IRsapdt <- function(S_Sadu, I_Ssap, I_Sadu,  #equation 5
                        S_IRsap, S_IRadu, I_IRsap, I_IRadu, 
                        S_Radu, I_Rsap, I_Radu, 
                        v=0.712, q=0.35, g=0.04, mu=0.643, beta=0.0174,
                        pS_IR=0.19, pIR_IR = 0.23, pR_IR = 0.25)
  v * (pS_IR * S_Sadu + pIR_IR * (S_IRadu + I_IRadu) + pR_IR * (S_Radu + I_Radu)) + 
  v * (1 - q) * pS_IR * I_Sadu - (mu + g) * S_IRsap -
  beta * S_IRsap * (I_Ssap + I_Sadu + I_IRsap + I_IRadu + I_Rsap + I_Radu)

dI_IRsapdt <- function(S_IRsap, I_Ssap, I_Sadu,  #equation 6
                        I_IRsap, I_IRadu, 
                        I_Rsap, I_Radu,
                        v=0.712, g=0.04, beta = 0.0174, mu=0.643, fai_IR=0.104, rho_IRsap=0.03508, delta = 4)
  beta * S_IRsap * (I_Ssap + I_Sadu + I_IRsap + I_IRadu + I_Rsap + I_Radu) -
  (mu + g * (1 - fai_IR)) * I_IRsap - rho_IRsap * I_IRsap[-delta]

dS_IRadudt <- function(I_Ssap, I_Sadu,  #equation 7
                        S_IRsap, S_IRadu, I_IRsap, I_IRadu, 
                        I_Rsap, I_Radu,
                        g = 0.04, beta = 0.0174)
  g * S_IRsap - beta * S_IRadu * (I_Ssap + I_Sadu + I_IRsap + I_IRadu + I_Rsap + I_Radu)

dI_IRadudt <- function(I_Ssap, I_Sadu,  #equation 8
                        S_IRadu, I_IRsap, I_IRadu, 
                        I_Rsap, I_Radu,
                        g = 0.04, beta = 0.0174, fai_IR=0.104, delta = 4, rho_IRadu = 0.0198)
  beta * S_IRadu * (I_Ssap + I_Sadu + I_IRsap + I_IRadu + I_Rsap + I_Radu) +
  g * (1 - fai_IR) * I_IRsap - rho_IRadu *I_IRadu[-delta]

dS_Rsapdt <- function(S_Sadu, S_IRadu, S_Radu, S_Rsap, I_Ssap, I_Sadu,  #equation 9
                       I_IRsap, I_IRadu, 
                       I_Rsap, I_Radu,
                       v=0.712, q=0.35, g = 0.04, mu=0.643, beta = 0.0174,
                       pS_R = 0.08, pIR_R = 0.13, pR_R = 0.15)
  v * (pS_R * S_Sadu + pIR_R * (S_IRadu + I_IRadu) + pR_R * (S_Radu + I_Radu)) +
  v * (1 - q) * pS_R * I_Sadu - (mu + g) * S_Rsap -
  beta * S_Rsap * (I_Ssap + I_Sadu + I_IRsap + I_IRadu + I_Rsap + I_Radu) 

dI_Rsapdt <- function(S_Rsap, I_Ssap, I_Sadu,  #equation 10
                       I_IRsap, I_IRadu, 
                       I_Rsap, I_Radu,
                       g = 0.04, mu=0.643, beta = 0.0174)
  beta * S_Rsap * (I_Ssap + I_Sadu + I_IRsap + I_IRadu + I_Rsap + I_Radu) - (mu + g) * I_IRsap
  
dS_Radudt <- function(S_Rsap, S_Radu, I_Ssap, I_Sadu,  #equation 11
                       I_IRsap, I_IRadu, 
                       I_Rsap, I_Radu,
                       g = 0.04, beta = 0.0174)
  g * S_Rsap - beta * S_Radu * (I_Ssap + I_Sadu + I_IRsap + I_IRadu + I_Rsap + I_Radu)
  
dI_Radudt <- function(S_Radu, I_Ssap, I_Sadu,  #equation 12
                       I_IRsap, I_IRadu, 
                       I_Rsap, I_Radu,
                       beta = 0.0174, g = 0.04)
  beta * S_Radu * (I_Ssap + I_Sadu + I_IRsap + I_IRadu + I_Rsap + I_Radu) + g * I_IRsap


x <- eode(dS_Ssapdt = dS_Ssapdt, dI_Ssapdt = dI_Ssapdt, dS_Sadudt = dS_Sadudt, dI_Sadudt = dI_Sadudt,
          dS_IRsapdt = dS_IRsapdt, dI_IRsapdt = dI_IRsapdt, dS_IRadudt = dS_IRadudt, dI_IRadudt = dI_IRadudt,
          dS_Rsapdt = dS_Rsapdt, dI_Rsapdt = dI_Rsapdt, dS_Radudt = dS_Radudt, dI_Radudt = dI_Radudt,
          constraint = c("S_Ssap>=0","S_Sadu>=0","I_Ssap>=0","I_Sadu>=0",
                         "S_IRsap>=0","S_IRadu>=0","I_IRsap>=0","I_IRadu>=0",
                         "S_Rsap>=0","S_Radu>=0","I_Rsap>=0","I_Radu>=0"))

x

#simulation---------------------
population_decline <- data.frame(
  resistance_ratio = c(),
  value = c(),
  period_label = c()
)
ratio_increased <- data.frame(
  resistance_ratio = c(),
  value = c(),
  period_label = c()
)

for(resistance_ratio in seq(0.05, 0.4, 0.05)){
  cat("resistance_ratio =", resistance_ratio)
  N_R <- round(268 * resistance_ratio)
  N_S <- 268 - round(268 * resistance_ratio)
  
  proj <- eode_proj(x, value0 = pp(list(
    S_Ssap = round(N_S * 0.069), I_Ssap = 0, S_Sadu = N_S - round(N_S * 0.069) - 1, I_Sadu = 1,
    S_IRsap = 0, I_IRsap = 0, S_IRadu = 0, I_IRadu = 0,
    S_Rsap = round(N_R * 0.069), I_Rsap = 0, S_Radu = N_R - round(N_R * 0.069), I_Radu = 0
  )), N = 2000, step = 0.01)
  
  N4 <- with(proj, S_Ssap[401]+I_Ssap[401]+S_Sadu[401]+I_Sadu[401]+
               S_IRsap[401]+I_IRsap[401]+S_IRadu[401]+I_IRadu[401]+
               S_Rsap[401]+I_Rsap[401]+S_Radu[401]+I_Radu[401])
  N15 <- with(proj, S_Ssap[1501]+I_Ssap[1501]+S_Sadu[1501]+I_Sadu[1501]+
               S_IRsap[1501]+I_IRsap[1501]+S_IRadu[1501]+I_IRadu[1501]+
               S_Rsap[1501]+I_Rsap[1501]+S_Radu[1501]+I_Radu[1501])
  N20 <- with(proj, S_Ssap[2001]+I_Ssap[2001]+S_Sadu[2001]+I_Sadu[2001]+
               S_IRsap[2001]+I_IRsap[2001]+S_IRadu[2001]+I_IRadu[2001]+
               S_Rsap[2001]+I_Rsap[2001]+S_Radu[2001]+I_Radu[2001])
  
  R10 <- (N4-N15)/N4
  R15 <- (N4-N20)/N4
  
  Ratio4 <- with(proj, S_Rsap[401]+I_Rsap[401]+S_Radu[401]+I_Radu[401])/N4
  Ratio15 <- with(proj, S_Rsap[1501]+I_Rsap[1501]+S_Radu[1501]+I_Radu[1501])/N15
  Ratio20 <- with(proj, S_Rsap[2001]+I_Rsap[2001]+S_Radu[2001]+I_Radu[2001])/N20
  
  Increase10 <- Ratio15 - Ratio4
  Increase15 <- Ratio20 - Ratio4
  
  population_decline <- rbind(population_decline, data.frame(
    resistance_ratio = rep(resistance_ratio,2),
    value = c(R10, R15),
    period_label = c("2021–2030","2021–2035")
  ))
  ratio_increased <- rbind(ratio_increased, data.frame(
    resistance_ratio = rep(resistance_ratio, 2),
    value = c(Increase10, Increase15),
    period_label = c("2021–2030","2021–2035")
  ))
}

#plot---------------
theme1 <- theme_bw()+
  theme(axis.text.x=element_text(size=16,angle=0,colour="black"),
        axis.text.y=element_text(size=16,angle=0,colour="black"),
        axis.title=element_text(size=18),
        axis.line=element_line(linetype=1,color="black",linewidth = 0.1),
        axis.ticks = element_line(colour="black"),
        panel.grid.major = element_blank(), #change the major and minor grid lines,
        panel.grid.minor = element_blank(), #if want to change, check this parameters, I think it's easier to dao that
        #strip.background = element_rect(colour = "black",size = 0.8),
        #panel.background = element_rect(colour="black", fill="white"),
        #panel.border = element_blank(),  
        panel.border = element_rect(colour = "black",fill=NA,linewidth = 1.2),
        plot.title=element_text(size=14,angle=0,colour="black", face = "italic"),
        plot.tag=element_text(size=14,angle=0,colour="black", face = "bold"),
        plot.caption=element_text(size=14,angle=0,colour="black",face = "italic"),
        axis.title.y=element_text(vjust=1.9),
        axis.title.x=element_text(vjust=0.5),
        legend.text=element_text(colour="black",size=14),
        legend.background= element_rect(fill = "transparent", color = NA),
        #legend.position = "bottom",
        legend.title = element_blank())


cols <- c("2021–2030"=rgb(240/255,131/255,131/255),"2021–2035"=rgb(242/255,70/255,70/255))
ggplot(population_decline, aes(x=resistance_ratio, y=value, color=period_label))+geom_line(size=0.7)+theme1+
  scale_colour_manual(values = cols) +xlab("Initial resistant ratio") + ylab("Population decline")

ggplot(ratio_increased, aes(x=resistance_ratio, y=value, color=period_label))+geom_line(size=0.7)+theme1+
  scale_colour_manual(values = cols) +xlab("Initial resistant ratio") + ylab("Ratio decreased")

#result--------------
"
> population_decline
resistance_ratio       value period_label
1              0.05  0.16647495    2021–2030
2              0.05  0.23089330    2021–2035
3              0.10  0.13357160    2021–2030
4              0.10  0.18322815    2021–2035
5              0.15  0.10364720    2021–2030
6              0.15  0.13988699    2021–2035
7              0.20  0.07173328    2021–2030
8              0.20  0.09366231    2021–2035
9              0.25  0.04269720    2021–2030
10             0.25  0.05161133    2021–2035
11             0.30  0.01407115    2021–2030
12             0.30  0.01015441    2021–2035
13             0.35 -0.01788892    2021–2030
14             0.35 -0.03615664    2021–2035
15             0.40 -0.04556684    2021–2030
16             0.40 -0.07624067    2021–2035


> ratio_increased
resistance_ratio      value period_label
1              0.05 0.04929695    2021–2030
2              0.05 0.07550750    2021–2035
3              0.10 0.05274689    2021–2030
4              0.10 0.07945150    2021–2035
5              0.15 0.05374832    2021–2030
6              0.15 0.07976645    2021–2035
7              0.20 0.05280532    2021–2030
8              0.20 0.07719147    2021–2035
9              0.25 0.05031366    2021–2030
10             0.25 0.07260081    2021–2035
11             0.30 0.04647289    2021–2030
12             0.30 0.06625439    2021–2035
13             0.35 0.04071047    2021–2030
14             0.35 0.05730423    2021–2035
15             0.40 0.03455806    2021–2030
16             0.40 0.04814660    2021–2035


"

