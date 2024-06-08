library(ecode)
library(stringr)

#model-------------------
dX_Cdt <- function(X_C, Y_C, X_A, Y_A, nu = 0.712, q = 0.35, beta = 0.174, mu = 0.643, g = 1/25)
  nu * X_A + nu * (1 - q) * Y_A - beta * X_C * (Y_C + Y_A) - (mu + g) * X_C

dY_Cdt <- function(X_C, Y_C, Y_A, beta = 0.174, mu = 0.643, fai = 0.26 , g = 1/25, rho_C = 0.0877, delta = 4)
  beta * X_C * (Y_C + Y_A) - (mu + g * (1 - fai)) * Y_C - rho_C * Y_C[-delta]

dX_Adt <- function(X_C, Y_C, X_A, Y_A, beta = 0.174, g = 1/25)
  g * X_C - beta * X_A * (Y_C + Y_A)

dY_Adt <- function(X_A, Y_C, Y_A, beta = 0.174, g = 1/25, fai = 0.26, rho_A = 0.0495, delta = 4)
  beta * X_A * (Y_C + Y_A) + g * (1 - fai) * Y_C - rho_A * Y_A[-delta]


x <- eode(dX_Cdt = dX_Cdt, dY_Cdt = dY_Cdt, dX_Adt = dX_Adt, dY_Adt = dY_Adt,
          constraint = c("X_C>=0","Y_C>=0","X_A>=0","Y_A>=0"))
x

#data---------------------
dat <- read.csv("validation_data.csv")
identifier <- 3  ## 1/2/3
use_dat <- dat[dat$identifier==identifier,]
use_dat$Time <- use_dat$observe_year - use_dat$detect_year

#update parameter-------------
AMR <- use_dat$AMR[1]
x <- eode_set_parameter(x, list(rho_A = AMR, rho_C = 0.0877/0.0495 * AMR))


# Projection--------------------------
proj <- eode_proj(x, value0 = pp(list(X_C = 18, Y_C = 0, X_A = 249, Y_A = 1)),
                  N = 2300, step = 0.01)

plot(proj)

get_CMR <- function(Time, proj){
  max_size <- proj$X_C[proj$t==400] +
    proj$X_A[proj$t==400] +
    proj$Y_C[proj$t==400] +
    proj$Y_A[proj$t==400]
  
  current_size <- proj$X_C[proj$t==100*Time] +
    proj$X_A[proj$t==100*Time] +
    proj$Y_C[proj$t==100*Time] +
    proj$Y_A[proj$t==100*Time]
  
  current_size/max_size*100 #(%)
}

compare <- data.frame(
  Time = rep(use_dat$Time, 2),
  CMR =  c(
    100-as.numeric(lapply(use_dat$Time, get_CMR, proj)),
    use_dat$CMR
  ),
  label = c(rep("modelled", nrow(use_dat)), rep("observed", nrow(use_dat)))
)


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

ggplot(compare, aes(x=Time, y=CMR, color=label))+geom_line(size =1)+theme1

