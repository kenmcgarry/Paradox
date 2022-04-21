#paradox_example3.R

library(Simpsons)
library(bnlearn)
library(ggdag)
library(ggthemes)
library(mosaic)
library(ggplot2)
library(scales)

setwd("C:/common_laptop/R-files/paradox")

set.seed(12345)  # reproducible

#load(url("http://www.bnlearn.com/bnrepository/sachs/sachs.rda"))
#sachs.fit <- bn; rm(bn)
#plot(sachs.fit)
#plot(bn.net(sachs.fit))

marks.dag <- model2network("[ALG][ANL|ALG][MECH|ALG:VECT][STAT|ALG:ANL][VECT|ALG]")
plot(marks.dag)

ALG.dist <- list(coef = c("(Intercept)" = 50.60), sd = 10.62)
ANL.dist <- list(coef = c("(Intercept)" = -3.57, ALG = 0.99), sd = 10.5)
MECH.dist <- list(coef = c("(Intercept)" = -12.36, ALG = 0.54, VECT = 0.46), sd = 13.97)
STAT.dist <- list(coef = c("(Intercept)" = -11.19, ALG = 0.76, ANL = 0.31), sd = 12.61)
VECT.dist <- list(coef = c("(Intercept)" = 12.41, ALG = 0.75), sd = 10.48)
ldist <- list(ALG = ALG.dist, ANL = ANL.dist, MECH = MECH.dist,STAT = STAT.dist, VECT = VECT.dist)
marks.fit <- custom.fit(marks.dag, ldist)
marks.fit[c("MECH", "STAT")]

# For conditional linear Gaussian models, consider the rats network, with a 
# discrete variable DRUG and SEX and real-valued WL1 and WL2 (weight loss in week one and week two):

rats.dag <- model2network("[SEX][DRUG|SEX][WL1|DRUG][WL2|WL1:DRUG]")
plot(rats.dag)

# For real-valued variables, we have a conditional regression for each combination of 
# discrete parents. For the discrete, we have CPTs.
SEX.lv <- c("M", "F")
DRUG.lv <- c("D1", "D2", "D3")
SEX.prob <- array(c(0.5, 0.5), dim = 2, dimnames = list(SEX = SEX.lv))
DRUG.prob <- array(c(0.3333, 0.3333, 0.3333, 0.3333, 0.3333, 0.3333),dim = c(3, 2), dimnames = list(DRUG = DRUG.lv, SEX = SEX.lv))
WL1.coef <- matrix(c(7, 7.50, 14.75), nrow = 1, ncol = 3,dimnames = list("(Intercept)", NULL))
WL1.dist <- list(coef = WL1.coef, sd = c(1.58, 0.447, 3.31))
WL2.coef <- matrix(c(1.02, 0.89, -1.68, 1.35, -1.83, 0.82), nrow = 2, ncol = 3,dimnames = list(c("(Intercept)", "WL1")))
WL2.dist <- list(coef = WL2.coef, sd = c(1.78, 2, 1.37))
ldist <- list(SEX = SEX.prob, DRUG = DRUG.prob, WL1 = WL1.dist, WL2 = WL2.dist)
rats.fit <- custom.fit(rats.dag, ldist)

# A column corresponds to a configuration of the discrete parents and a row to one 
# of the continuous parents.
rats.fit$WL2






