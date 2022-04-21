# paradox_main.R
# Detects instances of Simpson's Paradox in datasets. It examines sub-populations in 
# the data, either user-defined or by means of cluster analysis, to test whether a regression 
# at the level of the group is in the opposite direction at the level of sub-populations.
# work commenced: 1/3/2022
# https://rpubs.com/lakenp/simpsonsparadox
# https://data-se.netlify.app/2020/04/16/simulation-berkson-s-paradox/
# https://www.r-bloggers.com/2022/01/simple-examples-to-understand-what-confounders-colliders-mediators-and-moderators-are-and-how-to-control-for-variables-in-r-with-regression-and-propensity-score-matching/
# https://www.r-bloggers.com/2016/01/how-to-create-confounders-with-regression-a-lesson-from-causal-inference/
# https://www.r-bloggers.com/2016/12/outlier-detection-and-treatment-with-r/

library(Simpsons)
library(bnlearn)
library(ggdag)
library(ggthemes)
library(mosaic)
library(ggplot2)
library(scales)
library(bootnet)
library(igraph)
library(dplyr)

setwd("C:/common_laptop/R-files/paradox")
source("paradox_berkson.R")

set.seed(42)  # reproducible
N <- 1e03
IQ = rnorm(N)
motivation = rnorm(N)
aptitude = 1/2 * IQ + 1/2 * motivation + rnorm(N, 0, .1)

df = data.frame(
  IQ = IQ,
  Motivation = motivation,
  Aptitude = aptitude,
  Student_bin = ifelse(aptitude > 0, "Hard worker", "Slacker"))

# correlation between IQ and Apt for hard workers
df1 <- 
df %>%
  filter(Student_bin == "Hard worker")
cor(df1$IQ,df1$Aptitude)

# correlation between IQ and Apt for slackers
df2 <- 
  df %>%
  filter(Student_bin == "Slacker")
cor(df2$IQ,df2$Aptitude)

# correlataion between IQ and Apt for general population
cor(df$IQ,df$Aptitude)

mosaicCore::tally(~ Student_bin, data = df)

gf_point(IQ ~ Motivation, data = df %>% 
           filter(Student_bin == "Hard worker")) %>% 
           gf_refine(scale_colour_colorblind())  %>% 
           gf_lm()

gf_point(IQ ~ aptitude, data = df) %>% 
  gf_refine(scale_colour_colorblind())  %>% 
  gf_lm()

gf_point(IQ ~ aptitude, color = ~Student_bin, data = df) %>% 
  gf_refine(scale_colour_colorblind())  %>% 
  gf_refine(theme(legend.position = c(0.87, 0.25))) %>% 
  gf_labs(color = "") %>% 
  gf_lm()

detach(name = "package:mosaic", unload = TRUE)
dag1 <- ggdag::dagify(S ~ A + IQ,
                      outcome = "S",
                      labels = c("S" = "Student",
                                 "IQ" = "Intelligence",
                                 "A" = "Success"))

dag1_p <- ggdag(dag1, use_labels = "label")  + theme_dag_blank()
dag1_p

#source("simpsons_functions.R")
#source("simpsons_build_BN.R")


#if (!requireNamespace("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
# BiocManager::install("Rgraphviz")



