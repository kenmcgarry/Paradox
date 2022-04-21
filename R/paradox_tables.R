# paradox_main.R
# Detects instances of Simpson's Paradox in datasets. It examines sub-populations in 
# the data, either user-defined or by means of cluster analysis, to test whether a regression 
# at the level of the group is in the opposite direction at the level of sub-populations.
# work commenced: 1/3/2022
# https://rpubs.com/lakenp/simpsonsparadox

library(Simpsons)
library(bnlearn)
library(ggdag)
library(ggthemes)
library(mosaic)
library(ggplot2)
library(scales)
library(tidyverse)

setwd("C:/common_laptop/R-files/paradox")
set.seed(42)  # reproducible


# Load Pearl's drug effects data
pearl <- read.csv(file="Drugs.csv", header=TRUE, stringsAsFactors=FALSE)

# Combined

  pearl %>%
    count(Drug,Recovered)  
  
drugsummary <- 
pearl %>%
  group_by(Drug,Recovered) %>%
  summarise(dcount = n()) %>%
  mutate(prop = dcount / sum(dcount))

recoveryrate <-
drugsummary %>% filter(Recovered == 1 && Drug == 1)
      



#if (!requireNamespace("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
# BiocManager::install("Rgraphviz")



