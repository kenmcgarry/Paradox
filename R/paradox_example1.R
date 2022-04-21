# paradox_example1.R
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

setwd("C:/common_laptop/R-files/paradox")

alpha = 0.5

set.seed(123)
n = 1000

Satisfaction = rnorm(n)
Performance = rnorm(n) + Satisfaction * 0.1

Performance = rescale(Performance, to = c(0, 100))
# summary(Performance)
Satisfaction = rescale(Satisfaction, to = c(0, 7))
# summary(Satisfaction)

data <- data.frame(
  Performance,
  Satisfaction
)

options = c("Technical","Service")
technical = 
  (data$Performance > mean(data$Performance) & 
     data$Satisfaction > mean(data$Satisfaction)) |
  (data$Performance < mean(data$Performance) & 
     data$Satisfaction < mean(data$Satisfaction))

data$Job[technical] <- sample(options, sum(technical), T, c(0.6, 0.2))
data$Job[is.na(data$Job)] <- sample(options, sum(is.na(data$Job)), T, c(0.2, 0.8))

p <- data %>% ggplot(aes(Satisfaction, Performance)) 
p + geom_point(alpha = alpha) + geom_smooth(method = 'lm')

p +
  geom_point(aes(col = Job), alpha = alpha) + 
  geom_smooth(aes(col = Job), method = 'lm') +
  theme(legend.background = element_rect(fill = "transparent"),
        legend.justification = c(0, 1),
        legend.position = c(0, 1))

set.seed(123)
n = 1000

Education = rbinom(n, 2, 0.5)
Satisfaction = rnorm(n) + Education
Salary = Education * 2 + rnorm(n) - Satisfaction * 0.3

Salary = sample(10000:11000,1) + rescale(Salary, to = c(0, 100000))
# summary(Salary)
Satisfaction = rescale(Satisfaction, to = c(0, 7))
# summary(Satisfaction)
Education = factor(Education, labels = c("Low", "Medium", "High"))

data <- data.frame(
  Salary,
  Satisfaction,
  Education
)

p <- data %>% ggplot(aes(Satisfaction, Salary)) 
p + geom_point(alpha = alpha) + geom_smooth(method = 'lm')

p + 
  geom_point(aes(col = Education), alpha = alpha) + 
  geom_smooth(aes(col = Education), method = 'lm') +
  theme(legend.background = element_rect(fill = "transparent"),
        legend.justification = c(0, 1),
        legend.position = c(0, 1))


