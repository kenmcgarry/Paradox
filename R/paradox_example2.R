# paradox_example2.R
# https://www.r-bloggers.com/2022/01/simple-examples-to-understand-what-confounders-colliders-mediators-and-moderators-are-and-how-to-control-for-variables-in-r-with-regression-and-propensity-score-matching/
# Simple examples to understand what confounders, colliders, mediators, and moderators are 
# and how to "control for" variables in R with regression and propensity-score matching.

library(Simpsons)
library(bnlearn)
library(ggdag)
library(ggthemes)
library(mosaic)
library(ggplot2)
library(scales)
library(dagitty)
library(ggrepel)
library(dplyr)

setwd("C:/common_laptop/R-files/paradox")

set.seed(12345)
g <- dagify(Y ~ X,
            X ~ A1,
            A2 ~ X,
            Y ~ M,
            M ~ X,
            Col ~ X,
            Col ~ Y,
            Y ~ A3,
            Y ~ Con,
            X ~ Con,
            exposure = "X",
            outcome = "Y",
            coords = data.frame(x=c(5,1,1,1,3,3,5,3),y=c(1,1,2,0,2,0,0,1.5),
                        name=c("Y","X","A1","A2","M","Col","A3","Con")))
            
g %>% 
      ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
      geom_dag_point(col="grey90") +
      geom_dag_edges() +
      geom_dag_text(label=c("A1","A3","Con","M","X","Y","A2","Col"),col = "black") + 
      theme_dag() 

d <- data.frame("Party_preference" = rep(c(rep("Conservatives", 4), rep("Greens", 4)),2),
                "Place_of_residence" = c(rep("City",8), rep("Countryside",8)),
                "Car_usage" = c(22,33,26,19,18,31,28,23,58,23,12,7,52,34,6,8),
                "Frequency" = rep(c("Daily", "Weekly", "Less often", "Never"),4),
                "Weight" = c(.30,.30,.30,.30,.75,.75,.75,.75,.70,.70,.70,.70,.25,.25,.25,.25))

d$Frequency <- factor(d$Frequency, levels = unique(d$Frequency))
d$Car_usage_weighted <- d$Car_usage*d$Weight

ggplot(d, aes(x=Frequency,y=Car_usage_weighted,fill=Party_preference)) + 
  geom_col() + theme_minimal() + 
  scale_fill_manual(values=c("darkgrey","darkgreen")) +
  xlab("Frequency of car usage") + ylab("Share in %") +
  ggtitle("Frequency of car usage by party preference",subtitle = "") +
  facet_wrap(~Party_preference)

ggplot(d, aes(x=Frequency,y=Car_usage,fill=Party_preference)) + 
  geom_col() + theme_minimal() + 
  scale_fill_manual(values=c("darkgrey","darkgreen")) +
  xlab("Frequency of car usage") + ylab("Share in %") +
  ggtitle("Frequency of car usage by party preference and place of residence",subtitle = "") +
  facet_wrap(~Party_preference+Place_of_residence)

## colliders
set.seed(2022)
population <- data.frame(smoking = c(rep("smoker", 20000), rep("non_smoker", 80000)),
                         covid_hospitalisation = rbinom(100000,1,.005))
population$other_hospitalisation[population$smoking=="smoker"] <- rbinom(20000,1,.07)
population$other_hospitalisation[population$smoking=="non_smoker"] <- rbinom(80000,1,.05)
population$in_hospital <- population$covid_hospitalisation | population$other_hospitalisation

# Let's check that in the general population, both groups have an equal proportion of Covid hospitalisation:
t.test(covid_hospitalisation~smoking, population)

# Now let's limit the data to all hospital patients and compare Covid rates among smokers and non-smokers:
hospital <- population[which(population$in_hospital),]
t.test(covid_hospitalisation~smoking, hospital)


# Mediator variables (a.k.a. mechanisms)
# Let's assume that households with children have on average a lower household income as 
# opposed to childless couples. One of the obvious reasons for this correlation is given 
# by the fact that mothers or fathers often reduce their working hours to work part-time 
# (or take maternal/paternal leaves) while their children are still young. Working less of 
# course translates into a lower household income. Let's look at the consequences in another 
# (made-up) example dataset:

set.seed(2022)
x = c(rep(0,1000),rep(1,500),rep(2,600),rep(3,300),rep(4,100))
m = rexp(2500,.2) *(-1) + 40
f = c(rexp(1000,.2) *(-1) + 40,rnorm(500,31,9),rnorm(600,30,10),rnorm(300,28,7),rnorm(100,25,5))
e = f* rnorm(2500,100,25)
d = data.frame(x,m,f,e)
ggplot(d, aes(x=factor(x),y=e)) + geom_boxplot() + theme_minimal() + ylim(0,8000) +
  xlab("Number of children <6 year old") + ylab("Household income in Euro") +
  ggtitle("Monthly income by number of children") +
  geom_smooth(method='lm',aes(group=1)) 

# Now, let's factor in the working time, broadly distinguishing between full-time and part-time workers:
ggplot(d[d$f>=35,], aes(x=factor(x),y=e)) + geom_boxplot() + theme_minimal() + ylim(0,8000) +
  xlab("Number of children <6 year old") + ylab("Household income in Euro") +
  ggtitle("Monthly income by number of children",subtitle = "only full-time (35+ hours/week)") +
  geom_smooth(method='lm',aes(group=1)) 

ggplot(d[d$f<=20,], aes(x=factor(x),y=e)) + geom_boxplot() + theme_minimal() + ylim(0,4000) +
  xlab("Number of children <6 year old") + ylab("Household income in Euro") +
  ggtitle("Monthly income by number of children",subtitle = "only part-time (<= 50%)") +
  geom_smooth(method='lm',aes(group=1)) 

# Two things are to be noted about these two graphs: if you look at the scales 
# of the y axis, it is obvious that part-time workers earn less as opposed to full-time 
# workers. And, interestingly, the association between the number of children and the income 
# is not negative in neither of the two groups; if anything, it is positive, such that persons 
# with more children earn on average just as much (or even a bit more) as opposed to childless 
# persons - if they work the same amount of hours, that is. 

# Controlling for the number of working hours, having small children in the household does apparently 
# not lead to less earnings (in this made-up example). Rather, our variable "working hours" fully 
# explains why households with small children earn less compared with childless couples. 

# Importantly, this is a different case conceptually as opposed to the confounder example above 
# about car usage among Green party voters. We did not reveal a correlation to be actually spurious 
# here; rather, we found the reason for why X affects Y. But we have little doubt that X does in 
# fact cause Y. Because the reduced working hours can plausibly be traced back to the small children 
# in the household. As a DAG, this looks like the following:






            