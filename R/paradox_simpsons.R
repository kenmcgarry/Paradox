# paradox_simpsons.R
# Detects instances of Simpson's Paradox in datasets. It examines sub-populations in the data, either 
# user-defined or by means of cluster analysis, to test whether a regression # at the level of the group 
# is in the opposite direction at the level of sub-populations.
# Kievit, R.A., Frankenhuis, W. & Borsboom, D. (2013). Simpson's Paradox in Psychological Science: 
# A Practical Guide. Frontiers in Psychology, 4, 513, 1-14.

# work commenced: 16/04/2022
# https://rpubs.com/lakenp/simpsonsparadox

library(Simpsons)
library(dplyr)
library(ggplot2)
library(hrbrthemes)

setwd("C:/common_laptop/R-files/paradox")

#Example 1. Here, we want to estimate the relationship between 'Coffee' 
#and 'Neuroticism', taking into account possible gender differences. 
#As we have measured gender, we supply this information using the #'clusterid' command. 
#This means that the function runs the analysis both for 
#the dataset as a whole and within the two subgroups. 
#It then checks whether the subgroups deviate significantly 
#from the regression at the level of the group.

#Simulating 100 males 
coffeem <- rnorm(100,100,15)
neuroticismm <- (coffeem*.8)+rnorm(100,15,8)
clusterid <- rep(1,100)
males <- cbind(coffeem,neuroticismm,clusterid)

#Simulating 100 females
coffeef <- rnorm(100,100,15)
neuroticismf <- 160+((coffeef*-.8)+rnorm(100,15,8))
clusterid <- rep(2,100)
females <- cbind(coffeef,neuroticismf,clusterid)

data1 <- data.frame(rbind(males,females))
colnames(data1) <- c("Coffee","Neuroticism","gender")

#'normal' data analysis: Plot & regression
plot(data1[,1:2])
a <- lm(data1[,1]~data1[,2])
abline(a)
summary(a) #A normal regression shows no effect

# Running a Simpsons Paradox analysis, using gender as known clustering variable
# Analyze the relationship between coffee and neuroticism for both males and females. 

example1 <- Simpsons(Coffee,Neuroticism,clusterid=gender, data=data1) 

data1$gender <- as.factor(data1$gender)
levels(data1$gender) <- c("Male", "Female")

# NOT using gender
ggplot(data=data1, aes(x = Coffee, y = Neuroticism))  +
  geom_point(size=2) +
  theme_ipsum() +
  xlab("Coffee Intake") +
  ylab("Neuroticism") +
  geom_smooth(method=lm, se=FALSE) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(legend.text = element_text(size = 12)) 
 

# using gender
  ggplot(data=data1, aes(x = Coffee, y = Neuroticism, color = gender))  +
  geom_point(size=2) +
  theme_ipsum() +
  xlab("Coffee Intake") +
  ylab("Neuroticism")  +
  geom_smooth(method=lm, se=FALSE) +
    theme(axis.title.x = element_text(size = 20)) +
    theme(axis.title.y = element_text(size = 20)) +
    theme(legend.text = element_text(size = 15)) +
    theme(legend.title = element_text(size = 15))     
  
  #geom_hline(yintercept=mean(surp), linetype='dotted', col = 'black',size=2) 
  #annotate("text", x = 100, y = mean(surp), label = "Bayesian 'Wow' Level", vjust = -0.5, size=6)

  #example 2. Here we estimate the relationship between 'Coffee' and 'Neuroticism'. 
  #As opposed to example 1, we have not measured any possible clustering #identifiers 
  #such as gender, so we want to estimate whether there is evidence for #clustering based 
  #only on the data we measured: Coffee and Neuroticism.
  
  #generating data 
  Coffee1=rnorm(100,100,15)
  Neuroticism1=(Coffee1*.8)+rnorm(100,15,8)
  g1=cbind(Coffee1, Neuroticism1)
  Coffee2=rnorm(100,170,15)
  Neuroticism2=(300-(Coffee2*.8)+rnorm(100,15,8))
  g2=cbind(Coffee2, Neuroticism2)
  Coffee3=rnorm(100,140,15)
  Neuroticism3=(200-(Coffee3*.8)+rnorm(100,15,8))
  g3=cbind(Coffee3, Neuroticism3)
  data2=data.frame(rbind(g1,g2,g3))
  colnames(data2) <- c("Coffee","Neuroticism")
  
  #'normal' data analysis: Plot & regression
  plot(data2)
  b=lm(data2[,1]~data2[,2]) 
  summary(b)
  abline(b)
  
  # Running the analysis tool identifies three clusters, and warns that the relationship 
  # between alcohol and coffee is in the opposite direction in two of the subclusters.
  example2=Simpsons(Coffee,Neuroticism,data=data1) 
  example2
  
  #In this final example, we want again want to analyse the relationship
  # between 'Alcohol' and 'Mood'. However, this time 
  #we have reason to believe that responses to a questionnaire 
  #will fall into clusters of response types. Therefore, we want to
  # estimate the clusters in the data on the basis of a different set
  # of variables. In this case, we have simulate three types of responses
  # to a questionnaire of nine questions, with continuous responses 
  #ranging between 1 and 7. We then first estimate the clusters on 
  #the basis of the questionnaire, and then examine the relationship 
  #between 'Alcohol' and 'Mood' based on these detected clusters.
  
  #group 1
  signal=matrix(rnorm(300,7,1),100,3)
  noise=matrix(rnorm(600,3.5,1),100,6)
  g1=cbind(signal,noise)
  
  #group 2
  signal=matrix(rnorm(300,1,1),100,3)
  noise=matrix(rnorm(600,3.5,1),100,6)
  g2=cbind(noise, signal)
  
  #group 3
  signal=matrix(rnorm(300,7,1),100,3)
  noise1=matrix(rnorm(300,3.5,1),100,3)
  noise2=matrix(rnorm(300,3.5,1),100,3)
  g3=cbind(noise1,signal,noise2)
  
  questionnaire=rbind(g1,g2,g3)
  colnames(questionnaire)=c('q1','q2','q3','q4','q5','q6','q7','q8','q9')
  
  Alc1=rnorm(100,10,8)
  Mood1=(Alc1*.4)+rnorm(100,3,4)
  A=cbind(Alc1, Mood1)
  Alc2=rnorm(100,15,8)
  Mood2=(Alc2*-.4)+rnorm(100,3,4)
  B=cbind(Alc2,Mood2)
  Alc3=rnorm(100,20,8)
  Mood3=(Alc3*.8)+rnorm(100,3,4)
  C=cbind(Alc3,Mood3)
  data=data.frame(rbind(A,B,C))
  colnames(data) <- c("Alcohol","Mood")
  alldata=cbind(questionnaire,data)
  alldata=as.data.frame(alldata)
  
  #Run Simpsons Paradox detection algorithm, clustering on the basis of the questionnaire
  example3=Simpsons(Alcohol,Mood,clustervars=c("q1","q2",'q3','q4',
                                               'q5','q6','q7','q8','q9'),data=alldata)
  example3 
  
  
  