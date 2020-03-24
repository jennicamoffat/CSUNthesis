#Cassiopea Polyp Data Stats-- New and Improved
#Making a new script to redo all of my models to include plate as a factor
#Created by Jennica Moffat March 24, 2020

#Clear the environment
rm(list=ls())
#Load PnR data
mydata<-read.csv("Data/Thesis_PolypData_Summary.csv")
mydata$Temp<-as.factor(mydata$Temp)
mydata$Plate<-as.factor(mydata$Plate)
View(mydata)

#load libraries
library(tidyverse)
library(car)
library(lme4)
library(lmerTest)

#Genotype (fixed) and Temperature (fixed) on dependent variables 

#Total average ephyra production####
#Removing aposymbiotic
NoApoData <- subset(mydata, Genotype != "Aposymbiotic")
View(NoApoData)

#Make model
Ephyramodel<-lmer(Total.Ephyra.Produced~Genotype*Temp+(1|Plate), data=NoApoData)

#Check assumptions
plot(Ephyramodel)
qqp(resid(Ephyramodel), "norm")
#Not really normal

#Log data
NoApoData$logTotalEphyra<-log(NoApoData$Total.Ephyra.Produced+1)
logEphyraModel<-lmer(logTotalEphyra~Genotype*Temp+(1|Plate), data=NoApoData)
plot(logEphyraModel)
qqp(resid(logEphyraModel), "norm")
#didn't do anything

#Squart root
NoApoData$sqrtTotalEphyra<-sqrt(NoApoData$Total.Ephyra.Produced)
sqrtEphyraModel<-lmer(sqrtTotalEphyra~Genotype*Temp+(1|Plate), data=NoApoData)
plot(sqrtEphyraModel)
qqp(resid(sqrtEphyraModel), "norm")
#Beautiful

anova(sqrtEphyraModel)
#I think this is saying that, when taking into account plate, the interaction of Geno*Temp 
#is significant (P=0.0007557)




