#Cassiopea Polyp Data Stats-- New and Improved
#Making a new script to redo all of my models to include plate as a factor
#Created by Jennica Moffat March 24, 2020

#Clear the environment
rm(list=ls())
#Load data
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
summary(sqrtEphyraModel)
#This is saying that, when taking into account plate, the interaction of Geno*Temp 
#is significant (P=0.0007557)

#Model selection
Ephyra.model2<-lm(sqrtTotalEphyra~Genotype*Temp, data=NoApoData)
anova(Ephyra.model2)
#Interaction P=0.00096

Ephyra.model3<-lm(sqrtTotalEphyra~Genotype*Temp + Plate, data=NoApoData)
anova(Ephyra.model3)
Ephyra.model4<-lm(sqrtTotalEphyra~Genotype*Temp*Plate, data=NoApoData)
anova(Ephyra.model4)

AIC(sqrtEphyraModel) #395.55
AIC(Ephyra.model2) #348.4
AIC(Ephyra.model3) #343.1
AIC(Ephyra.model4) #342.8
#Model with all interactions is lowest...

#Time to strobilation####

#I need to remove the NA's
StrobilatedData <- subset(NoApoData, Days.to.Strobilation != "NA")
View(StrobilatedData)

#Make model
Strob.model<-lmer(Days.to.Strobilation~Genotype*Temp+(1|Plate), data=StrobilatedData)
#Check assumptions
plot(Strob.model)
qqp(resid(Strob.model), "norm")
#Not really normal

#Log data
StrobilatedData$logStrob.Days<-log(StrobilatedData$Days.to.Strobilation)
logStrobmodel<-lmer(logStrob.Days~Genotype*Temp+(1|Plate), data=StrobilatedData)
qqp(resid(logStrobmodel), "norm")
#Better, but not good enough. 

#Double log
StrobilatedData$loglogStrob.Days<-log(StrobilatedData$logStrob.Days)
loglogStrobmodel<-lmer(loglogStrob.Days~Genotype*Temp+(1|Plate), data=StrobilatedData)
qqp(resid(loglogStrobmodel), "norm")

#Triple log?
StrobilatedData$log3Strob.Days<-log(StrobilatedData$loglogStrob.Days)
log3Strobmodel<-lmer(log3Strob.Days~Genotype*Temp+(1|Plate), data=StrobilatedData)
qqp(resid(log3Strobmodel), "norm")
#Ugh it's so close.
plot(loglogStrobmodel)

#Square root
StrobilatedData$sqrtStrob.Days<-sqrt(StrobilatedData$Days.to.Strobilation)
sqrtStrobmodel<-lmer(sqrtStrob.Days~Genotype*Temp+(1|Plate), data=StrobilatedData)
qqp(resid(sqrtStrobmodel), "norm")

#Fourth root
StrobilatedData$fourthrt.Strob.Days<-sqrt(StrobilatedData$sqrtStrob.Days)
fourthrtStrobmodel<-lmer(fourthrt.Strob.Days~Genotype*Temp+(1|Plate), data=StrobilatedData)
qqp(resid(fourthrtStrobmodel), "norm")
#No different from square root


anova(loglogStrobmodel)
#Interaction: P=0.02743

#Model selection
Strob.model2<-lm(loglogStrob.Days~Genotype*Temp, data=StrobilatedData)
anova(Strob.model2)
#Interaction P=0.03549

Strob.model3<-lm(loglogStrob.Days~Genotype*Temp + Plate, data=StrobilatedData)
anova(Strob.model3)
Strob.model4<-lm(loglogStrob.Days~Genotype*Temp*Plate, data=StrobilatedData)
anova(Strob.model4)

AIC(loglogStrobmodel) #-348.2
AIC(Strob.model2) #-431.7
AIC(Strob.model3) #-433.4
AIC(Strob.model4) #-488.2
#Model with all interactions is lowest again. 


#Time to inoculation####
#I need to remove the NA's
InocData <- subset(NoApoData, Days.to.Inoculation != "NA")
View(InocData)

#Make model
Inoc.model<-lmer(Days.to.Inoculation~Genotype*Temp+(1|Plate), data=InocData)
plot(Inoc.model)
#Not great, early days are clustered. 
qqp(resid(Inoc.model), "norm")
#Woof. Horrible. 

InocData$logIDays<-log(InocData$Days.to.Inoculation)
log.Inoc.model<-lmer(logIDays~Genotype*Temp+(1|Plate), data=InocData)
logInocmodelres<-resid(logInocmodel)
qqp(resid(log.Inoc.model), "norm")
#Better, but not good enough. 

#double log
InocData$loglogIDays<-log(InocData$logIDays)
loglog.Inoc.model<-lmer(loglogIDays~Genotype*Temp+(1|Plate), data=InocData)
qqp(resid(loglog.Inoc.model), "norm")
#Same as log

#Square root
InocData$sqrtIDays<-sqrt(InocData$Days.to.Inoculation)
sqrtInocmodel<-lmer(sqrtIDays~Genotype*Temp+(1|Plate), data=InocData)
qqp(resid(sqrtInocmodel), "norm")
#Worse than log. 

#Quad root
InocData$quadrtIDays<-sqrt(InocData$sqrtIDays)
quadrtInocmodel<-lmer(quadrtIDays~Genotype*Temp+(1|Plate), data=InocData)
qqp(resid(quadrtInocmodel), "norm")
#Worse


#Gotta do generalized linear model. 
library(MASS)
#lognormal distribution
qqp(InocData$Days.to.Inoculation, "lnorm")
#Uh, no I don't think so. 

gamma<-fitdistr(InocData$Days.to.Inoculation, "gamma")
qqp(InocData$Days.to.Inoculation, "gamma", shape = gamma$estimate[[1]], rate=gamma$estimate[[2]])
#No

#Try negative binomial
nbinom <- fitdistr(InocData$Days.to.Inoculation, "Negative Binomial")
qqp(InocData$Days.to.Inoculation, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])
#no

#Try poisson
poisson <- fitdistr(InocData$Days.to.Inoculation, "Poisson")
qqp(InocData$Days.to.Inoculation, "pois", poisson$estimate, lambda=8)

Inoc.model<-glmer(Days.to.Inoculation ~ Genotype*Temp+(1|Plate), data = InocData, family = poisson(link = "log"))
summary(Inoc.model)
anova(Inoc.model)
#I have no idea what I'm doing...

Inoc.model2<-glmer(Days.to.Inoculation ~ Genotype*Temp+(1|Plate), data = InocData, family = poisson(link = "log"))
anova(Inoc.model2)
summary(Inoc.model2)

hist(InocData$Days.to.Inoculation)


#Time to ephyra####
#I need to remove the NA's
TimetoEphyraData <- subset(NoApoData, Days.to.Ephyra != "NA")
View(TimetoEphyraData)

#Make model
TEphyramodel<-lmer(Days.to.Ephyra~Genotype*Temp+(1|Plate), data=TimetoEphyraData)
#Check assumptions
plot(TEphyramodel)
qqp(resid(TEphyramodel), "norm")

#log transform
TimetoEphyraData$log.E.Days<-log(TimetoEphyraData$Days.to.Ephyra)
logTEphyramodel<-lmer(log.E.Days~Genotype*Temp+(1|Plate), data=TimetoEphyraData)
qqp(resid(logTEphyramodel), "norm")

#double log
TimetoEphyraData$loglog.E.Days<-log(TimetoEphyraData$log.E.Days)
loglogTEphyramodel<-lmer(loglog.E.Days~Genotype*Temp+(1|Plate), data=TimetoEphyraData)
qqp(resid(loglogTEphyramodel), "norm")
#Mehhhh close enough?
anova(loglogTEphyramodel)

#Model selection
TEphyra.model2<-lm(loglog.E.Days~Genotype*Temp, data=TimetoEphyraData)
TEphyra.model3<-lm(loglog.E.Days~Genotype*Temp + Plate, data=TimetoEphyraData)
TEphyra.model4<-lm(loglog.E.Days~Genotype*Temp*Plate, data=TimetoEphyraData)
anova(TEphyra.model4)

AIC(loglogTEphyramodel) #-452.34
AIC(TEphyra.model2) #-538.4.7
AIC(TEphyra.model3) #-546.5
AIC(TEphyra.model4) #-547.6

#Bud production####
Budmodel<-lmer(Total.Buds~Genotype*Temp+(1|Plate), data=mydata)
plot(Budmodel)
qqp(resid(Budmodel), "norm")
#Yes, normal!!

Budmodel2<-lm(Total.Buds~Genotype*Temp, data=mydata)
Budmodel3<-lm(Total.Buds~Genotype*Temp+Plate, data=mydata)
Budmodel4<-lm(Total.Buds~Genotype*Temp*Plate, data=mydata)
anova(Budmodel4)
#Plate actually has no effect on any other variables, for once. 

AIC(Budmodel) #1580
AIC(Budmodel2) #1567
#Without plate is best
anova(Budmodel2)
#Interaction: P=0.002318


#Survival####
#To do log-linear analysis, I need a new dataframe
#Columns: Geno, Temp, Plate, Survived (yes or no), and Frequency (count of that combo)
Survival<- mydata%>%group_by(Genotype, Temp, Plate, Survive.to.End)%>%
  tally(Survive.to.End == "Yes")
View(Survival)

#I have one NA for survial b/c I spilled it and lost it. Just gonna remove it. 
mydata2<-subset(mydata, Survive.to.End != "NA")

Survival<- mydata2%>%group_by(Genotype, Temp, Plate, Survive.to.End)%>%
  tally()
View(Survival)
Survival$Survived<-Survival$Survive.to.End
#The only thing missing is when there are zeroes. Dataframe just doesn't include it. 
#Let's see if I can still run the model with it as is. 

Survival.model<-glm(n~Genotype:Survived + Temp:Survived + Plate:Survived + Genotype:Temp:Survived + Genotype:Plate:Survived + Temp:Plate:Survived + Genotype:Temp:Plate:Survived, family=poisson, data=Survival)
anova(Survival.model, test="Chisq")
#all of the interactions are 1. Only effect is Genotype, I think because they mostly all survived. 
#I might just not use survival for CSUNposium
