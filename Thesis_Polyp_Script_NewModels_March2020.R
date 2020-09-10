#Cassiopea Polyp Data Stats-- New and Improved
#Making a new script to redo all of my models to include plate as a factor
#Created by Jennica Moffat March 24, 2020

#Clear the environment
rm(list=ls())
#load libraries
library(tidyverse)
library(car)
library(lme4)
library(lmerTest)
library(MASS)
library("gmodels")

#Load data
mydata<-read.csv("Data/Thesis_PolypData_Summary.csv")
mydata$Temp<-as.factor(mydata$Temp)
mydata$Plate<-as.factor(mydata$Plate)
NoApoData <- mydata %>%
  filter(Genotype != "Aposymbiotic") %>%
  droplevels

#Genotype (fixed) and Temperature (fixed) on dependent variables 

#Total average ephyra production####

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
#Model with all interactions is lowest.
#But use model with plate as random factor to account for it. 
Anova(sqrtEphyraModel, type="III")

#Time to strobilation####

#I need to remove the NA's
StrobilatedData <- NoApoData %>%
  filter(Days.to.Strobilation != "NA")

#Make model
Strob.model<-lmer(Days.to.Strobilation~Genotype*Temp+(1|Plate), data=StrobilatedData)
#Check assumptions
plot(Strob.model)
qqp(resid(Strob.model), "norm")
#Not normal

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
#No

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

#Boxcox
full.strob.model<-lm(Days.to.Strobilation ~ Genotype*Temp*Plate, data=StrobilatedData)
step.strob.model<-stepAIC(full.strob.model, direction="both", trace = F)

boxcox<-boxcox(step.strob.model,lambda = seq(-5, 5, 1/1000),plotit = TRUE )

Selected.Power<-boxcox$x[boxcox$y==max(boxcox$y)]
Selected.Power
#-0.352

StrobilatedData$Strob.trans<-(StrobilatedData$Days.to.Strobilation)^-0.352
trans.strob.model<-lm(Strob.trans~Genotype*Temp*Plate, data = StrobilatedData)
qqp(resid(trans.strob.model), "norm")
#I think that's close enough
Anova(trans.strob.model, type="III")
#error about aliased coefficients in the model?
trans.strob.model2<-lmer(Strob.trans~Genotype*Temp+(1|Plate), data=StrobilatedData)
Anova(trans.strob.model2, type="III")

#GLMER 
#lognormal distribution
qqp(StrobilatedData$Days.to.Strobilation, "lnorm")
#No

gamma<-fitdistr(StrobilatedData$Days.to.Strobilation, "gamma")
qqp(StrobilatedData$Days.to.Strobilation, "gamma", shape = gamma$estimate[[1]], rate=gamma$estimate[[2]])
#Not really

#Try negative binomial
nbinom <- fitdistr(StrobilatedData$Days.to.Strobilation, "Negative Binomial")
qqp(StrobilatedData$Days.to.Strobilation, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])
#Close.

#Try poisson
poisson <- fitdistr(StrobilatedData$Days.to.Strobilation, "Poisson")
qqp(StrobilatedData$Days.to.Strobilation, "pois", poisson$estimate, lambda=8)
#Probably worse than gamma?
hist(StrobilatedData$Days.to.Strobilation)

#I think negative binomial is the closest
Strob.model<-glmer(Days.to.Strobilation ~ Genotype*Temp+(1|Plate), data = StrobilatedData, family = binomial(link = "logit"))
strob.summary<-StrobilatedData%>%
  group_by(Genotype, Temp)%>%
  summarize(mean=mean(Days.to.Strobilation, na.rm=TRUE), SE=sd(Days.to.Strobilation, na.rm=TRUE)/sqrt(length(na.omit(Days.to.Strobilation))), sd=sd(Days.to.Strobilation))
strob.summary
#Having issues with this code. Says y-values need to be between 0 and 1?
#I will come back to this. 

#Why don't we try z-scores
StrobilatedData$Strob.z<-scale(StrobilatedData$Days.to.Strobilation, center = TRUE, scale = TRUE)
zstrob.model<-lmer(Strob.z~Genotype*Temp+(1|Plate), data=StrobilatedData)
#Check assumptions
qqp(resid(zstrob.model), "norm")



  
#Time to inoculation####
#I need to remove the NA's
InocData<-NoApoData%>%
  filter(!is.na(Days.to.Inoculation ))

#Make model
Inoc.model<-lmer(Days.to.Inoculation~Genotype*Temp+(1|Plate), data=InocData)
plot(Inoc.model)
#Not great, early days are clustered. 
qqp(resid(Inoc.model), "norm")
#Woof. Horrible. 

InocData$logIDays<-log(InocData$Days.to.Inoculation)
log.Inoc.model<-lmer(logIDays~Genotype*Temp+(1|Plate), data=InocData)
logInocmodelres<-resid(log.Inoc.model)
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

#Running it with the logged data for now
log.Inoc.model.full<-lm(logIDays~Genotype*Temp*Plate, data=InocData)
anova(log.Inoc.model.full)
#plate has an effect. Need to use original model
Anova(log.Inoc.model, type="III")
#interaction: P=0.007982
AIC(log.Inoc.model) #214

#Gotta do generalized linear model. 
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
#I think that's the best fit. 
hist(InocData$Days.to.Inoculation)

Inoc.model<-glmer(Days.to.Inoculation ~ Genotype*Temp+(1|Plate), data = InocData, family = poisson(link = "log"))
summary(Inoc.model)
Anova(Inoc.model, type="III")
print(summary(Inoc.model), correlation=TRUE)

Inoc.model.6<-glmer(Days.to.Inoculation ~ Genotype*Temp+(1|Plate), data = InocData.6, family = poisson(link = "log"))
Anova(Inoc.model.6, type = "III")

Inoc.model2<-glm(Days.to.Inoculation ~ Genotype*Temp*Plate, data = InocData, family = poisson(link = "log"))
Anova(Inoc.model2, type="III")
summary(Inoc.model2)

Inoc.model3<-glm(Days.to.Inoculation ~ Genotype*Temp, data = InocData, family = poisson(link = "log"))
Anova(Inoc.model3)

Inoc.model4<-glmer(Days.to.Inoculation ~ Genotype+Temp+(1|Plate), data = InocData, family = poisson(link = "log"))
Anova(Inoc.model4)

AIC(Inoc.model) #1601.8
AIC(Inoc.model2) #1592
AIC(Inoc.model3) #1601.7
AIC(Inoc.model4) #1600.9

#Quasipoisson
Inoc.model.QP<-glm(Days.to.Inoculation ~ Genotype*Temp*Plate, data = InocData, family = "quasipoisson")
Anova(Inoc.model.QP)
summary(Inoc.model.QP)
#Dispersion parameter is 0.97, so very close to 1. Since you can't use QP with glmer, poisson is just as good a fit. 

#Time to ephyra####
#I need to remove the NA's
TEData <- NoApoData %>%
  filter(Days.to.Ephyra != "NA")

#Make model
TEmodel<-lmer(Days.to.Ephyra~Genotype*Temp+(1|Plate), data=TEData)
#Check assumptions
plot(TEmodel)
qqp(resid(TEmodel), "norm")

#log transform
TEData$log.E.Days<-log(TEData$Days.to.Ephyra)
logTEmodel<-lmer(log.E.Days~Genotype*Temp+(1|Plate), data=TEData)
qqp(resid(logTEmodel), "norm")

#double log
TEData$loglog.E.Days<-log(TEData$log.E.Days)
loglogTEmodel<-lmer(loglog.E.Days~Genotype*Temp+(1|Plate), data=TEData)
qqp(resid(loglogTEmodel), "norm")
#Mehhhh
anova(loglogTEmodel)

#Square root
TEData$sqrtEDays<-sqrt(TEData$Days.to.Ephyra)
sqrtTEmodel<-lmer(sqrtEDays~Genotype*Temp+(1|Plate), data=TEData)
qqp(resid(sqrtTEmodel), "norm")
#No

#Fourth root
TEData$fourthrtEDays<-sqrt(TEData$sqrtEDays)
fourthrtTEmodel<-lmer(fourthrtEDays~Genotype*Temp+(1|Plate), data=TEData)
qqp(resid(fourthrtTEmodel), "norm")
#no

#Double log is best
#Model selection
loglogTEmodel<-lmer(loglog.E.Days~Genotype*Temp+(1|Plate), data=TEData)
TEphyra.model2<-lm(loglog.E.Days~Genotype*Temp*Plate, data=TEData)
TEphyra.model3<-lm(loglog.E.Days~Genotype*Temp, data=TEData)
TEphyra.model4<-lmer(loglog.E.Days~Genotype+Temp+(1|Plate), data=TEData)

AIC(loglogTEmodel) #-452
AIC(TEphyra.model2) #-547
AIC(TEphyra.model3) #-538
AIC(TEphyra.model4) #-494
#full model is best
Anova(TEphyra.model2)#Plate matters
Anova(loglogTEmodel, type="III")#Interaction is 0.08
Anova(TEphyra.model4, type="III")

#Boxcox
full.TE.model<-lm(Days.to.Ephyra ~ Genotype*Temp*Plate, data=TEData)
step.TE.model<-stepAIC(full.TE.model, direction="both", trace = F)

boxcox<-boxcox(step.TE.model,lambda = seq(-5, 5, 1/1000),plotit = TRUE )

Selected.Power<-boxcox$x[boxcox$y==max(boxcox$y)]
Selected.Power
#-0.409

TEData$TE.trans<-(TEData$Days.to.Ephyra)^-0.409
trans.TE.model<-lm(TE.trans~Genotype*Temp*Plate, data = TEData)
qqp(resid(trans.TE.model), "norm")
#I think that's close enough
plot(trans.TE.model)
Anova(trans.TE.model, type="III")
#Aliased coefficients error?
Anova(trans.TE.model) #Plate matters
trans.TE.model2<-lmer(TE.trans~Genotype*Temp+(1|Plate), data = TEData)
plot(trans.TE.model2)
qqp(resid(trans.TE.model2), "norm")
Anova(trans.TE.model2, Type="III")

trans.TE.model3<-lmer(TE.trans~Genotype+Temp+(1|Plate), data = TEData)
Anova(trans.TE.model3, type="III")

AIC(trans.TE.model2) #-920
AIC(trans.TE.model3) #-978


#GLMM
#lognormal distribution
qqp(TEData$Days.to.Ephyra, "lnorm")
#No

gamma<-fitdistr(TEData$Days.to.Ephyra, "gamma")
qqp(TEData$Days.to.Ephyra, "gamma", shape = gamma$estimate[[1]], rate=gamma$estimate[[2]])
#Not really

#Try negative binomial
nbinom <- fitdistr(TEData$Days.to.Ephyra, "Negative Binomial")
qqp(TEData$Days.to.Ephyra, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])
#no

#Try poisson
poisson <- fitdistr(TEData$Days.to.Ephyra, "Poisson")
qqp(TEData$Days.to.Ephyra, "pois", poisson$estimate, lambda=5)
#Probably closest
#But changes if I change lambda...
#5 fits the best
TE.model.poi<-glmer(Days.to.Ephyra ~ Genotype*Temp+(1|Plate), data = TEData, family = poisson(link = "log"))
summary(TE.model.poi)
Anova(TE.model.poi, type="III")

TE.model.poi2<-glm(Days.to.Ephyra ~ Genotype*Temp*Plate, data = TEData, family = poisson(link = "log"))
Anova(TE.model.poi2)

TE.model.poi3<-glm(Days.to.Ephyra ~ Genotype*Temp, data = TEData, family = poisson(link = "log"))
Anova(TE.model.poi3)

TE.model.poi4<-glm(Days.to.Ephyra ~ Genotype+Temp, data = TEData, family = poisson(link = "log"))
Anova(TE.model.poi4)

TE.model.poi5<-glmer(Days.to.Ephyra ~ Genotype+Temp+(1|Plate), data = TEData, family = poisson(link = "log"))
summary(TE.model.poi5)
Anova(TE.model.poi5, type="III")

AIC(TE.model.poi) #1328
AIC(TE.model.poi2) #1250
AIC(TE.model.poi3) #1330
AIC(TE.model.poi4) #1327
AIC(TE.model.poi5) #1324
#No significant effect of plate, no interaction between geno and temp.

#Why don't we try z-scores
TEData$TE.z<-scale(TEData$Days.to.Ephyra, center = TRUE, scale = TRUE)
TE.z.model<-lmer(TE.z~Genotype*Temp+(1|Plate), data=TEData)
#Check assumptions
qqp(resid(TE.z.model), "norm")
#That's exactly the same as non-transormed...Just on a different scale. 


#Bud production####
Budmodel<-lmer(Total.Buds~Genotype*Temp+(1|Plate), data=mydata)
plot(Budmodel)
qqp(resid(Budmodel), "norm")
#Yes, normal!!

Budmodel2<-lm(Total.Buds~Genotype*Temp, data=mydata)
Budmodel3<-lm(Total.Buds~Genotype*Temp+Plate, data=mydata)
Budmodel4<-lm(Total.Buds~Genotype*Temp*Plate, data=mydata)
Anova(Budmodel4, type="III")
#Plate actually has no effect on any other variables, for once. 

AIC(Budmodel) #1580
AIC(Budmodel2) #1567
AIC(Budmodel3)#1569
AIC(Budmodel4)#1631
#Without plate is best
Anova(Budmodel2, type="III")
#Interaction: P=0.002318


#Survival####
#To do log-linear analysis, I need a new dataframe
#Columns: Geno, Temp, Plate, Survived (yes or no), and Frequency (count of that combo)
Survival<- mydata%>%group_by(Genotype, Temp, Plate, Survive.to.End)%>%
  tally(Survive.to.End == "Yes")
#I have one NA for survial b/c I spilled it and lost it. Just gonna remove it. 
mydata2<-mydata%>%
  filter(Survive.to.End != "NA")

#Making dataframe to run chi-square test
mydata2_df <- mydata2 %>% modify_if(is.character, as.factor)
Survival<- mydata2_df%>%group_by(Genotype, Temp, Plate, Survive.to.End, .drop=FALSE)%>%
  tally(name="Freq")
#.drop=FALSE makes df keep tallies of 0's

#Renaming Survive.to.End to Survived because I'm dumb and made a very long column name
names(Survival)[names(Survival) == 'Survive.to.End'] <- 'Survived'

#Run model
Survival.model<-glm(Freq~Genotype:Survived + Temp:Survived + Plate:Survived + Genotype:Temp:Survived + Genotype:Plate:Survived + Temp:Plate:Survived + Genotype:Temp:Plate:Survived, family=poisson, data=Survival)
anova(Survival.model, test="Chisq")
summary(Survival.model)
#all of the interactions are 1 or very close to one. I think because they mostly all survived. 
#Genotype and Temp signficiantly affected survival, but no interaction. 

Survival.model2<-glm(Freq~Genotype:Survived + Temp:Survived + Genotype:Temp:Survived, family=poisson, data=Survival)
anova(Survival.model2, test="Chisq")
#removing plate gives me a more normal output. Geno*Temp P=0.94245
#But sig effect of geno and temp independently

Survival.model3<-glm(Freq~Temp:Survived + Genotype:Survived + Genotype:Temp:Survived, family=poisson, data=Survival)
anova(Survival.model3, test="Chisq")
#Well, order matters. Fun. 


#Different code for Chi-squared test
mod0 <- glm(Freq ~ Genotype + Temp + Plate + Survived, 
            data = Survival, family = poisson)
summary(mod0)

full.mod<-glm(Freq ~ Genotype*Temp*Plate*Survived, 
          data = Survival, family = poisson)
deviance(full.mod)
anova(full.mod, test="Chisq")
summary(full.mod)
#No sig effect of plate

mod2<-glm(Freq~Genotype*Temp*Survived, data=Survival, family=poisson)
anova(mod2, test="Chisq")
#No sig interaction of geno and temp on survival

mod3<-glm(Freq~(Genotype*Survived)+(Temp*Survived), data=Survival, family=poisson)
anova(mod3, test="Chisq")
#So genotype doesn't matter, just temp?

mod4<-glm(Freq~(Temp*Survived)+(Genotype*Survived), data=Survival, family=poisson)
anova(mod4,test="Chisq")
#Hooray, order doesn't matter now!
Anova(mod4, type="III")

