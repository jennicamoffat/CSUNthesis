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
library("lmtest")
library(MASS)
library("gmodels")
library(emmeans)

#Load data
mydata<-read.csv("Data/Thesis_PolypData_Summary.csv")
mydata$Temp<-as.factor(mydata$Temp)
mydata$Plate<-as.factor(mydata$Plate)
NoApoData <- mydata %>%
  filter(Genotype != "Aposymbiotic") %>%
  droplevels
#Adding columns for each developmental stage to see percentage that actually reached that stage in 28 days
Developed.data<-NoApoData%>%
  mutate(Inoc = ifelse(is.na(Days.to.Inoculation), "0", "1"), #0=event did not occur, 1=event occurred
         Strob=ifelse(is.na(Days.to.Strobilation), "0", "1"),
         Ephyra=ifelse(is.na(Days.to.Ephyra), "0", "1"))

#Genotype (fixed) and Temperature (fixed) on dependent variables 

#Time to inoculation####
#Start with log-linear analysis

#Poisson #WRONG STATS - UPDATED 1/30/22 TO USE BINOMIAL DISTRIBUTION, SEE "Inoc Binomial" BELOW AND IGNORE THIS SECTION
#Columns: Geno, Temp, Plate, inoc (yes or no), and Frequency (count of that combo)
Developed.data.df <- Developed.data %>% modify_if(is.character, as.factor)
InocFreq<- Developed.data.df%>%group_by(Genotype, Temp, Plate, Inoc, .drop=FALSE)%>%
  tally(name="Count")
#.drop=FALSE makes df keep tallies of 0's
#Different code for Chi-squared test
InocFreq<-  mutate(InocFreq, Freq=(Count/6))

full.mod<-glm(Count ~ Genotype*Temp*Plate*Inoc, 
              data = InocFreq, family = poisson())
deviance(full.mod)

Anova(full.mod, type="III")
#Plate does not matter 

mod1<-glm(Freq ~ Genotype*Temp*Inoc, 
          data = InocFreq, family = poisson)
Anova(mod1, type="III")

lrtest(mod1, full.mod)
#Definitely use the simpler model without plate. 

#Inoc Binomial####
full.mod<-glm(Inoc ~ Genotype*Temp*Plate, 
              data = Developed.data.df, family = binomial)
mod1<-glm(Inoc ~ Genotype*Temp, 
          data = Developed.data.df, family = binomial)
Anova(full.mod, type="III") #plate not significant
Anova(mod1, type="III")

lrtest(mod1, full.mod) #p=0.36, use simpler model


#General linear model with those that did inoculate####
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
#Pretty close. 

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

#Running it with the logged data
log.Inoc.model.full<-lm(logIDays~Genotype*Temp*Plate, data=InocData)
Anova(log.Inoc.model.full, type="III")
#Plate has no effect
log.Inoc.model2<-lm(logIDays~Genotype*Temp, data=InocData)

lrtest(log.Inoc.model2, log.Inoc.model.full)
#Sig = use full model, even though plate has no effect. 

AIC(log.Inoc.model) #214
AIC(log.Inoc.model.full) #125
AIC(log.Inoc.model2) #162

#Using lmer model to include plate but still be able to determine GxT interaction
Anova(log.Inoc.model, type="III")

#emmeans
emm1 = emmeans(log.Inoc.model, specs= pairwise~Genotype:Temp)
emm1$emmeans
#emmean is mean from model (so logged data)
#SE are calculated from the model as well

#Time to inoc mean
inoc.summary<-mydata%>%
  group_by(Genotype, Temp) %>%
  summarize(mean=mean(Days.to.Inoculation, na.rm=TRUE), SE=sd(Days.to.Inoculation, na.rm=TRUE)/sqrt(length(na.omit(Days.to.Inoculation))))

inoc.tukey<-emmeans(log.Inoc.model, pairwise ~ Temp | Genotype)
inoc.tukey$contrasts

#Graph to visualize means across temp by genotype
emmeans.inoc<-emmip(log.Inoc.model, Genotype~Temp)
emmeans.inoc

#Time to strobilation####

#Start with log-linear analysis
#Poisson - WRONG STATS, UPDATED 1/30/22 TO USE BINOMIAL, SEE BELOW####
#Columns: Geno, Temp, Plate, inoc (yes or no), and Frequency (count of that combo)
Developed.data.df <- Developed.data %>% modify_if(is.character, as.factor)
StrobFreq<- Developed.data.df%>%group_by(Genotype, Temp, Plate, Strob, .drop=FALSE)%>%
  tally(name="Freq")
#.drop=FALSE makes df keep tallies of 0's
#Different code for Chi-squared test

full.mod<-glm(Freq ~ Genotype*Temp*Plate*Strob, 
              data = StrobFreq, family = poisson)
deviance(full.mod)
Anova(full.mod, type="III")
#Plate is 0.051 with interaction with Temp

mod1<-glm(Freq ~ Genotype*Temp*Strob, 
          data = StrobFreq, family = poisson)
Anova(mod1, type="III")

lrtest(mod1, full.mod)
#I can use the simpler model without plate.

#Strob binomial####
full.mod<-glm(Strob ~ Genotype*Temp*Plate, 
              data = Developed.data.df, family = binomial)
mod1<-glm(Strob ~ Genotype*Temp, 
          data = Developed.data.df, family = binomial)
Anova(full.mod, type="III") #plate not significant
Anova(mod1, type="III")

lrtest(mod1, full.mod) #p=0.009, use full model
Developed.data.df$y_pred = predict(full.mod, Developed.data.df, type="response")

#Linear model strob####
#I need to remove the NA's
StrobilatedData <- NoApoData %>%
  filter(Days.to.Strobilation != "NA")
StrobilatedData<-StrobilatedData%>%
  mutate_if(is.character, as.factor)

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

Anova(loglogStrobmodel, type="III")
#Interaction: P=0.02743

#Model selection
Strob.mod.full<-lm(loglogStrob.Days~Genotype*Temp*Plate, data=StrobilatedData)
Strob.model2<-lmer(loglogStrob.Days~Genotype*Temp + (1|Plate), data=StrobilatedData)
Strob.model3<-lm(loglogStrob.Days~Genotype*Temp, data=StrobilatedData)

Anova(Strob.mod.full, type="III")
Anova(Strob.model2, type="III")
Anova(Strob.model3, type="III")

AIC(Strob.mod.full) #-448
AIC(Strob.model2) #-348
AIC(Strob.model3) #-431
#Model with all interactions is lowest, but can't even run because of aliased coefficients. 

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
#error about aliased coefficients in the model. It's because some plates only had one well strobilate. 
trans.strob.model2<-lmer(Strob.trans~Genotype*Temp+(1|Plate), data=StrobilatedData)
Anova(trans.strob.model2, type="III")

#mean time to strobilation
Summary.strob <- mydata %>%
  group_by(Genotype, Temp) %>%
  summarize(mean=mean(Days.to.Strobilation, na.rm=TRUE), SE=sd(Days.to.Strobilation, na.rm=TRUE)/sqrt(length(na.omit(Days.to.Strobilation))))

#tukey
strob.tukey<-emmeans(trans.strob.model2, pairwise ~ Temp | Genotype)
strob.tukey$contrasts

#Graph to visualize means across temp by genotype
emmeans.strob<-emmip(trans.strob.model2, Genotype~Temp)
emmeans.strob

#Time to ephyra####
#Start with log-linear analysis

#Poisson - WRONG STATS, UPDATED 1/30/22 TO USE BINOMIAL. SEE BELOW. ####
#Columns: Geno, Temp, Plate, inoc (yes or no), and Frequency (count of that combo)
Developed.data.df <- Developed.data %>% modify_if(is.character, as.factor)
EphyraFreq<- Developed.data.df%>%group_by(Genotype, Temp, Plate, Ephyra, .drop=FALSE)%>%
  tally(name="Freq")
#.drop=FALSE makes df keep tallies of 0's
full.mod<-glm(Freq ~ Genotype*Temp*Plate*Ephyra, 
              data = EphyraFreq, family = poisson)
deviance(full.mod)
Anova(full.mod, type="III")
#Plate is 0.06 with interaction with Temp

mod1<-glm(Freq ~ Genotype*Temp*Ephyra, 
          data = EphyraFreq, family = poisson)
Anova(mod1, type="III")

lrtest(mod1, full.mod)
#I can use the simpler model without plate.

#Produced ephyra binomial
full.mod<-glm(Ephyra ~ Genotype*Temp*Plate, 
              data = Developed.data.df, family = binomial)
mod1<-glm(Ephyra ~ Genotype*Temp, 
          data = Developed.data.df, family = binomial)
Anova(full.mod, type="III") #plate not significant, same warning as strob
Anova(mod1, type="III")

lrtest(mod1, full.mod) #p=0.013, use full model
Developed.data.df$y_pred = predict(full.mod, Developed.data.df, type="response")


#General linear model time to ephyra####
#I need to remove the NA's
TEData <- NoApoData %>%
  filter(Days.to.Ephyra != "NA")
TEData<-TEData%>%
  mutate_if(is.character, as.factor)

#Make model
TEmodel<-lm(Days.to.Ephyra~Genotype*Temp*Plate, data=TEData)
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
#Better than any other transformations
plot(trans.TE.model)
#Leverage of one = groups with only one rep

trans.TE.model2<-lmer(TE.trans~Genotype*Temp+(1|Plate), data = TEData)
qqp(resid(trans.TE.model2), "norm")
Anova(trans.TE.model2, Type="III")

trans.TE.model3<-lmer(TE.trans~Genotype+Temp+(1|Plate), data = TEData)
Anova(trans.TE.model3, type="III")

AIC(trans.TE.model2) #-920
AIC(trans.TE.model3) #-978
lrtest(trans.TE.model3, trans.TE.model2)
#Sig LRT test = use bigger model

#Mean days to ephyra
Summary.days.ephyra <- mydata %>%
  group_by(Genotype, Temp) %>%
  summarize(mean=mean(Days.to.Ephyra, na.rm=TRUE), SE=sd(Days.to.Ephyra, na.rm=TRUE)/sqrt(length(na.omit(Days.to.Ephyra))))
#Tukey
TE.tukey<-emmeans(trans.TE.model2, pairwise ~ Temp | Genotype)
TE.tukey$contrasts
#Graph to visualize means across temp by genotype
emmeans.TE<-emmip(trans.TE.model2, Genotype~Temp)
emmeans.TE

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

summary(sqrtEphyraModel)
#This is saying that, when taking into account plate, the interaction of Geno*Temp 
#is significant (P=0.0007557)

#Model selection
Ephyra.model2<-lm(sqrtTotalEphyra~Genotype*Temp, data=NoApoData)
Ephyra.model3<-lm(sqrtTotalEphyra~Genotype*Temp + Plate, data=NoApoData)
Ephyra.model4<-lm(sqrtTotalEphyra~Genotype*Temp*Plate, data=NoApoData)

AIC(sqrtEphyraModel) #395.55
AIC(Ephyra.model2) #348.4
AIC(Ephyra.model3) #343.1
AIC(Ephyra.model4) #342.8
#Model with all interactions is lowest.
#But use model with plate as random factor to account for it. 
Anova(sqrtEphyraModel, type="III") #Inherently doing LRT comparing model without factor and with

summary(sqrtEphyraModel)

#Ephyra mean
ephyra.summary<-mydata%>%
  group_by(Genotype, Temp) %>%
  summarize(mean=mean(Total.Ephyra.Produced, na.rm=TRUE), SE=sd(Total.Ephyra.Produced, na.rm=TRUE)/sqrt(length(na.omit(Total.Ephyra.Produced))))
emm.GP = emmeans(sqrtEphyraModel, specs= pairwise~Genotype:Temp)
emm.GP$emmeans

#Tukey
Ephyra.tukey<-emmeans(sqrtEphyraModel, pairwise ~ Temp | Genotype)
Ephyra.tukey$contrasts
#Graph to visualize means across temp by genotype
emmeans.ephyra<-emmip(sqrtEphyraModel, Genotype~Temp)
emmeans.ephyra

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

Summary.buds <- mydata %>%
  group_by(Genotype, Temp) %>%
  summarize(mean=mean(Total.Buds, na.rm=TRUE), SE=sd(Total.Buds, na.rm=TRUE)/sqrt(length(na.omit(Total.Buds))))

#Tukey
#Tukey
Bud.tukey<-emmeans(Budmodel2, pairwise ~ Temp | Genotype)
Bud.tukey$contrasts
#Graph to visualize means across temp by genotype
emmeans.buds<-emmip(Budmodel2, Genotype~Temp)
emmeans.buds

#Without Apo
Budmodel2.full<-lm(Total.Buds~Genotype*Temp*Plate, data=NoApoData)
plot(Budmodel2.full)
qqp(resid(Budmodel2.full), "norm")
#Normal

Budmodel2.mixed<-lmer(Total.Buds~Genotype*Temp+(1|Plate), data=NoApoData)
Budmodel2.simp<-lm(Total.Buds~Genotype*Temp, data=NoApoData)
Budmodel2.supersimp<-lm(Total.Buds~Genotype+Temp, data=NoApoData)
Anova(Budmodel2.full, type="III") #No effect of plate. 
Anova(Budmodel2.simp, type="III") #G*T P=0.0059

lrtest(Budmodel2.simp, Budmodel2.full)
lrtest(Budmodel2.supersimp,Budmodel2.simp)
#LRT says to use Budmodel2.simp
#Still a sig interaction, even without Apo

#Survival####
#WRONG STATS - UPDATED 1/30/22 TO USE BINOMIAL - To do log-linear analysis, I need a new dataframe####
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

mod0 <- glm(Freq ~ Genotype + Temp + Plate + Survived, 
            data = Survival, family = poisson)
summary(mod0)

full.mod<-glm(Freq ~ Genotype*Temp*Plate*Survived, 
          data = Survival, family = poisson)
deviance(full.mod)
Anova(full.mod, type="III")
summary(full.mod)
#No sig effect of plate, or anything.

mod2<-glm(Freq~Genotype*Temp*Survived, data=Survival, family=poisson)
Anova(mod2, type="III")
#Nothing significant

mod3<-glm(Freq~(Genotype*Survived)+(Temp*Survived), data=Survival, family=poisson)
Anova(mod3, type="III")
#So now temp sig affects survival, but not geno

mod4<-glm(Freq~Temp*Survived, data=Survival, family=poisson)
Anova(mod4, type="III")

lrtest(mod3, mod2)
#mod3 better than mod2
lrtest(mod4, mod3)
#mod4 better than mod3

#RIGHT STATS - Survival binomial####
#I have one NA for survial b/c I spilled it and lost it. Just gonna remove it. 
mydata2<-mydata%>%
  filter(Survive.to.End != "NA")
mydata2$Survive.to.End<-as.factor(mydata2$Survive.to.End)
Survive.data<-mutate(mydata2, Survived = ifelse(Survive.to.End=="No", "0", "1"))#0=event did not occur, 1=event occurred
Survive.data$Survived<-as.factor(Survive.data$Survived)

full.mod<-glm(Survived ~ Genotype*Temp*Plate, 
              data = Survive.data, family = binomial)
mod1<-glm(Survived ~ Genotype*Temp, 
          data = Survive.data, family = binomial)
Anova(full.mod, type="III") #plate not significant
Anova(mod1, type="III")

lrtest(mod1, full.mod) #p=0.8, use simple model

#Survived to end or only died after producing an ephyra
mydata3<-mydata%>%
  filter(Survive.to.Ephyra != "NA")
mydata3$Survive.to.Ephyra<-as.factor(mydata2$Survive.to.Ephyra)
Survive.ephyra.data<-mutate(mydata3, Survived.ephyra = ifelse(Survive.to.Ephyra=="No", "0", "1"))#0=event did not occur, 1=event occurred
Survive.ephyra.data$Survived.ephyra<-as.factor(Survive.ephyra.data$Survived.ephyra)

full.mod<-glm(Survived.ephyra ~ Genotype*Temp*Plate, 
              data = Survive.ephyra.data, family = binomial)
mod1<-glm(Survived.ephyra ~ Genotype*Temp, 
          data = Survive.ephyra.data, family = binomial)
Anova(full.mod, type="III") #plate not significant. nothing significant
Anova(mod1, type="III") #Nothing significant

lrtest(mod1, full.mod) #p=0.998, use simple model

summary(Survive.ephyra.data$Survive.to.Ephyra)
#10 polyps "prematurely" died (before the end without producing an ephyra)
summary(Survive.data$Survive.to.End)
#23 polyps died before the end of the 28 days, but 13 of those died after producing an ephyra, which is normal

#Trying other things####
fit.1 <- glm(Inoc == "yes" ~ Genotype+Temp, data = Developed.data, family = binomial)
summary(fit.1)
summary(Developed.data)

#Correlations
library(hrbrthemes)

ggplot(NoApoData, aes(x=Days.to.Ephyra, y=Total.Buds, color=Genotype, shape=Temp)) + 
  geom_point(size=2, alpha=3)+
  theme_minimal()

ggplot(NoApoData, aes(x=Total.Buds, y=Total.Ephyra.Produced, color=Genotype)) + 
  geom_point(size=3, position=position_jitterdodge(jitter.width=0.05), alpha=3)+
  theme_minimal()
#For Spearman's rho
BudEphyracorr.spear<-cor.test(NoApoData$Total.Buds, NoApoData$Total.Ephyra.Produced, method="spearman", na.rm=TRUE)
BudEphyracorr.spear
#Pearson's
BudEphyracorr.pear<-cor.test(NoApoData$Total.Buds, NoApoData$Total.Ephyra.Produced, method="pearson", na.rm=TRUE)
BudEphyracorr.pear
#Kendall tau
BudEphyracorr.ken<-cor.test(NoApoData$Total.Buds, NoApoData$Total.Ephyra.Produced, method="kendall", na.rm=TRUE)
BudEphyracorr.ken
summary(BudEphyracorr.ken)
summary(BudEphyracorr.pear)
