#Symbiodinium microadriaticum growth curves
#Created by Jennica Moffat August, 2019

#Clear the environment
rm(list=ls())
#Load June growth data
mydata<-read.csv("JunePart1.csv")
View(mydata)

#load libraries
library(tidyverse)
library(RColorBrewer)
library(ggpmisc)

#Renaming Date column because it transfered weird
names(mydata)[1]<-"Date"
View(mydata)

#Very bad plot of all growth measurements over time
ggplot(data=mydata,aes(x = Date, y= Densityx10000))+geom_line()+geom_point()
#Looks like it starts to flatten at about 6/12/2019
#Need to clean up data to have "days" instead of "dates"

#Clear the environment
rm(list=ls())
#Load July growth data
mydata<-read.csv("JulyGrowthPart1_r.csv")
View(mydata)

#Very bad plot of all growth measurements over time
ggplot(data=mydata,aes(x = Day, y=Density.10.4))+geom_point()


#Making temp a factor rather than numeric
mydata$Temp<-as.factor(mydata$Temp)

#Growth curves
CCMP2458Growth<-ggplot(subset(mydata, Genotype == "CCMP2458"), aes(x=Day, y=Densityx10000, group=Temp, color=Temp))+
  geom_point()+
  theme_classic()+
  ggtitle("Growth Curve CCMP2458")+
  geom_smooth()
CCMP2458Growth


CCMP2464Growth<-ggplot(subset(mydata, Genotype == "CCMP2464"), aes(x=Day, y=Densityx10000, group=Temp, color=Temp))+
  geom_point()+
  theme_classic()+
  ggtitle("Growth Curve CCMP2464")+
  geom_smooth()
CCMP2464Growth


FLCassGrowth<-ggplot(subset(mydata, Genotype == "FLCass"), aes(x=Day, y=Densityx10000, group=Temp, color=Temp))+
  geom_point()+
  theme_classic()+
  ggtitle("Growth Curve FLCass")+
  geom_smooth()
FLCassGrowth

RT362Growth<-ggplot(subset(mydata, Genotype == "RT362"), aes(x=Day, y=Densityx10000, group=Temp, color=Temp))+
  geom_point()+
  theme_classic()+
  ggtitle("Growth Curve RT362")+
  geom_smooth()
RT362Growth


KB8Growth<-ggplot(subset(mydata, Genotype == "KB8"), aes(x=Day, y=Densityx10000, group=Temp, color=Temp))+
  geom_point()+
  theme_classic()+
  ggtitle("Growth Curve KB8")+
  geom_smooth()
KB8Growth


#######################################################################################
#Now going through with combined data from trial 1 and trial 2
#Clear the environment
rm(list=ls())
#Load June growth data
mydata<-read.csv("GrowthDataCombined_r.csv")
View(mydata)

#load libraries
library(tidyverse)
library(RColorBrewer)

#Renaming Date column because it transferred weird
names(mydata)[1]<-"Date"
View(mydata)

#Making temp a factor rather than numeric
mydata$Temp<-as.factor(mydata$Temp)

#First I want to compare between rounds, just to see how it looks

#Growth curves
CCMP2458Growth<-ggplot(subset(mydata, Genotype == "CCMP2458"), aes(x=Day, y=Densityx10000, color=Temp, shape=Round))+
  geom_point()+
  theme_classic()+
  ggtitle("Growth Curve CCMP2458")+
  geom_smooth()
CCMP2458Growth
#Worked, but hard to tell. Gonna make separate graphs for each temp

CCMP2458Growth26<-ggplot(subset(mydata, Genotype == "CCMP2458" & Temp=="26"), aes(x=Day, y=Densityx10000, color=Round))+
geom_point()+
  theme_classic()+
  ggtitle("Growth Curve CCMP2458 - 26*")+
  geom_smooth()
CCMP2458Growth26

CCMP2458Growth30<-ggplot(subset(mydata, Genotype == "CCMP2458" & Temp=="30"), aes(x=Day, y=Densityx10000, color=Round))+
  geom_point()+
  theme_classic()+
  ggtitle("Growth Curve CCMP2458 - 30*")+
  geom_smooth()
CCMP2458Growth30

CCMP2458Growth32<-ggplot(subset(mydata, Genotype == "CCMP2458" & Temp=="32"), aes(x=Day, y=Densityx10000, color=Round))+
  geom_point()+
  theme_classic()+
  ggtitle("Growth Curve CCMP2458 - 32*")+
  geom_smooth()
CCMP2458Growth32


#CCMP2464, not really worth doing but whatever
CCMP2464Growth<-ggplot(subset(mydata, Genotype == "CCMP2464"), aes(x=Day, y=Densityx10000, color=Temp, shape=Round))+
  geom_point()+
  theme_classic()+
  ggtitle("Growth Curve CCMP2464")+
  geom_smooth()
CCMP2464Growth
#So not helpful. Moving on. 


#FLCass
FLCassGrowth<-ggplot(subset(mydata, Genotype == "FLCass"), aes(x=Day, y=Densityx10000, color=Temp, shape=Round))+
  geom_point()+
  theme_classic()+
  ggtitle("Growth Curve FLCass")+
  geom_smooth()
FLCassGrowth
#Worked, but hard to tell. Although looks a little better than CCMP2458.

FLCassGrowth26<-ggplot(subset(mydata, Genotype == "FLCass" & Temp=="26"), aes(x=Day, y=Densityx10000, color=Round))+
  geom_point()+
  theme_classic()+
  ggtitle("Growth Curve FLCass - 26*")+
  geom_smooth()
FLCassGrowth26

FLCassGrowth30<-ggplot(subset(mydata, Genotype == "FLCass" & Temp=="30"), aes(x=Day, y=Densityx10000, color=Round))+
  geom_point()+
  theme_classic()+
  ggtitle("Growth Curve FLCass - 30*")+
  geom_smooth()
FLCassGrowth30

FLCassGrowth32<-ggplot(subset(mydata, Genotype == "FLCass" & Temp=="32"), aes(x=Day, y=Densityx10000, color=Round))+
  geom_point()+
  theme_classic()+
  ggtitle("Growth Curve FLCass - 32*")+
  geom_smooth()
FLCassGrowth32

#RT362
#Combined
RT362Growth<-ggplot(subset(mydata, Genotype == "RT362"), aes(x=Day, y=Densityx10000, color=Temp, shape=Round))+
  geom_point()+
  theme_classic()+
  ggtitle("Growth Curve RT362")+
  geom_smooth()
RT362Growth


RT362Growth26<-ggplot(subset(mydata, Genotype == "RT362" & Temp=="26"), aes(x=Day, y=Densityx10000, color=Round))+
  geom_point()+
  theme_classic()+
  ggtitle("Growth Curve RT362 - 26*")+
  geom_smooth()
RT362Growth26

RT362Growth30<-ggplot(subset(mydata, Genotype == "RT362" & Temp=="30"), aes(x=Day, y=Densityx10000, color=Round))+
  geom_point()+
  theme_classic()+
  ggtitle("Growth Curve RT362 - 30*")+
  geom_smooth()
RT362Growth30

RT362Growth32<-ggplot(subset(mydata, Genotype == "RT362" & Temp=="32"), aes(x=Day, y=Densityx10000, color=Round))+
  geom_point()+
  theme_classic()+
  ggtitle("Growth Curve RT362 - 32*")+
  geom_smooth()
RT362Growth32


#KB8
#Combined
KB8Growth<-ggplot(subset(mydata, Genotype == "KB8"), aes(x=Day, y=Densityx10000, color=Temp, shape=Round))+
  geom_point()+
  theme_classic()+
  ggtitle("Growth Curve KB8")+
  geom_smooth()
KB8Growth
#Uh-oh. 26 sits between the two 32's...

KB8Growth26<-ggplot(subset(mydata, Genotype == "KB8" & Temp=="26"), aes(x=Day, y=Densityx10000, color=Round))+
  geom_point()+
  theme_classic()+
  ggtitle("Growth Curve KB8 - 26*")+
  geom_smooth()
KB8Growth26

KB8Growth30<-ggplot(subset(mydata, Genotype == "KB8" & Temp=="30"), aes(x=Day, y=Densityx10000, color=Round))+
  geom_point()+
  theme_classic()+
  ggtitle("Growth Curve KB8 - 30*")+
  geom_smooth()
KB8Growth30

KB8Growth32<-ggplot(subset(mydata, Genotype == "KB8" & Temp=="32"), aes(x=Day, y=Densityx10000, color=Round))+
  geom_point()+
  theme_classic()+
  ggtitle("Growth Curve KB8 - 32*")+
  geom_smooth()
KB8Growth32


###########################################################################
#Calculating growth rates using library("growthrates")
#Outlined here: https://cran.r-project.org/web/packages/growthrates/vignettes/Introduction.html
library(growthrates)

splitted.data <- multisplit(mydata, c("Temp", "Genotype", "Flask")) 
#Splits data by temp, genotype, and flask so we work with one replicate of one treatement at a time

dat <- splitted.data[[1]] #First subset of data is named "dat" CCMP2458, 26*, flask 1
View(dat)

fit <- fit_easylinear(dat$Day, dat$Densityx10000)
#Can't do this for combined data, because I have repeats of days. For now will just do with June data. 

#Clear the environment
rm(list=ls())
#Load June growth data
mydata<-read.csv("JunePart1.csv")
View(mydata)

splitted.data <- multisplit(mydata, c("Temp", "Genotype", "Flask")) 
#Splits data by temp, genotype, and flask so we work with one replicate of one treatement at a time

dat <- splitted.data[[1]] #First subset of data is named "dat" CCMP2458, 26*, flask 1
View(dat)

fit <- fit_easylinear(dat$Day, dat$Densityx10000)
#This method fits segments of linear models to the log-transformed data and tries to find the maximum growth rate.
#Im not sure how it knows to log transform it, might just be part of the code.

summary(fit)  #To inspect the outcome of this model fit
#Im not sure what any of it means.

coef(fit)     #exponential growth parameters
rsquared(fit)  # coefficient of determination (of log-transformed data)
deviance(fit)  # residual sum of squares of log-transformed data
#I think this fits an exponential curve to the highest growth rate 

#plot data
par(mfrow = c(1, 2))
plot(fit, log = "y")
plot(fit)

#Fit nonlinear parametric growth model (ie: logistic growth)

#Vector of start parameters and intial values of growth model
p<-c(y0=0.1,mumax=0.2,K=170)
# unconstraied fitting
fit1 <- fit_growthmodel(FUN = grow_logistic, p = p, dat$Day, dat$Densityx10000)
coef(fit1)
summary(fit1)

plot(fit1, log="y")
#I have no idea what I'm doing....


#####Let's try something else that might make sense
fit1 <- fit_spline(dat$Day, dat$Densityx10000)
plot(fit1, log="y")
plot(fit1)

## derive start parameters from spline fit
p <- coef(fit1)

## use parameters from spline fit and take K from the data maximum
p <- c(coef(fit1), K = max(dat$Densityx10000))
fit2 <- fit_growthmodel(grow_logistic, p=p, time=dat$Day, y=dat$Densityx10000, transform="log")
plot(fit1)
lines(fit2, col="green")
#???
#Idk what this is telling me or why there are two different logistic curves. 


#############################################################
#Per Capita Growth curves
#Clear the environment
rm(list=ls())
#Load June growth data
mydata<-read.csv("GrowthDataCombined_r.csv")
View(mydata)
mydata$Temp<-as.factor(mydata$Temp)

KB8Growth32<-ggplot(subset(mydata, Genotype == "KB8" & Temp=="32"), aes(x=Densityx10000, y=PerCapitaGrowthRate, color=Round))+
  geom_point()+
  theme_classic()+
  ggtitle("Per Capita Growth Curve KB8 - 32*")+
  geom_quantile(formula=y~x)+
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)
KB8Growth32


