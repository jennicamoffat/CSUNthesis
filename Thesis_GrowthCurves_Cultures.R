#Symbiodinium microadriaticum growth curves
#Created by Jennica Moffat August, 2019

#load libraries
library(tidyverse)
library(RColorBrewer)
library(ggpmisc)
library(car)
library(agricolae)

library(reshape2)
library(growthcurver)
library(purrr)

#Clear the environment
rm(list=ls())
#Load growth data
mydata<-read.csv("JunePart1.csv")
mydata<-read.csv("GrowthDataCombined_r.csv")

#Renaming Date column because it transfered weird
names(mydata)[1]<-"Date"
mydata$Temp<-as.factor(mydata$Temp)


#July Graphs###########################################
#Graphs of July Data
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



#Graphs of combined data#####

#Now going through with combined data from trial 1 and trial 2
#Clear the environment
rm(list=ls())
#load libraries
library(tidyverse)
library(RColorBrewer)

#Load combined growth data
mydata<-read.csv("GrowthDataCombined_r.csv")
View(mydata)

#Renaming Date column because it transferred weird
names(mydata)[1]<-"Date"
View(mydata)
#Making temp a factor rather than numeric
mydata$Temp<-as.factor(mydata$Temp)


#Graphs comparing between rounds#####
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


#Combined data to make plots (without accounting for round)#####
#Using combined data to make plots without accounting for round
CCMP2458<-ggplot(subset(mydata, Genotype == "CCMP2458"), aes(x=Day, y=Densityx10000, color=Temp))+
  geom_point()+
  theme_classic()+
  ggtitle("Growth Curve CCMP2458")+
  geom_smooth()
CCMP2458

CCMP2464<-ggplot(subset(mydata, Genotype == "CCMP2464" & Round == "July"), aes(x=Day, y=Densityx10000, color=Temp))+
  geom_point()+
  theme_classic()+
  ggtitle("Growth Curve CCMP2464")+
  geom_smooth()
CCMP2464

FLCass<-ggplot(subset(mydata, Genotype == "FLCass"), aes(x=Day, y=Densityx10000, color=Temp))+
  geom_point()+
  theme_classic()+
  ggtitle("Growth Curve FLCass")+
  geom_smooth()
FLCass

RT362<-ggplot(subset(mydata, Genotype == "RT362"), aes(x=Day, y=Densityx10000, color=Temp))+
  geom_point()+
  theme_classic()+
  ggtitle("Growth Curve RT362")+
  geom_smooth()
RT362

KB8<-ggplot(subset(mydata, Genotype == "KB8"), aes(x=Day, y=Densityx10000, color=Temp))+
  geom_point()+
  theme_classic()+
  ggtitle("Growth Curve KB8")+
  geom_smooth()
KB8


#Growth rate graphs for just 26 and 30#####

#New data frame without 32
mydata2 <- subset(mydata, Temp == 26 | Temp == 30)
View(mydata2)

CCMP2458<-ggplot(subset(mydata2, Genotype == "CCMP2458"), aes(x=Day, y=Densityx10000, color=Temp))+
  geom_point()+
  theme_classic()+
  theme(plot.title = element_text(face = "bold", size=16), axis.text.x=element_text(color="black", size=13), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=14), axis.title.y = element_text(face="bold", color="black", size=14),
  panel.grid.major=element_blank(), panel.grid.minor=element_blank(), legend.position = "none") +
  ggtitle("CCMP2458 Growth Curve")+
  labs(x="Day", y="Density (10,000 cells/mL)")+
  scale_color_manual(values = c("skyblue3", "darkgoldenrod2"))+
  geom_smooth(formula = y ~ x, se = TRUE, size=2.5) 
CCMP2458


CCMP2464<-ggplot(subset(mydata2, Genotype == "CCMP2464" & Round == "July"), aes(x=Day, y=Densityx10000, color=Temp))+
  geom_point()+
  theme_classic()+
  ggtitle("CCMP2464 Growth Curve")+
  theme(plot.title = element_text(face = "bold", size=16), axis.text.x=element_text(color="black", size=13), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=14), axis.title.y = element_text(face="bold", color="black", size=14),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), legend.position = "none") +
  geom_smooth(formula = y ~ x, se = TRUE, size=2.5)+
  labs(x="Day", y="Density (10,000 cells/mL)")+
  scale_color_manual(values = c("skyblue3", "darkgoldenrod2"))
CCMP2464

FLCass<-ggplot(subset(mydata2, Genotype == "FLCass"), aes(x=Day, y=Densityx10000, color=Temp))+
  geom_point()+
  theme_classic()+
  ggtitle("FLCass Growth Curve")+
  theme(plot.title = element_text(face = "bold", size=16), axis.text.x=element_text(color="black", size=13), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=14), axis.title.y = element_text(face="bold", color="black", size=14),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), legend.position = "none") +
  geom_smooth(formula = y ~ x, se = TRUE, size=2.5)+
  labs(x="Day", y="Density (10,000 cells/mL)")+
  scale_color_manual(values = c("skyblue3", "darkgoldenrod2"))
FLCass

RT362<-ggplot(subset(mydata2, Genotype == "RT362"), aes(x=Day, y=Densityx10000, color=Temp))+
  geom_point()+
  theme_classic()+
  ggtitle("RT362 Growth Curve")+
  theme(plot.title = element_text(face = "bold", size=16), axis.text.x=element_text(color="black", size=13), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=14), axis.title.y = element_text(face="bold", color="black", size=14),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), legend.position = "none") +
  geom_smooth(formula = y ~ x, se = TRUE, size=2.5)+
  labs(x="Day", y="Density (10,000 cells/mL)")+
  scale_color_manual(values = c("skyblue3", "darkgoldenrod2"))
RT362

KB8<-ggplot(subset(mydata2, Genotype == "KB8"), aes(x=Day, y=Densityx10000, color=Temp))+
  geom_point()+
  theme_classic()+
  ggtitle("KB8 Growth Curve")+
  theme(plot.title = element_text(face = "bold", size=16), axis.text.x=element_text(color="black", size=13), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=14), axis.title.y = element_text(face="bold", color="black", size=14),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), legend.position = "none") +
  geom_smooth(formula = y ~ x, se = TRUE, size=2.5)+
  labs(x="Day", y="Density (10,000 cells/mL)")+
  scale_color_manual(values = c("skyblue3", "darkgoldenrod2"))
KB8




#Exponential Graphs for 26 and 30#####

#New data frame without 32, and days 5-15
rm(list=ls())
mydata<-read.csv("GrowthDataCombined_r.csv")
mydata$Temp<-as.factor(mydata$Temp)
mydata2 <- subset(mydata, Temp == 26 | Temp == 30)
mydata2<- subset(mydata2, Day <16)
View(mydata2)


CCMP2458<-ggplot(subset(mydata2, Genotype == "CCMP2458"), aes(x=Day, y=Densityx10000, color=Temp))+
  geom_point()+
  theme_classic()+
  theme(plot.title = element_text(face = "bold", size=16), axis.text.x=element_text(color="black", size=13), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=14), axis.title.y = element_text(face="bold", color="black", size=14),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), legend.position = "none") +
  ggtitle("CCMP2458 Growth Curve")+
  labs(x="Day", y="Density (10,000 cells/mL)")+
  scale_color_manual(values = c("skyblue3", "darkgoldenrod2"))+
  geom_smooth(method = "glm", se = TRUE, size = 2) 
CCMP2458

CCMP2464<-ggplot(subset(mydata2, Genotype == "CCMP2464" & Round == "July"), aes(x=Day, y=Densityx10000, color=Temp))+
  geom_point()+
  theme_classic()+
  ggtitle("CCMP2464 Growth Curve")+
  theme(plot.title = element_text(face = "bold", size=16), axis.text.x=element_text(color="black", size=13), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=14), axis.title.y = element_text(face="bold", color="black", size=14),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), legend.position = "none") +
  geom_smooth(method = "glm", se = TRUE, size=2)+
  labs(x="Day", y="Density (10,000 cells/mL)")+
  scale_color_manual(values = c("skyblue3", "darkgoldenrod2"))
CCMP2464

FLCass<-ggplot(subset(mydata2, Genotype == "FLCass"), aes(x=Day, y=Densityx10000, color=Temp))+
  geom_point()+
  theme_classic()+
  ggtitle("FLCass Growth Curve")+
  theme(plot.title = element_text(face = "bold", size=16), axis.text.x=element_text(color="black", size=13), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=14), axis.title.y = element_text(face="bold", color="black", size=14),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), legend.position = "none") +
  geom_smooth(method = "glm", se = TRUE, size=2)+
  labs(x="Day", y="Density (10,000 cells/mL)")+
  scale_color_manual(values = c("skyblue3", "darkgoldenrod2"))
FLCass

RT362<-ggplot(subset(mydata2, Genotype == "RT362"), aes(x=Day, y=Densityx10000, color=Temp))+
  geom_point()+
  theme_classic()+
  ggtitle("RT362 Growth Curve")+
  theme(plot.title = element_text(face = "bold", size=16), axis.text.x=element_text(color="black", size=13), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=14), axis.title.y = element_text(face="bold", color="black", size=14),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), legend.position = "none") +
  geom_smooth(method = "glm", se = TRUE, size=2)+
  labs(x="Day", y="Density (10,000 cells/mL)")+
  scale_color_manual(values = c("skyblue3", "darkgoldenrod2"))
RT362

KB8<-ggplot(subset(mydata2, Genotype == "KB8"), aes(x=Day, y=Densityx10000, color=Temp))+
  geom_point()+
  theme_classic()+
  ggtitle("KB8 Growth Curve")+
  theme(plot.title = element_text(face = "bold", size=16), axis.text.x=element_text(color="black", size=13), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=14), axis.title.y = element_text(face="bold", color="black", size=14),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), legend.position = "none") +
  geom_smooth(method = "glm", se = TRUE, size=2)+
  labs(x="Day", y="Density (10,000 cells/mL)")+
  scale_color_manual(values = c("skyblue3", "darkgoldenrod2"))
KB8


#Calculating growth rates using library("growthrates")#####
#Outlined here: https://cran.r-project.org/web/packages/growthrates/vignettes/Introduction.html
library(growthrates)
#Clear the environment
rm(list=ls())
#Load combined growth data
mydata<-read.csv("GrowthDataCombined_r.csv")
mydata$Temp<-as.factor(mydata$Temp)
names(mydata)[1]<-"Date"
View(mydata)

splitted.data <- multisplit(mydata, c("Temp", "Genotype", "Flask", "Round")) 
#Splits data by temp, genotype, flask, and trial("round") so we work with one replicate of one treatement at a time

dat <- splitted.data[[1]] #First subset of data is named "dat" CCMP2458, 26*, flask 1, July
View(dat)

fit <- fit_easylinear(dat$Day, dat$Densityx10000)
#This method fits segments of linear models to the log-transformed data and tries to find the maximum growth rate.
#Im not sure how it knows to log transform it, might just be part of the code.

summary(fit)  #To inspect the outcome of this model fit
#I think it's just telling me that the model fits?

coef(fit)     #exponential growth parameters
#I think, mumax is maximum growth rate. Lag is how long it takes to get to exponential growth (I got a negative value??)

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
coef(fit2)
summary(fit2)


#Trying code from section titled "Nonparametric Smoothing Splines"#####
#Clear the environment
rm(list=ls())
#Load combined growth data
mydata<-read.csv("GrowthDataCombined_r.csv")
mydata$Temp<-as.factor(mydata$Temp)
names(mydata)[1]<-"Date"
View(mydata)

splitted.data <- multisplit(mydata, c("Temp", "Genotype", "Flask", "Round")) 
#Splits data by temp, genotype, flask, and trial("round") so we work with one replicate of one treatement at a time

dat <- splitted.data[[1]]
View(dat)
time <- dat$Day
y    <- dat$Densityx10000

## automatic smoothing with cv
res <- fit_spline(time, y)

par(mfrow = c(1, 2))
plot(res, log = "y")
plot(res)
#Not sure what the plots give me exactly, but the fits are both horrible. 
coef(res)
#mumax=0.44581721
#I'm getting different max growth rates with every strategy, and I have no idea
#what any of them are doing.



#Using "growthcurver" package#####
rm(list=ls())
mydata<-read.csv("GrowthDataCombined_r.csv")
mydata$Temp<-as.factor(mydata$Temp)
mydata$Flask<-as.factor(mydata$Flask)
View(mydata)

MayJuneData<-subset(mydata, Round=="MayJune")
JulyData<-subset(mydata,Round == "July")

#To run growthcurver package, each treatment/replicate needs its own row. 
#Try to pivot data
DensityTimeData<-pivot_wider(mydata, id_cols = NULL, names_from = c(Genotype,Flask,Temp,Round),
                      names_prefix = "", names_repair = "check_unique",
                      values_from = Density_cellspermL, values_fill = NULL, values_fn = NULL)
View(DensityTimeData)

#SO this added new columns for each Genotype_Flask#_Temp_Round, with the values for each day of Density (cells per mL)
#Lot's of NA's because I didn't tell it to combine by day. So there is still one row for each replicate day. 

#Going to try to run the model for one replicate
model.FLCass_1_26_MayJune <- SummarizeGrowth(DensityTimeData$Day, DensityTimeData$FLCass_1_26_MayJune)
#Let's see how it looks.
plot(model.FLCass_1_26_MayJune)
#Did not work. I need to remove the NA's. 

#Making a data frame of just FLCass_1_26_MayJune
FLCass_1_26_MayJune<- subset(DensityTimeData, select = c(Day, FLCass_1_26_MayJune))
View(FLCass_1_26_MayJune)
#Remove NA's
data.FLCass.1.26.MayJune<-na.omit(FLCass_1_26_MayJune)
View(data.FLCass.1.26.MayJune)

#Going to try to run the model 
model.FLCass.1.26.MayJune <- SummarizeGrowth(data.FLCass.1.26.MayJune$Day, data.FLCass.1.26.MayJune$FLCass_1_26_MayJune)
#Let's see how it looks.
plot(model.FLCass.1.26.MayJune)
#To get the output
model.FLCass.1.26.MayJune
#Well it worked!
# To see all the available metrics 
str(model.FLCass_1_26_MayJune$vals)

#Okay, now I want to try to use the growthcurver for multiple replicates at once. 
#Need to make a second df for next replicate
#Making a data frame of just FLCass_2_26_MayJune
data.FLCass.2.26.MayJune<- subset(DensityTimeData, select = c(Day, FLCass_2_26_MayJune))
View(data.FLCass.2.26.MayJune)
#Remove NA's
data.FLCass.2.26.MayJune<-na.omit(data.FLCass.2.26.MayJune)
View(data.FLCass.2.26.MayJune)

combined.data<-merge(data.FLCass.1.26.MayJune,
                     data.FLCass.2.26.MayJune,
                     by="Day")
View(combined.data)
#Okay, combined dataframe looks good. 
#Need to rename "Day" column to "time" for the package. 
names(combined.data)[1]<-"time"

# Now, we'll use Growthcurver to summarize the growth curve data for the entire
# plate using the default background correction method ("min").
gc_out <- SummarizeGrowthByPlate(combined.data, plot_fit = TRUE)
head(gc_out)
#Dope. Okay I guess I just have to create 120 dataframes and combine them all. Great. 

#To remove NA's, and group by Day
data <- DensityTimeData %>% group_by(Day) %>% summarise_all(funs( na.omit(unique(.)) ))
#Did not work. I need to remove all of the extra columns.
GrowthData <- DensityTimeData[ -c(1,3:17) ]
View(GrowthData)
data <- GrowthData %>% group_by(Day) %>% summarise_all(funs( na.omit(unique(.)) ))

#Let's try one round at a time
MayJuneData<-subset(mydata, Round=="MayJune")
View(MayJuneData)
MayJuneGrowthData<-pivot_wider(MayJuneData, id_cols = NULL, names_from = c(Genotype,Flask,Temp),
                             names_prefix = "", names_repair = "check_unique",
                             values_from = Density_cellspermL, values_fill = NULL, values_fn = NULL)
View(MayJuneGrowthData)
MayJuneGrowthData <- MayJuneGrowthData[ -c(1,2,4:18) ]
View(MayJuneGrowthData)

data<-MayJuneGrowthData %>%
  gather(var, val, -Day, na.rm = TRUE) %>%
  group_by(Day, var) %>%
  distinct(val) %>%
  spread(var, val)
View(data)
#IT WOOOOOOOOORKED


#Per capita growth curves from hand calculated growth rates#####

#Clear the environment
rm(list=ls())
#Load growth data
mydata<-read.csv("GrowthDataCombined_r.csv")
View(mydata)
mydata$Temp<-as.factor(mydata$Temp)

KB8Growth32F1<-ggplot(subset(mydata, Genotype == "KB8" & Temp=="32" & Flask=="1"), aes(x=Densityx10000, y=PerCapitaGrowthRate, color=Round))+
  geom_point()+
  theme_classic()+
  ggtitle("Per Capita Growth Curve KB8 - 32*")+
  geom_quantile(formula=y~x)+
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)
KB8Growth32F1
#"Missing data" is the last data point for each trial, since there is no per capita growth 
# rate for the last point. 
# y-int is r (highest growth rate), x-int is K (carrying capacity)
#For KB8, 32*, Flask 1, July: r= 2.38, K=82.64(x10,000)
#For KB8, 32*, Flask 1, MayJune: r=1.03, K=92.79(x10,000)

#Gonna see what the growthcurves package gives me

CCMP2458PerCap26F1<-ggplot(subset(mydata, Genotype == "CCMP2458" & Temp=="26" & Flask=="1"), aes(x=Densityx10000, y=PerCapitaGrowthRate, color=Round))+
  geom_point()+
  theme_classic()+
  ggtitle("Per Capita Growth Curve CCMP2458 - 26*")+
  geom_quantile(formula=y~x)+
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)
CCMP2458PerCap26F1
#For CCMP2458, 26*, Flask 1, July: r= 1.34, K=57.26(x10,000)
#For KB8, 32*, Flask 1, MayJune: r=1.12, K=146.6(x10,000)
#Growthrates package for MayJune gives me this output
#y0          y0_lm     mumax       lag 
#2.5555556 2.2217903 0.3080364 0.4543499 



#Calculating logistic growth rates#####
#New strategy
#Looking at all of the graphs, they are all in logistic growth from day 5-15, so I'm 
#going to calculate a line from that, and use that as growth rate

#Clear the environment
rm(list=ls())
#Load growth data
mydata<-read.csv("GrowthDataCombined_r.csv")
View(mydata)
mydata$Temp<-as.factor(mydata$Temp)


CCMP2458<-ggplot(subset(mydata, Genotype == "CCMP2458" & Temp=="26" & Flask=="1" & Day > 4 & Day < 16), aes(x=Densityx10000, y=PerCapitaGrowthRate, color=Round))+
  geom_point()+
  theme_classic()+
  ggtitle("Per Capita Growth Curve CCMP2458 - 26*")+
  geom_quantile(formula=y~x)+
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)
CCMP2458

#Graph of 2458 @ 26* Flask 1, across BOTH trials, for days 5-15
CCMP2458<-ggplot(subset(mydata, Genotype == "CCMP2458" & Temp =="26" & Flask =="1" & Day > 4 & Day < 16), aes(x=Day, y=Densityx10000, color=Temp))+
  geom_point()+
  theme_classic()+
  theme(plot.title = element_text(face = "bold", size=16), axis.text.x=element_text(color="black", size=13), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=14), axis.title.y = element_text(face="bold", color="black", size=14),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), legend.position = "none") +
  ggtitle("CCMP2458 26 deg flaks 1")+
  labs(x="Day", y="Density (10,000 cells/mL)")+
  geom_smooth(method="lm")+
  geom_quantile(formula=y~x)+
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)
CCMP2458

ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}
fit1 <- lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "CCMP2458" & Temp =="26" & Flask =="1" & Day < 16))
ggplotRegression(fit1)
#Slope=6.8501, Intercept = -27.1, Adj R2 = 0.61124
#Both methods give me the same values!! Hooray!!
#Second method is less code, so I'm going to do that. 

#CCMP2458, 26*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "CCMP2458" & Temp =="26" & Flask =="1" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "CCMP2458" & Temp =="26" & Flask =="2" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "CCMP2458" & Temp =="26" & Flask =="3" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "CCMP2458" & Temp =="26" & Flask =="4" & Day < 16)))

#CCMP2458, 30*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "CCMP2458" & Temp =="30" & Flask =="1" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "CCMP2458" & Temp =="30" & Flask =="2" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "CCMP2458" & Temp =="30" & Flask =="3" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "CCMP2458" & Temp =="30" & Flask =="4" & Day < 16)))

#CCMP2464, 26*, flask 1-4 (July only!)
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "CCMP2464" & Temp =="26" & Flask =="1" & Day < 16 & Round == "July")))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "CCMP2464" & Temp =="26" & Flask =="2" & Day < 16 & Round == "July")))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "CCMP2464" & Temp =="26" & Flask =="3" & Day < 16 & Round == "July")))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "CCMP2464" & Temp =="26" & Flask =="4" & Day < 16 & Round == "July")))

#CCMP2464, 30*, flask 1-4 (July only!)
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "CCMP2464" & Temp =="30" & Flask =="1" & Day < 16 & Round == "July")))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "CCMP2464" & Temp =="30" & Flask =="2" & Day < 16 & Round == "July")))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "CCMP2464" & Temp =="30" & Flask =="3" & Day < 16 & Round == "July")))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "CCMP2464" & Temp =="30" & Flask =="4" & Day < 16 & Round == "July")))

#FLCass, 26*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "FLCass" & Temp =="26" & Flask =="1" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "FLCass" & Temp =="26" & Flask =="2" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "FLCass" & Temp =="26" & Flask =="3" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "FLCass" & Temp =="26" & Flask =="4" & Day < 16)))

#FLCass, 30*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "FLCass" & Temp =="30" & Flask =="1" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "FLCass" & Temp =="30" & Flask =="2" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "FLCass" & Temp =="30" & Flask =="3" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "FLCass" & Temp =="30" & Flask =="4" & Day < 16)))

#KB8, 26*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "KB8" & Temp =="26" & Flask =="1" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "KB8" & Temp =="26" & Flask =="2" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "KB8" & Temp =="26" & Flask =="3" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "KB8" & Temp =="26" & Flask =="4" & Day < 16)))

#KB8, 30*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "KB8" & Temp =="30" & Flask =="1" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "KB8" & Temp =="30" & Flask =="2" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "KB8" & Temp =="30" & Flask =="3" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "KB8" & Temp =="30" & Flask =="4" & Day < 16)))

#RT362, 26*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "RT362" & Temp =="26" & Flask =="1" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "RT362" & Temp =="26" & Flask =="2" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "RT362" & Temp =="26" & Flask =="3" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "RT362" & Temp =="26" & Flask =="4" & Day < 16)))

#RT362, 30*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "RT362" & Temp =="30" & Flask =="1" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "RT362" & Temp =="30" & Flask =="2" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "RT362" & Temp =="30" & Flask =="3" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "RT362" & Temp =="30" & Flask =="4" & Day < 16)))

#Created new data with all of those values
#Clear the environment
rm(list=ls())
#Load growth data
mydata<-read.csv("ExponentialGrowthRates_Day1to15.csv")
mydata$Temperature<-as.factor(mydata$Temperature)
View(mydata)

#Make a model
#Genotype(fixed) and Temperature(fixed) on Slope (ie: growth rate, NOT PER CAPITA, problem?)
model1<-lm(Slope~Genotype*Temperature, data=mydata)
model1res<-resid(model1)
qqp(model1res, "norm")

TukeyHSD(aov(model1))

#Normal. Dope.
plot(model1)
anova(model1)
#Very significant interaction between temp and geno. Cool beans. 

HSD.test(model1, ~Temperature)

#Barplot of slopes
Summary <- mydata %>%
  group_by(Genotype, Temperature) %>%
  summarize(mean=mean(Slope, na.rm=TRUE), SE=sd(Slope, na.rm=TRUE)/sqrt(length(na.omit(Slope))))
Summary

SlopesGraph<-ggplot(Summary, aes(x=Genotype, y=mean, fill=factor(Temperature), group=factor(Temperature)))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(legend.position = "none", plot.title = element_text(face = "bold", size=18), axis.text.x=element_text(color="black", size=13), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=12), axis.title.y = element_text(color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  scale_y_continuous(expand=c(0,0), limits=c(0, 10))+
  geom_bar(stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Genotype", y="Exponential Phase Growth Rate (10,000 cells/mL/day)", fill="Temperature")+  #labels the x and y axes
  scale_fill_manual(values = c("skyblue3", "darkgoldenrod2"), labels=c("26°C", "30°C"))+
  ggtitle("Growth Rates of Symbiont Strains at 26°C and 30°C")+
  ggsave("ExponentialGrowthGraph.pdf", width=10, height=6.19, dpi=300, unit="in")
SlopesGraph



#ANCOVA#####

#Clear the environment
rm(list=ls())
#Load growth data
mydata<-read.csv("GrowthDataCombined_r.csv")
View(mydata)
mydata$Temp<-as.factor(mydata$Temp)

#New data frame with just data between days 5-15
mydata2<-subset(mydata, Day > 4 & Day < 16)
View(mydata2)

model1<-lm(Densityx10000 ~ Genotype * Temp * Round * Day, data=mydata2)
#Make sure that the relationship with covariate is linear
plot(Densityx10000~Day, data=mydata2)
#looks linear to me

plot(model1)
model1res<-resid(model1)
qqp(model1res, "norm")
#Hm, not very normal. 

mydata2$logDensity<-log(mydata2$Densityx10000)
model2<-lm(logDensity ~ Genotype *Round * Temp * Day, data=mydata2)
logresid<-resid(model2)
qqp(logresid, "norm")
#that's better. 
#Rest of assumptions for logged data
plot(model2)
plot(logDensity~Day, data=mydata2)
#Run model
anova(model2)

#Round is signficant. Remove CCMP2464 cuz it's weird. 
mydata3<-subset(mydata2, Genotype != "CCMP2464")
View(mydata3)
#Also need to remove NA's for the FLCass flask that spilled
mydata3 <- subset(mydata3, !is.na(Densityx10000))
View(mydata3)
mydata3%>%count(Genotype) #Yep, 5 less FLCass counts than the other three, so it removed the NAs

model3<-lm(Densityx10000 ~ Genotype * Temp * Round * Day, data=mydata3)
model3res<-resid(model3)
qqp(model3res, "norm")
#Not very normal

model4<-lm(logDensity ~ Genotype * Temp * Day * Round, data=mydata3)
model4res<-resid(model4)
qqp(model4res, "norm")
#Okay, that's normal

anova(model4, type="III")
#All of the round interactions are significant...

model5<-lm(logDensity ~ Genotype * Temp * Day, data=mydata3)
model5res<-resid(model5)
qqp(model5res, "norm")
#Close enough for now
anova(model5)


predBF<-predict(model4) #Gets the predicted values from the regression lines in the ANCOVA
graph.data<-cbind(mydata3, predBF) #attaches those predictions to the dataset

library(ggplot2)
ggplot(data=graph.data, aes(logT, logBF, color=species)) +
  theme_bw()+
  theme(legend.title=element_text(colour="black", size=14), axis.text.x=element_text(face="bold", color="black", size=16), axis.text.y=element_text(face="bold", color="black", size=13), axis.title.x = element_text(color="black", size=18, face="bold"), axis.title.y = element_text(color="black", size=18, face="bold"),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_point() + geom_line(aes(y=predBF)) + 
  labs(x="Log Thickness", y="Log Break Force", fill="Species")+ #Fill=two colors for species
  scale_color_manual(values=c("steelblue", "salmon"), name="Species", labels=c("C. cripus", "M. stellatus")) #Edits legend for not a bargraph

