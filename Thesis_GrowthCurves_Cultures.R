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

#Renaming Date column because it transferred weird
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

FLCassGrowth26<-ggplot(subset(mydata, Genotype == "FlCass" & Temp=="26"), aes(x=Day, y=Densityx10000, color=Round))+
  geom_point()+
  theme_classic()+
  ggtitle("Growth Curve FLCass - 26*")+
  geom_smooth()
FLCassGrowth26

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

