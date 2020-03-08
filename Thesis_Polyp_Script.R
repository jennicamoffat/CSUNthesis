#Cassiopea Polyp Data
#Created by Jennica Moffat 02162020

#Clear the environment
rm(list=ls())
#Load PnR data
mydata<-read.csv("Data/Thesis_PolypData_Summary.csv")
View(mydata)

#load libraries
library(tidyverse)
library(car)

#GRAPHS####
#Graph of total average ephyra production including dead ones####
#New data frame without Apo (since no ephyra)
NoApoData <- subset(mydata, Genotype != "Aposymbiotic")

TotalEphyra <- NoApoData %>%
  group_by(Genotype, Temp) %>%
  summarize(mean=mean(Total.Ephyra.Produced, na.rm=TRUE), SE=sd(Total.Ephyra.Produced, na.rm=TRUE)/sqrt(length(na.omit(Total.Ephyra.Produced))))
TotalEphyra

TotalEphyraPlot<-ggplot(TotalEphyra, aes(x=Genotype, y=mean, fill=factor(Temp), group=factor(Temp)))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(axis.text.x=element_text(color="black", size=14), axis.text.y=element_text(face="bold", color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=16), axis.title.y = element_text(face="bold", color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Genotype", y="Average Ephyra per polyp", fill="Temperature")+  #labels the x and y axes
  ggtitle("Average Ephyra produced per Polyp (dead included)")+
  scale_fill_manual(values = c("skyblue3", "darkgoldenrod2", "brown3"), labels=c("26°C", "30°C", "32°C"))
TotalEphyraPlot

TotalEphyraPlot+ggsave("Graphs/Polyps/EphyraProduced_deadincluded.pdf", width=11, height=6.19, dpi=300, unit="in")


#New dataframe without dead
NoApoNoDeadData <- subset(NoApoData, Survive.to.End == "Yes")
View(NoApoNoDeadData)

#Average Total Ephyra Produced#
TotalEphyraNoDead <- NoApoNoDeadData %>%
  group_by(Genotype, Temp) %>%
  summarize(mean=mean(Total.Ephyra.Produced, na.rm=TRUE), SE=sd(Total.Ephyra.Produced, na.rm=TRUE)/sqrt(length(na.omit(Total.Ephyra.Produced))))
TotalEphyraNoDead

TotalEphyraNoDeadPlot<-ggplot(TotalEphyraNoDead, aes(x=Genotype, y=mean, fill=factor(Temp), group=factor(Temp)))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(axis.text.x=element_text(color="black", size=14), axis.text.y=element_text(face="bold", color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=16), axis.title.y = element_text(face="bold", color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Genotype", y="Average Ephyra per polyp", fill="Temperature")+  #labels the x and y axes
  ggtitle("Average Ephyra produced per Polyp (dead removed)")+
  scale_fill_manual(values = c("skyblue3", "darkgoldenrod2", "brown3"), labels=c("26°C", "30°C", "32°C"))
TotalEphyraNoDeadPlot

TotalEphyraNoDeadPlot+ggsave("Graphs/Polyps/EphyraProduced_deadremoved.pdf", width=11, height=6.19, dpi=300, unit="in")

#Average Total Ephyra Produced as percent that inoculated 
InoculatedData <- subset(NoApoNoDeadData, Days.to.Inoculation != "NA")

EphyraifInoc <- InoculatedData %>%
  group_by(Genotype, Temp) %>%
  summarize(mean=mean(Total.Ephyra.Produced, na.rm=TRUE), SE=sd(Total.Ephyra.Produced, na.rm=TRUE)/sqrt(length(na.omit(Total.Ephyra.Produced))))
EphyraifInoc

EphyraifInocPlot<-ggplot(EphyraifInoc, aes(x=Genotype, y=mean, fill=factor(Temp), group=factor(Temp)))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(axis.text.x=element_text(color="black", size=14), axis.text.y=element_text(face="bold", color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=16), axis.title.y = element_text(face="bold", color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Genotype", y="Average Ephyra per polyp", fill="Temperature")+  #labels the x and y axes
  ggtitle("Average Ephyra produced per Polyp (if inoculated)")+
  scale_fill_manual(values = c("skyblue3", "darkgoldenrod2", "brown3"), labels=c("26°C", "30°C", "32°C"))
EphyraifInocPlot

EphyraifInocPlot+ggsave("Graphs/Polyps/EphyraifInoc.pdf", width=11, height=6.19, dpi=300, unit="in")



#Time to strobilation####
#Removing the NA's, which are the ones that never strobilated. Losing part of the story. 
#How to manage?
DaystoStrobNoDead <- NoApoNoDeadData %>%
  group_by(Genotype, Temp) %>%
  summarize(mean=mean(Days.to.Strobilation, na.rm=TRUE), SE=sd(Days.to.Strobilation, na.rm=TRUE)/sqrt(length(na.omit(Days.to.Strobilation))))
DaystoStrobNoDead

DaystoStrobNoDeadPlot<-ggplot(DaystoStrobNoDead, aes(x=Genotype, y=mean, fill=factor(Temp), group=factor(Temp)))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(axis.text.x=element_text(color="black", size=14), axis.text.y=element_text(face="bold", color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=16), axis.title.y = element_text(face="bold", color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Genotype", y="Avg days to strobilation", fill="Temperature")+  #labels the x and y axes
  ggtitle("Average Days to Strobilation (dead removed)")+
  scale_fill_manual(values = c("skyblue3", "darkgoldenrod2", "brown3"), labels=c("26°C", "30°C", "32°C"))
DaystoStrobNoDeadPlot

DaystoStrobNoDeadPlot+ggsave("Graphs/Polyps/DaystoStrob_deadremoved.pdf", width=11, height=6.19, dpi=300, unit="in")

#Boxplot of data to see distribution
ggplot(NoApoNoDeadData, aes(x=Genotype, y=Days.to.Strobilation, fill=factor(Temp)))+  #basic plot
  theme_bw()+
  geom_boxplot()+
  labs(x="Genotype", y="Avg days to strobilation", fill="Temperature")+  #labels the x and y axes
  ggtitle("Average Days to Strobilation (dead removed)")+
  scale_fill_manual(values = c("skyblue3", "darkgoldenrod2", "brown3"), labels=c("26°C", "30°C", "32°C"))
#Automatically removes all NA's values, so graph is just of those that did strobilate

#Time to inoculation (same issue as strobilation)####
DaystoInocNoDead <- NoApoNoDeadData %>%
  group_by(Genotype, Temp) %>%
  summarize(mean=mean(Days.to.Inoculation, na.rm=TRUE), SE=sd(Days.to.Inoculation, na.rm=TRUE)/sqrt(length(na.omit(Days.to.Inoculation))))
DaystoInocNoDead

DaystoInocNoDeadPlot<-ggplot(DaystoInocNoDead, aes(x=Genotype, y=mean, fill=factor(Temp), group=factor(Temp)))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(axis.text.x=element_text(color="black", size=14), axis.text.y=element_text(face="bold", color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=16), axis.title.y = element_text(face="bold", color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Genotype", y="Avg days to inoculation", fill="Temperature")+  #labels the x and y axes
  ggtitle("Average Days to Inoculation (dead removed)")+
  scale_fill_manual(values = c("skyblue3", "darkgoldenrod2", "brown3"), labels=c("26°C", "30°C", "32°C"))
DaystoInocNoDeadPlot

DaystoInocNoDeadPlot+ggsave("Graphs/Polyps/DaystoInoc_deadremoved.pdf", width=11, height=6.19, dpi=300, unit="in")

#time to ephyra####
#same issue as above


DaystoEphyraNoDead <- NoApoNoDeadData %>%
  group_by(Genotype, Temp) %>%
  summarize(mean=mean(Days.to.Ephyra, na.rm=TRUE), SE=sd(Days.to.Ephyra, na.rm=TRUE)/sqrt(length(na.omit(Days.to.Ephyra))))
DaystoEphyraNoDead

DaystoEphyraNoDeadPlot<-ggplot(DaystoEphyraNoDead, aes(x=Genotype, y=mean, fill=factor(Temp), group=factor(Temp)))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(axis.text.x=element_text(color="black", size=14), axis.text.y=element_text(face="bold", color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=16), axis.title.y = element_text(face="bold", color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Genotype", y="Avg days", fill="Temperature")+  #labels the x and y axes
  ggtitle("Average Days to Produce an Ephyra (dead removed)")+
  scale_fill_manual(values = c("skyblue3", "darkgoldenrod2", "brown3"), labels=c("26°C", "30°C", "32°C"))
DaystoEphyraNoDeadPlot

DaystoEphyraNoDeadPlot+ggsave("Graphs/Polyps/DaystoEphyra_deadremoved.pdf", width=11, height=6.19, dpi=300, unit="in")


#Survival####
#I have one weird NA from a spilled one. Just gonna remove it. 
mydata2<-subset(mydata, Survive.to.End != "NA")

Survival<- mydata2%>%group_by(Genotype, Temp)%>%
  tally(Survive.to.End == "Yes")
Survival

Survival$SurvivalRate<-Survival$n/24
Survival

#Not quite because I need SE somehow
Survival2<- mydata2%>%group_by(Genotype, Temp, Plate, WellNum)%>%
  tally(Survive.to.End == "Yes")
Survival2

SurvivalSummary<-Survival2%>%
  group_by(Genotype, Temp) %>%
  summarize(mean=mean(n, na.rm=TRUE), SE=sd(n, na.rm=TRUE)/sqrt(length(na.omit(n))))
SurvivalSummary
#I think this is what I need. Double check!

SurvivalPlot<-ggplot(SurvivalSummary, aes(x=Genotype, y=mean, fill=factor(Temp), group=factor(Temp)))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(axis.text.x=element_text(color="black", size=14), axis.text.y=element_text(face="bold", color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=16), axis.title.y = element_text(face="bold", color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Genotype", y="Avg Survival", fill="Temperature")+  #labels the x and y axes
  ggtitle("Average Survival")+
  scale_fill_manual(values = c("skyblue3", "darkgoldenrod2", "brown3"), labels=c("26°C", "30°C", "32°C"))
SurvivalPlot

SurvivalPlot+ggsave("Graphs/Polyps/SurvivalPlot.pdf", width=11, height=6.19, dpi=300, unit="in")


#Bud production####
#Removing dead polyps
NoDeadData <- subset(mydata, Survive.to.End == "Yes")
NoDeadData

BudData<-NoDeadData%>%
  group_by(Genotype, Temp)%>%
  summarize(mean=mean(Total.Buds, na.rm=TRUE), SE=sd(Total.Buds, na.rm=TRUE)/sqrt(length(na.omit(Total.Buds))))
BudData

BudPlot<-ggplot(BudData, aes(x=Genotype, y=mean, fill=factor(Temp), group=factor(Temp)))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(axis.text.x=element_text(color="black", size=14), axis.text.y=element_text(face="bold", color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=16), axis.title.y = element_text(face="bold", color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Genotype", y="Avg Buds per Polyp", fill="Temperature")+  #labels the x and y axes
  ggtitle("Average buds produced per polyp")+
  scale_fill_manual(values = c("skyblue3", "darkgoldenrod2", "brown3"), labels=c("26°C", "30°C", "32°C"))
BudPlot

BudPlot+ggsave("Graphs/Polyps/BudPlot_deadremoved.pdf", width=11, height=6.19, dpi=300, unit="in")



#STATS####
#Clear the environment
rm(list=ls())
#Load PnR data
mydata<-read.csv("Data/Thesis_PolypData_Summary.csv")
View(mydata)

#Total average ephyra production####
#Removing aposymbiotic
NoApoData <- subset(mydata, Genotype != "Aposymbiotic")
View(NoApoData)


#Genotype (fixed) and Temperature (fixed) on dependent variables (total ephyra production)

#Make model
Ephyramodel<-aov(Total.Ephyra.Produced~Genotype*Temp, data=NoApoData)
#Check assumptions
plot(Ephyramodel)
model1res<-resid(Ephyramodel)
qqp(model1res, "norm")
#I think it's normal enough. A few outliers(?) 338, 361, 290, and 313

#Run model
EphyraAnova<-anova(Ephyramodel)
EphyraAnova
#Yay! Significant interaction between Genotype and Temp (unsurprisingly)
#Genotype:Temp, P=0.0008394 ***

#Time to strobilation####

#I need to remove the NA's
StrobilatedData <- subset(NoApoData, Days.to.Strobilation != "NA")
View(StrobilatedData)

#Make model
Strobmodel<-aov(Days.to.Strobilation~Genotype*Temp, data=StrobilatedData)
#Check assumptions
plot(Strobmodel)
Strobmodelres<-resid(Strobmodel)
qqp(Strobmodelres, "norm")
#Not great. A few outliers: 337, 318, 186, and 194

#transform
StrobilatedData$logDays<-log(StrobilatedData$Days.to.Strobilation)
logStrobmodel<-aov(logDays~Genotype*Temp, data=StrobilatedData)
logStrobmodelres<-resid(logStrobmodel)
qqp(logStrobmodelres, "norm")
#Better,let's log one more time

StrobilatedData$loglogDays<-log(StrobilatedData$logDays)
loglogStrobmodel<-aov(loglogDays~Genotype*Temp, data=StrobilatedData)
loglogStrobmodelres<-resid(loglogStrobmodel)
qqp(loglogStrobmodelres, "norm")
#Slightly better, but a lot of points still just above CI's 


#Square root
StrobilatedData$sqrtDays<-sqrt(StrobilatedData$Days.to.Strobilation)
sqrtStrobmodel<-aov(sqrtDays~Genotype*Temp, data=StrobilatedData)
sqrtStrobmodelres<-resid(sqrtStrobmodel)
qqp(sqrtStrobmodelres, "norm")
#Barely changed. 

#Using model of double logged data
anova(loglogStrobmodel)
#interesting...no interaction. Geno and temp are sig on their own, but P=0.5 for interaction.

#Time to inoculation####
#I need to remove the NA's
InocData <- subset(NoApoData, Days.to.Inoculation != "NA")
View(InocData)

#Make model
Inocmodel<-aov(Days.to.Inoculation~Genotype*Temp, data=InocData)
#Check assumptions
plot(Inocmodel)
Inocmodelres<-resid(Inocmodel)
qqp(Inocmodelres, "norm")
#Ew. That's pretty bad. 

InocData$logIDays<-log(InocData$Days.to.Inoculation)
logInocmodel<-aov(logIDays~Genotype*Temp, data=InocData)
logInocmodelres<-resid(logInocmodel)
qqp(logInocmodelres, "norm")
#Ugh, possibly worse. 

#double log
InocData$loglogIDays<-log(InocData$logIDays)
loglogInocmodel<-aov(loglogIDays~Genotype*Temp, data=InocData)
loglogInocmodelres<-resid(loglogInocmodel)
qqp(loglogInocmodelres, "norm")
#Oh that is better

#triple log??
InocData$trplogIDays<-log(InocData$loglogIDays)
trplogInocmodel<-aov(trplogIDays~Genotype*Temp, data=InocData)
trplogInocmodelres<-resid(trplogInocmodel)
qqp(trplogInocmodelres, "norm")
#Idk man. No.

hist(InocData$trplogIDays)

#Square root
InocData$sqrtIDays<-sqrt(InocData$Days.to.Inoculation)
sqrtInocmodel<-aov(sqrtIDays~Genotype*Temp, data=InocData)
sqrtInocmodelres<-resid(sqrtInocmodel)
qqp(sqrtInocmodelres, "norm")
#No. 

#Fourth root
InocData$fourthrtIDays<-sqrt(InocData$sqrtIDays)
fourthrtInocmodel<-aov(fourthrtIDays~Genotype*Temp, data=InocData)
fourthrtInocmodelres<-resid(fourthrtInocmodel)
qqp(fourthrtInocmodelres, "norm")
#No.

#I think I have to do a GLMM. Later....

#Time to ephyra####
#I need to remove the NA's
TimetoEphyraData <- subset(NoApoData, Days.to.Ephyra != "NA")
View(TimetoEphyraData)

#Make model
TEphyramodel<-aov(Days.to.Ephyra~Genotype*Temp, data=TimetoEphyraData)
#Check assumptions
plot(TEphyramodel)
TEphyramodelres<-resid(TEphyramodel)
qqp(TEphyramodelres, "norm")
#Not too bad, but needs transforming. 

hist(TimetoEphyraData$loglogEDays)

#Log
TimetoEphyraData$logEDays<-log(TimetoEphyraData$Days.to.Ephyra)
logEphyraDaysmodel<-aov(logEDays~Genotype*Temp, data=TimetoEphyraData)
logEphyraDaysmodelres<-resid(logEphyraDaysmodel)
qqp(logEphyraDaysmodelres, "norm")
#Almost. 

#Double log
TimetoEphyraData$loglogEDays<-log(TimetoEphyraData$logEDays)
loglogEphyraDaysmodel<-aov(loglogEDays~Genotype*Temp, data=TimetoEphyraData)
loglogEphyraDaysmodelres<-resid(loglogEphyraDaysmodel)
qqp(loglogEphyraDaysmodelres, "norm")
#Mehhhhhh close enough?
anova(loglogEphyraDaysmodel)
#Again, interaction is not significant. This one makes more sense. 


#Survival####
#To get a rate I need to tally the number that survived, divided by 24 because that's how 
#many each group started with
Survival<- mydata%>%group_by(Genotype, Temp, Plate, WellNum)%>%
  tally(Survive.to.End == "Yes")
View(Survival)
#1 is yes survived, 0 is no did not survive
#So average should be the survival rate?
Survivalmodel<-aov(n~Genotype*Temp, data=Survival)
plot(Survivalmodel)

#It's bi-model. So I think I need to do something other than anova.
#I need to do a chi-squared test I think

#To see survival overall, not separated by geno or temp
table(Survival$n)
prop.table(table(Survival$n))

chisq.test(Survival$n, Survival$Genotype)
#can't do interactions with chi-squared

#Cochran-Mantel-Haenszel test
mantelhaen.test(Survival$n, Survival$Temp, Survival$Genotype)
#Order matters

#Bud production####