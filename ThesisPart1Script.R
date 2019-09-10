#Symbiodinium microadriaticum growth curves
#Created by Jennica Moffat 071219

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


#Switching to PnR
rm(list=ls())
#Load PnR data
mydata<-read.csv("AugustPnR_r.csv")
View(mydata)

#Summarizing data for bargraph
#Net photosynthesis
SummaryNP <- mydata %>%
  group_by(Genotype, Temperature) %>%
  summarize(mean=mean(NetPhotoPer10000cells, na.rm=TRUE), SE=sd(NetPhotoPer10000cells, na.rm=TRUE)/sqrt(length(na.omit(NetPhotoPer10000cells))))
SummaryNP

NetPhotoGraph<-ggplot(SummaryNP, aes(x=Genotype, y=mean, fill=factor(Temperature), group=factor(Temperature)))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(axis.text.x=element_text(color="black", size=14), axis.text.y=element_text(face="bold", color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=16), axis.title.y = element_text(face="bold", color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Genotype", y="Net Photo. per 10,000 cells", fill="Temperature")+  #labels the x and y axes
  scale_fill_brewer(palette = "RdYlBu")
NetPhotoGraph

#Choosing color palette using RColorBrewer
display.brewer.all(colorblindFriendly = TRUE)

#Respiration
SummaryRespiration <- mydata %>%
  group_by(Genotype, Temperature) %>%
  summarize(mean=mean(RespirationPer10000cells, na.rm=TRUE), SE=sd(RespirationPer10000cells, na.rm=TRUE)/sqrt(length(na.omit(RespirationPer10000cells))))
SummaryRespiration

RespirationGraph<-ggplot(SummaryRespiration, aes(x=Genotype, y=mean, fill=factor(Temperature), group=factor(Temperature)))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(axis.text.x=element_text(color="black", size=14), axis.text.y=element_text(face="bold", color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=16), axis.title.y = element_text(face="bold", color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Genotype", y="Respiration per 10,000 cells", fill="Temperature")+  #labels the x and y axes
  scale_fill_brewer(palette = "RdYlBu")
RespirationGraph

#Gross Photosynthesis
SummaryGP <- mydata %>%
  group_by(Genotype, Temperature) %>%
  summarize(mean=mean(GrossPhotoPer10000cells, na.rm=TRUE), SE=sd(GrossPhotoPer10000cells, na.rm=TRUE)/sqrt(length(na.omit(GrossPhotoPer10000cells))))
SummaryGP

GPGraph<-ggplot(SummaryGP, aes(x=Genotype, y=mean, fill=factor(Temperature), group=factor(Temperature)))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(axis.text.x=element_text(color="black", size=14), axis.text.y=element_text(face="bold", color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=16), axis.title.y = element_text(face="bold", color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Genotype", y="Gross Photo. per 10,000 cells", fill="Temperature")+  #labels the x and y axes
  scale_fill_brewer(palette = "RdYlBu")
GPGraph


#Alright time to start some actual stats...
View(mydata)
#Genotype (fixed) and Temperature (fixed) on dependent variables (NP, GP, and Resp)

#Make model
model1<-aov(NetPhotoPer10000cells~Genotype*Temperature, data=mydata)
anova(model1)
#Check assumptions
plot(model1)
library("car")
model1res<-resid(model1)
qqp(model1res, "norm")
#Not normal. Gotta to GLMM instead of ANOVA

