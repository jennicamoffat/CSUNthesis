#Symbiodinium microadriaticum PnR NOT standardized by DI curves
#Created by Jennica Moffat 091419

#Clear the environment
rm(list=ls())
#Load PnR data
mydata<-read.csv("AugustPnR_r_noDI_github.csv")
View(mydata)
#load libraries
library(tidyverse)
library(car)
library(MASS)
library(RColorBrewer)

#Renaming Temperature column cuz it transferred weird
names(mydata)[1]<-"Temperature"
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
  scale_fill_brewer(palette = "RdYlBu")+
  ggtitle("Net Photosynthesis")
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
  scale_fill_brewer(palette = "RdYlBu")+
  ggtitle("Respiration")
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
  scale_fill_brewer(palette = "RdYlBu")+
  ggtitle("Gross Photosynthesis")
GPGraph

########################
#Going to do all of the graphs again with just 26 and 30, not 32

#Making new dataframe without 32*
mydata2 <- subset(mydata, Temperature == 26 | Temperature == 30)
View(mydata2)

#GrossPhoto -- No 32 and no DI standard
SummaryGPno32noDI <- mydata2 %>%
  group_by(Genotype, Temperature) %>%
  summarize(mean=mean(GrossPhotoPer10000cells, na.rm=TRUE), SE=sd(GrossPhotoPer10000cells, na.rm=TRUE)/sqrt(length(na.omit(GrossPhotoPer10000cells))))
SummaryGPno32noDI

GPGraphno32noDI<-ggplot(SummaryGPno32noDI, aes(x=Genotype, y=mean, fill=factor(Temperature), group=factor(Temperature)))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(axis.text.x=element_text(color="black", size=14), axis.text.y=element_text(face="bold", color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=16), axis.title.y = element_text(face="bold", color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Genotype", y="Gross Photo. per 10,000 cells", fill="Temperature")+  #labels the x and y axes
  scale_fill_brewer(palette = "RdYlBu")+
  ggtitle("Gross Photosynthesis of Symbiont Strains at 26* and 30*")
GPGraphno32noDI

#NetPhoto -- No 32 and no DI standard
SummaryNPno32noDI <- mydata2 %>%
  group_by(Genotype, Temperature) %>%
  summarize(mean=mean(NetPhotoPer10000cells, na.rm=TRUE), SE=sd(NetPhotoPer10000cells, na.rm=TRUE)/sqrt(length(na.omit(NetPhotoPer10000cells))))
SummaryNPno32noDI

NPGraphno32noDI<-ggplot(SummaryNPno32noDI, aes(x=Genotype, y=mean, fill=factor(Temperature), group=factor(Temperature)))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(axis.text.x=element_text(color="black", size=14), axis.text.y=element_text(face="bold", color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=16), axis.title.y = element_text(face="bold", color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Genotype", y="Net Photo. per 10,000 cells", fill="Temperature")+  #labels the x and y axes
  scale_fill_brewer(palette = "RdYlBu")+
  ggtitle("Net Photosynthesis of Symbiont Strains at 26* and 30*")
NPGraphno32noDI

#Respiration -- No 32 and no DI standard
SummaryRespNo32noDI <- mydata2 %>%
  group_by(Genotype, Temperature) %>%
  summarize(mean=mean(RespirationPer10000cells, na.rm=TRUE), SE=sd(RespirationPer10000cells, na.rm=TRUE)/sqrt(length(na.omit(RespirationPer10000cells))))
SummaryRespNo32noDI

RespGraphno32noDI<-ggplot(SummaryRespNo32noDI, aes(x=Genotype, y=mean, fill=factor(Temperature), group=factor(Temperature)))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(axis.text.x=element_text(color="black", size=14), axis.text.y=element_text(face="bold", color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=16), axis.title.y = element_text(face="bold", color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Genotype", y="Respiration per 10,000 cells", fill="Temperature")+  #labels the x and y axes
  scale_fill_brewer(palette = "RdYlBu")+
  ggtitle("Respiration of Symbiont Strains at 26* and 30*")
RespGraphno32noDI

###########################################################
#Alright time to start some actual stats...
View(mydata)
#Genotype (fixed) and Temperature (fixed) on dependent variables (NP, GP, and Resp)

#Make model - Respiration
model1<-aov(AvgResp~Genotype*Temperature, data=mydata)
anova(model1)
#Check assumptions
plot(model1)
model1res<-resid(model1)
qqp(model1res, "norm")
#It looks less normal than with DI standards merp. But still maybe good enough

#Transforming Respiration
mydata$logRespiration<-log(mydata$AvgResp+1)
#Make model with logged data
model3<-aov(logRespiration~Genotype*Temperature, data=mydata)
model3res<-resid(model3)
qqp(model3res, "norm")
#Even closer
anova(model3)


#Make model - GP
model2<-aov(AvgGP~Genotype*Temperature, data=mydata)
anova(model2)
#Check assumptions
plot(model2)
model2res<-resid(model2)
qqp(model2res, "norm")
#Not normal

#gamma?
gamma<-fitdistr(mydata$AvgGP, "gamma")
qqp(mydata$AvgGP, "gamma", shape = gamma$estimate[[1]], rate=gamma$estimate[[2]])
#Bad

#log normal?
qqp(mydata$AvgGP, "lnorm")
#Nope

#Log log
mydata$loglogGP<-log(log(mydata$AvgGP+1))
View(mydata)
modelloglogGP<-aov(loglogGP~Genotype*Temperature, data=mydata)
model4res<-resid(modelloglogGP)
qqp(model4res, "norm")
#Hmm, it's closer.

