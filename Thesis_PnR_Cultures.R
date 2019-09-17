#Symbiodinium microadriaticum PnR 
#Created by Jennica Moffat 091019

#Clear the environment
rm(list=ls())
#Load PnR data
mydata<-read.csv("AugustPnR_r_github.csv")
View(mydata)
#load libraries
library(tidyverse)
library(car)
library(MASS)
library(RColorBrewer)

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

###########################################################
#Making new dataframe without 32*
mydata2 <- subset(mydata, Temperature == 26 | Temperature == 30)
View(mydata2)

SummaryGPno32 <- mydata2 %>%
  group_by(Genotype, Temperature) %>%
  summarize(mean=mean(GrossPhotoPer10000cells, na.rm=TRUE), SE=sd(GrossPhotoPer10000cells, na.rm=TRUE)/sqrt(length(na.omit(GrossPhotoPer10000cells))))
SummaryGPno32

GPGraphno32<-ggplot(SummaryGPno32, aes(x=Genotype, y=mean, fill=factor(Temperature), group=factor(Temperature)))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(axis.text.x=element_text(color="black", size=14), axis.text.y=element_text(face="bold", color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=16), axis.title.y = element_text(face="bold", color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Genotype", y="Gross Photo. per 10,000 cells", fill="Temperature")+  #labels the x and y axes
  scale_fill_brewer(palette = "RdYlBu")+
  ggtitle("Gross Photosynthesis of Symbiont Strains at 26* and 30*")
GPGraphno32

#NetPhoto -- No 32
SummaryNPno32 <- mydata2 %>%
  group_by(Genotype, Temperature) %>%
  summarize(mean=mean(NetPhotoPer10000cells, na.rm=TRUE), SE=sd(NetPhotoPer10000cells, na.rm=TRUE)/sqrt(length(na.omit(NetPhotoPer10000cells))))
SummaryNPno32

NPGraphno32<-ggplot(SummaryNPno32, aes(x=Genotype, y=mean, fill=factor(Temperature), group=factor(Temperature)))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(axis.text.x=element_text(color="black", size=14), axis.text.y=element_text(face="bold", color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=16), axis.title.y = element_text(face="bold", color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Genotype", y="Net Photo. per 10,000 cells", fill="Temperature")+  #labels the x and y axes
  scale_fill_brewer(palette = "RdYlBu")+
  ggtitle("Net Photosynthesis of Symbiont Strains at 26* and 30*")
NPGraphno32

#Respiration -- No 32
SummaryRespNo32 <- mydata2 %>%
  group_by(Genotype, Temperature) %>%
  summarize(mean=mean(RespirationPer10000cells, na.rm=TRUE), SE=sd(RespirationPer10000cells, na.rm=TRUE)/sqrt(length(na.omit(RespirationPer10000cells))))
SummaryRespNo32

RespGraphno32<-ggplot(SummaryRespNo32, aes(x=Genotype, y=mean, fill=factor(Temperature), group=factor(Temperature)))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(axis.text.x=element_text(color="black", size=14), axis.text.y=element_text(face="bold", color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=16), axis.title.y = element_text(face="bold", color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Genotype", y="Respiration per 10,000 cells", fill="Temperature")+  #labels the x and y axes
  scale_fill_brewer(palette = "RdYlBu")+
  ggtitle("Respiration of Symbiont Strains at 26* and 30*")
RespGraphno32

#########################################################################################
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
#OMG IT'S NORMAL

#Make model - GP
model2<-aov(AvgGP~Genotype*Temperature, data=mydata)
anova(model2)
#Check assumptions
plot(model2)
model2res<-resid(model2)
qqp(model2res, "norm")
#Shit, not normal

#Transformation time
#log transform GP
mydata$logAvgGP<-log(mydata$AvgGP+1)
View(mydata)

#Make model with logged data - GP
modellogGP<-aov(logAvgGP~Genotype*Temperature, data=mydata)
anova(modellogGP)
#Check assumptions
plot(modellogGP)
model2res<-resid(modellogGP)
qqp(model2res, "norm")
#Ugh that did nothing

#log normal?
qqp(mydata$AvgGP, "lnorm")
#Ugh nope. 
qqp(mydata$logAvgGP, "lnorm")
#That did nothing, again. 

#gamma?
gamma<-fitdistr(mydata$AvgGP, "gamma")
qqp(mydata$AvgGP, "gamma", shape = gamma$estimate[[1]], rate=gamma$estimate[[2]])
#Maybe a smidge better?
#Try gamma with logged data
gamma2<-fitdistr(mydata$logAvgGP, "gamma")
qqp(mydata$logAvgGP, "gamma", shape = gamma$estimate[[1]], rate=gamma$estimate[[2]])
View(mydata)
#Logging seems to do nothing for all distributions. 

#Other transformations

#Square root
mydata$sqrtAvgGP<-sqrt(mydata$AvgGP)
View(mydata)
modelsqrtAvgGP<-aov(sqrtAvgGP~Genotype*Temperature, data=mydata)
anova(modelsqrtAvgGP)
#Check assumptions
plot(modellogGP)
model3res<-resid(modelsqrtAvgGP)
qqp(model3res, "norm")

#Log log
mydata$loglogGP<-log(log(mydata$AvgGP+1))
View(mydata)
modelloglogGP<-aov(loglogGP~Genotype*Temperature, data=mydata)
model4res<-resid(modelloglogGP)
qqp(model4res, "norm")
#Hmm, it's closer. 

##Switching to NP, I give up on GP

#Make model - NP
model5<-aov(AvgNP~Genotype*Temperature, data=mydata)
anova(model5)
#Check assumptions
plot(model5)
model5res<-resid(model5)
qqp(model5res, "norm")
#Also not really normal

#Log data
mydata$logNP<-log(mydata$AvgNP)
#Make model
model6<-aov(logNP~Genotype*Temperature, data=mydata)
model6res<-resid(model6)
qqp(model6res, "norm")
#Same as untransformed

#Double log
mydata$loglogNP<-log(log(mydata$AvgNP+1))
model7<-aov(loglogNP~Genotype*Temperature, data=mydata)
model7res<-resid(model7)
qqp(model7res, "norm")
#Didn't change

#Try gamma distribution
gamma3<-fitdistr(mydata$AvgNP, "gamma")
qqp(mydata$AvgNP, "gamma", shape = gamma$estimate[[1]], rate=gamma$estimate[[2]])
