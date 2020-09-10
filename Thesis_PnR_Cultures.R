#Symbiodinium microadriaticum PnR 
#Created by Jennica Moffat 091019

#Clear the environment
rm(list=ls())
#August PnR - DO NOT USE. Photorespirometer was not calibrated properly.####
#Load PnR data
mydata<-read.csv("Data/AugustPnR_r_github.csv")
View(mydata)

#load libraries
library(tidyverse)
library(car)
library(MASS)
library(RColorBrewer)
library(broman)

#Current PnR values are actually X umol of O2 per 10,000,000 (or 10^7) cells  (per minute)
#To get it to be a more reasonable number on the x-axis, and per 100,000 cells instead, 
#multiply the values by 100 (turns PER 10^7 cells into PER 10^5 cells)

mydata$AvgRespPer100000<-mydata$AvgResp*100
mydata$AvgNPPer100000<-100*mydata$AvgNP
mydata$AvgGPPer100000<-100*mydata$AvgGP


#All data Graphs
#Summarizing data for bargraph
#Net photosynthesis
SummaryNP <- mydata %>%
  group_by(Genotype, Temperature) %>%
  summarize(mean=mean(AvgNP, na.rm=TRUE), SE=sd(AvgNP, na.rm=TRUE)/sqrt(length(na.omit(AvgNP))))
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
  summarize(mean=mean(AvgResp, na.rm=TRUE), SE=sd(AvgResp, na.rm=TRUE)/sqrt(length(na.omit(AvgResp))))
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
  summarize(mean=mean(AvgGP, na.rm=TRUE), SE=sd(AvgGP, na.rm=TRUE)/sqrt(length(na.omit(AvgGP))))
SummaryGP

GPGraph<-ggplot(SummaryGP, aes(x=Genotype, y=mean, fill=factor(Temperature), group=factor(Temperature)))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(axis.text.x=element_text(color="black", size=14), axis.text.y=element_text(face="bold", color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=16), axis.title.y = element_text(face="bold", color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Genotype", y="Gross Photo. per 10,000 cells", fill="Temperature")+  #labels the x and y axes
  scale_fill_brewer(palette = "RdYlBu")
GPGraph

#August - Making new dataframe without 32*####
mydata2 <- subset(mydata, Temperature == 26 | Temperature == 30)
View(mydata2)

SummaryGPno32 <- mydata2 %>%
  group_by(Genotype, Temperature) %>%
  summarize(mean=mean(AvgGPPer100000, na.rm=TRUE), SE=sd(AvgGPPer100000, na.rm=TRUE)/sqrt(length(na.omit(AvgGPPer100000))))
SummaryGPno32

GPGraphno32<-ggplot(SummaryGPno32, aes(x=Genotype, y=mean, fill=factor(Temperature), group=factor(Temperature)))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(plot.title = element_text(face = "bold", size=18), axis.text.x=element_text(color="black", size=13), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=14), axis.title.y = element_text(face="bold", color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  scale_y_continuous(expand=c(0,0), limits=c(0, 9.5))+
  geom_bar(stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Genotype", y=expression(Gross~Photosynthesis~(µmol~O[2]/10^{"5"}~cells/min)), fill="Temperature")+  #labels the x and y axes
  scale_fill_manual(values = c("skyblue3", "darkgoldenrod2"), labels=c("26°C", "30°C"))+
  ggtitle("Gross Photosynthesis of Symbiont Strains at 26°C and 30°C")+
  ggsave("Graphs/PnR/GPGraph_no32.pdf", width=10, height=6.19, dpi=300, unit="in")
GPGraphno32
 

#NetPhoto -- No 32
SummaryNPno32 <- mydata2 %>%
  group_by(Genotype, Temperature) %>%
  summarize(mean=mean(AvgNPPer100000, na.rm=TRUE), SE=sd(AvgNPPer100000, na.rm=TRUE)/sqrt(length(na.omit(AvgNPPer100000))))
SummaryNPno32

NPGraphno32<-ggplot(SummaryNPno32, aes(x=Genotype, y=mean, fill=factor(Temperature), group=factor(Temperature)))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(plot.title = element_text(face = "bold", size=18), axis.text.x=element_text(color="black", size=13), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=14), axis.title.y = element_text(face="bold", color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  scale_y_continuous(expand=c(0,0), limits=c(0,13)) +
  geom_bar(stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Genotype", y=expression(Net~Photosynthesis~(µmol~O[2]/10^{"5"}~cells/min)), fill="Temperature")+  #labels the x and y axes
  scale_fill_manual(values = c("skyblue3", "darkgoldenrod2"), labels=c("26°C", "30°C"))+
  ggtitle("Net Photosynthesis of Symbiont Strains at 26°C and 30°C")+
  ggsave("Graphs/PnR/NPGraph_no32.pdf", width=11, height=6.19, dpi=300, unit="in")
NPGraphno32

#Respiration -- No 32
SummaryRespNo32 <- mydata2 %>%
  group_by(Genotype, Temperature) %>%
  summarize(mean=mean(AvgRespPer100000, na.rm=TRUE), SE=sd(AvgRespPer100000, na.rm=TRUE)/sqrt(length(na.omit(AvgRespPer100000))))
SummaryRespNo32

RespGraphno32<-ggplot(SummaryRespNo32, aes(x=Genotype, y=mean, fill=factor(Temperature), group=factor(Temperature)))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(plot.title = element_text(face = "bold", size=18), axis.text.x=element_text(color="black", size=13), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=14), axis.title.y = element_text(face="bold", color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  scale_y_continuous(expand=c(0,0), limits=c(-3.6, 0))+
  geom_bar(stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Genotype", y=expression(Respiration~(µmol~O[2]/10^{"5"}~cells/min)), fill="Temperature")+  #labels the x and y axes
  scale_fill_manual(values = c("skyblue3", "darkgoldenrod2"), labels=c("26°C", "30°C"))+
  ggtitle("Respiration of Symbiont Strains at 26°C and 30°C")+
  ggsave("Graphs/PnR/RespGraph_no32.pdf", width=11, height=6.19, dpi=300, unit="in")
RespGraphno32


#August - Alright time to start some actual stats...####

#Clear the environment
rm(list=ls())
#Load PnR data
mydata<-read.csv("AugustPnR_r_github.csv")
View(mydata)
#Genotype (fixed) and Temperature (fixed) on dependent variables (NP, GP, and Resp)

#Current PnR values are actually X umol of O2 per 10,000,000 (or 10^7) cells  (per minute)
#To get it to be a more reasonable number on the x-axis, and per 100,000 cells instead, 
#multiply the values by 100 (turns PER 10^7 cells into PER 10^5 cells)

mydata$AvgRespPer100000<-mydata$AvgResp*100
mydata$AvgNPPer100000<-100*mydata$AvgNP
mydata$AvgGPPer100000<-100*mydata$AvgGP


#August - Respiration####
model1<-aov(AvgRespPerCell~Genotype*Temperature, data=mydata)
anova(model1)
#Check assumptions
plot(model1)
qqp(resid(model1), "norm")
#OMG IT'S NORMAL

#August - GP####
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

#August - Switching to NP, I give up on GP####

#Make model - NP
model5<-aov(AvgNPPerCell~Genotype*Temperature, data=mydata)
anova(model5)
#Check assumptions
plot(model5)
qqp(resid(model5), "norm")
#Also not really normal
hist(mydata$AvgNPPerCell)

#Try with bigger numbers. Shouldn't be different, though. 
NP.model<-aov(AvgNPPer100000~Genotype*Temperature, data=mydata)
qqp(resid(NP.model),"norm")
#Okay, yes it is the same. 

#Log data
mydata$logNP<-log(mydata$AvgNPPer100000)
#Make model
model6<-aov(logNP~Genotype*Temperature, data=mydata)
model6res<-resid(model6)
qqp(model6res, "norm")
#Same as untransformed

#Double log
mydata$loglogNP<-log(mydata$logNP)
model7<-aov(loglogNP~Genotype*Temperature, data=mydata)
model7res<-resid(model7)
qqp(model7res, "norm")
#Closer...
View(mydata)
#Triple logged
mydata$log3NP<-log(mydata$loglogNP+3)
log3.NP.model<-aov(log3NP~Genotype*Temperature, data=mydata)
qqp(resid(log3.NP.model), "norm")
#That's normal...shit ton of transformations but whatever. 

anova(log3.NP.model)


#August - Stats for 26 and 30 degree only####
#Clear the environment
rm(list=ls())
#Load PnR data
mydata<-read.csv("AugustPnR_r_github.csv")
View(mydata)
#Genotype (fixed) and Temperature (fixed) on dependent variables (NP, GP, and Resp)

#new dataframe without 32
mydata2 <- subset(mydata, Temperature == 26 | Temperature == 30)


#August - Respiration no 32####
model1<-aov(AvgResp~Genotype*Temperature, data=mydata2)
anova(model1)
#Check assumptions
plot(model1)
model1res<-resid(model1)
qqp(model1res, "norm")
#Almost normal 
shapiro.test(model1res)
#Shapiro says it's not normal. P-value of 0.0007

#Histogram just to see what it looks like
hist(mydata2$AvgResp, col="blue", xlab="Average Respiration")
#Weighted heavily closer to 0, evenly distributed from -0.015 to -0.04


#Log transform data?
mydata2$logAvgResp<-log(mydata2$AvgResp+1)
#Make model - Respiration
model2<-aov(logAvgResp~Genotype*Temperature, data=mydata2)
anova(model2)
#Check assumptions
plot(model2)
model2res<-resid(model2)
qqp(model2res, "norm")
shapiro.test(model2res)
#Shapiro says it's not normal. P-value of 0.0006

#Square root
mydata2$sqrtAvgResp<-sqrt(mydata2$AvgResp+0.1)
#Make model - Respiration
model3<-aov(sqrtAvgResp~Genotype*Temperature, data=mydata2)
anova(model3)
#Check assumptions
plot(model3)
model3res<-resid(model3)
qqp(model3res, "norm")
shapiro.test(model3res)
#Keeps getting less normal...

#Fourth root
mydata2$fourthrtAvgResp<-sqrt(sqrt(mydata2$AvgResp+0.1))
#Make model - Respiration
model3<-aov(fourthrtAvgResp~Genotype*Temperature, data=mydata2)
anova(model3)
#Check assumptions
plot(model3)
model3res<-resid(model3)
qqp(model3res, "norm")
shapiro.test(model3res)
#Less normal than square root. 

#Squared?
mydata2$sqrdAvgResp<-(mydata2$AvgResp*mydata2$AvgResp)
model4<-aov(sqrdAvgResp~Genotype*Temperature, data=mydata2)
model4res<-resid(model4)
qqp(model4res, "norm")
shapiro.test(model4res)
#HAH absolutely not.

#convert to z scores
zscoreResp<-scale(mydata$AvgResp, center=TRUE, scale=TRUE) #Center centers the data on the mean (subtracts mean); Scale divides by s.d.
hist(zscoreResp)
#that did not help

#Okay, time to try other distributions.
#Lognormal distribution
qqp(mydata2$AvgResp, "lnorm")
#Worse than normal I think

#Gamma. can't do negatives so gonna add 1 to everything
mydata2$AvgResp1<-mydata2$AvgResp+1
gamma<-fitdistr(mydata2$AvgResp1, "gamma")
qqp(mydata2$AvgResp1, "gamma", shape = gamma$estimate[[1]], rate=gamma$estimate[[2]])
#Actually better than normal.

#Tried poisson, worse than all the others. 
#Gonna stick with Gamma

#So now let's fit a model:
library(lme4)
#Full model - gamma
model1<-glm(AvgResp1~Genotype*Temperature, family=Gamma(link="inverse"), data=mydata2)
anova(model1)
summary(model1)
#I'm an idiot and can't remember how to make sense of this output. I think I'm just
#going to say it's "normal enough" for now and move on. 


#August - GP no 32 ####
model2<-aov(AvgGP~Genotype*Temperature, data=mydata2)
anova(model2)
#Check assumptions
plot(model2)
model2res<-resid(model2)
qqp(model2res, "norm")
#Same as respiration. Close enough, I guess?
hist(mydata2$AvgGP, col="salmon", xlab="Average Gross Photo")
#weighted towards 0

#Transformations
#Square root
mydata2$sqrtAvgGP<-sqrt(mydata2$AvgGP)
#Make model - Gross photo
model3<-aov(sqrtAvgGP~Genotype*Temperature, data=mydata2)
model3res<-resid(model3)
qqp(model3res, "norm")
#Wow looks great. Just one outlier (#19)
#Check assumptions
plot(model3)
anova(model3)



#August - NP no 32 ####
model4<-aov(AvgNP~Genotype*Temperature, data=mydata2)
model4res<-resid(model4)
qqp(model4res, "norm")

hist(mydata2$AvgNP, col="salmon", xlab="Average Gross Photo")

#Transformations
#Square root
mydata2$sqrtAvgNP<-sqrt(mydata2$AvgNP)
#Make model - Net photo
model5<-aov(sqrtAvgNP~Genotype*Temperature, data=mydata2)
model5res<-resid(model5)
qqp(model5res, "norm")
#Wow looks great again! Just one outlier (#19)
#Check assumptions
plot(model5)
anova(model5)



#October PnR#####
#August PnR had weird issue with 32* because the calibration was not set up for 32*
#So I had to rerun it in October using one-point calibration
#Clear the environment
rm(list=ls())
#Load PnR data
mydata<-read.csv("Data/OctoberPnR_r.csv")
mydata$Temperature<-as.factor(mydata$Temperature)

#October PnR graphs#####
#Adding columns for Resp/GP/NP per billion cells
#Avg GP/Resp/NP_umol_L_min is average of the two wells minus the average of the control wells (DI)
#10^9*(value/avg count) = umol O2 per min per billion cells
mydata <- mutate(mydata, GP = 1000000000*(AvgGP_umol_L_min/CellsPerL),
                 NP=1000000000*(AvgNP_umol_L_min/CellsPerL),
                 Resp=1000000000*(AvgResp_umol_L_min/CellsPerL))

SummaryGP <- mydata %>%
  group_by(Genotype, Temperature) %>%
  summarize(mean=mean(GP, na.rm=TRUE), SE=sd(GP, na.rm=TRUE)/sqrt(length(na.omit(GP))))

pal<-c("#ac8eab", "#f2cec7", "#c67b6f") #purples
pal<-c("skyblue3", "darkgoldenrod2", "firebrick3")
GPGraph<-ggplot(SummaryGP, aes(x=Genotype, y=mean, fill=factor(Temperature), group=factor(Temperature)))+  #basic plot
  theme_bw()+ #Removes grey background
  scale_y_continuous(expand=c(0,0), limits=c(0, 5.5))+
  theme(plot.title = element_text(face = "bold", size=16),
        axis.text.x=element_text(color="black", size=11), 
        axis.text.y=element_text(color="black", size=12), 
        axis.title.x = element_text(color="black", size=16), 
        axis.title.y = element_text(color="black", size=16),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Genotype", y=expression(GP~(µmol~O[2]~min^{"-1"}~10^{"9"}~cells^{"-1"})), fill="Temperature")+  #labels the x and y axes
  scale_fill_manual(values = pal, labels=c("26°C", "30°C","32°C"))+
  ggtitle("Gross Photosynthesis of Symbionts in Culture")
GPGraph+ggsave("Graphs/PnR/OctCulturePnR/GPGraph_Oct.png",width=8, height=5)

#GP Boxplot
#boxplot
culture.GP.boxplot<-mydata%>%
  ggplot(aes(x=Genotype, y=GP, fill=Temperature))+
  geom_boxplot()+
  theme_bw()+
  geom_jitter(color="black", size=0.5, alpha=0.7)+
  ggtitle("Gross Photosynthesis of Cultures")+
  theme(plot.title = element_text(face="bold"), 
        axis.text.x=element_text(size=10), 
        axis.text.y=element_text(size=10), 
        axis.title.y = element_text(face="bold", size=12), 
        axis.title.x = element_text(face="bold", size=12))+
  labs(fill="Temperature", x="Genotype", y=expression(Gross~Photo.~(µmol~O[2]~min^{"-1"}~10^{"9"}~cells^{"-1"})))+
  scale_fill_manual(values = c("#679A99", "#9DB462", "#E4C7E5"), labels=c("26°C", "30°C","32°C"))
culture.GP.boxplot+ggsave("Graphs/PnR/OctCulturePnR/Oct_GP_Boxplot.png", width=8, height=5)

#Net Photo. NP = umol O2 per billion cells. 
SummaryNP <- mydata %>%
  group_by(Genotype, Temperature) %>%
  summarize(mean=mean(NP, na.rm=TRUE), SE=sd(NP, na.rm=TRUE)/sqrt(length(na.omit(NP))))
SummaryNP

pal<-c("#ac8eab", "#f2cec7", "#c67b6f") #purples
pal<-c("skyblue3", "darkgoldenrod2", "firebrick3")
NPGraph<-ggplot(SummaryNP, aes(x=Genotype, y=mean, fill=factor(Temperature), group=factor(Temperature)))+  #basic plot
  theme_bw()+ #Removes grey background
  scale_y_continuous(expand=c(0,0), limits=c(0, 7))+
  theme(plot.title = element_text(face = "bold", size=16),
        axis.text.x=element_text(color="black", size=11), 
        axis.text.y=element_text(color="black", size=12), 
        axis.title.x = element_text(color="black", size=16), 
        axis.title.y = element_text(color="black", size=16),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Genotype", y=expression(NP~(µmol~O[2]~min^{"-1"}~10^{"9"}~cells^{"-1"})), fill="Temperature")+  #labels the x and y axes
  scale_fill_manual(values = pal, labels=c("26°C", "30°C","32°C"))+
  ggtitle("Net Photosynthesis of Symbionts in Culture")
NPGraph
NPGraph+ggsave("Graphs/PnR/OctCulturePnR/NPGraph_Oct.png",width=8, height=5)

#NP Boxplot
culture.NP.boxplot<-mydata%>%
  ggplot(aes(x=Genotype, y=NP, fill=Temperature))+
  geom_boxplot()+
  theme_bw()+
  geom_jitter(color="black", size=0.5, alpha=0.7)+
  ggtitle("Net Photosynthesis of Cultures")+
  theme(plot.title = element_text(face="bold"), 
        axis.text.x=element_text(size=10), 
        axis.text.y=element_text(size=10), 
        axis.title.y = element_text(face="bold", size=12), 
        axis.title.x = element_text(face="bold", size=12))+
  labs(fill="Temperature", x="Genotype", y=expression(Net~Photo.~(µmol~O[2]~min^{"-1"}~10^{"9"}~cells^{"-1"})))+
  scale_fill_manual(values = c("#679A99", "#9DB462", "#E4C7E5"), labels=c("26°C", "30°C","32°C"))
culture.NP.boxplot+ggsave("Graphs/PnR/OctCulturePnR/Oct_NP_boxplot.png",width=10, height=5)

#Respiration
SummaryResp <- mydata %>%
  group_by(Genotype, Temperature) %>%
  summarize(mean=mean(Resp, na.rm=TRUE), SE=sd(Resp, na.rm=TRUE)/sqrt(length(na.omit(Resp))))
SummaryResp

pal<-c("skyblue3", "darkgoldenrod2", "firebrick3")
RespGraph<-ggplot(SummaryResp, aes(x=Genotype, y=mean, fill=factor(Temperature), group=factor(Temperature)))+  #basic plot
  theme_bw()+ #Removes grey background
  scale_y_continuous(expand=c(0,0), limits=c(-1.8,0))+
  theme(plot.title = element_text(face = "bold", size=16),
        axis.text.x=element_text(color="black", size=11), 
        axis.text.y=element_text(color="black", size=12), 
        axis.title.x = element_text(color="black", size=16), 
        axis.title.y = element_text(color="black", size=16),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Genotype", y=expression(Respiration~(µmol~O[2]~min^{"-1"}~10^{"9"}~cells^{"-1"})), fill="Temperature")+  #labels the x and y axes
  scale_fill_manual(values = pal, labels=c("26°C", "30°C","32°C"))+
  ggtitle("Respiration of Symbionts in Culture")
RespGraph+ggsave("Graphs/PnR/OctCulturePnR/RespGraph_Oct.png",width=8, height=5)

#Respiration boxplot
culture.Resp.boxplot<-mydata%>%
  ggplot(aes(x=Genotype, y=Resp, fill=Temperature))+
  geom_boxplot()+
  theme_bw()+
  ggtitle("Respiration of Cultures")+
  theme(plot.title = element_text(face="bold"), 
        axis.text.x=element_text(size=10), 
        axis.text.y=element_text(size=10), 
        axis.title.y = element_text(face="bold", size=12), 
        axis.title.x = element_text(face="bold", size=12))+
  labs(fill="Temperature", x="Genotype", y=expression(Respiration~(µmol~O[2]~min^{"-1"}~10^{"9"}~cells^{"-1"})))+
  scale_fill_manual(values = c("#679A99", "#9DB462", "#E4C7E5"), labels=c("26°C", "30°C","32°C"))
culture.Resp.boxplot+ggsave("Graphs/PnR/OctCulturePnR/Oct_Resp_Boxplot.png",width=10, height=5)

#October PnR Stats#####

#Genotype (fixed) and Temperature (fixed) on dependent variables (NP, GP, and Resp)
#Respiration
#Right now, data is really small number per 1000 cells. To get it to be 
#a more resonible number, times it by 1,000,000 to get resp per billion cells. 
mydata$Resp<-1000000*mydata$AvgRespPer1000Cell

model1<-lm(Resp~Genotype*Temperature, data=mydata)
#Check assumptions
model1res<-resid(model1)
qqp(model1res, "norm")
#The ends are out of the CI 

#Log transform
mydata$logResp<-log(mydata$Resp+3)
log.Resp.model<-lm(logResp~Genotype*Temperature, data=mydata)
qqp(resid(log.Resp.model), "norm")
#Different, but not better?

#Square root
mydata$sqrtResp<-sqrt(mydata$Resp+3)
sqrt.Resp.model<-lm(sqrtResp~Genotype*Temperature, data=mydata)
qqp(resid(sqrt.Resp.model), "norm")
#Not better

#Fourth root
mydata$quadrtResp<-sqrt(mydata$sqrtResp)
quardrt.Resp.model<-lm(quadrtResp~Genotype*Temperature, data=mydata)
qqp(resid(quardrt.Resp.model), "norm")
#Worse than square. I'm just gonna run it with square root for now.
anova(sqrt.Resp.model)

#Trying generalized linear model. 
#lognormal distribution
qqp(mydata$Resp, "lnorm")
#No

gamma<-fitdistr(mydata$Resp, "gamma")
qqp(mydata$Resp, "gamma", shape = gamma$estimate[[1]], rate=gamma$estimate[[2]])
#Doesn't work with negatives 

#Try negative binomial
nbinom <- fitdistr(mydata$Resp, "Negative Binomial")
qqp(InocData$Days.to.Inoculation, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])
#Doesn't work with negatives or non-integars

#Try poisson
poisson <- fitdistr(mydata$Resp, "Poisson")
qqp(InocData$Days.to.Inoculation, "pois", poisson$estimate, lambda=8)
#Doesn't work with negatives or non-integars

#BoxCox transformation
#Response variable must be positive. Adding 3
mydata$PosResp<-mydata$Resp+3
View(mydata)
full.resp.model<-lm(PosResp ~ Genotype*Temperature, data = mydata)
step.resp.model<-stepAIC(full.resp.model, direction="both", trace = F)

boxcox<-boxcox(step.resp.model,lambda = seq(-5, 5, 1/1000),plotit = TRUE )

Selected.Power<-boxcox$x[boxcox$y==max(boxcox$y)]
Selected.Power
#3.051

mydata$cubedResp<-(mydata$PosResp)^3.051
hist(mydata$Resp)
hist(mydata$PosResp)
hist(mydata$cubedResp)

cubed.resp.model<-lm(cubedResp~Genotype*Temperature, data = mydata)
qqp(resid(cubed.resp.model), "norm")
#Hey that's pretty damn close!
plot(cubed.resp.model)
Anova(cubed.resp.model, type="III")

#Net Photo. NP = umol O2 per billion cells. ####
mydata$NP<-1000000*mydata$AvgNPPer1000Cell

NP.model<-lm(NP~Genotype*Temperature, data=mydata)
#Check assumptions
qqp(resid(NP.model), "norm")

#Square root
mydata$sqrtNP<-sqrt(mydata$NP)
sqrt.NP.model<-lm(sqrtNP~Genotype*Temperature, data=mydata)
qqp(resid(sqrt.NP.model), "norm")
#Better

#Fourth root
mydata$fourthrtNP<-sqrt(sqrt(mydata$NP))
quadrt.NP.model<-lm(fourthrtNP~Genotype*Temperature, data=mydata)
qqp(resid(quadrt.NP.model), "norm")
#That's pretty much normal. Just one outlier
Anova(quadrt.NP.model, type="III")


#Gross photosynthesis. GP = gross photo umol O2 per billion cells####
mydata$GP<-1000000*mydata$AvgGPPer1000Cell

model1<-lm(GP~Genotype*Temperature, data=mydata)
#Check assumptions
model1res<-resid(model1)
qqp(model1res, "norm")

#Fourth root
mydata$fourthrtGP<-sqrt(sqrt(mydata$GP))
model1<-lm(fourthrtGP~Genotype*Temperature, data=mydata)
model1res<-resid(model1)
qqp(model1res, "norm")
#That's normal
Anova(model1, type="III")
