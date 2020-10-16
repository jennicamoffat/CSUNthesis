#Symbiodinium microadriaticum PnR 
#Created by Jennica Moffat 091019

#Clear the environment
rm(list=ls())
#load libraries
library(tidyverse)
library(car)
library(MASS)
library(RColorBrewer)
library(broman)

#Load October PnR data (August PnR is BAD data)
mydata<-read.csv("Data/OctoberPnR_r.csv")
mydata$Temperature<-as.factor(mydata$Temperature)
mydata<-mydata%>%
  mutate_if(is.character, as.factor)


#Adding columns for Resp/GP/NP per billion cells####
#Avg GP/Resp/NP_umol_L_min is average of the two wells minus the average of the control wells (DI)
#10^9*(value/avg count) = umol O2 per min per billion cells
mydata <- mutate(mydata, GP = 1000000000*(AvgGP_umol_L_min/CellsPerL),
                 NP=1000000000*(AvgNP_umol_L_min/CellsPerL),
                 Resp=1000000000*(AvgResp_umol_L_min/CellsPerL))
#GP graphs####
SummaryGP <- mydata %>%
  group_by(Genotype, Temperature) %>%
  summarize(mean=mean(GP, na.rm=TRUE), SE=sd(GP, na.rm=TRUE)/sqrt(length(na.omit(GP))))

pal<-c("#679A99", "#9DB462", "#E4C7E5") #blue, green, pink
GPGraph<-ggplot(SummaryGP, aes(x=Genotype, y=mean, fill=factor(Temperature), group=factor(Temperature)))+  #basic plot
  theme_bw()+ #Removes grey background
  scale_y_continuous(expand=c(0,0), limits=c(0, 5.5))+
  theme(plot.title = element_text(face = "bold", size=16),
        axis.text.x=element_text(color="black", size=11), 
        axis.text.y=element_text(color="black", size=12), 
        axis.title.x = element_text(color="black", size=16), 
        axis.title.y = element_text(color="black", size=16),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6, color="black") + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Symbiont Genotype", y=expression(GP~(µmol~O[2]~min^{"-1"}~10^{"9"}~cells^{"-1"})), fill="Temperature")+  #labels the x and y axes
  scale_fill_manual(values = pal, labels=c("26°C", "30°C","32°C"))+
  ggtitle("Gross Photosynthesis of Symbionts in Culture")
GPGraph+ggsave("Graphs/FinalGraphs/Culture_GPBar_Oct.png",width=8, height=5)

#GP Boxplot
#boxplot
culture.GP.boxplot<-mydata%>%
  ggplot(aes(x=Genotype, y=GP, fill=Temperature))+
  geom_boxplot()+
  theme_bw()+
  geom_jitter(color="black", size=0.5, alpha=0.7)+
  theme(axis.text.x=element_text(color="black", size=11), 
        axis.text.y=element_text(color="black", size=12), 
        axis.title.x = element_text(color="black", size=16), 
        axis.title.y = element_text(color="black", size=16),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_fill_manual(values = c("#679A99", "#9DB462", "#E4C7E5"), labels=c("26°C", "30°C","32°C"))+
  labs(fill="Temperature", x="Symbiont Genotype", y=expression(Gross~Photo.~(µmol~O[2]~min^{"-1"}~10^{"9"}~cells^{"-1"})))+
  ggtitle("Gross Photosynthesis of Symbionts in Culture")
culture.GP.boxplot+ggsave("Graphs/FinalGraphs/Culture_GPBox_Oct.png", width=8, height=5)

#NP graphs####
#Net Photo. NP = umol O2 per billion cells. 
SummaryNP <- mydata %>%
  group_by(Genotype, Temperature) %>%
  summarize(mean=mean(NP, na.rm=TRUE), SE=sd(NP, na.rm=TRUE)/sqrt(length(na.omit(NP))))

pal<-c("#ac8eab", "#f2cec7", "#c67b6f") #purples
pal<-c("#679A99", "#9DB462", "#E4C7E5") #blue, green, pink
NPGraph<-ggplot(SummaryNP, aes(x=Genotype, y=mean, fill=factor(Temperature), group=factor(Temperature)))+  #basic plot
  theme_bw()+ #Removes grey background
  scale_y_continuous(expand=c(0,0), limits=c(0, 7))+
  theme(axis.text.x=element_text(color="black", size=11), 
        axis.text.y=element_text(color="black", size=12), 
        axis.title.x = element_text(color="black", size=16), 
        axis.title.y = element_text(color="black", size=16),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6, color="black") + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  scale_fill_manual(values = pal, labels=c("26°C", "30°C","32°C"))+
  labs(x="Symbiont Genotype", y=expression(NP~(µmol~O[2]~min^{"-1"}~10^{"9"}~cells^{"-1"})), fill="Temperature")+
  ggtitle("Net Photosynthesis of Symbionts in Culture")
NPGraph+ggsave("Graphs/FinalGraphs/Culture_NPBar_Oct.png",width=8, height=5)

#NP Boxplot
culture.NP.boxplot<-mydata%>%
  ggplot(aes(x=Genotype, y=NP, fill=Temperature))+
  geom_boxplot()+
  theme_bw()+
  geom_jitter(color="black", size=0.5, alpha=0.7)+
  theme(axis.text.x=element_text(color="black", size=11), 
        axis.text.y=element_text(color="black", size=12), 
        axis.title.x = element_text(color="black", size=16), 
        axis.title.y = element_text(color="black", size=16),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_fill_manual(values = c("#679A99", "#9DB462", "#E4C7E5"), labels=c("26°C", "30°C","32°C"))+
  labs(fill="Temperature", x="Symbiont Genotype", y=expression(Net~Photo.~(µmol~O[2]~min^{"-1"}~10^{"9"}~cells^{"-1"})))+
  ggtitle("Net Photosynthesis of Symbionts in Culture")
culture.NP.boxplot+ggsave("Graphs/FinalGraphs/Culture_NPBox_Oct.png",width=8, height=5)

#Respiration graphs####
SummaryResp <- mydata %>%
  group_by(Genotype, Temperature) %>%
  summarize(mean=mean(Resp, na.rm=TRUE), SE=sd(Resp, na.rm=TRUE)/sqrt(length(na.omit(Resp))))

pal<-c("#679A99", "#9DB462", "#E4C7E5") #blue, green, pink
RespGraph<-ggplot(SummaryResp, aes(x=Genotype, y=mean, fill=factor(Temperature), group=factor(Temperature)))+  #basic plot
  theme_bw()+ #Removes grey background
  scale_y_continuous(expand=c(0,0), limits=c(-1.8,0))+
  theme(axis.text.x=element_text(color="black", size=11), 
        axis.text.y=element_text(color="black", size=12), 
        axis.title.x = element_text(color="black", size=16), 
        axis.title.y = element_text(color="black", size=16),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6, color="black") + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  scale_fill_manual(values = pal, labels=c("26°C", "30°C","32°C"))+
  labs(x="Symbiont Genotype", y=expression(Respiration~(µmol~O[2]~min^{"-1"}~10^{"9"}~cells^{"-1"})), fill="Temperature")+  #labels the x and y axes
  ggtitle("Respiration of Symbionts in Culture")
RespGraph+ggsave("Graphs/FinalGraphs/Culture_RespBar_Oct.png",width=8, height=5)

#Respiration boxplot
culture.Resp.boxplot<-mydata%>%
  ggplot(aes(x=Genotype, y=Resp, fill=Temperature))+
    geom_boxplot()+
  theme_bw()+
  geom_jitter(color="black", size=0.5, alpha=0.7)+
  theme(axis.text.x=element_text(color="black", size=11), 
        axis.text.y=element_text(color="black", size=12), 
        axis.title.x = element_text(color="black", size=16), 
        axis.title.y = element_text(color="black", size=16),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_fill_manual(values = c("#679A99", "#9DB462", "#E4C7E5"), labels=c("26°C", "30°C","32°C"))+
  labs(fill="Temperature", x="Symbiont Genotype", y=expression(Respiration~(µmol~O[2]~min^{"-1"}~10^{"9"}~cells^{"-1"})))+
  ggtitle("Respiration of Symbionts in Culture")
culture.Resp.boxplot+ggsave("Graphs/FinalGraphs/Culture_RespBox_Oct.png",width=8, height=5)

#October PnR Stats#####

#Genotype (fixed) and Temperature (fixed) on dependent variables (NP, GP, and Resp)
#Clear the environment
rm(list=ls())
#Load PnR data
mydata<-read.csv("Data/OctoberPnR_r.csv")
mydata$Temperature<-as.factor(mydata$Temperature)

mydata <- mutate(mydata, GP = 1000000000*(AvgGP_umol_L_min/CellsPerL),
                 NP=1000000000*(AvgNP_umol_L_min/CellsPerL),
                 Resp=1000000000*(AvgResp_umol_L_min/CellsPerL))

model1<-lm(Resp~Genotype*Temperature, data=mydata)
#Check assumptions
model1res<-resid(model1)
qqp(model1res, "norm")
#The ends are out of the CI 

summary(mydata$Resp)
#Ranges from -2.2 to -0.2
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
full.resp.model<-lm(PosResp ~ Genotype*Temperature, data = mydata)
step.resp.model<-stepAIC(full.resp.model, direction="both", trace = F)

boxcox<-boxcox(step.resp.model,lambda = seq(-5, 5, 1/1000),plotit = TRUE )

Selected.Power<-boxcox$x[boxcox$y==max(boxcox$y)]
Selected.Power
#3.052

mydata$cubedResp<-(mydata$PosResp)^3.052
hist(mydata$Resp)
hist(mydata$PosResp)
hist(mydata$cubedResp)

cubed.resp.model<-lm(cubedResp~Genotype*Temperature, data = mydata)
qqp(resid(cubed.resp.model), "norm")
#Hey that's pretty damn close!
plot(cubed.resp.model)
Anova(cubed.resp.model, type="III")

cubed.resp.model.simp<-lm(cubedResp~Genotype+Temperature, data = mydata)
lrtest(cubed.resp.model, cubed.resp.model.simp)
#Sig=use full model

#Net Photo. NP = umol O2 per billion cells. ####

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
#That's normal. Just one outlier
Anova(quadrt.NP.model, type="III")

NP.model.simp<-lm(fourthrtNP~Genotype+Temperature, data=mydata)
lrtest(quadrt.NP.model, NP.model.simp)
#Sig=use full model

#Gross photosynthesis. GP = gross photo umol O2 per billion cells####
model1<-lm(GP~Genotype*Temperature, data=mydata)
#Check assumptions
model1res<-resid(model1)
qqp(model1res, "norm")

mydata$sqrtGP<-sqrt(mydata$GP)
sqrtGPmodel<-lm(sqrtGP~Genotype*Temperature, data=mydata)
qqp(resid(sqrtGPmodel), "norm")
plot(sqrtGPmodel)
#That's normal
Anova(sqrtGPmodel, type="III")
GP.model.simp<-lm(sqrtGP~Genotype+Temperature, data=mydata)
lrtest(sqrtGPmodel, GP.model.simp)
#Sig = use ful model
