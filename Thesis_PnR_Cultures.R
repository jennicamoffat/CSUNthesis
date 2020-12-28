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
library(lmtest)

#Load October PnR data (August PnR is BAD data)
mydata_wrong<-read.csv("Data/OctoberPnR_r.csv")
mydata_wrong$Temperature<-as.factor(mydata_wrong$Temperature)
mydata_wrong<-mydata_wrong%>%
  mutate_if(is.character, as.factor)

#Dec 23, 2020: Turns out I got GP and NP mixed up. 
#I had the light slopes (after subtracting the blanks and averaging) as GP instead of NP
#And calculated NP as GP-Resp, and since resp is negative, it was GP+|Resp|
#But, the light slope should be NP and GP=NP-Resp AKA NP+|Resp| (GP must be greater than NP!)
#So, I calculated it correctly, NP and GP are just switched. So rather than switching them in the data, I'm gonna switch them here so I know what I did
mydata<-mydata_wrong %>% 
  rename(
    AvgNP_umol_L_min_fixed = AvgGP_umol_L_min,
    AvgGP_umol_L_min_fixed = AvgNP_umol_L_min,
    AvgNPPer1000Cell_fixed = AvgGPPer1000Cell,
    AvgGPPer1000Cell_fixed = AvgNPPer1000Cell
    )
#Double checking that I did that correctly.
summary(mydata_wrong$AvgGP_umol_L_min)
summary(mydata$AvgNP_umol_L_min_fixed)
#Same. Good. 
summary(mydata_wrong$AvgNP_umol_L_min)
summary(mydata$AvgGP_umol_L_min_fixed)
#Same
summary(mydata_wrong$AvgGPPer1000Cell)
summary(mydata$AvgNPPer1000Cell_fixed)
#Same
summary(mydata_wrong$AvgNPPer1000Cell)
summary(mydata$AvgGPPer1000Cell_fixed)
#Same. 
#So GP and NP has been fixed. mydata is correct, all the following calculations will now be done with the correct GP and NP.

#Adding columns for Resp/GP/NP per billion cells####
#Avg GP/Resp/NP_umol_L_min is average of the two wells minus the average of the control wells (DI)
#10^9*(value/avg count) = umol O2 per min per billion cells
mydata <- mutate(mydata, GP = 1000000000*(AvgGP_umol_L_min_fixed/CellsPerL),
                 NP=1000000000*(AvgNP_umol_L_min_fixed/CellsPerL),
                 Resp=1000000000*(AvgResp_umol_L_min/CellsPerL))
#GP graphs####
SummaryGP <- mydata %>%
  group_by(Genotype, Temperature) %>%
  summarize(mean=mean(GP, na.rm=TRUE), SE=sd(GP, na.rm=TRUE)/sqrt(length(na.omit(GP))))

pal<-c("#679A99", "#9DB462", "#E4C7E5") #blue, green, pink
GPGraph<-ggplot(SummaryGP, aes(x=Genotype, y=mean, fill=factor(Temperature), group=factor(Temperature)))+  #basic plot
  theme_bw()+ #Removes grey background
  scale_y_continuous(expand=c(0,0), limits=c(0, 7))+
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
GPGraph+ggsave("Graphs/FinalGraphs/Culture_GP_Oct_Bar.png",width=8, height=5)

#GP Boxplot
#boxplot
culture.GP.boxplot<-mydata%>%
  ggplot(aes(x=Genotype, y=GP, fill=Temperature))+
  geom_boxplot()+
  theme_bw()+
  geom_point(pch=21, position=position_jitterdodge(jitter.width=0.1), size=1)+
  theme(plot.title = element_text(face = "bold", size=16),
        axis.text.x=element_text(color="black", size=11), 
        axis.text.y=element_text(color="black", size=12), 
        axis.title.x = element_text(color="black", size=16), 
        axis.title.y = element_text(color="black", size=16),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_fill_manual(values = c("#679A99", "#9DB462", "#E4C7E5"), labels=c("26°C", "30°C","32°C"))+
  labs(fill="Temperature", x="Symbiont Genotype", y=expression(Gross~Photo.~(µmol~O[2]~min^{"-1"}~10^{"9"}~cells^{"-1"})))+
  ggtitle("Gross Photosynthesis of Symbionts in Culture")
culture.GP.boxplot+ggsave("Graphs/FinalGraphs/Culture_GP_Oct_Box.png", width=8, height=5)
#GP Boxplot no jitter
culture.GP.boxplot.nojitter<-mydata%>%
  ggplot(aes(x=Genotype, y=GP, fill=Temperature))+
  geom_boxplot()+
  theme_bw()+
  theme(plot.title = element_text(face = "bold", size=16),
        axis.text.x=element_text(color="black", size=11), 
        axis.text.y=element_text(color="black", size=12), 
        axis.title.x = element_text(color="black", size=16), 
        axis.title.y = element_text(color="black", size=16),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_fill_manual(values = c("#679A99", "#9DB462", "#E4C7E5"), labels=c("26°C", "30°C","32°C"))+
  labs(fill="Temperature", x="Symbiont Genotype", y=expression(Gross~Photo.~(µmol~O[2]~min^{"-1"}~10^{"9"}~cells^{"-1"})))+
  ggtitle("Gross Photosynthesis of Symbionts in Culture")
culture.GP.boxplot.nojitter+ggsave("Graphs/FinalGraphs/Culture_GP_Oct_Box_nojitter.png", width=8, height=5)

#NP graphs####
#Net Photo. NP = umol O2 per billion cells. 
SummaryNP <- mydata %>%
  group_by(Genotype, Temperature) %>%
  summarize(mean=mean(NP, na.rm=TRUE), SE=sd(NP, na.rm=TRUE)/sqrt(length(na.omit(NP))))

pal<-c("#ac8eab", "#f2cec7", "#c67b6f") #purples
pal<-c("#679A99", "#9DB462", "#E4C7E5") #blue, green, pink
NPGraph<-ggplot(SummaryNP, aes(x=Genotype, y=mean, fill=factor(Temperature), group=factor(Temperature)))+  #basic plot
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
  scale_fill_manual(values = pal, labels=c("26°C", "30°C","32°C"))+
  labs(x="Symbiont Genotype", y=expression(NP~(µmol~O[2]~min^{"-1"}~10^{"9"}~cells^{"-1"})), fill="Temperature")+
  ggtitle("Net Photosynthesis of Symbionts in Culture")
NPGraph+ggsave("Graphs/FinalGraphs/Culture_NP_Oct_Bar.png",width=8, height=5)

#NP Boxplot
culture.NP.boxplot<-mydata%>%
  ggplot(aes(x=Genotype, y=NP, fill=Temperature))+
  geom_boxplot()+
  theme_bw()+
  geom_point(pch=21, position=position_jitterdodge(jitter.width=0.1), size=1)+
  theme(plot.title = element_text(face = "bold", size=16),
        axis.text.x=element_text(color="black", size=11), 
        axis.text.y=element_text(color="black", size=12), 
        axis.title.x = element_text(color="black", size=16), 
        axis.title.y = element_text(color="black", size=16),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_fill_manual(values = c("#679A99", "#9DB462", "#E4C7E5"), labels=c("26°C", "30°C","32°C"))+
  labs(fill="Temperature", x="Symbiont Genotype", y=expression(Net~Photo.~(µmol~O[2]~min^{"-1"}~10^{"9"}~cells^{"-1"})))+
  ggtitle("Net Photosynthesis of Symbionts in Culture")
culture.NP.boxplot+ggsave("Graphs/FinalGraphs/Culture_NP_Oct_Box.png",width=8, height=5)
#NP Boxplot NO JITTER
culture.NP.boxplot.nojitter<-mydata%>%
  ggplot(aes(x=Genotype, y=NP, fill=Temperature))+
  geom_boxplot()+
  theme_bw()+
  theme(plot.title = element_text(face = "bold", size=16),
        axis.text.x=element_text(color="black", size=11), 
        axis.text.y=element_text(color="black", size=12), 
        axis.title.x = element_text(color="black", size=16), 
        axis.title.y = element_text(color="black", size=16),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_fill_manual(values = c("#679A99", "#9DB462", "#E4C7E5"), labels=c("26°C", "30°C","32°C"))+
  labs(fill="Temperature", x="Symbiont Genotype", y=expression(Net~Photo.~(µmol~O[2]~min^{"-1"}~10^{"9"}~cells^{"-1"})))+
  ggtitle("Net Photosynthesis of Symbionts in Culture")
culture.NP.boxplot.nojitter+ggsave("Graphs/FinalGraphs/Culture_NP_Oct_Box_nojitter.png",width=8, height=5)

#Respiration graphs####
SummaryResp <- mydata %>%
  group_by(Genotype, Temperature) %>%
  summarize(mean=mean(Resp, na.rm=TRUE), SE=sd(Resp, na.rm=TRUE)/sqrt(length(na.omit(Resp))))

pal<-c("#679A99", "#9DB462", "#E4C7E5") #blue, green, pink
RespGraph<-ggplot(SummaryResp, aes(x=Genotype, y=mean, fill=factor(Temperature), group=factor(Temperature)))+  #basic plot
  theme_bw()+ #Removes grey background
  scale_y_continuous(expand=c(0,0), limits=c(-1.8,0))+
  theme(plot.title = element_text(face = "bold", size=16),
        axis.text.x=element_text(color="black", size=11), 
        axis.text.y=element_text(color="black", size=12), 
        axis.title.x = element_text(color="black", size=16), 
        axis.title.y = element_text(color="black", size=16),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6, color="black") + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  scale_fill_manual(values = pal, labels=c("26°C", "30°C","32°C"))+
  labs(x="Symbiont Genotype", y=expression(Respiration~(µmol~O[2]~min^{"-1"}~10^{"9"}~cells^{"-1"})), fill="Temperature")+  #labels the x and y axes
  ggtitle("Respiration of Symbionts in Culture")
RespGraph+ggsave("Graphs/FinalGraphs/Culture_Resp_Oct_Bar.png",width=8, height=5)

#Respiration boxplot
culture.Resp.boxplot<-mydata%>%
  ggplot(aes(x=Genotype, y=Resp, fill=Temperature))+
  geom_boxplot()+
  theme_bw()+
  geom_point(pch=21, position=position_jitterdodge(jitter.width=0.1), size=1)+
  theme(plot.title = element_text(face = "bold", size=16),
        axis.text.x=element_text(color="black", size=11), 
        axis.text.y=element_text(color="black", size=12), 
        axis.title.x = element_text(color="black", size=16), 
        axis.title.y = element_text(color="black", size=16),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_fill_manual(values = c("#679A99", "#9DB462", "#E4C7E5"), labels=c("26°C", "30°C","32°C"))+
  labs(fill="Temperature", x="Symbiont Genotype", y=expression(Respiration~(µmol~O[2]~min^{"-1"}~10^{"9"}~cells^{"-1"})))+
  ggtitle("Respiration of Symbionts in Culture")
culture.Resp.boxplot+ggsave("Graphs/FinalGraphs/Culture_Resp_Oct_Box.png",width=8, height=5)
#Respiration boxplot no jitter
culture.Resp.boxplot.nojitter<-mydata%>%
  ggplot(aes(x=Genotype, y=Resp, fill=Temperature))+
  geom_boxplot()+
  theme_bw()+
  theme(plot.title = element_text(face = "bold", size=16),
        axis.text.x=element_text(color="black", size=11), 
        axis.text.y=element_text(color="black", size=12), 
        axis.title.x = element_text(color="black", size=16), 
        axis.title.y = element_text(color="black", size=16),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_fill_manual(values = c("#679A99", "#9DB462", "#E4C7E5"), labels=c("26°C", "30°C","32°C"))+
  labs(fill="Temperature", x="Symbiont Genotype", y=expression(Respiration~(µmol~O[2]~min^{"-1"}~10^{"9"}~cells^{"-1"})))+
  ggtitle("Respiration of Symbionts in Culture")
culture.Resp.boxplot.nojitter+ggsave("Graphs/FinalGraphs/Culture_Resp_Oct_Box_nojitter.png",width=8, height=5)

#October PnR Stats#####

#Genotype (fixed) and Temperature (fixed) on dependent variables (NP, GP, and Resp)
#Clear the environment
rm(list=ls())
#Load PnR data
mydata_wrong<-read.csv("Data/OctoberPnR_r.csv")
mydata_wrong$Temperature<-as.factor(mydata_wrong$Temperature)
mydata_wrong<-mydata_wrong%>%
  mutate_if(is.character, as.factor)

#Dec 23, 2020: Turns out I got GP and NP mixed up. 
#I had the light slopes (after subtracting the blanks and averaging) as GP instead of NP
#And calculated NP as GP-Resp, and since resp is negative, it was GP+|Resp|
#But, the light slope should be NP and GP=NP-Resp AKA NP+|Resp| (GP must be greater than NP!)
#So, I calculated it correctly, NP and GP are just switched. So rather than switching them in the data, I'm gonna switch them here so I know what I did
mydata<-mydata_wrong %>% 
  rename(
    AvgNP_umol_L_min_fixed = AvgGP_umol_L_min,
    AvgGP_umol_L_min_fixed = AvgNP_umol_L_min,
    AvgNPPer1000Cell_fixed = AvgGPPer1000Cell,
    AvgGPPer1000Cell_fixed = AvgNPPer1000Cell
  )

mydata <- mutate(mydata, GP = 1000000000*(AvgGP_umol_L_min_fixed/CellsPerL),
                 NP=1000000000*(AvgNP_umol_L_min_fixed/CellsPerL),
                 Resp=1000000000*(AvgResp_umol_L_min/CellsPerL))

#Respiration####
model1<-lm(Resp~Genotype*Temperature, data=mydata)
#Bunch of transformations I tried that didn't work for Resp ####
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

#BoxCox transformation for Resp ####
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

#mean values
SummaryResp <- mydata %>%
  group_by(Genotype, Temperature) %>%
  summarize(mean=mean(Resp, na.rm=TRUE), SE=sd(Resp, na.rm=TRUE)/sqrt(length(na.omit(Resp))))
emm.resp = emmeans(cubed.resp.model, specs= pairwise~Genotype:Temperature)
emm.resp.data<-as.data.frame(emm.resp$emmeans)
emm.resp$emmeans

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

#mean values
SummaryNP <- mydata %>%
  group_by(Genotype, Temperature) %>%
  summarize(mean=mean(NP, na.rm=TRUE), SE=sd(NP, na.rm=TRUE)/sqrt(length(na.omit(NP))))
emm.NP = emmeans(quadrt.NP.model, specs= pairwise~Genotype:Temperature)
emm.NP$emmeans

#Gross photosynthesis. GP = gross photo umol O2 per billion cells####
model1<-lm(GP~Genotype*Temperature, data=mydata)
#Check assumptions
model1res<-resid(model1)
qqp(model1res, "norm")

mydata$sqrtGP<-sqrt(mydata$GP)
sqrtGPmodel<-lm(sqrtGP~Genotype*Temperature, data=mydata)
qqp(resid(sqrtGPmodel), "norm")
plot(sqrtGPmodel)
#That's very close

mydata$fourthrtGP<-sqrt(mydata$sqrtGP)
fourthrtGPmodel<-lm(fourthrtGP~Genotype*Temperature, data=mydata)
qqp(resid(fourthrtGPmodel), "norm")
#That's even more normal, so I'll use that. 

GP.model.simp<-lm(fourthrtGP~Genotype+Temperature, data=mydata)
lrtest(fourthrtGPmodel, GP.model.simp)
#Sig = use full model

Anova(fourthrtGPmodel, type="III")
#mean values
SummaryGP <- mydata %>%
  group_by(Genotype, Temperature) %>%
  summarize(mean=mean(GP, na.rm=TRUE), SE=sd(GP, na.rm=TRUE)/sqrt(length(na.omit(GP))))
emm.GP = emmeans(fourthrtGPmodel, specs= pairwise~Genotype:Temperature)
emm.GP.data<-as.data.frame(emm.GP$emmeans)

#GP emmean bargraph backtransformed
pal<-c("#679A99", "#9DB462", "#E4C7E5") #blue, green, pink
GPGraph<-ggplot(emm.GP.data, aes(x=Genotype, y=emmean^4, fill=factor(Temperature), group=factor(Temperature)))+  #basic plot
  theme_bw()+ #Removes grey background
  scale_y_continuous(expand=c(0,0), limits=c(0, 7))+
  theme(plot.title = element_text(face = "bold", size=16),
        axis.text.x=element_text(color="black", size=11), 
        axis.text.y=element_text(color="black", size=12), 
        axis.title.x = element_text(color="black", size=16), 
        axis.title.y = element_text(color="black", size=16),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6, color="black") + #determines the bar width
  geom_errorbar(aes(ymax=(upper.CL^4), ymin=(lower.CL^4)), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Symbiont Genotype", y=expression(GP~(µmol~O[2]~min^{"-1"}~10^{"9"}~cells^{"-1"})), fill="Temperature")+  #labels the x and y axes
  scale_fill_manual(values = pal, labels=c("26°C", "30°C","32°C"))+
  ggtitle("Gross Photosynthesis of Symbionts in Culture")
GPGraph

