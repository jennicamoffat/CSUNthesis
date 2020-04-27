#Thesis
#Polyp Photosynthesis and Respiration
#Created on 4/17/2020 by Jennica Moffat

#Clear the environment
rm(list=ls())
#load libraries
library(tidyverse)
library(car)
library(lme4)
library(lmerTest)
library(emmeans)


#Combining and cleaning data sets to get PnR data####
#Load data
pnr.data<-read.csv("Data/PnR/Jan_Polyp_PnR.csv")
polyp.data<-read.csv("Data/Thesis_PolypData_Summary.csv")
area.data<-read.csv("Data/PolypAreas.csv")
View(area.data)

#I need to combine data sets so that the area, PnR values, and temp/plate number are all together
#they need to be combined by Polyp ("WellNum")

#Start with pnr and pic data
pnr.area.data<-full_join(pnr.data, area.data, by = "WellNum", copy = FALSE, suffix = c(".ph", ".ar"))
View(pnr.area.data)

#Combine that with polyp summary data
all.data<-full_join(pnr.area.data, polyp.data, by = "WellNum", copy = FALSE)
View(all.data)
#Wow that worked seemlessly. Go me!

#Need to make temp and plate factors
all.data$Temp<-as.factor(all.data$Temp)
all.data$Plate<-as.factor(all.data$Plate)

#Now, I need to add column for average count
all.data <- mutate(all.data, avgct = ((Count1+Count2)/2))

#Now, all pnr values divided by polyp area/count
#Current count is in #cells/0.1mm3 
# #cells/0.1mm3 * (1000 mm3/mL) = # cells/mL 
all.data <- mutate(all.data, cells.per.mL = ((avgct)/0.1)*1000)

# cells.per.mL * 150mL = cells per polyp (because I crushed each polyp in 150mL). No units, just a count.
all.data <- mutate(all.data, cells.per.polyp = (cells.per.mL*150))

#cell density = cells.per.polyp/polyp area (in mm2) = cells/mm2
all.data<-mutate(all.data, cells.per.area=(cells.per.polyp/Area))

#Now, each slope divided by cells.per.area 
#Slope= (O2 umol/L)/second
#slope/cells.per.area = [(O2 umol/L)/sec]/(cells/mm2)
#Respiration
all.data<-mutate(all.data, Resp.per.cell.density = (Respiration/cells.per.area))
#And just because they are tiny numbers, let's multiply that by 10^9 (one billion)
all.data<-mutate(all.data, Resp=(Resp.per.cell.density*1000000000))
#So now it is [(O2 umol/L)/sec]/(10^9 cells/mm2)

#Gross Photosynthesis
all.data<-mutate(all.data, GP.per.cell.density = (GrossPhoto/cells.per.area))
all.data<-mutate(all.data, GP = (GP.per.cell.density*1000000000))

#Net photosynthesis
all.data<-mutate(all.data, NP.per.cell.density = (NetPhoto/cells.per.area))
all.data<-mutate(all.data, NP = (NP.per.cell.density*1000000000))


#So I also want to test if I run everything without accounting for area, just doing total cells
#Slope= (O2 umol/L)/second
#slope/cells.per.polyp = [(O2 umol/L)/sec]/(cells)
#Respiration
all.data<-mutate(all.data, Resp.per.cell = (Respiration/cells.per.polyp))
#And just because they are tiny numbers, let's multiply that by 10^9 (one billion)
all.data<-mutate(all.data, Resp.per.bill.cell=(Resp.per.cell*1000000000))
#So now it is [(O2 umol/L)/sec]/(10^9 cells)

#Gross Photosynthesis
all.data<-mutate(all.data, GP.per.cell = (GrossPhoto/cells.per.polyp))
all.data<-mutate(all.data, GP.per.bill.cell = (GP.per.cell*1000000000))

#Net photosynthesis
all.data<-mutate(all.data, NP.per.cell = (NetPhoto/cells.per.polyp))
all.data<-mutate(all.data, NP.per.bill.cell = (NP.per.cell*1000000000))

#Finally, remove all NA rows so that I just have the clean PnR data
all.pnr.data<-subset(all.data, NP.per.bill.cell != "NA")
View(all.pnr.data)

#All I need is WellNum, Date, Genotype, Temp, Plate, Resp, GP, and NP
pnr.data.cleaned = all.pnr.data[c("WellNum", "Date", "Genotype", "Temp", "Plate", "Resp", "GP", "NP", "Resp.per.bill.cell", "GP.per.bill.cell", "NP.per.bill.cell")]
View(pnr.data.cleaned)

#Export as CSV so I don't have to go through all of that every time. 
write.csv(pnr.data.cleaned,"Data/Polyp_PnR_data_cleaned.csv", row.names = FALSE)

#PnR graphs#####
mydata<-read.csv("Data/Polyp_PnR_data_cleaned.csv")
View(mydata)

#Resp
SummaryResp <- mydata %>%
  group_by(Genotype, Temp) %>%
  summarize(mean=mean(Resp), SE=sd(Resp)/sqrt(length(na.omit(Resp))))
SummaryResp

Resp.polyp.graph<-ggplot(SummaryResp, aes(x=Genotype, y=mean, fill=factor(Temp), group=factor(Temp)))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(plot.title = element_text(face = "bold", size=18), axis.text.x=element_text(color="black", size=13), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=14), axis.title.y = element_text(face="bold", color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Genotype", y=expression(Respiration~((µmol~O[2]/L)/sec)/(10^{"9"}~cells/mm^{"3"})), fill="Temperature")+  #labels the x and y axes
  scale_fill_manual(values = c("skyblue3", "darkgoldenrod2", "firebrick3"), labels=c("26°C", "30°C","32°C"))+
  ggtitle("Respiration of Polyps by Symbiont Genotype")+
  ggsave("Graphs/PnR/PolypResp.pdf", width=11, height=6.19, dpi=300, unit="in")
Resp.polyp.graph

#Resp Genotype
SummaryRespGeno <- mydata %>%
  group_by(Genotype) %>%
  summarize(mean=mean(Resp), SE=sd(Resp)/sqrt(length(na.omit(Resp))))
SummaryRespGeno

Resp.polyp.graph.geno<-ggplot(SummaryRespGeno, aes(x=Genotype, y=mean))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(plot.title = element_text(face = "bold", size=18), axis.text.x=element_text(color="black", size=13), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=14), axis.title.y = element_text(face="bold", color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6, fill="darkseagreen3") + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Genotype", y=expression(Respiration~((µmol~O[2]/L)/sec)/(10^{"9"}~cells/mm^{"3"})))+  #labels the x and y axes
  ggtitle("Respiration of Polyps by Symbiont Genotype")+
  ggsave("Graphs/PnR/PolypRespbyGeno.pdf", width=11, height=6.19, dpi=300, unit="in")
Resp.polyp.graph.geno

#Resp by temp
SummaryRespTemp <- mydata %>%
  group_by(Temp) %>%
  summarize(mean=mean(Resp), SE=sd(Resp)/sqrt(length(na.omit(Resp))))
SummaryRespTemp

Resp.polyp.graph.temp<-ggplot(SummaryRespTemp, aes(x=Temp, y=mean))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(plot.title = element_text(face = "bold", size=18), axis.text.x=element_text(color="black", size=13), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=14), axis.title.y = element_text(face="bold", color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6, fill="mediumorchid4") + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Temp", y=expression(Respiration~((µmol~O[2]/L)/sec)/(10^{"9"}~cells/mm^{"3"})))+  #labels the x and y axes
  ggtitle("Respiration of Polyps by Temperature")+
  ggsave("Graphs/PnR/PolypRespbyTemp.pdf", width=11, height=6.19, dpi=300, unit="in")
Resp.polyp.graph.temp

#Resp by temp, apo removed
SummaryRespTemp.no.apo<-no.apo.data %>% 
  group_by(Temp) %>%
  summarize(mean=mean(Resp), SE=sd(Resp)/sqrt(length(na.omit(Resp))))
SummaryRespTemp.no.apo

Resp.polyp.graph.temp.no.apo<-ggplot(SummaryRespTemp.no.apo, aes(x=Temp, y=mean))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(plot.title = element_text(face = "bold", size=18), axis.text.x=element_text(color="black", size=13), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=14), axis.title.y = element_text(face="bold", color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6, fill="mediumorchid4") + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Temp", y=expression(Respiration~((µmol~O[2]/L)/sec)/(10^{"9"}~cells/mm^{"3"})))+  #labels the x and y axes
  ggtitle("Respiration of Polyps by Temperature (Apo removed)")+
  ggsave("Graphs/PnR/PolypResp.Temp.no.apo.pdf", width=11, height=6.19, dpi=300, unit="in")

Resp.polyp.graph.temp.no.apo

#GP
SummaryGP <- mydata %>%
  group_by(Genotype, Temp) %>%
  summarize(mean=mean(GP), SE=sd(GP)/sqrt(length(na.omit(GP))))
SummaryGP

GP.polyp.graph<-ggplot(SummaryGP, aes(x=Genotype, y=mean, fill=factor(Temp), group=factor(Temp)))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(plot.title = element_text(face = "bold", size=18), axis.text.x=element_text(color="black", size=13), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=14), axis.title.y = element_text(face="bold", color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Genotype", y=expression(GP~((µmol~O[2]/L)/sec)/(10^{"9"}~cells/mm^{"3"})), fill="Temperature")+  #labels the x and y axes
  scale_fill_manual(values = c("skyblue3", "darkgoldenrod2", "firebrick3"), labels=c("26°C", "30°C","32°C"))+
  ggtitle("Gross Photosynthesis of Polyps by Symbiont Genotype")+
  ggsave("Graphs/PnR/PolypGP.pdf", width=11, height=6.19, dpi=300, unit="in")
GP.polyp.graph

#GP by genotype
SummaryGP.geno <- mydata %>%
  group_by(Genotype) %>%
  summarize(mean=mean(GP), SE=sd(GP)/sqrt(length(na.omit(GP))))
SummaryGP.geno

GP.polyp.graph.geno<-ggplot(SummaryGP.geno, aes(x=Genotype, y=mean))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(plot.title = element_text(face = "bold", size=18), axis.text.x=element_text(color="black", size=13), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=14), axis.title.y = element_text(face="bold", color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6, fill="darkseagreen3") + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Genotype", y=expression(GP~((µmol~O[2]/L)/sec)/(10^{"9"}~cells/mm^{"3"})))+  #labels the x and y axes
  ggtitle("Gross Photosynthesis of Polyps by Symbiont Genotype")+
  ggsave("Graphs/PnR/PolypGP.geno.pdf", width=11, height=6.19, dpi=300, unit="in")
GP.polyp.graph.geno

#GP by temp
SummaryGP.temp <- mydata %>%
  group_by(Temp) %>%
  summarize(mean=mean(GP), SE=sd(GP)/sqrt(length(na.omit(GP))))
SummaryGP.temp

GP.polyp.graph.temp<-ggplot(SummaryGP.temp, aes(x=Temp, y=mean))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(plot.title = element_text(face = "bold", size=18), axis.text.x=element_text(color="black", size=13), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=14), axis.title.y = element_text(face="bold", color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6, fill="mediumorchid4") + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Temperature", y=expression(GP~((µmol~O[2]/L)/sec)/(10^{"9"}~cells/mm^{"3"})))+  #labels the x and y axes
  ggtitle("Gross Photosynthesis of Polyps by Temperature")+
  ggsave("Graphs/PnR/PolypGP.temp.pdf", width=11, height=6.19, dpi=300, unit="in")
GP.polyp.graph.temp


#GP by temp, no APO
no.apo.data<-subset(mydata, Genotype != "Aposymbiotic")
SummaryGP.temp.no.apo <- no.apo.data %>%
  group_by(Temp) %>%
  summarize(mean=mean(GP), SE=sd(GP)/sqrt(length(na.omit(GP))))
SummaryGP.temp.no.apo

GP.polyp.graph.temp.no.apo<-ggplot(SummaryGP.temp.no.apo, aes(x=Temp, y=mean))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(plot.title = element_text(face = "bold", size=18), axis.text.x=element_text(color="black", size=13), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=14), axis.title.y = element_text(face="bold", color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6, fill="mediumorchid4") + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Temperature", y=expression(GP~((µmol~O[2]/L)/sec)/(10^{"9"}~cells/mm^{"3"})))+  #labels the x and y axes
  ggtitle("Gross Photosynthesis of Polyps by Temperature (Apo removed)")+
  ggsave("Graphs/PnR/PolypGP.temp.no.apo.pdf", width=11, height=6.19, dpi=300, unit="in")
GP.polyp.graph.temp.no.apo

#NP
SummaryNP <- mydata %>%
  group_by(Genotype, Temp) %>%
  summarize(mean=mean(NP), SE=sd(NP)/sqrt(length(na.omit(NP))))
SummaryNP

NP.polyp.graph<-ggplot(SummaryNP, aes(x=Genotype, y=mean, fill=factor(Temp), group=factor(Temp)))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(plot.title = element_text(face = "bold", size=18), axis.text.x=element_text(color="black", size=13), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=14), axis.title.y = element_text(face="bold", color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Genotype", y=expression(NP~((µmol~O[2]/L)/sec)/(10^{"9"}~cells/mm^{"3"})), fill="Temperature")+  #labels the x and y axes
  scale_fill_manual(values = c("skyblue3", "darkgoldenrod2", "firebrick3"), labels=c("26°C", "30°C","32°C"))+
  ggtitle("Net Photosynthesis of Polyps by Symbiont Genotype")+
  ggsave("Graphs/PnR/PolypNP.pdf", width=11, height=6.19, dpi=300, unit="in")
NP.polyp.graph

#NP by genotype
SummaryNP.geno <- mydata %>%
  group_by(Genotype) %>%
  summarize(mean=mean(NP), SE=sd(NP)/sqrt(length(na.omit(NP))))
SummaryNP.geno

NP.polyp.graph.geno<-ggplot(SummaryNP.geno, aes(x=Genotype, y=mean))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(plot.title = element_text(face = "bold", size=18), axis.text.x=element_text(color="black", size=13), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=14), axis.title.y = element_text(face="bold", color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6, fill="darkseagreen3") + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Genotype", y=expression(NP~((µmol~O[2]/L)/sec)/(10^{"9"}~cells/mm^{"3"})))+  #labels the x and y axes
  ggtitle("Net Photosynthesis of Polyps by Symbiont Genotype")+
  ggsave("Graphs/PnR/PolypNP.geno.pdf", width=11, height=6.19, dpi=300, unit="in")
NP.polyp.graph.geno

#NP by temp
SummaryNP.temp <- mydata %>%
  group_by(Temp) %>%
  summarize(mean=mean(NP), SE=sd(NP)/sqrt(length(na.omit(NP))))
SummaryNP.temp

NP.polyp.graph.temp<-ggplot(SummaryNP.temp, aes(x=Temp, y=mean))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(plot.title = element_text(face = "bold", size=18), axis.text.x=element_text(color="black", size=13), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=14), axis.title.y = element_text(face="bold", color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6, fill="mediumorchid4") + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Temperature", y=expression(GP~((µmol~O[2]/L)/sec)/(10^{"9"}~cells/mm^{"3"})))+  #labels the x and y axes
  ggtitle("Net Photosynthesis of Polyps by Temperature")+
  ggsave("Graphs/PnR/PolypNP.temp.pdf", width=11, height=6.19, dpi=300, unit="in")
NP.polyp.graph.temp

#NP by temp, Apo removed 
SummaryNP.temp.no.apo<- no.apo.data %>%
  group_by(Temp) %>%
  summarize(mean=mean(NP), SE=sd(NP)/sqrt(length(na.omit(NP))))
SummaryNP.temp.no.apo

NP.polyp.graph.temp.no.apo<-ggplot(SummaryNP.temp.no.apo, aes(x=Temp, y=mean))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(plot.title = element_text(face = "bold", size=18), axis.text.x=element_text(color="black", size=13), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=14), axis.title.y = element_text(face="bold", color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6, fill="mediumorchid4") + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Temperature", y=expression(GP~((µmol~O[2]/L)/sec)/(10^{"9"}~cells/mm^{"3"})))+  #labels the x and y axes
  ggtitle("Net Photosynthesis of Polyps by Temperature (Apo removed)")+
  ggsave("Graphs/PnR/PolypNP.temp.no.apo.pdf", width=11, height=6.19, dpi=300, unit="in")
NP.polyp.graph.temp.no.apo

#Polyp PnR Stats####
mydata<-read.csv("Data/Polyp_PnR_data_cleaned.csv")
mydata$Temp<-as.factor(mydata$Temp)
mydata$Plate<-as.factor(mydata$Plate)

#Respiration####
resp.model<-lmer(Resp~Genotype*Temp+(1|Plate), data=mydata)
#Check assumptions
plot(resp.model)
qqp(resid(resp.model), "norm")
#Not really normal at ends

View(mydata)
#I want to log transform, but variables range from -12 to 10
mydata$logResp<-log(mydata$Resp+13)

logresp.model<-lmer(logResp~Genotype*Temp+(1|Plate), data=mydata)
#Check assumptions
plot(logresp.model)
qqp(resid(logresp.model), "norm")

mydata$loglogResp<-log(mydata$logResp+1)
loglogresp.model<-lmer(loglogResp~Genotype*Temp+(1|Plate), data=mydata)
plot(loglogresp.model)
qqp(resid(loglogresp.model), "norm")
#Well, at least it's basically normal. 

anova(loglogresp.model) #nothing is is significant, because I have no power. The SE bars are huge.
summary(loglogresp.model)

resp.model2<-lm(loglogResp~Genotype*Temp, data=mydata)
anova(resp.model2)

#Why don't we try z-scores
mydata$Resp.z<-scale(mydata$Resp, center = TRUE, scale = TRUE)
View(mydata)
zresp.model<-lmer(Resp.z~Genotype*Temp+(1|Plate), data=mydata)
#Check assumptions
plot(zresp.model)
qqp(resid(zresp.model), "norm")
#That's exactly the same as non-transormed? Just on a different scale. 
#Maybe that code doesn't work. Trying it manually. 
mydata<-mutate(mydata, Resp.z2 = (Resp - mean(Resp))/sd(Resp))
#Okay, yes they're the same. So z-score didn't help, maybe that's not what I want. 

#Try removing Apo
no.apo.data<-subset(mydata, Genotype != "Aposymbiotic")

Resp.no.apo.model<-lmer(Resp~Genotype*Temp+(1|Plate), data=no.apo.data)
qqp(resid(Resp.no.apo.model), "norm")
#ranges -12.4 to 4.3
#log
no.apo.data$logResp<-log(no.apo.data$Resp+13)
logResp.no.apo.model<-lmer(logResp~Genotype*Temp+(1|Plate), data=no.apo.data)
qqp(resid(logResp.no.apo.model), "norm")
#log log
no.apo.data$loglogResp<-log(no.apo.data$logResp+1)
loglogResp.no.apo.model<-lmer(loglogResp~Genotype*Temp+(1|Plate), data=no.apo.data)
qqp(resid(loglogResp.no.apo.model), "norm")
#Good, just  couple outliers per usual

anova(loglogResp.no.apo.model)
#Still no significant effects

#emmeans
#Means averaged by genotype and then Temp, without taking into account the other. 
emmeans(loglogresp.model, "Genotype")
emmeans(loglogresp.model, "Temp")

#Contrasts using Tukey
resp.tukey<-emmeans(loglogresp.model, pairwise ~ Genotype | Temp)
resp.tukey
#Graph to visualize respiration means across temp by genotype
emmeans.resp<-emmip(loglogresp.model, Genotype~Temp)
emmeans.resp

#GP####
GP.model<-lmer(GP~Genotype*Temp+(1|Plate), data=mydata)
#Check assumptions
plot(GP.model)
qqp(resid(GP.model), "norm")
#Not really normal at ends again

#GP ranges from -15.3 to 7...
mydata$logGP<-log(mydata$GP+16)
logGP.model<-lmer(logGP~Genotype*Temp+(1|Plate), data=mydata)
plot(logGP.model)
qqp(resid(logGP.model), "norm")
#Just two outliers, 110 nd 106

anova(logGP.model)
#Genotype is significant, nothing else. 

#emmeans
#Means averaged by genotype and then Temp, without taking into account the other. 
emmeans(logGP.model, "Genotype")
emmeans(logGP.model, "Temp")

#Contrasts using Tukey
GP.tukey<-emmeans(logGP.model, pairwise ~ Genotype | Temp)
GP.tukey
#JFC. The only one that differs is Apo 26-30 (P=0.0271) and 30-32 (P=0.0081)

#Graph to visualize respiration means across temp by genotype
emmeans.GP<-emmip(logGP.model, Genotype~Temp)
emmeans.GP
#Okay, looking at the graph, that makes sense. 

#So let's run the stats without Apo
no.apo.data<-subset(mydata, Genotype != "Aposymbiotic")

GP.no.apo.model<-lmer(GP~Genotype*Temp+(1|Plate), data=no.apo.data)
#Check assumptions
plot(GP.no.apo.model)
qqp(resid(GP.no.apo.model), "norm")
#Not really normal at ends again
View(no.apo.data)
#GP ranges from -5.4 to 7.
no.apo.data$logGP<-log(no.apo.data$GP+6)
logGP.no.apo.model<-lmer(logGP~Genotype*Temp+(1|Plate), data=no.apo.data)
plot(logGP.no.apo.model)
qqp(resid(logGP.no.apo.model), "norm")
#Weirdly, that looks worse

no.apo.data$loglogGP<-log(no.apo.data$logGP+1)
loglogGP.no.apo.model<-lmer(loglogGP~Genotype*Temp+(1|Plate), data=no.apo.data)
qqp(resid(loglogGP.no.apo.model), "norm")
#Still not great but close enough maybe

anova(loglogGP.no.apo.model)
#Yep, now nothing is signficant

#NP####
NP.model<-lmer(NP~Genotype*Temp+(1|Plate), data=mydata)
#Check assumptions
plot(NP.model)
qqp(resid(NP.model), "norm")
#Not quite
anova(NP.model)
#Sig Temp, almost sig Geno, but not interaction

#log transforming
#NP ranges from -13.6 to 11
mydata$logNP<-log(mydata$NP+14)
logNP.model<-lmer(logNP~Genotype*Temp+(1|Plate), data=mydata)
qqp(resid(logNP.model), "norm")
#Better, just a couple outliers (66 and 110)
anova(logNP.model)
#Well, now there is a signficant interaction. 

#emmeans
#Means averaged by genotype and then Temp, without taking into account the other. 
emmeans(logNP.model, "Genotype")
emmeans(logNP.model, "Temp")
#Can't use those, though.

#Contrasts using Tukey
NP.tukey<-emmeans(logNP.model, pairwise ~ Genotype | Temp)
NP.tukey
#All difference is in the Apo.(26-30 and 30-32)

#Graph to visualize respiration means across temp by genotype
emmeans.NP<-emmip(logNP.model, Genotype~Temp)
emmeans.NP
#Okay, looking at the graph, that makes sense. 

#Okay, now let's see what happens when I remove Apo
NP.no.apo.model<-lmer(NP~Genotype*Temp+(1|Plate), no.apo.data)
qqp(resid(NP.no.apo.model), "norm")
#Whoa. Not normal. 

View(no.apo.data)
#range -5.3 to 11
no.apo.data$logNP<-log(no.apo.data$NP+6)
logNP.no.apo.model<-lmer(logNP~Genotype*Temp+(1|Plate), no.apo.data)
qqp(resid(logNP.no.apo.model), "norm")

no.apo.data$loglogNP<-log(no.apo.data$logNP+1)
loglogNP.no.apo.model<-lmer(loglogNP~Genotype*Temp+(1|Plate), no.apo.data)
qqp(resid(loglogNP.no.apo.model), "norm")
#Still some outliers, but okay I guess

anova(loglogNP.no.apo.model)
#Still a sig temp effect

#Tukey
NP.tukey.no.apo<-emmeans(loglogNP.no.apo.model, pairwise ~ Temp | Genotype)
NP.tukey.no.apo
#Okay, just realized this is not showing everything, but within genotypes only


#Stats with slopes/cell count, instead of slope/(cells/polyp area)####
mydata<-read.csv("Data/Polyp_PnR_data_cleaned.csv")
mydata$Temp<-as.factor(mydata$Temp)
mydata$Plate<-as.factor(mydata$Plate)

#Respiration####
resp.model<-lmer(Resp.per.bill.cell~Genotype*Temp+(1|Plate), data=mydata)
#Check assumptions
plot(resp.model)
qqp(resid(resp.model), "norm")
#Not really normal at ends (surprise, surprise)

View(mydata)
#I want to log transform, but variables range from -56 to 23
mydata$logResp<-log(mydata$Resp.per.bill.cell+58)
logresp.model<-lmer(logResp~Genotype*Temp+(1|Plate), data=mydata)
#Check assumptions
plot(logresp.model)
qqp(resid(logresp.model), "norm")
#Outliers: 154, 143

#loglog
mydata$loglogResp<-log(mydata$logResp)
loglogresp.model<-lmer(loglogResp~Genotype*Temp+(1|Plate), data=mydata)
plot(loglogresp.model)
qqp(resid(loglogresp.model), "norm")
#Only slightly better than just one log 

anova(loglogresp.model)
#Nothing significant



#GP####
GP.model<-lmer(GP.per.bill.cell~Genotype*Temp+(1|Plate), data=mydata)
#Check assumptions
plot(GP.model)
qqp(resid(GP.model), "norm")
#Not really normal at ends again

View(mydata)
#GP ranges from -39 to 20
#GP ranges from -15.3 to 7...
mydata$logGP<-log(mydata$GP.per.bill.cell+40)
logGP.model<-lmer(logGP~Genotype*Temp+(1|Plate), data=mydata)
plot(logGP.model)
qqp(resid(logGP.model), "norm")

#loglog
mydata$loglogGP<-log(mydata$logGP+1)
loglogGP.model<-lmer(loglogGP~Genotype*Temp+(1|Plate), data=mydata)
plot(loglogGP.model)
qqp(resid(loglogGP.model), "norm")

anova(loglogGP.model)
#nothing is significant

#NP####
NP.model<-lmer(NP.per.bill.cell~Genotype*Temp+(1|Plate), data=mydata)
#Check assumptions
plot(NP.model)
qqp(resid(NP.model), "norm")
#Not at ends

#NP ranges from -35 to 65
mydata$logNP<-log(mydata$NP.per.bill.cell+36)
logNP.model<-lmer(logNP~Genotype*Temp+(1|Plate), data=mydata)
qqp(resid(logNP.model), "norm")
#Better
anova(logNP.model)
#Nothing significant

#loglog
mydata$loglogNP<-log(mydata$logNP+1)
loglogNP.model<-lmer(loglogNP~Genotype*Temp+(1|Plate), data=mydata)
qqp(resid(loglogNP.model), "norm")

anova(loglogNP.model)

#Graphs for PnR by number of cells instead of cell density####

#Resp
SummaryResp <- mydata %>%
  group_by(Genotype, Temp) %>%
  summarize(mean=mean(Resp.per.bill.cell), SE=sd(Resp.per.bill.cell)/sqrt(length(na.omit(Resp.per.bill.cell))))
SummaryResp

Resp.polyp.graph<-ggplot(SummaryResp, aes(x=Genotype, y=mean, fill=factor(Temp), group=factor(Temp)))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(plot.title = element_text(face = "bold", size=18), axis.text.x=element_text(color="black", size=13), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=14), axis.title.y = element_text(face="bold", color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Genotype", y=expression(Respiration~((µmol~O[2]/L)/sec)/(10^{"9"}~cells)), fill="Temperature")+  #labels the x and y axes
  scale_fill_manual(values = c("skyblue3", "darkgoldenrod2", "firebrick3"), labels=c("26°C", "30°C","32°C"))+
  ggtitle("Respiration of Polyps by Symbiont Genotype")+
  ggsave("Graphs/PnR/PolypResp.per.cell.pdf", width=11, height=6.19, dpi=300, unit="in")
Resp.polyp.graph


#GP
SummaryGP <- mydata %>%
  group_by(Genotype, Temp) %>%
  summarize(mean=mean(GP.per.bill.cell), SE=sd(GP.per.bill.cell)/sqrt(length(na.omit(GP.per.bill.cell))))
SummaryGP

GP.polyp.graph<-ggplot(SummaryGP, aes(x=Genotype, y=mean, fill=factor(Temp), group=factor(Temp)))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(plot.title = element_text(face = "bold", size=18), axis.text.x=element_text(color="black", size=13), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=14), axis.title.y = element_text(face="bold", color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Genotype", y=expression(GP~((µmol~O[2]/L)/sec)/(10^{"9"}~cells)), fill="Temperature")+  #labels the x and y axes
  scale_fill_manual(values = c("skyblue3", "darkgoldenrod2", "firebrick3"), labels=c("26°C", "30°C","32°C"))+
  ggtitle("Gross Photosynthesis of Polyps by Symbiont Genotype")+
  ggsave("Graphs/PnR/PolypGP.per.cell.pdf", width=11, height=6.19, dpi=300, unit="in")
GP.polyp.graph


#NP
SummaryNP <- mydata %>%
  group_by(Genotype, Temp) %>%
  summarize(mean=mean(NP.per.bill.cell), SE=sd(NP.per.bill.cell)/sqrt(length(na.omit(NP.per.bill.cell))))
SummaryNP

NP.polyp.graph<-ggplot(SummaryNP, aes(x=Genotype, y=mean, fill=factor(Temp), group=factor(Temp)))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(plot.title = element_text(face = "bold", size=18), axis.text.x=element_text(color="black", size=13), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=14), axis.title.y = element_text(face="bold", color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Genotype", y=expression(NP~((µmol~O[2]/L)/sec)/(10^{"9"}~cells)), fill="Temperature")+  #labels the x and y axes
  scale_fill_manual(values = c("skyblue3", "darkgoldenrod2", "firebrick3"), labels=c("26°C", "30°C","32°C"))+
  ggtitle("Net Photosynthesis of Polyps by Symbiont Genotype")+
  ggsave("Graphs/PnR/PolypNP.per.cell.pdf", width=11, height=6.19, dpi=300, unit="in")
NP.polyp.graph
