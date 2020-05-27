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
# Convert: #cells/0.1mm3 * (1000 mm3/mL) = # cells/mL 
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

#I need WellNum, Date, Genotype, Temp, Plate, Resp, GP, and NP, and Resp,GP,NP per.cell
pnr.data.cleaned = all.pnr.data[c("WellNum", "Date", "Genotype", "Temp", "Plate", "Resp", "GP", "NP", "Resp.per.bill.cell", "GP.per.bill.cell", "NP.per.bill.cell")]
View(pnr.data.cleaned)

#Export as CSV so I don't have to go through all of that every time. 
write.csv(pnr.data.cleaned,"Data/Polyp_PnR_data_cleaned.csv", row.names = FALSE)
#Exporting full pnr data, too, so I have counts and everything. 
write.csv(all.pnr.data, "Data/Polyp_PnR_full_combined_data.csv", row.names= FALSE)

#PnR graphs by count/area (density)#####
mydata<-read.csv("Data/Polyp_PnR_data_cleaned.csv")
mydata$Temp<-as.factor(mydata$Temp)
View(mydata)

#Resp graphs####
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
no.apo.data<-subset(mydata, Genotype != "Aposymbiotic")
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
  ggtitle("Respiration of Polyps by Temperature (Apo removed)")
  ggsave("Graphs/PnR/PolypResp.Temp.no.apo.pdf", width=11, height=6.19, dpi=300, unit="in")

Resp.polyp.graph.temp.no.apo

#Resp boxplot
polyp.Resp.boxplot<-mydata%>%
  ggplot(aes(x=Genotype, y=Resp, fill=Temp))+
  geom_boxplot()+
  theme_bw()+
  ggtitle("Respiration of Holobiont")+
  geom_jitter(color="black", size=0.5, alpha=0.7)+
  theme(plot.title = element_text(face="bold"), 
        axis.text.x=element_text(size=10), 
        axis.text.y=element_text(size=10), 
        axis.title.y = element_text(face="bold", size=12), 
        axis.title.x = element_text(face="bold", size=12))+
  labs(fill="Temperature", x="Genotype", y=expression(Respiration~(µmol~O[2]/min/10^{"9"}~cells)))+
  scale_fill_manual(values = c("#79CFDB", "#859A51", "#DFADE1"), labels=c("26°C", "30°C","32°C"))
polyp.Resp.boxplot+ggsave("Graphs/PnR/PolypPnR/PolypRespbyArea.boxplot.png", width=10, height=5)


#GP counts/area graphs####
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
  ggtitle("Gross Photosynthesis of Polyps by Temperature (Apo removed)")
  ggsave("Graphs/PnR/PolypGP.temp.no.apo.pdf", width=11, height=6.19, dpi=300, unit="in")
GP.polyp.graph.temp.no.apo

#GP boxplot
polyp.GP.boxplot<-mydata%>%
  ggplot(aes(x=Genotype, y=GP, fill=Temp))+
  geom_boxplot()+
  theme_bw()+
  ggtitle("Gross Photosynthesis of Holobiont")+
  geom_jitter(color="black", size=0.5, alpha=0.7)+
  theme(plot.title = element_text(face="bold"), 
        axis.text.x=element_text(size=10), 
        axis.text.y=element_text(size=10), 
        axis.title.y = element_text(face="bold", size=12), 
        axis.title.x = element_text(face="bold", size=12))+
  labs(fill="Temperature", x="Genotype", y=expression(Gross~Photo.~(µmol~O[2]/min/10^{"9"}~cells)))+
  scale_fill_manual(values = c("#79CFDB", "#859A51", "#DFADE1"), labels=c("26°C", "30°C","32°C"))
polyp.GP.boxplot
polyp.GP.boxplot+ggsave("Graphs/PnR/PolypPnR/PolypGPbyArea.boxplot.png", width=10, height=5)


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

#GP boxplot



#Polyp PnR Stats####
mydata<-read.csv("Data/Polyp_PnR_data_cleaned.csv")
mydata$Temp<-as.factor(mydata$Temp)
mydata$Plate<-as.factor(mydata$Plate)
NoApoData <- mydata %>%
  filter(Genotype != "Aposymbiotic") %>%
  droplevels

mydata <- mydata %>% drop_na()

#Respiration####
resp.model<-lmer(Resp~Genotype*Temp+(1|Plate), data=mydata)
#Check assumptions
plot(resp.model)
qqp(resid(resp.model), "norm")
#Not normal at ends
hist(mydata$Resp)

#I want to log transform, but variables range from -12 to 10
mydata$logResp<-log(mydata$Resp+13)

logresp.model<-lmer(logResp~Genotype*Temp+(1|Plate), data=mydata)
#Check assumptions
plot(logresp.model)
qqp(resid(logresp.model), "norm")
hist(mydata$logResp)

mydata$loglogResp<-log(mydata$logResp+1)
loglogresp.model<-lmer(loglogResp~Genotype*Temp+(1|Plate), data=mydata)
plot(loglogresp.model)
qqp(resid(loglogresp.model), "norm")
#Still not good enough 
hist(mydata$loglogResp)

#Removing outlier
mydata2<-mydata[-143,]
loglogresp.model2<-lmer(loglogResp~Genotype*Temp+(1|Plate), data=mydata2)
qqp(resid(loglogresp.model2), "norm")
#Still  bad

#Squareroot
mydata$sqrtResp<-sqrt(mydata$Resp+13)
sqrtresp.model<-lmer(sqrtResp~Genotype*Temp+(1|Plate), data=mydata)
qqp(resid(sqrtresp.model), "norm")
#No

#Fourthroot
mydata$fourthrtResp<-sqrt(mydata$sqrtResp)
fourthrtresp.model<-lmer(fourthrtResp~Genotype*Temp+(1|Plate), data=mydata)
qqp(resid(fourthrtresp.model), "norm")
#Does nothing for the small values because it makes them disproportionately smaller. 

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

#BoxCox transformation
#Response variable must be positive. Adding 13
mydata$PosResp<-mydata$Resp+13
View(mydata)
full.resp.model<-lm(PosResp ~ Genotype*Temp*Plate, data=mydata)
step.resp.model<-stepAIC(full.resp.model, direction="both", trace = F)

boxcox<-boxcox(step.resp.model,lambda = seq(-5, 5, 1/1000),plotit = TRUE )

Selected.Power<-boxcox$x[boxcox$y==max(boxcox$y)]
Selected.Power
#1.49

mydata$Resp.trans<-(mydata$PosResp)^1.49
trans.resp.model<-lm(Resp.trans~Genotype*Temp*Plate, data = mydata)
qqp(resid(trans.resp.model), "norm")
#Well that didn't work as well as I hoped. 
hist(mydata$Resp.trans)
Anova(trans.resp.model, type="III")
plot(trans.resp.model)

trans.resp.model2<-lmer(Resp.trans~Genotype*Temp+(1|Plate), data=mydata)
qqp(resid(trans.resp.model2), "norm")
Anova(trans.resp.model2, type="III")

trans.resp.model3<-lmer(Resp.trans~Genotype+Temp+(1|Plate), data=mydata)
Anova(trans.resp.model3, type="III")

AIC(trans.resp.model)
AIC(trans.resp.model2) #By far the lowest
AIC(trans.resp.model3)

#GLMER
mydata$Resp.z.pos<-mydata$Resp.z+5
#lognormal distribution
qqp(mydata$Resp.z.pos, "lnorm")
#No

gamma<-fitdistr(mydata$Resp.z.pos, "gamma")
qqp(mydata$Resp.z.pos, "gamma", shape = gamma$estimate[[1]], rate=gamma$estimate[[2]])
#NA's produced

#Try negative binomial
nbinom <- fitdistr(mydata$Resp.z.pos, "Negative Binomial")
qqp(StrobilatedData$Days.to.Strobilation, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])
#Error

#Try poisson
poisson <- fitdistr(mydata$Resp.z.pos, "Poisson")
qqp(mydata$Resp.z.pos, "pois", poisson$estimate, lambda=3)
#Error

#Try removing Apo
NoApoData<-mydata %>%
  filter(Genotype != "Aposymbiotic")%>%
  droplevels

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
no.apo.data$Temp<-as.factor(no.apo.data$Temp)
no.apo.data$Plate<-as.factor(no.apo.data$Plate)


GP.no.apo.model<-lmer(GP~Genotype*Temp+(1|Plate), data=no.apo.data)
#Singular fit error.
summary(GP.no.apo.model)

summary<-no.apo.data%>%
  group_by(Genotype, Temp, Plate)%>%
  summarize(tally=n())
View(summary)
#I think it's because some plates only have two reps 

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

#Because sometimes I get the singular fit error, and sometimes I don't
GP.no.apo2<-lm(GP~Genotype*Temp, data=no.apo.data)
qqp(resid(GP.no.apo2), "norm")

logGP.no.apo2<-lm(logGP~Genotype*Temp, data=no.apo.data)
qqp(resid(logGP.no.apo2), "norm")
loglogGP.no.apo2<-lm(loglogGP~Genotype*Temp, data=no.apo.data)
qqp(resid(loglogGP.no.apo2), "norm")
anova(loglogGP.no.apo2)


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
#Not normal. 

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


#Not including plate in model
NPmodel2<-lm(NP~Genotype*Temp*Plate, no.apo.data)
qqp(resid(NPmodel2), "norm")
anova(NPmodel2)
loglogNPmodel2<-lm(loglogNP~Genotype*Temp*Plate, no.apo.data)
qqp(resid(loglogNPmodel2), "norm")
anova(loglogNPmodel2)


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




#Does cell count differ between genotype/temps?####
mydata<-read.csv("Data/Polyp_PnR_full_combined_data.csv")
mydata$Temp<-as.factor(mydata$Temp)
mydata$Plate<-as.factor(mydata$Plate)
View(mydata)

cell.ct.model<-lmer(cells.per.mL~Genotype*Temp+(1|Plate), data=mydata)
#Check assumptions
plot(cell.ct.model)
#Merp, looks like a cone...
qqp(resid(cell.ct.model), "norm")
#Close to normal, though. Just higher values are outside CI. 

mydata$logct<-log(mydata$cells.per.mL)

logct.model<-lmer(logct~Genotype*Temp+(1|Plate), data=mydata)
plot(logct.model)
qqp(resid(logct.model), "norm")
anova(logct.model)
#Temp is sig

#Graph
SummaryCt <- mydata %>%
  group_by(Genotype, Temp) %>%
  summarize(mean=mean(cells.per.mL), SE=sd(cells.per.mL)/sqrt(length(na.omit(cells.per.mL))))
SummaryCt

Cell.ct.graph<-ggplot(SummaryCt, aes(x=Genotype, y=mean, fill=factor(Temp), group=factor(Temp)))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(plot.title = element_text(face = "bold", size=18), axis.text.x=element_text(color="black", size=13), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=14), axis.title.y = element_text(face="bold", color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Genotype", y="Cells/mL", fill="Temperature")+  #labels the x and y axes
  scale_fill_manual(values = c("skyblue3", "darkgoldenrod2", "firebrick3"), labels=c("26°C", "30°C","32°C"))+
  ggtitle("Average symbiont count (cells/mL) in polyps")+
  ggsave("Graphs/Polyps/Polyp.symbiont.cells.per.mL.pdf", width=11, height=6.19, dpi=300, unit="in")
Cell.ct.graph

#Does polyp are differ between genos/temp?####
mydata<-read.csv("Data/Polyp_PnR_full_combined_data.csv")
mydata$Temp<-as.factor(mydata$Temp)
mydata$Plate<-as.factor(mydata$Plate)
View(mydata)

polyp.area.model<-lmer(Area~Genotype*Temp+(1|Plate), data=mydata, na.action="na.omit")
#Check assumptions
plot(polyp.area.model)
qqp(resid(polyp.area.model), "norm")
#Woooooow it's normal for once. 
anova(polyp.area.model)

SummaryArea <- mydata %>%
  group_by(Genotype, Temp) %>%
  summarize(mean=mean(Area, na.rm=TRUE), SE=sd(Area, na.rm=TRUE)/sqrt(length(na.omit(Area))))
SummaryArea

Polyp.area.graph<-ggplot(SummaryArea, aes(x=Genotype, y=mean, fill=factor(Temp), group=factor(Temp)))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(plot.title = element_text(face = "bold", size=18), axis.text.x=element_text(color="black", size=13), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=14), axis.title.y = element_text(face="bold", color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Genotype", y="Polyp Area (mm^2)", fill="Temperature")+  #labels the x and y axes
  scale_fill_manual(values = c("skyblue3", "darkgoldenrod2", "firebrick3"), labels=c("26°C", "30°C","32°C"))+
  ggtitle("Polyp Area")+
  ggsave("Graphs/Polyps/Polyp.area.pdf", width=11, height=6.19, dpi=300, unit="in")
Polyp.area.graph



#Does cell density (avg ct/area) differ between genos/temp?####
#cell density = cells.per.polyp/polyp area (in mm2) = cells/mm2
cell.density.model<-lm(cells.per.area~Genotype*Temp*Plate, data=mydata, na.action="na.omit")
#Check assumptions
plot(cell.density.model)
#Merp, looks like a cone...
qqp(resid(cell.density.model), "norm")
#Close to normal.

mydata$log.cell.density<-log(mydata$cells.per.area)
log.cd.model<-lm(log.cell.density~Genotype*Temp*Plate, data=mydata, na.action="na.omit")
qqp(resid(log.cd.model), "norm")
anova(log.cd.model)

#SO I get a warning with this model but it still runs
cell.density.model<-lmer(cells.per.area~Genotype*Temp+(1|Plate), data=mydata, na.action="na.omit")
plot(cell.density.model)
qqp(resid(cell.density.model), "norm") #Normal
anova(cell.density.model)


Summary <- mydata %>%
  group_by(Genotype, Temp) %>%
  summarize(mean=mean(cells.per.area, na.rm=TRUE), SE=sd(cells.per.area, na.rm=TRUE)/sqrt(length(na.omit(cells.per.area))))
Summary

Cells.per.area.graph<-ggplot(Summary, aes(x=Genotype, y=mean, fill=factor(Temp), group=factor(Temp)))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(plot.title = element_text(face = "bold", size=18), axis.text.x=element_text(color="black", size=13), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=14), axis.title.y = element_text(face="bold", color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Genotype", y="Cells per polyp Area (mm^2)", fill="Temperature")+  #labels the x and y axes
  scale_fill_manual(values = c("skyblue3", "darkgoldenrod2", "firebrick3"), labels=c("26°C", "30°C","32°C"))+
  ggtitle("Cell density in polyps")+
  ggsave("Graphs/Polyps/Polyp.cells.per.area.pdf", width=11, height=6.19, dpi=300, unit="in")
Cells.per.area.graph

#Running PnR by cell count models with polyp area as covariate####
mydata<-read.csv("Data/Polyp_PnR_full_combined_data.csv")
mydata$Temp<-as.factor(mydata$Temp)
mydata$Plate<-as.factor(mydata$Plate)
View(mydata)

model1<-lmer(Resp.per.bill.cell~Genotype*Temp*Area+(1|Plate), data=mydata)
#Check assumptions
plot(model1)
#Merp, looks like a cone...
qqp(resid(model1), "norm")
#Noooope

#I want to log transform, but variables range from -56 to 23
mydata$logResp<-log(mydata$Resp.per.bill.cell+58)
logresp.model<-lmer(logResp~Genotype*Temp*Area+(1|Plate), data=mydata)
#Check assumptions
plot(logresp.model)
qqp(resid(logresp.model), "norm")
#Outliers: 154, 143

#loglog
mydata$loglogResp<-log(mydata$logResp)
loglogresp.model<-lmer(loglogResp~Genotype*Temp*Area+(1|Plate), data=mydata)
plot(loglogresp.model)
qqp(resid(loglogresp.model), "norm")
#Only slightly better than just one log 

anova(loglogresp.model)
#Linear relationship?
plot(Resp.per.bill.cell~Area, data=mydata)
#Well, kinda. Just cuz most of the values are zero. 
plot(avgct~Area, data=mydata)
plot(Resp~Area, data=mydata)

model2<-lmer(Resp.per.bill.cell~Geno*Temp+Area+(1|Plate), data=mydata)
qqp(resid(model2), "norm")
anova(model2)
summary(model2)

