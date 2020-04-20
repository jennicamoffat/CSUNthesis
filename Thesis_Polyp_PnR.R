#Thesis
#Polyp Photosynthesis and Respiration
#Created on 4/17/2020 by Jennica Moffat

#Clear the environment
rm(list=ls())
#load libraries
library(tidyverse)
library(car)


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

#Finally, remove all NA rows so that I just have the clean PnR data
all.pnr.data<-subset(all.data, NP != "NA")
View(all.pnr.data)

#All I need is WellNum, Date, Genotype, Temp, Plate, Resp, GP, and NP
pnr.data.cleaned = all.pnr.data[c("WellNum", "Date", "Genotype", "Temp", "Plate", "Resp", "GP", "NP")]
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


