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
library(MASS)
library(lmtest)
library(patchwork)


#Combining and cleaning data sets to get PnR data####
#Load data
pnr.data_wrong<-read.csv("Data/PnR/Jan_Polyp_PnR.csv")
polyp.data<-read.csv("Data/Thesis_PolypData_Summary.csv")
area.data<-read.csv("Data/PolypAreas.csv")

#Notes: 
#The values in Respiration, NP, and GP are slope values minus the average of the blank wells (DI)
#Units: umol O2 per L per minute

#Dec 23, 2020: Turns out I got GP and NP mixed up. 
#I had the light slopes (after subtracting the blanks and averaging) as GP instead of NP
#And calculated NP as GP-Resp, and since resp is negative, it was GP+|Resp|
#But, the light slope should be NP and GP=NP-Resp AKA NP+|Resp| (GP must be greater than NP!)
#So, I calculated it correctly, NP and GP are just switched. So rather than switching them in the data, I'm gonna switch them here so I know what I did
pnr.data<-pnr.data_wrong %>% 
  rename(
    GrossPhoto_fixed = NetPhoto,
    NetPhoto_fixed = GrossPhoto
  )
#Double checking that I did that correctly.
summary(pnr.data_wrong$GrossPhoto)
summary(pnr.data$NetPhoto_fixed)
#Same. Good. 
summary(pnr.data_wrong$NetPhoto)
summary(pnr.data$GrossPhoto_fixed)
#Same.Good.
#Going to rename the columns back to just NetPhoto and GrossPhoto
pnr.data<-pnr.data %>% 
  rename(
    GrossPhoto = GrossPhoto_fixed,
    NetPhoto = NetPhoto_fixed
  )
#So GP and NP has been fixed. pnr.data is correct, all the following calculations will now be done with the correct GP and NP.

#I need to combine data sets so that the area, PnR values, and temp/plate number are all together
#they need to be combined by Polyp ("WellNum")

#Start with pnr and pic data
pnr.area.data<-full_join(pnr.data, area.data, by = "WellNum", copy = FALSE, suffix = c(".ph", ".ar"))

#Combine that with polyp summary data
all.data<-full_join(pnr.area.data, polyp.data, by = "WellNum", copy = FALSE)
#Wow that worked seemlessly. Go me!

#Need to make temp and plate factors
all.data$Temp<-as.factor(all.data$Temp)
all.data$Plate<-as.factor(all.data$Plate)

#Now, I need to add column for average count
all.data <- mutate(all.data, avgct = ((Count1+Count2)/2))

#Now, all pnr values divided by cell density

#Current count is in #cells/0.1mm3 
#Convert: #cells/0.1mm3 * (1000 mm3/mL) = # cells/mL 
#This is the cell density in the POLYP not in the well of the photoresp
all.data <- mutate(all.data, polyp.cells.per.mL = ((avgct)/0.1)*1000)

# avgct(in cells/mm3) * 150uL = cells per polyp (because I crushed each polyp in 150mL). No units, just a count.
all.data <- mutate(all.data, cells.per.polyp = (avgct*150))

#cell density = cells.per.polyp/polyp area (in mm2) = cells/mm2
all.data<-mutate(all.data, cells.per.area=(cells.per.polyp/Area))

#Now, each slope divided by cells.per.area 
#Slope= (O2 umol/L)/second
#slope/cells.per.area = [(O2 umol/L)/sec]/(cells/mm2)
#Respiration
all.data<-mutate(all.data, Resp.polyp.area.density = (Respiration/cells.per.area))
#And just because they are tiny numbers, let's multiply that by 10^9 (one billion)
all.data<-mutate(all.data, Resp.polyp.area.billion.cells=(Resp.polyp.area.density*1000000000))
#So now it is [(O2 umol/L)/sec]/(10^9 cells/mm2)

#Gross Photosynthesis
all.data<-mutate(all.data, GP.polyp.area.density = (GrossPhoto/cells.per.area))
all.data<-mutate(all.data, GP.polyp.area.billion.cells = (GP.polyp.area.density*1000000000))

#Net photosynthesis
all.data<-mutate(all.data, NP.polyp.area.density = (NetPhoto/cells.per.area))
all.data<-mutate(all.data, NP.polyp.area.billion.cells = (NP.polyp.area.density*1000000000))


#So I also want to test if I run everything without accounting for area, just doing total cells
#Slope (labeled as respiration, netphoto, or grossphoto)= (O2 umol/L)/second
#Final PnR measurements will be umol of O2/second/cell

#I need to divide by volume of the photorespirometer well to get cells/L when taking PnR measurments
#Volume of the well is 2mL=0.002L
all.data <- mutate(all.data, well.cells.per.L = (cells.per.polyp/0.002))

#Respiration
all.data<-mutate(all.data, Resp.per.cell = (Respiration/well.cells.per.L))
#And just because they are tiny numbers, let's multiply that by 10^9 (one billion)
all.data<-mutate(all.data, Resp.per.bill.cell=(Resp.per.cell*1000000000))

#Gross Photosynthesis
all.data<-mutate(all.data, GP.per.cell = (GrossPhoto/well.cells.per.L))
all.data<-mutate(all.data, GP.per.bill.cell = (GP.per.cell*1000000000))

#Net photosynthesis
all.data<-mutate(all.data, NP.per.cell = (NetPhoto/well.cells.per.L))
all.data<-mutate(all.data, NP.per.bill.cell = (NP.per.cell*1000000000))

#Finally, remove all NA rows so that I just have the clean PnR data
all.pnr.data<-subset(all.data, NP.per.bill.cell != "NA")

#I need WellNum, Date, Genotype, Temp, Plate, and Resp,GP,NP per.bill.cell
pnr.data.cleaned = all.pnr.data[c("WellNum", "Date", "Genotype", "Temp", "Plate", "Resp.per.bill.cell", "GP.per.bill.cell", "NP.per.bill.cell")]

#Export as CSV so I don't have to go through all of that every time. 
write.csv(pnr.data.cleaned,"Data/Polyp_PnR_data_cleaned.csv", row.names = FALSE)
#Exporting full pnr data, too, so I have counts and everything. 
write.csv(all.pnr.data, "Data/Polyp_PnR_full_combined_data.csv", row.names= FALSE)

#Graphs for PnR by number of cells####
rm(list=ls())
mydata<-read.csv("Data/Polyp_PnR_data_cleaned.csv")
mydata$Temp<-as.factor(mydata$Temp)

#Resp graphs####
SummaryResp <- mydata %>%
  group_by(Genotype, Temp) %>%
  summarize(mean=mean(Resp.per.bill.cell), SE=sd(Resp.per.bill.cell)/sqrt(length(na.omit(Resp.per.bill.cell))))

pal<-c("#679A99", "#9DB462", "#E4C7E5") #blue, green, pink
Resp.polyp.graph<-ggplot(SummaryResp, aes(x=Genotype, y=mean, fill=factor(Temp), group=factor(Temp)))+  #basic plot
  labs(x="Symbiont Genotype", y="Maximum growth rate (r)", fill="Temperature")+#labels the x and y axes
  theme_bw()+ #Removes grey background
  theme(axis.text.x=element_text(color="black", size=12), 
        axis.text.y=element_text(color="black", size=12), 
        axis.title.x = element_text(color="black", size=16),
        axis.title.y = element_text(color="black", size=16), 
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 13), 
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  geom_bar(color="black", stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  scale_fill_manual(values=pal, labels = c("26°C", "30°C", "32°C"))+
  labs(x="Symbiont Genotype", y=expression(Respiration~(µmol~O[2]~min^{"-1"}~10^{"9"}~cells^{"-1"})), fill="Temperature")+  #labels the x and y axes
  ggtitle("Respiration of Holobiont")
Resp.polyp.graph
ggsave("Graphs/FinalGraphs/Polyp_Resp_Bar.png", width=8, height=5)

#Resp boxplot
pal<-c("#679A99", "#9DB462", "#E4C7E5") #blue, green, pink
polyp.Resp.boxplot<-mydata%>%
  ggplot(aes(x=Genotype, y=Resp.per.bill.cell, fill=Temp))+
  geom_boxplot()+
  theme_bw()+
  geom_point(pch=21, position=position_jitterdodge(jitter.width=0.1), size=1)+
  theme(plot.title = element_text(face = "bold", size=16),
        axis.text.x=element_text(color="black", size=11), 
        axis.text.y=element_text(color="black", size=12), 
        axis.title.x = element_text(color="black", size=16), 
        axis.title.y = element_text(color="black", size=16),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  scale_fill_manual(values = pal, labels=c("26°C", "30°C","32°C"))+
  labs(fill="Temperature", x="Symbiont Genotype", y=expression(Respiration~(µmol~O[2]~min^{"-1"}~10^{"9"}~cells^{"-1"})))+
  ggtitle("Respiration of Holobiont")
polyp.Resp.boxplot
polyp.Resp.boxplot+ggsave("Graphs/FinalGraphs/Polyp_Resp_boxplot.png", width=8, height=5)

#Resp boxplot NO JITTER
pal<-c("#679A99", "#9DB462", "#E4C7E5") #blue, green, pink
polyp.Resp.boxplot.nojitter<-mydata%>%
  ggplot(aes(x=Genotype, y=Resp.per.bill.cell, fill=Temp))+
  geom_boxplot()+
  theme_bw()+
  theme(plot.title = element_text(face = "bold", size=16),
        axis.text.x=element_text(color="black", size=11), 
        axis.text.y=element_text(color="black", size=12), 
        axis.title.x = element_text(color="black", size=16), 
        axis.title.y = element_text(color="black", size=16),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  scale_fill_manual(values = pal, labels=c("26°C", "30°C","32°C"))+
  labs(fill="Temperature", x="Symbiont Genotype", y=expression(Respiration~(µmol~O[2]~min^{"-1"}~10^{"9"}~cells^{"-1"})))+
  ggtitle("Respiration of Holobiont")
polyp.Resp.boxplot.nojitter
polyp.Resp.boxplot.nojitter+ggsave("Graphs/FinalGraphs/Polyp_Resp_boxplot.nojitter.png", width=8, height=5)


#GP graphs####
SummaryGP <- mydata %>%
  group_by(Genotype, Temp) %>%
  summarize(mean=mean(GP.per.bill.cell), SE=sd(GP.per.bill.cell)/sqrt(length(na.omit(GP.per.bill.cell))))

pal<-c("#679A99", "#9DB462", "#E4C7E5") #blue, green, pink
GP.polyp.graph<-ggplot(SummaryGP, aes(x=Genotype, y=mean, fill=factor(Temp), group=factor(Temp)))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(axis.text.x=element_text(color="black", size=12), 
        axis.text.y=element_text(color="black", size=12), 
        axis.title.x = element_text(color="black", size=16),
        axis.title.y = element_text(color="black", size=16), 
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 13), 
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  geom_bar(color="black", stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  scale_fill_manual(values=pal, labels = c("26°C", "30°C", "32°C"))+
  labs(x="Symbiont Genotype", y=expression(GP~(µmol~O[2]~min^{"-1"}~10^{"9"}~cells^{"-1"})), fill="Temperature")+  #labels the x and y axes
  ggtitle("Gross Photosynthesis of Holobiont")
GP.polyp.graph
ggsave("Graphs/FinalGraphs/Polyp_GP_bargraph.png", width=8, height=5)

#GP boxplot
polyp.GP.boxplot<-mydata%>%
  ggplot(aes(x=Genotype, y=GP.per.bill.cell, fill=Temp))+
  geom_boxplot()+
  theme_bw()+
  geom_point(pch=21, position=position_jitterdodge(jitter.width=0.1), size=1)+
  theme(plot.title = element_text(face = "bold", size=16),
        axis.text.x=element_text(color="black", size=11), 
        axis.text.y=element_text(color="black", size=12), 
        axis.title.x = element_text(color="black", size=16), 
        axis.title.y = element_text(color="black", size=16),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  scale_fill_manual(values = pal, labels=c("26°C", "30°C","32°C"))+
  ggtitle("Grossphotosynthesis of Holobiont")+
  labs(fill="Temperature", x="Symbiont Genotype", y=expression(GP~(µmol~O[2]~min^{"-1"}~10^{"9"}~cells^{"-1"})))
polyp.GP.boxplot
polyp.GP.boxplot+ggsave("Graphs/FinalGraphs/Polyp_GP_boxplot.png", width=8, height=5)

#GP boxplot NO JITTER
polyp.GP.boxplot.nojitter<-mydata%>%
  ggplot(aes(x=Genotype, y=GP.per.bill.cell, fill=Temp))+
  geom_boxplot()+
  theme_bw()+
  theme(plot.title = element_text(face = "bold", size=16),
        axis.text.x=element_text(color="black", size=11), 
        axis.text.y=element_text(color="black", size=12), 
        axis.title.x = element_text(color="black", size=16), 
        axis.title.y = element_text(color="black", size=16),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  scale_fill_manual(values = pal, labels=c("26°C", "30°C","32°C"))+
  ggtitle("Grossphotosynthesis of Holobiont")+
  labs(fill="Temperature", x="Symbiont Genotype", y=expression(GP~(µmol~O[2]~min^{"-1"}~10^{"9"}~cells^{"-1"})))
polyp.GP.boxplot.nojitter
polyp.GP.boxplot.nojitter+ggsave("Graphs/FinalGraphs/Polyp_GP_boxplot_nojitter.png", width=8, height=5)

#NP graphs####
SummaryNP <- mydata %>%
  group_by(Genotype, Temp) %>%
  summarize(mean=mean(NP.per.bill.cell), SE=sd(NP.per.bill.cell)/sqrt(length(na.omit(NP.per.bill.cell))))

pal<-c("#679A99", "#9DB462", "#E4C7E5") #blue, green, pink
NP.polyp.graph<-ggplot(SummaryNP, aes(x=Genotype, y=mean, fill=factor(Temp), group=factor(Temp)))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(axis.text.x=element_text(color="black", size=12), 
        axis.text.y=element_text(color="black", size=12), 
        axis.title.x = element_text(color="black", size=16),
        axis.title.y = element_text(color="black", size=16), 
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 13), 
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  geom_bar(color="black", stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  scale_fill_manual(values=pal, labels = c("26°C", "30°C", "32°C"))+
  labs(x="Symbiont Genotype", y=expression(NP~(µmol~O[2]~min^{"-1"}~10^{"9"}~cells^{"-1"})), fill="Temperature")+  #labels the x and y axes
  ggtitle("Net Photosynthesis of Holobiont")
NP.polyp.graph
NP.polyp.graph+ggsave("Graphs/FinalGraphs/Polyp_NP_bar.png", width=8, height=5)

#NP boxplot
polyp.NP.boxplot<-mydata%>%
  ggplot(aes(x=Genotype, y=NP.per.bill.cell, fill=Temp))+
  geom_boxplot()+
  theme_bw()+
  geom_point(pch=21, position=position_jitterdodge(jitter.width=0.1), size=1)+
  theme(plot.title = element_text(face = "bold", size=16),
        axis.text.x=element_text(color="black", size=11), 
        axis.text.y=element_text(color="black", size=12), 
        axis.title.x = element_text(color="black", size=16), 
        axis.title.y = element_text(color="black", size=16),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  scale_fill_manual(values = pal, labels=c("26°C", "30°C","32°C"))+
  labs(fill="Temperature", x="Symbiont Genotype", y=expression(NP~(µmol~O[2]~min^{"-1"}~10^{"9"}~cells^{"-1"})))+
  ggtitle("Net Photosynthesis of Holobiont")
polyp.NP.boxplot
polyp.NP.boxplot+ggsave("Graphs/FinalGraphs/Polyp_NP_box.png", width=8, height=5)

#NP boxplot NO JITTER
polyp.NP.boxplot.nojitter<-mydata%>%
  ggplot(aes(x=Genotype, y=NP.per.bill.cell, fill=Temp))+
  geom_boxplot()+
  theme_bw()+
  theme(plot.title = element_text(face = "bold", size=16),
        axis.text.x=element_text(color="black", size=11), 
        axis.text.y=element_text(color="black", size=12), 
        axis.title.x = element_text(color="black", size=16), 
        axis.title.y = element_text(color="black", size=16),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  scale_fill_manual(values = pal, labels=c("26°C", "30°C","32°C"))+
  labs(fill="Temperature", x="Symbiont Genotype", y=expression(NP~(µmol~O[2]~min^{"-1"}~10^{"9"}~cells^{"-1"})))+
  ggtitle("Net Photosynthesis of Holobiont")
polyp.NP.boxplot.nojitter
polyp.NP.boxplot.nojitter+ggsave("Graphs/FinalGraphs/Polyp_NP_box_nojitter.png", width=8, height=5)

#Patchwork PnR plots####
#Making plots simplified. Removing titles, x-axis from GP and NP, and units on y-axis
Resp2<-ggplot(SummaryResp, aes(x=Genotype, y=mean, fill=factor(Temp), group=factor(Temp)))+  #basic plot
  labs(x="Symbiont Genotype", y="Maximum growth rate (r)", fill="Temperature")+#labels the x and y axes
  theme_bw()+ #Removes grey background
  theme(axis.text.x=element_text(color="black", size=12), 
        axis.text.y=element_text(color="black", size=12), 
        axis.title.x = element_text(color="black", size=16),
        axis.title.y = element_text(color="black", size=16), 
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 13), 
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  geom_bar(color="black", stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  scale_fill_manual(values=pal, labels = c("26°C", "30°C", "32°C"))+
  labs(x="Symbiont Genotype", y="Respiration", fill="Temperature")  #labels the x and y axes

GP2<-ggplot(SummaryGP, aes(x=Genotype, y=mean, fill=factor(Temp), group=factor(Temp)))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(axis.text.x=element_blank(), 
        axis.text.y=element_text(color="black", size=12), 
        axis.title.x = element_text(color="black", size=16),
        axis.title.y = element_text(color="black", size=16), 
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 13), 
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  geom_bar(color="black", stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  scale_fill_manual(values=pal, labels = c("26°C", "30°C", "32°C"))+
  labs(x="", y="Gross Photosynthesis", fill="Temperature")  #labels the x and y axes
GP2

NP2<-ggplot(SummaryNP, aes(x=Genotype, y=mean, fill=factor(Temp), group=factor(Temp)))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(axis.text.x=element_blank(), 
        axis.text.y=element_text(color="black", size=12), 
        axis.title.x = element_text(color="black", size=16),
        axis.title.y = element_text(color="black", size=16), 
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 13), 
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  geom_bar(color="black", stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  scale_fill_manual(values=pal, labels = c("26°C", "30°C", "32°C"))+
  labs(x="", y="Net Photosynthesis", fill="Temperature")
NP2

pw<-GP2/NP2/Resp2+plot_layout(guides = 'collect')+plot_annotation(tag_levels = 'a', tag_prefix = '(',tag_suffix = ')')
pw+ggsave("Graphs/FinalGraphs/polyps_PnR_patchwork.png",width=9, height=12)

#
#PnR graphs by count/polyp area #####
mydata<-read.csv("Data/Polyp_PnR_data_cleaned.csv")
mydata$Temp<-as.factor(mydata$Temp)

#Resp graphs
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
polyp.Resp.boxplot
polyp.Resp.boxplot+ggsave("Graphs/PnR/PolypPnR/PolypRespbyArea.boxplot.png", width=10, height=5)


#GP counts/area graphs
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

#NP boxplot
polyp.NP.boxplot<-mydata%>%
  ggplot(aes(x=Genotype, y=NP, fill=Temp))+
  geom_boxplot()+
  theme_bw()+
  ggtitle("Net Photosynthesis of Holobiont")+
  geom_jitter(color="black", size=0.5, alpha=0.7)+
  theme(plot.title = element_text(face="bold"), 
        axis.text.x=element_text(size=10), 
        axis.text.y=element_text(size=10), 
        axis.title.y = element_text(face="bold", size=12), 
        axis.title.x = element_text(face="bold", size=12))+
  labs(fill="Temperature", x="Genotype", y=expression(Net~Photo.~(µmol~O[2]/min/10^{"9"}~cells)))+
  scale_fill_manual(values = c("#79CFDB", "#859A51", "#DFADE1"), labels=c("26°C", "30°C","32°C"))
polyp.NP.boxplot+ggsave("Graphs/PnR/PolypPnR/PolypNPbyArea.boxplot.png", width=10, height=5)


#Polyp PnR Stats####

#Stats with slopes/cell count####
rm(list=ls())
mydata<-read.csv("Data/Polyp_PnR_data_cleaned.csv")
mydata<-mydata%>%
  mutate_if(is.character, as.factor)
mydata$Plate<-as.factor(mydata$Plate)
mydata$Temp<-as.factor(mydata$Temp)

summary(mydata)
#These ranges are stupid... ugh this data sucks.  
#Resp: -1121 to 472
#NP: -788 to 366
#GP: -703 to 1312

#Outlier test####
# Outlier removal by the Tukey rules on quartiles +/- 1.5 IQR
# 2017 Klodian Dhana


outlierKD <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  tot <- sum(!is.na(var_name))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(var_name))
  message("Outliers identified: ", na2 - na1, " from ", tot, " observations")
  message("Proportion (%) of outliers: ", (na2 - na1) / tot*100)
  message("Mean of the outliers: ", mo)
  m2 <- mean(var_name, na.rm = T)
  message("Mean without removing outliers: ", m1)
  message("Mean if we remove outliers: ", m2)
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    message("Outliers successfully removed", "\n")
    return(invisible(dt))
  } else{
    message("Nothing changed", "\n")
    return(invisible(var_name))
  }
}
outlierKD(mydata, Resp.per.bill.cell)
outlierKD(mydata, NP.per.bill.cell)
outlierKD(mydata, GP.per.bill.cell)
mydata2<-mydata %>% drop_na()
summary<-mydata2%>%
  group_by(Genotype, Temp)%>%
  count()
summary(mydata2)
#resp: -193-1123
#gp: -145-217
#np: -143-130
mydata2$logResp<-log(mydata2$Resp.per.bill.cell+194)
mydata2$logGP<-log(mydata2$GP.per.bill.cell+146)
mydata2$logNP<-log(mydata2$NP.per.bill.cell+144)
resp.model<-lm(Resp.per.bill.cell~Genotype*Temp, data=mydata2)
plot(resp.model)
qqp(resid(resp.model), "norm")
Anova(resp.model, type="III")
GP.model<-lm(GP.per.bill.cell~Genotype*Temp, data=mydata2)
plot(GP.model)
Anova(GP.model, type="III")
NP.model<-lm(NP.per.bill.cell~Genotype*Temp, data=mydata2)
plot(NP.model)
Anova(NP.model, type="III")

#Respiration####
resp.model.full<-lm(Resp.per.bill.cell~Genotype*Temp*Plate, data=mydata)
#Check assumptions
plot(resp.model.full)
qqp(resid(resp.model.full), "norm")
#Not really normal at ends (surprise, surprise). But actually a little closer to normal than I expected. 
#154, 156, 157 outliers
df2 <- mydata %>% slice(-c(143, 154))
full.model.df2<-lm(Resp.per.bill.cell~Genotype*Temp*Plate, data=df2)
plot(full.model.df2)
qqp(resid(full.model.df2))
#New outliers pop up, and still just as not normal. 

resp.model.mixed<-lmer(Resp.per.bill.cell~Genotype*Temp+(1|Plate), data=mydata)
resp.model.simple<-lm(Resp.per.bill.cell~Genotype*Temp, data=mydata)
resp.model.super.simple<-lm(Resp.per.bill.cell~Genotype+Temp, data=mydata)

lrtest(resp.model.full, resp.model.simple)
#Sig = use full model
AIC(resp.model.full)#2820
AIC(resp.model.simple)#2792
AIC(resp.model.super.simple)#2780
AIC(resp.model.mixed)#2621

Anova(resp.model.mixed, type="III")
Anova(resp.model.full, type="III")

#BoxCox Respiration####
#Response variable must be positive. Adding 1122
mydata$PosResp<-mydata$Resp.per.bill.cell+1122
full.resp.model<-lm(PosResp ~ Genotype*Temp*Plate, data=mydata)
step.resp.model<-stepAIC(full.resp.model, direction="both", trace = F)

boxcox<-boxcox(step.resp.model,lambda = seq(-5, 5, 1/1000),plotit = TRUE )

Selected.Power<-boxcox$x[boxcox$y==max(boxcox$y)]
Selected.Power
#2.28
mydata$boxresp<-mydata$PosResp^2.28
box.model.resp<-lm(boxresp~Genotype*Temp*Plate, data=mydata)
plot(box.model.resp)
qqp(resid(box.model.resp), "norm")
#Not really normal, but at least the variances are much better. 

mydata$logboxresp<-log(mydata$boxresp)
log.box.model.resp<-lm(logboxresp~Genotype*Temp*Plate, data=mydata)
qqp(resid(log.box.model.resp), "norm")
plot(log.box.model.resp)
#So logging it actually really messes it up a lot. 

box.resp.model.simp<-lm(boxresp~Genotype*Temp, data=mydata)
box.resp.model.super.simp<-lm(boxresp~Genotype+Temp, data=mydata)
box.resp.mixed.model<-lmer(boxresp~Genotype*Temp+(1|Plate), data=mydata)
#eigenvalue close to zero, so model doesn't converge. 

AIC(box.model.resp)#6832
AIC(box.resp.model.simp)#6818
AIC(box.resp.model.super.simp)#6807
AIC(box.resp.mixed.model)#6305

lrtest(box.resp.model.simp, box.model.resp)
#Sig = use bigger model
lrtest(box.resp.model.super.simp, box.resp.model.simp)
#Not sig - use simpler model. So confused. 
lrtest(box.resp.model.super.simp, box.model.resp)
#Sig=use full model

Anova(box.model.resp, type="III")
Anova(box.resp.model.simp, type="III")
Anova(box.resp.model.super.simp, type="III")
Anova(box.resp.mixed.model, type="III")

#log respiration####
#I want to log transform
mydata$logResp<-log(mydata$Resp.per.bill.cell+1122)
logresp.model.full<-lm(logResp~Genotype*Temp*Plate, data=mydata)
#Check assumptions
plot(logresp.model.full)
qqp(resid(logresp.model.full), "norm")

logresp.model.simp<-lm(logResp~Genotype*Temp, data=mydata)
plot(logresp.model.simp)
qqp(resid(logresp.model.simp), "norm")

logresp.model.super.simp<-lm(logResp~Genotype+Temp, data=mydata)
plot(logresp.model.super.simp)
qqp(resid(logresp.model.super.simp), "norm")

logresp.model.mixed<-lmer(logResp~Genotype*Temp+(1|Plate), data=mydata)

AIC(logresp.model.mixed) #425
AIC(logresp.model.full)#432
AIC(logresp.model.simp) #393
AIC(logresp.model.super.simp)#380

Anova(logresp.model.full, type="III")

lrtest(logresp.model.simp, logresp.model.full)
#Not sig (but close to sig) = smaller model better.

lrtest(logresp.model.super.simp, logresp.model.simp)
#Not sig=simpler model is better

lrtest(logresp.model.super.simp, logresp.model.full)
#Not sig=simpler model is better

Anova(logresp.model.full, type="III") #Plate is not significant in any interaction, so can remove
Anova(logresp.model.super.simp, type="III") 
Anova(logresp.model.simp, type="III")
Anova(logresp.model.mixed, type="III")
#No matter what, nothing is significant. So I'm just gonna do the logged data 


#mean values Respiration####
Summary.resp <- mydata %>%
  group_by(Genotype, Temp) %>%
  summarize(mean=mean(Resp.per.bill.cell, na.rm=TRUE), SE=sd(Resp.per.bill.cell, na.rm=TRUE)/sqrt(length(na.omit(Resp.per.bill.cell))))
emm.resp = emmeans(resp.model, specs= pairwise~Genotype:Temp)
emm.resp<-as.data.frame(emm.resp$emmeans)
emm.resp$emmeans
summary(mydata)


#GP####
GP.model.mixed<-lmer(GP.per.bill.cell~Genotype*Temp+(1|Plate), data=mydata)
#Check assumptions
plot(GP.model.mixed)
qqp(resid(GP.model.mixed), "norm")
#Not really normal at ends again

mydata$logGP<-log(mydata$GP.per.bill.cell+704)
logGP.model.mixed<-lmer(logGP~Genotype*Temp+(1|Plate), data=mydata)
plot(logGP.model.mixed)
qqp(resid(logGP.model.mixed), "norm")


logGP.model.simp<-lm(logGP~Genotype*Temp, data=mydata)
logGP.model.super.simp<-lm(logGP~Genotype+Temp, data=mydata)
logGP.model.full<-lm(logGP~Genotype*Plate*Temp, data=mydata)
plot(logGP.model.full)
qqp(resid(logGP.model.full), "norm")
#199, 195, 197 outliers

AIC(logGP.model.mixed)#347
AIC(logGP.model.simp)#307
AIC(logGP.model.super.simp)#296
AIC(logGP.model.full)#336

#Super simple model is best
Anova(logGP.model.full, type="III")
#Plate is not sig, so I can take it out
Anova(logGP.model.super.simp, type="III")
Anova(logGP.model.mixed, type="III")
Anova(logGP.model.simp, type="III")
#AIC says to use super simple model. 
lrtest(logGP.model.super.simp, logGP.model.simp)#not sig

#Mean values GP
Summary.GP <- mydata %>%
  group_by(Genotype, Temp) %>%
  summarize(mean=mean(GP.per.bill.cell, na.rm=TRUE), SE=sd(GP.per.bill.cell, na.rm=TRUE)/sqrt(length(na.omit(GP.per.bill.cell))))
emm.GP = emmeans(logGP.model.super.simp, specs= pairwise~Genotype:Temp)
emm.GP$emmeans

#NP####
NP.model.mixed<-lmer(NP.per.bill.cell~Genotype*Temp+(1|Plate), data=mydata)
#Check assumptions
plot(NP.model.mixed)
qqp(resid(NP.model.mixed), "norm")
#Not at ends, although it is very close
NP.model.full<-lm(NP.per.bill.cell~Genotype*Temp*Plate, data=mydata)
plot(NP.model.full)

NP.model.simp<-lm(NP.per.bill.cell~Genotype*Temp, data=mydata)
NP.model.super.simp<-lm(NP.per.bill.cell~Genotype+Temp, data=mydata)

AIC(NP.model.full)#2640
AIC(NP.model.simp)#2602
AIC(NP.model.super.simp)#2593
AIC(NP.model.mixed)#2448

#log NP####
mydata$logNP<-log(mydata$NP.per.bill.cell+788)

logNP.model.full<-lm(logNP~Genotype*Temp*Plate, data=mydata)
plot(logNP.model.full)
qqp(resid(logNP.model.full), "norm")
#Better

logNP.model.full<-lm(logNP~Genotype*Temp*Plate, data=mydata)
qqp(resid(logNP.model.full), "norm")
plot(logNP.model.full)

logNP.model.simp<-lm(logNP~Genotype*Temp, data=mydata)
logNP.model.super.simp<-lm(logNP~Genotype+Temp, data=mydata)
logNP.model.mixed<-lmer(logNP~Genotype*Temp+(1|Plate), data=mydata)


AIC(logNP.model.full)#364
AIC(logNP.model.simp)#324
AIC(logNP.model.super.simp)#316
AIC(logNP.model.mixed)#363

lrtest(NP.model.simp, NP.model.full)
#Not quite sig = smaller model better.

lrtest(NP.model.super.simp, NP.model.simp)
#Not sig=simpler model is better

lrtest(NP.model.super.simp, NP.model.full)
#Not quite sig = smaller model is better

Anova(logNP.model.full, type="III")#Plate*Geno is sig
Anova(logNP.model.mixed, type="III")
#But AIC and lrtest tells me to use simplest model

Anova(logNP.model.super.simp, type="III") #Neither are sig
Anova(logNP.model.simp, type="III") #Genotype is sig...
#I do say I use AIC to pick model. So all PnR results say to use simplest model. So I guess I'll do that

#Mean NP
Summary.NP <- mydata %>%
  group_by(Genotype, Temp) %>%
  summarize(mean=mean(NP.per.bill.cell, na.rm=TRUE), SE=sd(NP.per.bill.cell, na.rm=TRUE)/sqrt(length(na.omit(NP.per.bill.cell))))


#Stats slope/(cells/polyp area))####
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

resp.model<-lm(Resp~Genotype*Temp*Plate, data=mydata)
summary(mydata)

#I want to log transform, but variables range from -12 to 
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
#Response variable must be positive. Adding 1121
mydata$PosResp<-mydata$Resp.per.bill.cell+1121
full.resp.model<-lm(PosResp ~ Genotype*Temp*Plate, data=mydata)
step.resp.model<-stepAIC(full.resp.model, direction="both", trace = F)

boxcox<-boxcox(step.resp.model,lambda = seq(-5, 5, 1/1000),plotit = TRUE )

Selected.Power<-boxcox$x[boxcox$y==max(boxcox$y)]
Selected.Power
#2.177

mydata$Resp.trans<-(mydata$PosResp)^2.177
trans.resp.model<-lm(Resp.trans~Genotype*Temp*Plate, data = mydata)
qqp(resid(trans.resp.model), "norm")
#Well that didn't work at all
hist(mydata$Resp.trans)
#It looks very normal in this, though. 
Anova(trans.resp.model, type="III")
plot(trans.resp.model)

trans.resp.model2<-lmer(Resp.trans~Genotype*Temp+(1|Plate), data=mydata)
qqp(resid(trans.resp.model2), "norm")
Anova(trans.resp.model2, type="III")

trans.resp.model3<-lm(Resp.trans~Genotype*Temp, data=mydata)
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


#Comparing blank slopes to polyp slopes. To compare randomness
polyp.pnr.data<-read.csv("Data/PnR/Jan_Polyp_PnR.csv")
blank.pnr.data<-read.csv("Data/PnR/Polyp_PnR_BlankSlopes.csv")
polyp.data<-read.csv("Data/Thesis_PolypData_Summary.csv")

#Add temp to polyp pnr data
polyp.data.temp<-full_join(polyp.pnr.data, polyp.data, by = "WellNum", copy = FALSE)
#Adding blanks
pnr.data<-full_join(blank.pnr.data, polyp.data.temp, copy = FALSE)
#Removing columns I don't need
pnr.data.cleaned = pnr.data[c("Date", "Genotype", "Temp", "Plate", "WellNum", "DarkSlope", "LightSlope")]
mydata<-pnr.data.cleaned%>%
  mutate(Genotype = replace_na(Genotype, "Blank"))
mydata$Temp<-as.factor(mydata$Temp)
mydata$Genotype<-as.factor(mydata$Genotype)


polyp.darkslope.boxplot<-mydata%>%
  ggplot(aes(x=Genotype, y=DarkSlope, fill=Temp))+
  geom_boxplot()+
  theme_bw()+
  ggtitle("Dark Slopes of Polyps")+
  geom_jitter(color="black", size=0.5, alpha=0.7)+
  theme(plot.title = element_text(face="bold"), 
        axis.text.x=element_text(size=10), 
        axis.text.y=element_text(size=10), 
        axis.title.y = element_text(face="bold", size=12), 
        axis.title.x = element_text(face="bold", size=12))+
  labs(fill="Temperature", x="Genotype", y="Slope")+
  scale_fill_manual(values = c("#79CFDB", "#859A51", "#DFADE1"), labels=c("26°C", "30°C","32°C"))
polyp.darkslope.boxplot
polyp.darkslope.boxplot+ggsave("Graphs/PnR/PolypPnR/Polyp.darkslope.boxplot.png", width=8, height=5)


polyp.lightslope.boxplot<-mydata%>%
  ggplot(aes(x=Genotype, y=LightSlope, fill=Temp))+
  geom_boxplot()+
  theme_bw()+
  ggtitle("Light Slopes of Polyps")+
  geom_jitter(color="black", size=0.5, alpha=0.7)+
  theme(plot.title = element_text(face="bold"), 
        axis.text.x=element_text(size=10), 
        axis.text.y=element_text(size=10), 
        axis.title.y = element_text(face="bold", size=12), 
        axis.title.x = element_text(face="bold", size=12))+
  labs(fill="Temperature", x="Genotype", y="Slope")+
  scale_fill_manual(values = c("#79CFDB", "#859A51", "#DFADE1"), labels=c("26°C", "30°C","32°C"))
polyp.lightslope.boxplot
polyp.lightslope.boxplot+ggsave("Graphs/PnR/PolypPnR/Polyp.lightslope.boxplot.png", width=8, height=5)
