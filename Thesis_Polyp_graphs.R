#Cassiopea Polyp Data
#Just the graphs
#Created by Jennica Moffat 02162020

#Clear the environment
rm(list=ls())

#load libraries
library(tidyverse)
library(car)

#Load data
mydata<-read.csv("Data/Thesis_PolypData_Summary.csv")
mydata<-mydata%>%
  mutate_if(is.character,as.factor)
mydata$Temp<-as.factor(mydata$Temp)

#Aposymbiotic removed (for developmental timing and ephyra)
NoApoData <- mydata %>%
  filter(Genotype != "Aposymbiotic") %>%
  droplevels
#Apo and dead removed 
NoApoNoDeadData <- subset(NoApoData, Survive.to.End == "Yes")

#Adding columns for each developmental stage to see percentage that actually reached that stage in 28 days
Developed.data<-NoApoData%>%
  mutate(Inoc = ifelse(is.na(Days.to.Inoculation), "No", "Yes"),
         Strob=ifelse(is.na(Days.to.Strobilation), "No", "Yes"),
         Ephyra=ifelse(is.na(Days.to.Ephyra), "No", "Yes"))

#GRAPHS#
#Total ephyra production####
#Bargraph of total average ephyra production including dead ones####
TotalEphyra <- NoApoData %>%
  group_by(Genotype, Temp) %>%
  summarize(mean=mean(Total.Ephyra.Produced, na.rm=TRUE), SE=sd(Total.Ephyra.Produced, na.rm=TRUE)/sqrt(length(na.omit(Total.Ephyra.Produced))))

TotalEphyraPlot<-ggplot(TotalEphyra, aes(x=Genotype, y=mean, fill=factor(Temp), group=factor(Temp)))+  #basic plot
  theme_bw()+ #Removes grey background
  scale_y_continuous(expand=c(0,0), limits=c(0, 1.6))+
  theme(plot.title = element_text(face = "bold", size=16), axis.text.x=element_text(color="black", size=12), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(color="black", size=16), axis.title.y = element_text(color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Symbiont Strain", y="Number of ephyra", fill="Temperature")+  #labels the x and y axes
  ggtitle("Average Ephyra produced per Polyp")+
  scale_fill_manual(values = c("skyblue3", "darkgoldenrod2", "brown3"), labels=c("26°C", "30°C", "32°C"))
TotalEphyraPlot

TotalEphyraPlot+ggsave("Graphs/Polyps/EphyraProduced_deadincluded.png", width=8, height=5)

#Total Ephyra final plot
pal<-c("#79CFDB", "#859A51", "#DFADE1") #blue, green, pink (not colorblind friendly)
pal<-c("#2c7fb8", "#7fcdbb", "#edf8b1") #blue greens
pal<-c("#88CCEE", "#DDCC77", "#CC6677") #blue, yellow, orange
pal<-c("#ac8eab", "#f2cec7", "#c67b6f") #purples

Ephyra.final<-ggplot(TotalEphyra, aes(x=Genotype, y=mean, fill=factor(Temp), group=factor(Temp)))+  #basic plot
  theme_bw()+ #Removes grey background
  labs(x="Symbiont Genotype", y="Ephyra Produced", fill="Temperature")+#labels the x and y axes
  theme(axis.text.x=element_text(color="black", size=11), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(color="black", size=16), axis.title.y = element_text(color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(color="black", stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  scale_fill_manual(values=pal, labels = c("26°C", "30°C", "32°C"))+
  scale_y_continuous(expand=c(0,0), limits=c(0,1.6))
Ephyra.final
Ephyra.final+ggsave("Graphs/FinalGraphs/polyp_totalephyra.png", width=8, height=5)

#Just temp, not geno
TotalEphyraTemp <- NoApoData %>%
  group_by(Temp) %>%
  summarize(mean=mean(Total.Ephyra.Produced, na.rm=TRUE), SE=sd(Total.Ephyra.Produced, na.rm=TRUE)/sqrt(length(na.omit(Total.Ephyra.Produced))))
TotalEphyraTemp

TotalEphyraPlotTemp<-ggplot(TotalEphyraTemp, aes(x=Temp, y=mean))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(axis.text.x=element_text(color="black", size=14), axis.text.y=element_text(face="bold", color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=16), axis.title.y = element_text(face="bold", color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6, fill="mediumorchid4") + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Temperature", y="Average Ephyra per polyp")+  #labels the x and y axes
  ggtitle("Average Ephyra produced per Polyp (dead included)")
  
TotalEphyraPlotTemp+ggsave("Graphs/Polyps/EphyraByTemp.png", width=10, height=5)

#Just geno, not temp
TotalEphyraGeno <- NoApoData %>%
  group_by(Genotype) %>%
  summarize(mean=mean(Total.Ephyra.Produced, na.rm=TRUE), SE=sd(Total.Ephyra.Produced, na.rm=TRUE)/sqrt(length(na.omit(Total.Ephyra.Produced))))
TotalEphyraGeno

TotalEphyraPlotGeno<-ggplot(TotalEphyraGeno, aes(x=Genotype, y=mean))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(axis.text.x=element_text(color="black", size=14), axis.text.y=element_text(face="bold", color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=16), axis.title.y = element_text(face="bold", color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6, fill="darkseagreen3") + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Genotype", y="Average Ephyra per polyp")+  #labels the x and y axes
  ggtitle("Average Ephyra produced per Polyp (dead included)")
TotalEphyraPlotGeno
TotalEphyraPlotGeno+ggsave("Graphs/Polyps/EphyraByGeno.png", width=10, height=5)

#Boxplot of total ephyra production####
NoApoDataEphyra<-NoApoData%>%
  filter(Total.Ephyra.Produced!="NA")

boxplot<-NoApoDataEphyra%>%
  ggplot(aes(x=Genotype, y=Total.Ephyra.Produced, fill=Temp))+
  geom_boxplot()+
  theme_bw()+
  geom_jitter(color="black", size=0.1, alpha=0.2)+
  scale_x_discrete(name = "Genotype") +
  scale_y_continuous(name = "Total Ephyra Produced")+
  theme(axis.text.x=element_text(color="black", size=11), 
        axis.text.y=element_text(color="black", size=12), 
        axis.title.x = element_text(color="black", size=16), 
        axis.title.y = element_text(color="black", size=16),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  labs(fill="Temperature")+
  scale_fill_manual(values = c("#ac8eab", "#f2cec7", "#c67b6f"), labels=c("26°C", "30°C","32°C"))
boxplot+ggsave("Graphs/FinalGraphs/TotalEphyraBox.png", width=8, height=5)
#Since they all either produced 0, 1, or 2, there's too many dots to use jitter
#I think bargraph is best to represent this one. 

violinplot<-NoApoDataEphyra%>%
  ggplot(aes(x=Genotype, y=Total.Ephyra.Produced, fill=Temp))+
  geom_violin()+
  theme_bw()+
  scale_x_discrete(name = "Genotype") +
  scale_y_continuous(name = "Total Ephyra Produced")+
  ggtitle("Total ephyra production per polyp")+
  theme(plot.title = element_text(hjust=0.5, face="bold"), 
        axis.text.x=element_text(size=10), 
        axis.text.y=element_text(size=10), 
        axis.title.y = element_text(face="bold", size=12), 
        axis.title.x = element_text(face="bold", size=12))+
  labs(fill="Temperature")+
  scale_fill_manual(values = c("#79CFDB", "#859A51", "#DFADE1"), labels=c("26°C", "30°C","32°C"))
violinplot
#This shows nothing, really.

#New dataframe without dead
NoApoNoDeadData <- subset(NoApoData, Survive.to.End == "Yes")
View(NoApoNoDeadData)

#Average Total Ephyra Produced no dead
TotalEphyraNoDead <- NoApoNoDeadData %>%
  group_by(Genotype, Temp) %>%
  summarize(mean=mean(Total.Ephyra.Produced, na.rm=TRUE), SE=sd(Total.Ephyra.Produced, na.rm=TRUE)/sqrt(length(na.omit(Total.Ephyra.Produced))))
TotalEphyraNoDead

TotalEphyraNoDeadPlot<-ggplot(TotalEphyraNoDead, aes(x=Genotype, y=mean, fill=factor(Temp), group=factor(Temp)))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(axis.text.x=element_text(color="black", size=14), axis.text.y=element_text(face="bold", color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=16), axis.title.y = element_text(face="bold", color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Genotype", y="Average Ephyra per polyp", fill="Temperature")+  #labels the x and y axes
  ggtitle("Average Ephyra produced per Polyp (dead removed)")+
  scale_fill_manual(values = c("skyblue3", "darkgoldenrod2", "brown3"), labels=c("26°C", "30°C", "32°C"))
TotalEphyraNoDeadPlot

TotalEphyraNoDeadPlot+ggsave("Graphs/Polyps/EphyraProduced_deadremoved.pdf", width=11, height=6.19, dpi=300, unit="in")

#Average Total Ephyra Produced as percent that inoculated 
InoculatedData <- subset(NoApoData, Days.to.Inoculation != "NA")

EphyraifInoc <- InoculatedData %>%
  group_by(Genotype, Temp) %>%
  summarize(mean=mean(Total.Ephyra.Produced, na.rm=TRUE), SE=sd(Total.Ephyra.Produced, na.rm=TRUE)/sqrt(length(na.omit(Total.Ephyra.Produced))))
EphyraifInoc

EphyraifInocPlot<-ggplot(EphyraifInoc, aes(x=Genotype, y=mean, fill=factor(Temp), group=factor(Temp)))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(axis.text.x=element_text(color="black", size=14), axis.text.y=element_text(face="bold", color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=16), axis.title.y = element_text(face="bold", color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Genotype", y="Average Ephyra per polyp", fill="Temperature")+  #labels the x and y axes
  ggtitle("Average Ephyra produced per Polyp (if inoculated)")+
  scale_fill_manual(values = c("skyblue3", "darkgoldenrod2", "brown3"), labels=c("26°C", "30°C", "32°C"))
EphyraifInocPlot
ggsave("Graphs/Polyps/EphyraifInoc.pdf", width=11, height=6.19, dpi=300, unit="in")
#Nothing noticeably different from whole data set

#Ephyra production proportions####
NoApoData$Total.Ephyra.Produced<-as.factor(NoApoData$Total.Ephyra.Produced)
NoApoData$Total.Ephyra.Produced<-NoApoData$Total.Ephyra.Produced %>% replace_na("0")

ephyra.prop<-NoApoData%>%
  group_by(Genotype,Temp, Total.Ephyra.Produced)%>%
  tally()

pal<-c("#2c7fb8","#7fcdbb", "#edf8b1") #blue green yellow
ephyra.prop.bar<-ggplot(ephyra.prop, aes(x=Temp, y=n, fill=Total.Ephyra.Produced))+  #basic plot
  theme_minimal()+
  theme(axis.text.x=element_text(color="black", size=11), axis.text.y=element_text(color="black", size=11), axis.title.x = element_text(color="black", size=13),strip.text.x = element_text(size = 11, colour = "black"))+
  geom_bar(position=position_stack(), stat="identity", color="black")+
  scale_fill_manual(values=pal)+
  labs(x="Temperature (°C)", y="", fill="Total Ephyra by Polyp")+#labels the x and y axes
  scale_y_continuous(expand=c(0,0), limits=c(0,25))+
  facet_grid(~Genotype)
ephyra.prop.bar+ggsave("Graphs/FinalGraphs/TotalEphyraProportion.png", width=8, height=5)

#Time to strobilation####

#First, let's see how many even got to strobilation
total.strob<-Developed.data%>%
  group_by(Genotype, Temp, Strob)%>%
  tally()

pal<-c("#DFADE1", "#859A51") #green, pink (potentially not colorblind friendly)
pal<-c("#DFADE1", "#2c7fb8")
pal<-c("#2c7fb8","#7fcdbb") #blue green
strob.prop<-ggplot(total.strob, aes(x=Temp, y=n, fill=Strob))+  #basic plot
  theme_minimal()+
  theme(axis.text.x=element_text(color="black", size=11), axis.text.y=element_text(color="black", size=11), axis.title.x = element_text(color="black", size=13),strip.text.x = element_text(size = 11, colour = "black"))+
  geom_bar(position=position_stack(), stat="identity", color="black")+
  scale_fill_manual(values=pal)+
  labs(x="Temperature (°C)", y="", fill="Strobilated")+#labels the x and y axes
  scale_y_continuous(expand=c(0,0), limits=c(0,25))+
  facet_grid(~Genotype)
strob.prop
strob.prop+ggsave("Graphs/FinalGraphs/Strob.Proportion.png", width=8, height=5)

pal<-c("#79CFDB", "#859A51", "#DFADE1") #blue, green, pink (potentially not colorblind friendly)
strob.prop2<-ggplot(total.strob, aes(x=Genotype, y=n, fill=Temp, group=Temp, alpha=Strob))+  #basic plot
  theme_bw()+
  geom_bar(position="dodge", stat="identity")+
  scale_fill_manual(values=pal, labels = c("26°C", "30°C", "32°C"))
strob.prop2
#Ugh, this is so close to what I want, but I can't do dodge and stacked position in a single graph.


#Bargraph of days to reach strobilation
DaystoStrob <- NoApoData %>%
  group_by(Genotype, Temp) %>%
  summarize(mean=mean(Days.to.Strobilation, na.rm=TRUE), SE=sd(Days.to.Strobilation, na.rm=TRUE)/sqrt(length(na.omit(Days.to.Strobilation))))

pal<-c("#ac8eab", "#f2cec7", "#c67b6f") #purples
DaystoStrobBar<-ggplot(DaystoStrob, aes(x=Genotype, y=mean, fill=factor(Temp), group=factor(Temp)))+  #basic plot
  theme_bw()+ #Removes grey background
  scale_y_continuous(expand=c(0,0), limits=c(0, 25))+
  theme(axis.text.x=element_text(color="black", size=11), 
        axis.text.y=element_text(color="black", size=12), 
        axis.title.x = element_text(color="black", size=16), 
        axis.title.y = element_text(color="black", size=16),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(color="black", stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Symbiont Genotype", y="Days to Strobilation", fill="Temperature")+  #labels the x and y axes
  scale_fill_manual(values=pal, labels = c("26°C", "30°C", "32°C"))
DaystoStrobBar
DaystoStrobBar+ggsave("Graphs/FinalGraphs/DaystoStrob.png", width=8, height=5)

#Boxplot
strob.boxplot<-NoApoData%>%
  ggplot(aes(x=Genotype, y=Days.to.Strobilation, fill=Temp))+
  geom_boxplot()+
  theme_bw()+
  geom_jitter(color="black", size=0.5, alpha=0.7)+
  scale_x_discrete(name = "Symbiont Genotype") +
  scale_y_continuous(name = "Days to Strobilation")+
  theme(axis.text.x=element_text(color="black", size=11), 
        axis.text.y=element_text(color="black", size=12), 
        axis.title.x = element_text(color="black", size=16), 
        axis.title.y = element_text(color="black", size=16),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  labs(fill="Temperature")+
  scale_fill_manual(values = c("#ac8eab", "#f2cec7", "#c67b6f"), labels=c("26°C", "30°C","32°C"))
strob.boxplot
strob.boxplot+ggsave("Graphs/FinalGraphs/DaystoStrob.boxplot.final.png", width=8, height=5)

#Just temp, not geno
DaystoStrobNoDeadTemp <- NoApoNoDeadData %>%
  group_by(Temp) %>%
  summarize(mean=mean(Days.to.Strobilation, na.rm=TRUE), SE=sd(Days.to.Strobilation, na.rm=TRUE)/sqrt(length(na.omit(Days.to.Strobilation))))
DaystoStrobNoDeadTemp

DaysStrobTemp<-ggplot(DaystoStrobNoDeadTemp, aes(x=Temp, y=mean))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(axis.text.x=element_text(color="black", size=14), axis.text.y=element_text(face="bold", color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=16), axis.title.y = element_text(face="bold", color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6, fill="mediumorchid4") + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Temperature", y="Days to strobilation")+  #labels the x and y axes
  ggtitle("Average Days to Strobilation (no dead)")
DaysStrobTemp+ggsave("Graphs/Polyps/DaystoStrobTemp.png", width=10, height=5)

#Just geno, not temp
DaystoStrobNoDeadGeno <- NoApoNoDeadData %>%
  group_by(Genotype) %>%
  summarize(mean=mean(Days.to.Strobilation, na.rm=TRUE), SE=sd(Days.to.Strobilation, na.rm=TRUE)/sqrt(length(na.omit(Days.to.Strobilation))))
DaystoStrobNoDeadGeno

DaysStrobGeno<-ggplot(DaystoStrobNoDeadGeno, aes(x=Genotype, y=mean))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(axis.text.x=element_text(color="black", size=14), axis.text.y=element_text(face="bold", color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=16), axis.title.y = element_text(face="bold", color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6, fill="darkseagreen3") + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Genotype", y="Days to Strobilation")+  #labels the x and y axes
  ggtitle("Average Days to Strobilation (no dead)")
DaysStrobGeno+ggsave("Graphs/Polyps/DaystoStrobGeno.png", width=10, height=5)


#Time to inoculation####
#First, let's see how many even inoculated
total.inoc<-Developed.data%>%
  group_by(Genotype, Temp, Inoc)%>%
  tally()

ggplot(total.inoc, aes(fill=Inoc, y=n, x=Temp))+
  geom_bar(position="stack", stat="identity")+
  facet_wrap(~Genotype)
#Inoculation proportion plot
pal<-c("#2c7fb8","#7fcdbb") #blue green
inoc.prop<-ggplot(total.inoc, aes(x=Temp, y=n, fill=Inoc))+  #basic plot
  theme_minimal()+
  theme(axis.text.x=element_text(color="black", size=11), axis.text.y=element_text(color="black", size=11), axis.title.x = element_text(color="black", size=13),strip.text.x = element_text(size = 11, colour = "black"))+
  geom_bar(position=position_stack(), stat="identity", color="black")+
  scale_fill_manual(values=pal)+
  labs(x="Temperature (°C)", y="", fill="Inoculated")+#labels the x and y axes
  scale_y_continuous(expand=c(0,0), limits=c(0,25))+
  facet_grid(~Genotype)
inoc.prop
inoc.prop+ggsave("Graphs/FinalGraphs/Inoc.prop.png", width=8, height=5)

#Bargraph
DaystoInoc <- NoApoData %>%
  group_by(Genotype, Temp) %>%
  summarize(mean=mean(Days.to.Inoculation, na.rm=TRUE), SE=sd(Days.to.Inoculation, na.rm=TRUE)/sqrt(length(na.omit(Days.to.Inoculation))))

pal<-c("#ac8eab", "#f2cec7", "#c67b6f") #purples
DaystoInocBar<-ggplot(DaystoInoc, aes(x=Genotype, y=mean, fill=factor(Temp), group=factor(Temp)))+  #basic plot
  theme_bw()+ #Removes grey background
  scale_y_continuous(expand=c(0,0), limits=c(0, 25))+
  theme(axis.text.x=element_text(color="black", size=11), 
        axis.text.y=element_text(color="black", size=12), 
        axis.title.x = element_text(color="black", size=16), 
        axis.title.y = element_text(color="black", size=16),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(color="black", stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Symbiont Genotype", y="Days to Inoculation", fill="Temperature")+  #labels the x and y axes
  scale_fill_manual(values=pal, labels = c("26°C", "30°C", "32°C"))
DaystoInocBar
DaystoInocBar+ggsave("Graphs/FinalGraphs/DaystoInoc.final.png", width=8, height=5)

#Just temp, not geno
DaystoInocNoDeadTemp <- NoApoNoDeadData %>%
  group_by(Temp) %>%
  summarize(mean=mean(Days.to.Inoculation, na.rm=TRUE), SE=sd(Days.to.Inoculation, na.rm=TRUE)/sqrt(length(na.omit(Days.to.Inoculation))))
DaystoInocNoDeadTemp

DaysInocTemp<-ggplot(DaystoInocNoDeadTemp, aes(x=Temp, y=mean))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(axis.text.x=element_text(color="black", size=14), axis.text.y=element_text(face="bold", color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=16), axis.title.y = element_text(face="bold", color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6, fill="mediumorchid4") + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Temperature", y="Days to inoculation")+  #labels the x and y axes
  ggtitle("Average Days to Inoculation (no dead)")
DaysInocTemp+ggsave("Graphs/Polyps/DaystoInocTemp.png", width=10, height=5)

#Just geno, not temp
DaystoInocNoDeadGeno <- NoApoNoDeadData %>%
  group_by(Genotype) %>%
  summarize(mean=mean(Days.to.Inoculation, na.rm=TRUE), SE=sd(Days.to.Inoculation, na.rm=TRUE)/sqrt(length(na.omit(Days.to.Inoculation))))
DaystoInocNoDeadGeno

DaysInocGeno<-ggplot(DaystoInocNoDeadGeno, aes(x=Genotype, y=mean))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(axis.text.x=element_text(color="black", size=14), axis.text.y=element_text(face="bold", color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=16), axis.title.y = element_text(face="bold", color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6, fill="darkseagreen3") + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Temperature", y="Days to inoculation")+  #labels the x and y axes
  ggtitle("Average Days to Inoculation (no dead)")
DaysInocGeno+ggsave("Graphs/Polyps/DaystoInocGeno.png", width=10, height=5)


#Time to inoculation boxplot####
inoc.boxplot<-NoApoData%>%
  ggplot(aes(x=Genotype, y=Days.to.Inoculation, fill=Temp))+
  geom_boxplot()+
  theme_bw()+
  geom_jitter(color="black", size=0.5, alpha=0.7)+
  scale_x_discrete(name = "Symbiont Genotype") +
  scale_y_continuous(name = "Days to Inoculation")+
  theme(axis.text.x=element_text(color="black", size=11), 
        axis.text.y=element_text(color="black", size=12), 
        axis.title.x = element_text(color="black", size=16), 
        axis.title.y = element_text(color="black", size=16),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  labs(fill="Temperature")+
  scale_fill_manual(values = c("#ac8eab", "#f2cec7", "#c67b6f"), labels=c("26°C", "30°C","32°C"))
inoc.boxplot
inoc.boxplot+ggsave("Graphs/FinalGraphs/inoc.boxplot.final.png", width=8, height=5)

#time to ephyra####
#First, let's see how many even got to strobilation
time.ephyra.total<-Developed.data%>%
  group_by(Genotype, Temp, Ephyra)%>%
  tally()
ggplot(time.ephyra.total, aes(fill=Ephyra, y=n, x=Temp))+
  geom_bar(position="stack", stat="identity")+
  facet_wrap(~Genotype)
#Ephyra proportion plot
pal<-c("#2c7fb8","#7fcdbb") #blue green
ephyra.prop<-ggplot(time.ephyra.total, aes(x=Temp, y=n, fill=Ephyra))+  #basic plot
  theme_minimal()+
  theme(axis.text.x=element_text(color="black", size=11), axis.text.y=element_text(color="black", size=11), axis.title.x = element_text(color="black", size=13),strip.text.x = element_text(size = 11, colour = "black"))+
  geom_bar(position=position_stack(), stat="identity", color="black")+
  scale_fill_manual(values=pal)+
  labs(x="Temperature (°C)", y="", fill="Produced\nan Ephyra")+#labels the x and y axes
  scale_y_continuous(expand=c(0,0), limits=c(0,25))+
  facet_grid(~Genotype)
ephyra.prop
ephyra.prop+ggsave("Graphs/FinalGraphs/ProducedEphyra.prop.png", width=8, height=5)


#Bargraph of those that did produce an ephyra
DaystoEphyra <- NoApoData %>%
  group_by(Genotype, Temp) %>%
  summarize(mean=mean(Days.to.Ephyra, na.rm=TRUE), SE=sd(Days.to.Ephyra, na.rm=TRUE)/sqrt(length(na.omit(Days.to.Ephyra))))

pal<-c("#ac8eab", "#f2cec7", "#c67b6f") #purples
DaystoEphyraBar<-ggplot(DaystoEphyra, aes(x=Genotype, y=mean, fill=factor(Temp), group=factor(Temp)))+  #basic plot
  theme_bw()+ #Removes grey background
  scale_y_continuous(expand=c(0,0), limits=c(0, 27))+
  theme(axis.text.x=element_text(color="black", size=11), 
        axis.text.y=element_text(color="black", size=12), 
        axis.title.x = element_text(color="black", size=16), 
        axis.title.y = element_text(color="black", size=16),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(color="black", stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Symbiont Genotype", y="Days to Produce an Ephyra", fill="Temperature")+  #labels the x and y axes
  scale_fill_manual(values=pal, labels = c("26°C", "30°C", "32°C"))
DaystoEphyraBar
DaystoEphyraBar+ggsave("Graphs/FinalGraphs/DaystoEphyraBar.final.png", width=8, height=5)

#boxplot
time.ephyra.boxplot<-NoApoData%>%
  ggplot(aes(x=Genotype, y=Days.to.Ephyra, fill=Temp))+
  geom_boxplot()+
  theme_bw()+
  geom_jitter(color="black", size=0.5, alpha=0.7)+
  scale_x_discrete(name = "Symbiont Genotype") +
  scale_y_continuous(name = "Days to Produce an Ephyra")+
  theme(axis.text.x=element_text(color="black", size=11), 
        axis.text.y=element_text(color="black", size=12), 
        axis.title.x = element_text(color="black", size=16), 
        axis.title.y = element_text(color="black", size=16),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  labs(fill="Temperature")+
  scale_fill_manual(values = c("#ac8eab", "#f2cec7", "#c67b6f"), labels=c("26°C", "30°C","32°C"))
time.ephyra.boxplot
time.ephyra.boxplot+ggsave("Graphs/FinalGraphs/DaystoEphyraBox.final.png", width=8, height=5)

#Just temp, not geno
DaystoEphyraNoDeadTemp <- NoApoNoDeadData %>%
  group_by(Temp) %>%
  summarize(mean=mean(Days.to.Ephyra, na.rm=TRUE), SE=sd(Days.to.Ephyra, na.rm=TRUE)/sqrt(length(na.omit(Days.to.Ephyra))))
DaystoEphyraNoDeadTemp

DaysEphyraTemp<-ggplot(DaystoEphyraNoDeadTemp, aes(x=Temp, y=mean))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(axis.text.x=element_text(color="black", size=14), axis.text.y=element_text(face="bold", color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=16), axis.title.y = element_text(face="bold", color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6, fill="mediumorchid4") + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Temperature", y="Days to ephyra")+  #labels the x and y axes
  ggtitle("Average Days to Ephyra (no dead)")
DaysEphyraTemp+ggsave("Graphs/Polyps/DaystoEphyraTemp.png", width=10, height=5)

#Just geno, not temp
DaystoEphyraNoDeadGeno <- NoApoNoDeadData %>%
  group_by(Genotype) %>%
  summarize(mean=mean(Days.to.Ephyra, na.rm=TRUE), SE=sd(Days.to.Ephyra, na.rm=TRUE)/sqrt(length(na.omit(Days.to.Ephyra))))
DaystoEphyraNoDeadGeno

DaysEphyraGeno<-ggplot(DaystoEphyraNoDeadGeno, aes(x=Genotype, y=mean))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(axis.text.x=element_text(color="black", size=14), axis.text.y=element_text(face="bold", color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=16), axis.title.y = element_text(face="bold", color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6, fill="darkseagreen3") + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Temperature", y="Days to ephyra")+  #labels the x and y axes
  ggtitle("Average Days to Ephyra (no dead)")
DaysEphyraGeno+ggsave("Graphs/Polyps/DaystoEphyraGeno.png", width=10, height=5)

#Survival####
#I have one weird NA from a spilled one. Just gonna make it a no. 
mydata$Survive.to.End<-mydata$Survive.to.End %>% replace_na("No")

survival<-mydata%>%
  group_by(Genotype, Temp, Survive.to.End)%>%
  tally()

ggplot(survival, aes(fill=Survive.to.End, y=n, x=Temp))+
  geom_bar(position="stack", stat="identity")+
  facet_wrap(~Genotype)

#Survival proportion plot
pal<-c("#2c7fb8","#7fcdbb") #blue green
survival.prop<-ggplot(survival, aes(x=Temp, y=n, fill=Survive.to.End))+  #basic plot
  theme_minimal()+
  theme(axis.text.x=element_text(color="black", size=11), axis.text.y=element_text(color="black", size=11), axis.title.x = element_text(color="black", size=13),strip.text.x = element_text(size = 11, colour = "black"))+
  geom_bar(position=position_stack(), stat="identity", color="black")+
  scale_fill_manual(values=pal)+
  labs(x="Temperature (°C)", y="", fill="Survived")+#labels the x and y axes
  scale_y_continuous(expand=c(0,0), limits=c(0,25))+
  facet_grid(~Genotype)
survival.prop+ggsave("Graphs/FinalGraphs/Survival.final.png", width=8, height=5)


#Survival by temp
survival.temp<-mydata%>%
  group_by(Temp, Survive.to.End)%>%
  tally()
pal<-c("#2c7fb8","#7fcdbb") #blue green
survival.temp.plot<-ggplot(survival.temp, aes(x=Temp, y=n, fill=Survive.to.End))+  #basic plot
  theme_minimal()+
  theme(axis.text.x=element_text(color="black", size=11), axis.text.y=element_text(color="black", size=11), axis.title.x = element_text(color="black", size=13),strip.text.x = element_text(size = 11, colour = "black"))+
  geom_bar(color="black", position=position_stack(), stat="identity", size=0.5)+
  scale_fill_manual(values=pal)+
  labs(x="Temperature (°C)", y="", fill="Survived")+#labels the x and y axes
  scale_y_continuous(expand=c(0,0), limits=c(0,145))
survival.temp.plot+ggsave("Graphs/FinalGraphs/Survival.temp.final.png", width=8, height=5)

#Bud production####
BudData<-mydata%>%
  group_by(Genotype, Temp)%>%
  summarize(mean=mean(Total.Buds, na.rm=TRUE), SE=sd(Total.Buds, na.rm=TRUE)/sqrt(length(na.omit(Total.Buds))))

pal<-c("#ac8eab", "#f2cec7", "#c67b6f") #purples
Buds.final<-ggplot(BudData, aes(x=Genotype, y=mean, fill=factor(Temp), group=factor(Temp)))+  #basic plot
  theme_bw()+ #Removes grey background
  labs(x="Symbiont Genotype", y="Number of Buds", fill="Temperature")+#labels the x and y axes
  theme(axis.text.x=element_text(color="black", size=11), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(color="black", size=16), axis.title.y = element_text(color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(color="black", stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  scale_fill_manual(values=pal, labels = c("26°C", "30°C", "32°C"))+
  scale_y_continuous(expand=c(0,0), limits=c(0,5.1))
Buds.final
Buds.final+ggsave("Graphs/FinalGraphs/Polyp_buds_bar.png", width=8, height=5)


#Boxplot
bud.boxplot<-mydata%>%
  ggplot(aes(x=Genotype, y=Total.Buds, fill=Temp))+
  geom_boxplot()+
  geom_jitter(color="black", size=0.5, alpha=0.7)+
  scale_x_discrete(name = "Symbiont Genotype") +
  scale_y_continuous(name = "Number of Buds")+
  theme(axis.text.x=element_text(color="black", size=11), 
        axis.text.y=element_text(color="black", size=12), 
        axis.title.x = element_text(color="black", size=16), 
        axis.title.y = element_text(color="black", size=16),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  labs(fill="Temperature")+
  scale_fill_manual(values = c("#ac8eab", "#f2cec7", "#c67b6f"), labels=c("26°C", "30°C","32°C"))
bud.boxplot+ggsave("Graphs/FinalGraphs/Bud.boxplot.final.png", width=8, height=5)

bud.boxplot.nojitter<-mydata%>%
  ggplot(aes(x=Genotype, y=Total.Buds, fill=Temp))+
  geom_boxplot()+
  theme_bw()+
  scale_x_discrete(name = "Symbiont Genotype") +
  scale_y_continuous(name = "Number of Buds")+
  theme(axis.text.x=element_text(color="black", size=11), 
        axis.text.y=element_text(color="black", size=12), 
        axis.title.x = element_text(color="black", size=16), 
        axis.title.y = element_text(color="black", size=16),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  labs(fill="Temperature")+
  scale_fill_manual(values = c("#ac8eab", "#f2cec7", "#c67b6f"), labels=c("26°C", "30°C","32°C"))
bud.boxplot.nojitter+ggsave("Graphs/FinalGraphs/Bud.boxplot.nojitter.png", width=8, height=5)

#violin
bud.violin<-mydata%>%
  ggplot(aes(x=Genotype, y=Total.Buds, fill=Temp))+
  geom_violin()+
  theme_bw()+
  scale_x_discrete(name = "Genotype") +
  scale_y_continuous(name = "Number of Buds")+
  ggtitle("Total Bud Production per Polyp")+
  theme(plot.title = element_text(face="bold"), 
        axis.text.x=element_text(size=10), 
        axis.text.y=element_text(size=10), 
        axis.title.y = element_text(face="bold", size=12), 
        axis.title.x = element_text(face="bold", size=12))+
  labs(fill="Temperature")+
  scale_fill_manual(values = c("#79CFDB", "#859A51", "#DFADE1"), labels=c("26°C", "30°C","32°C"))
bud.violin
#Nothing too crazy, but good to double check. 


#Just temp, not geno
BudDataTemp<-NoDeadData%>%
  group_by(Temp)%>%
  summarize(mean=mean(Total.Buds, na.rm=TRUE), SE=sd(Total.Buds, na.rm=TRUE)/sqrt(length(na.omit(Total.Buds))))
BudDataTemp

BudTemp<-ggplot(BudDataTemp, aes(x=Temp, y=mean))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(axis.text.x=element_text(color="black", size=14), axis.text.y=element_text(face="bold", color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=16), axis.title.y = element_text(face="bold", color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6, fill="mediumorchid4") + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Temperature", y="Number of buds")+  #labels the x and y axes
  ggtitle("Average Bud Production (no dead)")
BudTemp+ggsave("Graphs/Polyps/BudsTemp.png", width=10, height=5)

#Just geno, not temp
BudDataGeno<-NoDeadData%>%
  group_by(Genotype)%>%
  summarize(mean=mean(Total.Buds, na.rm=TRUE), SE=sd(Total.Buds, na.rm=TRUE)/sqrt(length(na.omit(Total.Buds))))
BudDataGeno

BudsGeno<-ggplot(BudDataGeno, aes(x=Genotype, y=mean))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(axis.text.x=element_text(color="black", size=14), axis.text.y=element_text(face="bold", color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=16), axis.title.y = element_text(face="bold", color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6, fill="darkseagreen3") + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Temperature", y="Number of Buds")+  #labels the x and y axes
  ggtitle("Average Bud Production (no dead)")
BudsGeno+ggsave("Graphs/Polyps/BudsGeno.png", width=10, height=5)
