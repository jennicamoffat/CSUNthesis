#Cassiopea Polyp Data
#Created by Jennica Moffat 02162020

#Clear the environment
rm(list=ls())

#load libraries
library(tidyverse)
library(car)

#Load data
mydata<-read.csv("Data/Thesis_PolypData_Summary.csv")
mydata$Temp<-as.factor(mydata$Temp)
NoApoData <- mydata %>%
  filter(Genotype != "Aposymbiotic") %>%
  droplevels
NoApoNoDeadData <- subset(NoApoData, Survive.to.End == "Yes")

#GRAPHS#
#Bargraph of total average ephyra production including dead ones####
#New data frame without Apo (since no ephyra)
NoApoData <- mydata %>%
  filter(Genotype != "Aposymbiotic") %>%
  droplevels

TotalEphyra <- NoApoData %>%
  group_by(Genotype, Temp) %>%
  summarize(mean=mean(Total.Ephyra.Produced, na.rm=TRUE), SE=sd(Total.Ephyra.Produced, na.rm=TRUE)/sqrt(length(na.omit(Total.Ephyra.Produced))))
TotalEphyra

TotalEphyraPlot<-ggplot(TotalEphyra, aes(x=Genotype, y=mean, fill=factor(Temp), group=factor(Temp)))+  #basic plot
  theme_bw()+ #Removes grey background
  scale_y_continuous(expand=c(0,0), limits=c(0, 1.6))+
  theme(plot.title = element_text(face = "bold", size=16), axis.text.x=element_text(color="black", size=12), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(color="black", size=16), axis.title.y = element_text(color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Genotype", y="Number of ephyra", fill="Temperature")+  #labels the x and y axes
  ggtitle("Average Ephyra produced per Polyp")+
  scale_fill_manual(values = c("skyblue3", "darkgoldenrod2", "brown3"), labels=c("26°C", "30°C", "32°C"))
TotalEphyraPlot

TotalEphyraPlot+ggsave("Graphs/Polyps/EphyraProduced_deadincluded.png", width=8, height=5)

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
  scale_x_discrete(name = "Genotype") +
  scale_y_continuous(name = "Total Ephyra Produced")+
  ggtitle("Total ephyra production per polyp")+
  theme(plot.title = element_text(face="bold"), 
        axis.text.x=element_text(size=10), 
        axis.text.y=element_text(size=10), 
        axis.title.y = element_text(face="bold", size=12), 
        axis.title.x = element_text(face="bold", size=12))+
  labs(fill="Temperature")+
  scale_fill_manual(values = c("#79CFDB", "#859A51", "#DFADE1"), labels=c("26°C", "30°C","32°C"))
boxplot
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


#New dataframe without dead
NoApoNoDeadData <- subset(NoApoData, Survive.to.End == "Yes")
View(NoApoNoDeadData)

#Average Total Ephyra Produced#
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
InoculatedData <- subset(NoApoNoDeadData, Days.to.Inoculation != "NA")

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

EphyraifInocPlot+ggsave("Graphs/Polyps/EphyraifInoc.pdf", width=11, height=6.19, dpi=300, unit="in")


#Time to strobilation####
#Removing the NA's, which are the ones that never strobilated. Losing part of the story. 
#How to manage?
DaystoStrobNoDead <- NoApoNoDeadData %>%
  group_by(Genotype, Temp) %>%
  summarize(mean=mean(Days.to.Strobilation, na.rm=TRUE), SE=sd(Days.to.Strobilation, na.rm=TRUE)/sqrt(length(na.omit(Days.to.Strobilation))))
DaystoStrobNoDead

DaystoStrobNoDeadPlot<-ggplot(DaystoStrobNoDead, aes(x=Genotype, y=mean, fill=factor(Temp), group=factor(Temp)))+  #basic plot
  theme_bw()+ #Removes grey background
  scale_y_continuous(expand=c(0,0), limits=c(0, 25))+
  theme(plot.title = element_text(face = "bold", size=16), axis.text.x=element_text(color="black", size=12), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(color="black", size=16), axis.title.y = element_text(color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Genotype", y="Number of days", fill="Temperature")+  #labels the x and y axes
  ggtitle("Average Days to Strobilation")+
  scale_fill_manual(values = c("skyblue3", "darkgoldenrod2", "brown3"), labels=c("26°C", "30°C", "32°C"))
DaystoStrobNoDeadPlot

DaystoStrobNoDeadPlot+ggsave("Graphs/Polyps/DaystoStrob_deadremoved.png", width=8, height=5)

#Boxplot of data to see distribution
ggplot(NoApoNoDeadData, aes(x=Genotype, y=Days.to.Strobilation, fill=factor(Temp)))+  #basic plot
  theme_bw()+
  geom_boxplot()+
  labs(x="Genotype", y="Avg days to strobilation", fill="Temperature")+  #labels the x and y axes
  ggtitle("Average Days to Strobilation (dead removed)")+
  scale_fill_manual(values = c("skyblue3", "darkgoldenrod2", "brown3"), labels=c("26°C", "30°C", "32°C"))
#Automatically removes all NA's values, so graph is just of those that did strobilate


strob.boxplot<-NoApoData%>%
  ggplot(aes(x=Genotype, y=Days.to.Strobilation, fill=Temp))+
  geom_boxplot()+
  theme_bw()+
  geom_jitter(color="black", size=0.5, alpha=0.7)+
  scale_x_discrete(name = "Genotype") +
  scale_y_continuous(name = "Days to Strobilation")+
  ggtitle("Days to Strobilation")+
  theme(plot.title = element_text(face="bold"), 
        axis.text.x=element_text(size=10), 
        axis.text.y=element_text(size=10), 
        axis.title.y = element_text(face="bold", size=12), 
        axis.title.x = element_text(face="bold", size=12))+
  labs(fill="Temperature")+
  scale_fill_manual(values = c("#79CFDB", "#859A51", "#DFADE1"), labels=c("26°C", "30°C","32°C"))

strob.boxplot+ggsave("Graphs/Polyps/DaystoStrob.boxplot.png", width=10, height=5)

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


#Time to inoculation (same issue as strobilation)####
DaystoInocNoDead <- NoApoNoDeadData %>%
  group_by(Genotype, Temp) %>%
  summarize(mean=mean(Days.to.Inoculation, na.rm=TRUE), SE=sd(Days.to.Inoculation, na.rm=TRUE)/sqrt(length(na.omit(Days.to.Inoculation))))
DaystoInocNoDead

DaystoInocNoDeadPlot<-ggplot(DaystoInocNoDead, aes(x=Genotype, y=mean, fill=factor(Temp), group=factor(Temp)))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(axis.text.x=element_text(color="black", size=14), axis.text.y=element_text(face="bold", color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=16), axis.title.y = element_text(face="bold", color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Genotype", y="Avg days to inoculation", fill="Temperature")+  #labels the x and y axes
  ggtitle("Average Days to Inoculation (dead removed)")+
  scale_fill_manual(values = c("skyblue3", "darkgoldenrod2", "brown3"), labels=c("26°C", "30°C", "32°C"))
DaystoInocNoDeadPlot

DaystoInocNoDeadPlot+ggsave("Graphs/Polyps/DaystoInoc_deadremoved.pdf", width=11, height=6.19, dpi=300, unit="in")

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
  scale_x_discrete(name = "Genotype") +
  scale_y_continuous(name = "Days to Inoculation")+
  ggtitle("Days to Inoculation")+
  theme(plot.title = element_text(face="bold"), 
        axis.text.x=element_text(size=10), 
        axis.text.y=element_text(size=10), 
        axis.title.y = element_text(face="bold", size=12), 
        axis.title.x = element_text(face="bold", size=12))+
  labs(fill="Temperature")+
  scale_fill_manual(values = c("#79CFDB", "#859A51", "#DFADE1"), labels=c("26°C", "30°C","32°C"))
inoc.boxplot+ggsave("Graphs/Polyps/Inoculation.boxplot.png", width=10, height=5)

#time to ephyra####
#same issue as above


DaystoEphyraNoDead <- NoApoNoDeadData %>%
  group_by(Genotype, Temp) %>%
  summarize(mean=mean(Days.to.Ephyra, na.rm=TRUE), SE=sd(Days.to.Ephyra, na.rm=TRUE)/sqrt(length(na.omit(Days.to.Ephyra))))
DaystoEphyraNoDead

DaystoEphyraNoDeadPlot<-ggplot(DaystoEphyraNoDead, aes(x=Genotype, y=mean, fill=factor(Temp), group=factor(Temp)))+  #basic plot
  theme_bw()+ #Removes grey background+
  scale_y_continuous(expand=c(0,0), limits=c(0, 28))+
  theme(plot.title = element_text(face = "bold", size=16), axis.text.x=element_text(color="black", size=12), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(color="black", size=16), axis.title.y = element_text(color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Genotype", y="Number of days", fill="Temperature")+  #labels the x and y axes
  ggtitle("Average Days to Produce an Ephyra")+
  scale_fill_manual(values = c("skyblue3", "darkgoldenrod2", "brown3"), labels=c("26°C", "30°C", "32°C"))
DaystoEphyraNoDeadPlot

DaystoEphyraNoDeadPlot+ggsave("Graphs/Polyps/DaystoEphyra_deadremoved.pdf", width=11, height=6.19, dpi=300, unit="in")

#boxplot
TE.boxplot<-NoApoData%>%
  ggplot(aes(x=Genotype, y=Days.to.Ephyra, fill=Temp))+
  geom_boxplot()+
  theme_bw()+
  geom_jitter(color="black", size=0.5, alpha=0.7)+
  scale_x_discrete(name = "Genotype") +
  scale_y_continuous(name = "Days to Ephyra")+
  ggtitle("Days to Produce First Ephyra")+
  theme(plot.title = element_text(face="bold"), 
        axis.text.x=element_text(size=10), 
        axis.text.y=element_text(size=10), 
        axis.title.y = element_text(face="bold", size=12), 
        axis.title.x = element_text(face="bold", size=12))+
  labs(fill="Temperature")+
  scale_fill_manual(values = c("#79CFDB", "#859A51", "#DFADE1"), labels=c("26°C", "30°C","32°C"))
TE.boxplot+ggsave("Graphs/Polyps/DaystoEphyra.boxplot.png", width=10, height=5)


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
#I have one weird NA from a spilled one. Just gonna remove it. 
mydata2<-subset(mydata, Survive.to.End != "NA")

Survival<- mydata2%>%group_by(Genotype, Temp)%>%
  tally(Survive.to.End == "Yes")
Survival

Survival$SurvivalRate<-Survival$n/24
Survival

#Not quite because I need SE somehow
Survival2<- mydata2%>%group_by(Genotype, Temp, Plate, WellNum)%>%
  tally(Survive.to.End == "Yes")
Survival2

SurvivalSummary<-Survival2%>%
  group_by(Genotype, Temp) %>%
  summarize(mean=mean(n, na.rm=TRUE), SE=sd(n, na.rm=TRUE)/sqrt(length(na.omit(n))))
SurvivalSummary
#I think this is what I need. Double check!

SurvivalPlot<-ggplot(SurvivalSummary, aes(x=Genotype, y=mean, fill=factor(Temp), group=factor(Temp)))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(axis.text.x=element_text(color="black", size=14), axis.text.y=element_text(face="bold", color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=16), axis.title.y = element_text(face="bold", color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Genotype", y="Avg Survival", fill="Temperature")+  #labels the x and y axes
  ggtitle("Average Survival")+
  scale_fill_manual(values = c("skyblue3", "darkgoldenrod2", "brown3"), labels=c("26°C", "30°C", "32°C"))
SurvivalPlot

SurvivalPlot+ggsave("Graphs/Polyps/SurvivalPlot.pdf", width=11, height=6.19, dpi=300, unit="in")


#Bud production####
#Removing dead polyps
NoDeadData <- subset(mydata, Survive.to.End == "Yes")
NoDeadData

BudData<-NoDeadData%>%
  group_by(Genotype, Temp)%>%
  summarize(mean=mean(Total.Buds, na.rm=TRUE), SE=sd(Total.Buds, na.rm=TRUE)/sqrt(length(na.omit(Total.Buds))))
BudData

BudPlot<-ggplot(BudData, aes(x=Genotype, y=mean, fill=factor(Temp), group=factor(Temp)))+  #basic plot
  theme_bw()+ #Removes grey background
  scale_y_continuous(expand=c(0,0), limits=c(0, 5.1))+
  theme(plot.title = element_text(face = "bold", size=16), axis.text.x=element_text(color="black", size=12), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(color="black", size=16), axis.title.y = element_text(color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Genotype", y="Number of buds", fill="Temperature")+  #labels the x and y axes
  ggtitle("Average buds produced per polyp")+
  scale_fill_manual(values = c("skyblue3", "darkgoldenrod2", "brown3"), labels=c("26°C", "30°C", "32°C"))
BudPlot

BudPlot+ggsave("Graphs/Polyps/BudPlot_deadremoved.png", width=8, height=5)

#Boxplot
bud.boxplot<-mydata%>%
  ggplot(aes(x=Genotype, y=Total.Buds, fill=Temp))+
  geom_boxplot()+
  theme_bw()+
  geom_jitter(color="black", size=0.5, alpha=0.7)+
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
bud.boxplot+ggsave("Graphs/Polyps/Bud.boxplot.png", width=10, height=5)

bud.boxplot.nojitter<-mydata%>%
  ggplot(aes(x=Genotype, y=Total.Buds, fill=Temp))+
  geom_boxplot()+
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
bud.boxplot.nojitter+ggsave("Graphs/Polyps/Bud.boxplot.nojitter.png", width=10, height=5)

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

#STATS####
#Clear the environment
rm(list=ls())
#Load data
mydata<-read.csv("Data/Thesis_PolypData_Summary.csv")
mydata$Temp<-as.factor(mydata$Temp)
View(mydata)

#Total average ephyra production####
#Removing aposymbiotic
NoApoData <- subset(mydata, Genotype != "Aposymbiotic")
View(NoApoData)

#Genotype (fixed) and Temperature (fixed) on dependent variables (total ephyra production)

#Make model
Ephyramodel<-aov(Total.Ephyra.Produced~Genotype*Temp*Plate, random="Plate", data=NoApoData)
#Check assumptions
plot(Ephyramodel)
model1res<-resid(Ephyramodel)
qqp(model1res, "norm")
#I think it's normal enough. A few outliers(?) 338, 361, 290, and 313

#Run model
EphyraAnova<-anova(Ephyramodel)
EphyraAnova
#Yay! Significant interaction between Genotype and Temp (unsurprisingly)
#Genotype:Temp, P=0.003441 ***

#Time to strobilation####

#I need to remove the NA's
StrobilatedData <- subset(NoApoData, Days.to.Strobilation != "NA")
View(StrobilatedData)

#Make model
Strobmodel<-aov(Days.to.Strobilation~Genotype*Temp, data=StrobilatedData)
#Check assumptions
plot(Strobmodel)
Strobmodelres<-resid(Strobmodel)
qqp(Strobmodelres, "norm")
#Not great. A few outliers: 337, 318, 186, and 194

#transform
StrobilatedData$logDays<-log(StrobilatedData$Days.to.Strobilation)
logStrobmodel<-aov(logDays~Genotype*Temp, data=StrobilatedData)
logStrobmodelres<-resid(logStrobmodel)
qqp(logStrobmodelres, "norm")
#Better,let's log one more time

StrobilatedData$loglogDays<-log(StrobilatedData$logDays)
loglogStrobmodel<-aov(loglogDays~Genotype*Temp, data=StrobilatedData)
loglogStrobmodelres<-resid(loglogStrobmodel)
qqp(loglogStrobmodelres, "norm")
#Slightly better, but a lot of points still just above CI's 


#Square root
StrobilatedData$sqrtDays<-sqrt(StrobilatedData$Days.to.Strobilation)
sqrtStrobmodel<-aov(sqrtDays~Genotype*Temp, data=StrobilatedData)
sqrtStrobmodelres<-resid(sqrtStrobmodel)
qqp(sqrtStrobmodelres, "norm")
#Barely changed. 

#Using model of double logged data
anova(loglogStrobmodel)
#Interaction = 0.03549*



#Time to inoculation####
#I need to remove the NA's
InocData <- subset(NoApoData, Days.to.Inoculation != "NA")
View(InocData)

#Make model
Inocmodel<-aov(Days.to.Inoculation~Genotype*Temp, data=InocData)
#Check assumptions
plot(Inocmodel)
Inocmodelres<-resid(Inocmodel)
qqp(Inocmodelres, "norm")
#Ew. That's pretty bad. 

InocData$logIDays<-log(InocData$Days.to.Inoculation)
logInocmodel<-aov(logIDays~Genotype*Temp, data=InocData)
logInocmodelres<-resid(logInocmodel)
qqp(logInocmodelres, "norm")
#Ugh, possibly worse. 

#double log
InocData$loglogIDays<-log(InocData$logIDays)
loglogInocmodel<-aov(loglogIDays~Genotype*Temp, data=InocData)
loglogInocmodelres<-resid(loglogInocmodel)
qqp(loglogInocmodelres, "norm")
#Oh that is better

#triple log??
InocData$trplogIDays<-log(InocData$loglogIDays)
trplogInocmodel<-aov(trplogIDays~Genotype*Temp, data=InocData)
trplogInocmodelres<-resid(trplogInocmodel)
qqp(trplogInocmodelres, "norm")
#Idk man. No.

hist(InocData$trplogIDays)

#Square root
InocData$sqrtIDays<-sqrt(InocData$Days.to.Inoculation)
sqrtInocmodel<-aov(sqrtIDays~Genotype*Temp, data=InocData)
sqrtInocmodelres<-resid(sqrtInocmodel)
qqp(sqrtInocmodelres, "norm")
#No. 

#Fourth root
InocData$fourthrtIDays<-sqrt(InocData$sqrtIDays)
fourthrtInocmodel<-aov(fourthrtIDays~Genotype*Temp, data=InocData)
fourthrtInocmodelres<-resid(fourthrtInocmodel)
qqp(fourthrtInocmodelres, "norm")
#No.

#I think I have to do a GLMM. Later....

#Time to ephyra####
#I need to remove the NA's
TimetoEphyraData <- subset(NoApoData, Days.to.Ephyra != "NA")
View(TimetoEphyraData)

#Make model
TEphyramodel<-aov(Days.to.Ephyra~Genotype*Temp, data=TimetoEphyraData)
#Check assumptions
plot(TEphyramodel)
TEphyramodelres<-resid(TEphyramodel)
qqp(TEphyramodelres, "norm")
#Not too bad, but needs transforming. 

hist(TimetoEphyraData$loglogEDays)

#Log
TimetoEphyraData$logEDays<-log(TimetoEphyraData$Days.to.Ephyra)
logEphyraDaysmodel<-aov(logEDays~Genotype*Temp, data=TimetoEphyraData)
logEphyraDaysmodelres<-resid(logEphyraDaysmodel)
qqp(logEphyraDaysmodelres, "norm")
#Almost. 

#Double log
TimetoEphyraData$loglogEDays<-log(TimetoEphyraData$logEDays)
loglogEphyraDaysmodel<-aov(loglogEDays~Genotype*Temp, data=TimetoEphyraData)
loglogEphyraDaysmodelres<-resid(loglogEphyraDaysmodel)
qqp(loglogEphyraDaysmodelres, "norm")
#Mehhhhhh close enough?
anova(loglogEphyraDaysmodel)
#Interaction is not significant. This one makes sense. 
#Genot and Temp on their own P=<2e-16
#Interaction P=0.3658

#Survival####
#To get a rate I need to tally the number that survived, divided by 24 because that's how 
#many each group started with
Survival<- mydata%>%group_by(Genotype, Temp, Plate, WellNum)%>%
  tally(Survive.to.End == "Yes")
View(Survival)
#1 is yes survived, 0 is no did not survive
#So average should be the survival rate?
Survivalmodel<-aov(n~Genotype*Temp, data=Survival)
plot(Survivalmodel)

#It's bi-model. So I think I need to do something other than anova.
#I need to do a chi-squared test I think

#To see survival overall, not separated by geno or temp
table(Survival$n)
prop.table(table(Survival$n))

chisq.test(Survival$n, Survival$Genotype)
#can't do interactions with chi-squared

#Cochran-Mantel-Haenszel test
mantelhaen.test(Survival$n, Survival$Temp, Survival$Genotype)
#Order matters


#Okay forget all that. I just need to do a log-linear analysis (G-test or chi-square)

#Bud production####
Budmodel<-aov(Total.Buds~Genotype*Temp, data=mydata)
plot(Budmodel)
Budres<-resid(Budmodel)
qqp(Budres, "norm")
#Beautifully normal

anova(Budmodel)
#interaction P=0.002318**
#Second time I run it, interaction = 0.1904

NoDeadData<-subset(mydata, Survive.to.End == "Yes")
View(NoDeadData)

BudmodelNodead<-aov(Total.Buds~Genotype*Temp, data=NoDeadData)
BudNoDeadres<-resid(BudmodelNodead)
qqp(BudNoDeadres, "norm")
#Still normal
anova(BudmodelNodead)
#Interaction P=0.001066
TukeyHSD(aov(Budmodel))
