#Choosing polyps to sequence 
#Created by Jennica Moffat August 27, 2020

#Clear the environment
rm(list=ls())
#load libraries
library(tidyverse)

#Load data
mydata<-read.csv("Data/Thesis_PolypData_Summary.csv")
mydata$Temp<-as.factor(mydata$Temp)
mydata$Plate<-as.factor(mydata$Plate)
NoApoData <- mydata %>%
  filter(Genotype != "Aposymbiotic") %>%
  droplevels

summary(NoApoData$Total.Ephyra.Produced)
NoApoData$Total.Ephyra.Produced<-as.factor(NoApoData$Total.Ephyra.Produced)
View(NoApoData)
#Making df of just those that produced 1 or 2 ephyra
FilteredData<-NoApoData %>%
  filter(Total.Ephyra.Produced == 1|Total.Ephyra.Produced == 2)
FilteredData$Genotype<-as.factor(FilteredData$Genotype)
summary(FilteredData$Genotype)

#How many wells produced at least one ephyra in each plate?
summary.data<-FilteredData%>%
  group_by(Genotype, Temp, Plate)%>%
  count()
#Finding number of plates that had less than 3 wells produce an ephyra
summary.data.fewer<-summary.data%>%
  filter(n<3)
sum(summary.data.fewer$n)
#22 total ephyra from plates where 3 or less were produced (need to do all of those)

#Finding number of plates that had 3 or more wells produce an ephyra
summary.data.more<-summary.data%>%
  filter(n>2)
#43 geno/temp/plate combos with 3+ ephyra produced, only need to do three from each row
43*3 = 129
#129+22=151 ephyra