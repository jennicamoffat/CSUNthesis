#Symbiodinium microadriaticum growth curves
#Created by Jennica Moffat August, 2019

#Clear the environment
rm(list=ls())
#Load June growth data
mydata<-read.csv("JunePart1.csv")
View(mydata)

#load libraries
library(tidyverse)
library(RColorBrewer)

#Renaming Date column because it transferred weird
names(mydata)[1]<-"Date"
View(mydata)

#Very bad plot of all growth measurements over time
ggplot(data=mydata,aes(x = Date, y= Densityx10000))+geom_line()+geom_point()
#Looks like it starts to flatten at about 6/12/2019
#Need to clean up data to have "days" instead of "dates"