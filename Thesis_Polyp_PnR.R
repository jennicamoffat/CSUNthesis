#Thesis
#Polyp Photosynthesis and Respiration
#Created on 4/17/2020 by Jennica Moffat

#Clear the environment
rm(list=ls())
#load libraries
library(tidyverse)
library(car)

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

#Now, I need to add column for average count, then all pnr values divided by polyp area/count

