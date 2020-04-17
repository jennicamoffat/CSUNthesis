#Thesis
#Polyp Photosynthesis and Respiration
#Created on 4/17/2020 by Jennica Moffat

#Clear the environment
rm(list=ls())
#Load data
PnR.data<-read.csv("Data/PnR/Jan_Polyp_PnR.csv")
polyp.data<-read.csv("Data/Thesis_PolypData_Summary.csv")
pic.data<-read.csv("Data/PolypAreas.csv")
View(pic.data)

#I need to combine data sets so that the area, PnR values, and temp/plate number are all together




#Need to make temp and plate factors
mydata$Temp<-as.factor(mydata$Temp)
mydata$Plate<-as.factor(mydata$Plate)

