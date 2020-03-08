#Symbiodinium microadriaticum growth curves
#Created by Jennica Moffat 071219

#Clear the environment
rm(list=ls())
#Load the data
mydata<-read.csv("Part1.csv")
View(mydata)
#Renaming Date column because it transferred weird
names(mydata)[1]<-"Date"
View(mydata)

library(ggplot2)
ggplot(data=mydata,aes(x = Date, y= Densityx10000))+geom_line()+geom_point()

library(tidyr)
library(dplyr)
df<-mydata %>%
  select(Date,Genotype,Temp)%>%
  gather(key="variable", value="value",-Date)
View(df)
