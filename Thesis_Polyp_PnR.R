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

