#Symbiodinium microadriaticum growth curves
#Created by Jennica Moffat August, 2019

#load libraries
library(tidyverse)
library(RColorBrewer)
library(wesanderson)
library(ggpmisc)
library(car)
library(agricolae)
library(hrbrthemes)
library(viridis)
library(PNWColors)
library(reshape2)
library(growthcurver)
library(purrr)
library(lme4)
library(lmerTest)
library("lmtest")

#Clear the environment
rm(list=ls())
#Load growth data
mydata<-read.csv("JunePart1.csv")
mydata<-read.csv("GrowthDataCombined_r.csv")

#Renaming Date column because it transfered weird
names(mydata)[1]<-"Date"
mydata$Temp<-as.factor(mydata$Temp)


#July Graphs###########################################
#Graphs of July Data
#Very bad plot of all growth measurements over time
ggplot(data=mydata,aes(x = Date, y= Densityx10000))+geom_line()+geom_point()
#Looks like it starts to flatten at about 6/12/2019
#Need to clean up data to have "days" instead of "dates"

#Clear the environment
rm(list=ls())
#Load July growth data
mydata<-read.csv("JulyGrowthPart1_r.csv")
View(mydata)

#Very bad plot of all growth measurements over time
ggplot(data=mydata,aes(x = Day, y=Density.10.4))+geom_point()


#Making temp a factor rather than numeric
mydata$Temp<-as.factor(mydata$Temp)

#Growth curves
CCMP2458Growth<-ggplot(subset(mydata, Genotype == "CCMP2458"), aes(x=Day, y=Densityx10000, group=Temp, color=Temp))+
  geom_point()+
  theme_classic()+
  ggtitle("Growth Curve CCMP2458")+
  geom_smooth()
CCMP2458Growth


CCMP2464Growth<-ggplot(subset(mydata, Genotype == "CCMP2464"), aes(x=Day, y=Densityx10000, group=Temp, color=Temp))+
  geom_point()+
  theme_classic()+
  ggtitle("Growth Curve CCMP2464")+
  geom_smooth()
CCMP2464Growth


FLCassGrowth<-ggplot(subset(mydata, Genotype == "FLCass"), aes(x=Day, y=Densityx10000, group=Temp, color=Temp))+
  geom_point()+
  theme_classic()+
  ggtitle("Growth Curve FLCass")+
  geom_smooth()
FLCassGrowth

RT362Growth<-ggplot(subset(mydata, Genotype == "RT362"), aes(x=Day, y=Densityx10000, group=Temp, color=Temp))+
  geom_point()+
  theme_classic()+
  ggtitle("Growth Curve RT362")+
  geom_smooth()
RT362Growth


KB8Growth<-ggplot(subset(mydata, Genotype == "KB8"), aes(x=Day, y=Densityx10000, group=Temp, color=Temp))+
  geom_point()+
  theme_classic()+
  ggtitle("Growth Curve KB8")+
  geom_smooth()
KB8Growth



#Graphs of combined data#####

#Now going through with combined data from trial 1 and trial 2
#Clear the environment
rm(list=ls())
#load libraries
library(tidyverse)
library(RColorBrewer)

#Load combined growth data
mydata<-read.csv("GrowthDataCombined_r.csv")
View(mydata)

#Renaming Date column because it transferred weird
names(mydata)[1]<-"Date"
View(mydata)
#Making temp a factor rather than numeric
mydata$Temp<-as.factor(mydata$Temp)


#Graphs comparing between rounds#####
#First I want to compare between rounds, just to see how it looks

#Growth curves
CCMP2458Growth<-ggplot(subset(mydata, Genotype == "CCMP2458"), aes(x=Day, y=Densityx10000, color=Temp, shape=Round))+
  geom_point()+
  theme_classic()+
  ggtitle("Growth Curve CCMP2458")+
  geom_smooth()
CCMP2458Growth
#Worked, but hard to tell. Gonna make separate graphs for each temp

CCMP2458Growth26<-ggplot(subset(mydata, Genotype == "CCMP2458" & Temp=="26"), aes(x=Day, y=Densityx10000, color=Round))+
geom_point()+
  theme_classic()+
  ggtitle("Growth Curve CCMP2458 - 26*")+
  geom_smooth()
CCMP2458Growth26

CCMP2458Growth30<-ggplot(subset(mydata, Genotype == "CCMP2458" & Temp=="30"), aes(x=Day, y=Densityx10000, color=Round))+
  geom_point()+
  theme_classic()+
  ggtitle("Growth Curve CCMP2458 - 30*")+
  geom_smooth()
CCMP2458Growth30

CCMP2458Growth32<-ggplot(subset(mydata, Genotype == "CCMP2458" & Temp=="32"), aes(x=Day, y=Densityx10000, color=Round))+
  geom_point()+
  theme_classic()+
  ggtitle("Growth Curve CCMP2458 - 32*")+
  geom_smooth()
CCMP2458Growth32


#CCMP2464, not really worth doing but whatever
CCMP2464Growth<-ggplot(subset(mydata, Genotype == "CCMP2464"), aes(x=Day, y=Densityx10000, color=Temp, shape=Round))+
  geom_point()+
  theme_classic()+
  ggtitle("Growth Curve CCMP2464")+
  geom_smooth()
CCMP2464Growth
#So not helpful. Moving on. 


#FLCass
FLCassGrowth<-ggplot(subset(mydata, Genotype == "FLCass"), aes(x=Day, y=Densityx10000, color=Temp, shape=Round))+
  geom_point()+
  theme_classic()+
  ggtitle("Growth Curve FLCass")+
  geom_smooth()
FLCassGrowth
#Worked, but hard to tell. Although looks a little better than CCMP2458.

FLCassGrowth26<-ggplot(subset(mydata, Genotype == "FLCass" & Temp=="26"), aes(x=Day, y=Densityx10000, color=Round))+
  geom_point()+
  theme_classic()+
  ggtitle("Growth Curve FLCass - 26*")+
  geom_smooth()
FLCassGrowth26

FLCassGrowth30<-ggplot(subset(mydata, Genotype == "FLCass" & Temp=="30"), aes(x=Day, y=Densityx10000, color=Round))+
  geom_point()+
  theme_classic()+
  ggtitle("Growth Curve FLCass - 30*")+
  geom_smooth()
FLCassGrowth30

FLCassGrowth32<-ggplot(subset(mydata, Genotype == "FLCass" & Temp=="32"), aes(x=Day, y=Densityx10000, color=Round))+
  geom_point()+
  theme_classic()+
  ggtitle("Growth Curve FLCass - 32*")+
  geom_smooth()
FLCassGrowth32

#RT362
#Combined
RT362Growth<-ggplot(subset(mydata, Genotype == "RT362"), aes(x=Day, y=Densityx10000, color=Temp, shape=Round))+
  geom_point()+
  theme_classic()+
  ggtitle("Growth Curve RT362")+
  geom_smooth()
RT362Growth


RT362Growth26<-ggplot(subset(mydata, Genotype == "RT362" & Temp=="26"), aes(x=Day, y=Densityx10000, color=Round))+
  geom_point()+
  theme_classic()+
  ggtitle("Growth Curve RT362 - 26*")+
  geom_smooth()
RT362Growth26

RT362Growth30<-ggplot(subset(mydata, Genotype == "RT362" & Temp=="30"), aes(x=Day, y=Densityx10000, color=Round))+
  geom_point()+
  theme_classic()+
  ggtitle("Growth Curve RT362 - 30*")+
  geom_smooth()
RT362Growth30

RT362Growth32<-ggplot(subset(mydata, Genotype == "RT362" & Temp=="32"), aes(x=Day, y=Densityx10000, color=Round))+
  geom_point()+
  theme_classic()+
  ggtitle("Growth Curve RT362 - 32*")+
  geom_smooth()
RT362Growth32


#KB8
#Combined
KB8Growth<-ggplot(subset(mydata, Genotype == "KB8"), aes(x=Day, y=Densityx10000, color=Temp, shape=Round))+
  geom_point()+
  theme_classic()+
  ggtitle("Growth Curve KB8")+
  geom_smooth()
KB8Growth
#Uh-oh. 26 sits between the two 32's...

KB8Growth26<-ggplot(subset(mydata, Genotype == "KB8" & Temp=="26"), aes(x=Day, y=Densityx10000, color=Round))+
  geom_point()+
  theme_classic()+
  ggtitle("Growth Curve KB8 - 26*")+
  geom_smooth()
KB8Growth26

KB8Growth30<-ggplot(subset(mydata, Genotype == "KB8" & Temp=="30"), aes(x=Day, y=Densityx10000, color=Round))+
  geom_point()+
  theme_classic()+
  ggtitle("Growth Curve KB8 - 30*")+
  geom_smooth()
KB8Growth30

KB8Growth32<-ggplot(subset(mydata, Genotype == "KB8" & Temp=="32"), aes(x=Day, y=Densityx10000, color=Round))+
  geom_point()+
  theme_classic()+
  ggtitle("Growth Curve KB8 - 32*")+
  geom_smooth()
KB8Growth32


#Combined data to make plots (without accounting for round)#####
#Using combined data to make plots without accounting for round
CCMP2458<-ggplot(subset(mydata, Genotype == "CCMP2458"), aes(x=Day, y=Densityx10000, color=Temp))+
  geom_point()+
  theme_classic()+
  ggtitle("Growth Curve CCMP2458")+
  geom_smooth()
CCMP2458

CCMP2464<-ggplot(subset(mydata, Genotype == "CCMP2464" & Round == "July"), aes(x=Day, y=Densityx10000, color=Temp))+
  geom_point()+
  theme_classic()+
  ggtitle("Growth Curve CCMP2464")+
  geom_smooth()
CCMP2464

FLCass<-ggplot(subset(mydata, Genotype == "FLCass"), aes(x=Day, y=Densityx10000, color=Temp))+
  geom_point()+
  theme_classic()+
  ggtitle("Growth Curve FLCass")+
  geom_smooth()
FLCass

RT362<-ggplot(subset(mydata, Genotype == "RT362"), aes(x=Day, y=Densityx10000, color=Temp))+
  geom_point()+
  theme_classic()+
  ggtitle("Growth Curve RT362")+
  geom_smooth()
RT362

KB8<-ggplot(subset(mydata, Genotype == "KB8"), aes(x=Day, y=Densityx10000, color=Temp))+
  geom_point()+
  theme_classic()+
  ggtitle("Growth Curve KB8")+
  geom_smooth()
KB8


#Growth rate graphs for just 26 and 30#####

#New data frame without 32
mydata2 <- subset(mydata, Temp == 26 | Temp == 30)
View(mydata2)

CCMP2458<-ggplot(subset(mydata2, Genotype == "CCMP2458"), aes(x=Day, y=Densityx10000, color=Temp))+
  geom_point()+
  theme_classic()+
  theme(plot.title = element_text(face = "bold", size=16), axis.text.x=element_text(color="black", size=13), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=14), axis.title.y = element_text(face="bold", color="black", size=14),
  panel.grid.major=element_blank(), panel.grid.minor=element_blank(), legend.position = "none") +
  ggtitle("CCMP2458 Growth Curve")+
  labs(x="Day", y="Density (10,000 cells/mL)")+
  scale_color_manual(values = c("skyblue3", "darkgoldenrod2"))+
  geom_smooth(formula = y ~ x, se = TRUE, size=2.5) 
CCMP2458


CCMP2464<-ggplot(subset(mydata2, Genotype == "CCMP2464" & Round == "July"), aes(x=Day, y=Densityx10000, color=Temp))+
  geom_point()+
  theme_classic()+
  ggtitle("CCMP2464 Growth Curve")+
  theme(plot.title = element_text(face = "bold", size=16), axis.text.x=element_text(color="black", size=13), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=14), axis.title.y = element_text(face="bold", color="black", size=14),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), legend.position = "none") +
  geom_smooth(formula = y ~ x, se = TRUE, size=2.5)+
  labs(x="Day", y="Density (10,000 cells/mL)")+
  scale_color_manual(values = c("skyblue3", "darkgoldenrod2"))
CCMP2464

FLCass<-ggplot(subset(mydata2, Genotype == "FLCass"), aes(x=Day, y=Densityx10000, color=Temp))+
  geom_point()+
  theme_classic()+
  ggtitle("FLCass Growth Curve")+
  theme(plot.title = element_text(face = "bold", size=16), axis.text.x=element_text(color="black", size=13), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=14), axis.title.y = element_text(face="bold", color="black", size=14),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), legend.position = "none") +
  geom_smooth(formula = y ~ x, se = TRUE, size=2.5)+
  labs(x="Day", y="Density (10,000 cells/mL)")+
  scale_color_manual(values = c("skyblue3", "darkgoldenrod2"))
FLCass

RT362<-ggplot(subset(mydata2, Genotype == "RT362"), aes(x=Day, y=Densityx10000, color=Temp))+
  geom_point()+
  theme_classic()+
  ggtitle("RT362 Growth Curve")+
  theme(plot.title = element_text(face = "bold", size=16), axis.text.x=element_text(color="black", size=13), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=14), axis.title.y = element_text(face="bold", color="black", size=14),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), legend.position = "none") +
  geom_smooth(formula = y ~ x, se = TRUE, size=2.5)+
  labs(x="Day", y="Density (10,000 cells/mL)")+
  scale_color_manual(values = c("skyblue3", "darkgoldenrod2"))
RT362

KB8<-ggplot(subset(mydata2, Genotype == "KB8"), aes(x=Day, y=Densityx10000, color=Temp))+
  geom_point()+
  theme_classic()+
  ggtitle("KB8 Growth Curve")+
  theme(plot.title = element_text(face = "bold", size=16), axis.text.x=element_text(color="black", size=13), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=14), axis.title.y = element_text(face="bold", color="black", size=14),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), legend.position = "none") +
  geom_smooth(formula = y ~ x, se = TRUE, size=2.5)+
  labs(x="Day", y="Density (10,000 cells/mL)")+
  scale_color_manual(values = c("skyblue3", "darkgoldenrod2"))
KB8




#Exponential Graphs for 26 and 30#####

#New data frame without 32, and days 5-15
rm(list=ls())
mydata<-read.csv("GrowthDataCombined_r.csv")
mydata$Temp<-as.factor(mydata$Temp)
mydata2 <- subset(mydata, Temp == 26 | Temp == 30)
mydata2<- subset(mydata2, Day <16)
View(mydata2)


CCMP2458<-ggplot(subset(mydata2, Genotype == "CCMP2458"), aes(x=Day, y=Densityx10000, color=Temp))+
  geom_point()+
  theme_classic()+
  theme(plot.title = element_text(face = "bold", size=16), axis.text.x=element_text(color="black", size=13), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=14), axis.title.y = element_text(face="bold", color="black", size=14),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), legend.position = "none") +
  ggtitle("CCMP2458 Growth Curve")+
  labs(x="Day", y="Density (10,000 cells/mL)")+
  scale_color_manual(values = c("skyblue3", "darkgoldenrod2"))+
  geom_smooth(method = "glm", se = TRUE, size = 2) 
CCMP2458

CCMP2464<-ggplot(subset(mydata2, Genotype == "CCMP2464" & Round == "July"), aes(x=Day, y=Densityx10000, color=Temp))+
  geom_point()+
  theme_classic()+
  ggtitle("CCMP2464 Growth Curve")+
  theme(plot.title = element_text(face = "bold", size=16), axis.text.x=element_text(color="black", size=13), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=14), axis.title.y = element_text(face="bold", color="black", size=14),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), legend.position = "none") +
  geom_smooth(method = "glm", se = TRUE, size=2)+
  labs(x="Day", y="Density (10,000 cells/mL)")+
  scale_color_manual(values = c("skyblue3", "darkgoldenrod2"))
CCMP2464

FLCass<-ggplot(subset(mydata2, Genotype == "FLCass"), aes(x=Day, y=Densityx10000, color=Temp))+
  geom_point()+
  theme_classic()+
  ggtitle("FLCass Growth Curve")+
  theme(plot.title = element_text(face = "bold", size=16), axis.text.x=element_text(color="black", size=13), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=14), axis.title.y = element_text(face="bold", color="black", size=14),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), legend.position = "none") +
  geom_smooth(method = "glm", se = TRUE, size=2)+
  labs(x="Day", y="Density (10,000 cells/mL)")+
  scale_color_manual(values = c("skyblue3", "darkgoldenrod2"))
FLCass

RT362<-ggplot(subset(mydata2, Genotype == "RT362"), aes(x=Day, y=Densityx10000, color=Temp))+
  geom_point()+
  theme_classic()+
  ggtitle("RT362 Growth Curve")+
  theme(plot.title = element_text(face = "bold", size=16), axis.text.x=element_text(color="black", size=13), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=14), axis.title.y = element_text(face="bold", color="black", size=14),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), legend.position = "none") +
  geom_smooth(method = "glm", se = TRUE, size=2)+
  labs(x="Day", y="Density (10,000 cells/mL)")+
  scale_color_manual(values = c("skyblue3", "darkgoldenrod2"))
RT362

KB8<-ggplot(subset(mydata2, Genotype == "KB8"), aes(x=Day, y=Densityx10000, color=Temp))+
  geom_point()+
  theme_classic()+
  ggtitle("KB8 Growth Curve")+
  theme(plot.title = element_text(face = "bold", size=16), axis.text.x=element_text(color="black", size=13), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=14), axis.title.y = element_text(face="bold", color="black", size=14),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), legend.position = "none") +
  geom_smooth(method = "glm", se = TRUE, size=2)+
  labs(x="Day", y="Density (10,000 cells/mL)")+
  scale_color_manual(values = c("skyblue3", "darkgoldenrod2"))
KB8


#Calculating growth rates using library("growthrates")#####
#Outlined here: https://cran.r-project.org/web/packages/growthrates/vignettes/Introduction.html
library(growthrates)
#Clear the environment
rm(list=ls())
#Load combined growth data
mydata<-read.csv("GrowthDataCombined_r.csv")
mydata$Temp<-as.factor(mydata$Temp)
names(mydata)[1]<-"Date"
View(mydata)

splitted.data <- multisplit(mydata, c("Temp", "Genotype", "Flask", "Round")) 
#Splits data by temp, genotype, flask, and trial("round") so we work with one replicate of one treatement at a time

dat <- splitted.data[[1]] #First subset of data is named "dat" CCMP2458, 26*, flask 1, July
View(dat)

fit <- fit_easylinear(dat$Day, dat$Densityx10000)
#This method fits segments of linear models to the log-transformed data and tries to find the maximum growth rate.
#Im not sure how it knows to log transform it, might just be part of the code.

summary(fit)  #To inspect the outcome of this model fit
#I think it's just telling me that the model fits?

coef(fit)     #exponential growth parameters
#I think, mumax is maximum growth rate. Lag is how long it takes to get to exponential growth (I got a negative value??)

rsquared(fit)  # coefficient of determination (of log-transformed data)
deviance(fit)  # residual sum of squares of log-transformed data
#I think this fits an exponential curve to the highest growth rate 

#plot data
par(mfrow = c(1, 2))
plot(fit, log = "y")
plot(fit)

#Fit nonlinear parametric growth model (ie: logistic growth)

#Vector of start parameters and intial values of growth model
p<-c(y0=0.1,mumax=0.2,K=170)
# unconstraied fitting
fit1 <- fit_growthmodel(FUN = grow_logistic, p = p, dat$Day, dat$Densityx10000)
coef(fit1)
summary(fit1)

plot(fit1, log="y")
#I have no idea what I'm doing....


#Let's try something else that might make sense
fit1 <- fit_spline(dat$Day, dat$Densityx10000)
plot(fit1, log="y")
plot(fit1)

## derive start parameters from spline fit
p <- coef(fit1)

## use parameters from spline fit and take K from the data maximum
p <- c(coef(fit1), K = max(dat$Densityx10000))
fit2 <- fit_growthmodel(grow_logistic, p=p, time=dat$Day, y=dat$Densityx10000, transform="log")
plot(fit1)
lines(fit2, col="green")
#???
#Idk what this is telling me or why there are two different logistic curves. 
coef(fit2)
summary(fit2)


#Trying code from section titled "Nonparametric Smoothing Splines"#####
#Clear the environment
rm(list=ls())
#Load combined growth data
mydata<-read.csv("GrowthDataCombined_r.csv")
mydata$Temp<-as.factor(mydata$Temp)
names(mydata)[1]<-"Date"
View(mydata)

splitted.data <- multisplit(mydata, c("Temp", "Genotype", "Flask", "Round")) 
#Splits data by temp, genotype, flask, and trial("round") so we work with one replicate of one treatement at a time

dat <- splitted.data[[1]]
View(dat)
time <- dat$Day
y    <- dat$Densityx10000

## automatic smoothing with cv
res <- fit_spline(time, y)

par(mfrow = c(1, 2))
plot(res, log = "y")
plot(res)
#Not sure what the plots give me exactly, but the fits are both horrible. 
coef(res)
#mumax=0.44581721
#I'm getting different max growth rates with every strategy, and I have no idea
#what any of them are doing.



#Using "growthcurver" package#####
rm(list=ls())
mydata<-read.csv("GrowthDataCombined_r.csv")
mydata$Temp<-as.factor(mydata$Temp)
mydata$Flask<-as.factor(mydata$Flask)

#Getting rid of NA's, but can only do one round at a time.
MayJuneDat<-subset(mydata, Round=="MayJune")
#Pivoting data to use growthcurver package
#Value for each new cell is cell density from average (cells/mL)
MayJuneGrowthData<-pivot_wider(MayJuneDat, id_cols = NULL, names_from = c(Genotype,Flask,Temp),
                               names_prefix = "", names_repair = "check_unique",
                               values_from = Density_cellspermL, values_fill = NULL, values_fn = NULL)
#removing unnecessary columns (round, method, calculations, individual counts)
MayJuneGrowthData2 <- MayJuneGrowthData[ -c(1,2,4:18) ]
#Getting rid of NA's
#From stack overflow response "Here it, first, performs a wide-to-long data-transformation, excluding the "A" column and removing the missing values. Second, it groups by "A" column and the variable names. Third, it removes the duplicate values. Finally, it returns the data to its original wide format."
MayJuneData<-MayJuneGrowthData2 %>%
  gather(var, val, -Day, na.rm = TRUE) %>%
  group_by(Day, var) %>%
  distinct(val) %>%
  spread(var, val)
#IT WOOOOOOOOORKED

ggplot(data=MayJuneData,aes(x = time, y=CCMP2464_1_32))+geom_point()

#Need to rename "Day" column to "time" for the package. 
names(MayJuneData)[1]<-"time"

# Now, we'll use Growthcurver to summarize the growth curve data for the entire
# plate using the default background correction method ("min").
gc_out <- SummarizeGrowthByPlate(MayJuneData, plot_fit = TRUE, plot_file="MayJuneGrowthValues.pdf")
head(gc_out)
View(gc_out)
#CCMP data are the only data that can't be fit, cuz it didn't grow.

#Export to excel file
library("xlsx")
write.xlsx(gc_out, file = "GrowthcurverData.xlsx",
           sheetName = "MayJune", append = FALSE)

#Going to try to run the model for one replicate
model.FLCass_1_26 <- SummarizeGrowth(MayJuneData$time, MayJuneData$FLCass_1_26)
#Let's see how it looks.
plot(model.FLCass_1_26)
# To see all the available metrics
str(model.FLCass_1_26$vals)

model.FLCass_2_26 <- SummarizeGrowth(MayJuneData$time, MayJuneData$FLCass_2_26)
plot(model.FLCass_2_26)

model.FLCass_3_26 <- SummarizeGrowth(MayJuneData$time, MayJuneData$FLCass_3_26)
plot(model.FLCass_3_26)

model.FLCass_4_26 <- SummarizeGrowth(MayJuneData$time, MayJuneData$FLCass_4_26)
plot(model.FLCass_4_26)


####So now the whole thing for July
JulyData<-subset(mydata,Round == "July")


#Pivoting data to use growthcurver package
JulyGrowthData<-pivot_wider(JulyData, id_cols = NULL, names_from = c(Genotype,Flask,Temp),
                               names_prefix = "", names_repair = "check_unique",
                               values_from = Density_cellspermL, values_fill = NULL, values_fn = NULL)
#removing unnecessary columns
JulyGrowthData <- JulyGrowthData[ -c(1,2,4:18) ]
View(JulyGrowthData)
#Getting rid of NA's
#From stack overflow response "Here it, first, performs a wide-to-long data-transformation, excluding the "A" column and removing the missing values. Second, it groups by "A" column and the variable names. Third, it removes the duplicate values. Finally, it returns the data to its original wide format."
JulyData<-JulyGrowthData %>%
  gather(var, val, -Day, na.rm = TRUE) %>%
  group_by(Day, var) %>%
  distinct(val) %>%
  spread(var, val)
View(JulyData)
#IT WOOOOOOOOORKED

#Need to rename "Day" column to "time" for the package. 
names(JulyData)[1]<-"time"

# Now, we'll use Growthcurver to summarize the growth curve data for the entire
# plate using the default background correction method ("min").
gc_out2 <- SummarizeGrowthByPlate(JulyData, plot_fit = TRUE, plot_file="JulyGrowthValues.pdf")
head(gc_out2)
View(gc_out2)
#Export to same excel file
write.xlsx(gc_out2, file = "GrowthcurverData.xlsx",
           sheetName = "July", append = TRUE)


#Stats on data output from growthcurver#####
rm(list=ls())
mydata<-read.csv("Data/GrowthcurverData_r.csv")
mydata$Temp<-as.factor(mydata$Temp)
mydata$Flask<-as.factor(mydata$Flask)
View(mydata)
#going to remove CCMP2464 from MayJune
mydata2<-mydata[-c(13:24),]

summary<-mydata2%>%
  group_by(Round)%>%
  summarize(sigma=mean(sigma), r=mean(r))
summary
#"sigma is a measure of the goodnesss of fit of the parameters of the logistic equation for the data; 
#it is the residual sum of squares from the nonlinear regression model. 
#Smaller sigma values indicate a better fit of the logistic curve to the data than larger values."
hist(mydata2$sigma, main = "Histogram of sigma values", xlab = "sigma")
# Show the top 5 samples with the largest sigma value 
#(with the worst model fit to the growth curve data)
mydata2 %>% top_n(5, sigma) %>% arrange(desc(sigma))


#Make a model
#Genotype(fixed), Temperature(fixed), and Round (fixed) on K, r, or n0
model1<-lm(k~Genotype*Temp*Round, data=mydata2)
model1res<-resid(model1)
qqp(model1res, "norm")

#Super not normal
mydata2$logk<-log(mydata2$k)
model1<-lm(logk~Genotype*Temp*Round, data=mydata2)
model1res<-resid(model1)
qqp(model1res, "norm")
#Even worse

mydata2$sqrtk<-sqrt(mydata2$k)
model1<-lm(sqrtk~Genotype*Temp*Round, data=mydata2)
model1res<-resid(model1)
qqp(model1res, "norm")

mydata2$fourthrtk<-sqrt(mydata2$sqrtk)
model1<-lm(fourthrtk~Genotype*Temp*Round, data=mydata2)
model1res<-resid(model1)
qqp(model1res, "norm")

hist(mydata$k)
Summary <- mydata2 %>%
  group_by(Genotype, Temp, Round) %>%
  summarize(mean=mean(k, na.rm=TRUE), SE=sd(k, na.rm=TRUE)/sqrt(length(na.omit(k))))
View(Summary)

Summary$Round <- factor(Summary$Round,levels = c("MayJune", "July"))

kGraph<-ggplot(Summary, aes(x=Genotype, y=mean, fill=factor(Temp), group=factor(Temp)))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(axis.text.x=element_text(color="black", size=12), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=16), axis.title.y = element_text(face="bold", color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Genotype", y="k", fill="Temperature")+ #labels the x and y axes
  facet_wrap(  ~ Round)+
  scale_fill_manual(values = wes_palette("Royal2"))
kGraph
kGraph + ggsave("GrowthCurverGraph_carrycapacity.pdf", width=10, height=6.19, dpi=300, unit="in")

#CCMP2458 and RT362 have much higher values, dwarfing everything else
mydata3<-subset(mydata2, Genotype != "CCMP2458" & Genotype != "RT362")
View(mydata3)

model1<-lm(k~Genotype*Temp*Round, data=mydata3)
model1res<-resid(model1)
qqp(model1res, "norm")

Summary3 <- mydata3 %>%
  group_by(Genotype, Temp, Round) %>%
  summarize(mean=mean(k, na.rm=TRUE), SE=sd(k, na.rm=TRUE)/sqrt(length(na.omit(k))))
View(Summary3)
Summary3$Round <- factor(Summary3$Round,levels = c("MayJune", "July"))


kGraph_noCCMP2458orRT362<-ggplot(Summary3, aes(x=Genotype, y=mean, fill=factor(Temp), group=factor(Temp)))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(axis.text.x=element_text(color="black", size=12), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=16), axis.title.y = element_text(face="bold", color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Genotype", y="k", fill="Temperature")+#labels the x and y axes
  facet_wrap(  ~ Round)+
  scale_fill_manual(values = wes_palette("Royal2"))
kGraph_noCCMP2458orRT362
kGraph_noCCMP2458orRT362 + ggsave("GrowthCurverGraph_carrycapacity2.pdf", width=10, height=6.19, dpi=300, unit="in")

#Well the carrying capacity is useless. I needed to take more and longer data. Varies completely from one round to another


#Graphing and analyzing r#####
rm(list=ls())
mydata<-read.csv("Data/GrowthcurverData_r.csv")
mydata<-mydata%>%
  mutate_if(is.character,as.factor)

#Change name of MayJune to just May
mydata<-mydata %>%
  mutate(Round = as.character(Round),
         Round = if_else(Round == 'MayJune', 'May', Round),
         Round = as.factor(Round))

#going to remove CCMP2464 altogether, because  I can't include in stats with Round (aliased coefficients=not equal replication)
mydata2<-mydata %>%
  filter(Genotype!="CCMP2464")%>%
  droplevels()

Summary2 <- mydata2 %>%
  group_by(Genotype, Temp, Round) %>%
  summarize(mean=mean(r, na.rm=TRUE), SE=sd(r, na.rm=TRUE)/sqrt(length(na.omit(r))))
Summary2$Round <- factor(Summary2$Round,levels = c("May", "July"))

pal=pnw_palette("Sailboat",3)
pal=wes_palette("Moonrise3", 3)
pal=pnw_palette("Sunset2", 3)
pal<-c("#2c6184", "#f9ad2a", "#cc5c76")
pal<-c("#675478", "#efbc82", "#c67b6f") #Modified Sunset
pal<-c("#ac8eab", "#f2cec7", "#c67b6f") #I think this is the one! Modified Shuksan and sunset combined

rGraph<-ggplot(Summary2, aes(x=Genotype, y=mean, fill=factor(Temp), group=factor(Temp)))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(axis.text.x=element_text(color="black", size=12), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(color="black", size=16), axis.title.y = element_text(color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Symbiont Genotype", y="Maximum growth rate (r)", fill="Temperature")+#labels the x and y axes
  scale_fill_manual(values=pal, labels = c("26°C", "30°C", "32°C"))+
  ggtitle("Exponential Growth Rates calculated with growthcurver package")+
  scale_y_continuous(expand=c(0,0), limits=c(0,1))+
  facet_wrap(  ~ Round)
rGraph

#Removing title, otherwise same as graph above
pal<-c("#ac8eab", "#f2cec7", "#c67b6f")
rGraph.final<-ggplot(Summary2, aes(x=Genotype, y=mean, fill=factor(Temp), group=factor(Temp)))+  #basic plot
  theme_bw()+ #Removes grey background
  labs(x="Symbiont Genotype", y="Maximum growth rate (r)", fill="Temperature")+#labels the x and y axes
  theme(axis.text.x=element_text(color="black", size=11, angle = 30, hjust=1), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(color="black", size=16), axis.title.y = element_text(color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  scale_fill_manual(values=pal, labels = c("26°C", "30°C", "32°C"))+
  scale_y_continuous(expand=c(0,0), limits=c(0,1))+
  facet_wrap(  ~ Round)
rGraph.final
rGraph.final+ggsave("Graphs/FinalGraphs/culture_growth.png", width=8, height=5)

#Putting CCMP2464 back in for July
mydata3<-mydata[-c(13:24),]

Summary3 <- mydata3 %>%
  group_by(Genotype, Temp, Round) %>%
  summarize(mean=mean(r, na.rm=TRUE), SE=sd(r, na.rm=TRUE)/sqrt(length(na.omit(r))))
Summary3$Round <- factor(Summary3$Round,levels = c("May", "July"))

pal<-c("#ac8eab", "#f2cec7", "#c67b6f")
rGraph.final.all<-ggplot(Summary3, aes(x=Genotype, y=mean, fill=factor(Temp), group=factor(Temp)))+  #basic plot
  theme_bw()+ #Removes grey background
  labs(x="Symbiont Genotype", y="Maximum growth rate (r)", fill="Temperature")+#labels the x and y axes
  theme(axis.text.x=element_text(color="black", size=11, angle = 30, hjust=1), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(color="black", size=16), axis.title.y = element_text(color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  scale_fill_manual(values=pal, labels = c("26°C", "30°C", "32°C"))+
  scale_y_continuous(expand=c(0,0), limits=c(0,1))+
  facet_wrap(  ~ Round)
rGraph.final.all
rGraph.final.all+ggsave("Graphs/FinalGraphs/culture_growth.CCMP2464.png", width=8, height=5)

mydata3<-mydata[-c(13:24),]
model1<-lm(r~Genotype*Temp*Round, data=mydata3)
model1res<-resid(model1)
qqp(model1res, "norm")

mydata3$logr<-log(mydata3$r)

logr.model<-lm(logr~Round*Genotype*Temp, data=mydata3)
qqp(resid(logr.model), "norm")
#Pretty close

mydata3$loglogr<-log(mydata3$logr+3)
loglogr.model<-lm(loglogr~Genotype*Temp*Round, data=mydata3)
qqp(resid(loglogr.model), "norm")
#Same as one log

mydata3$sqrtr<-sqrt(mydata3$r)
sqrtr.model<-lm(sqrtr~Round*Genotype*Temp, data=mydata3)
qqp(resid(sqrtr.model), "norm")
#Not as good as log

Anova(logr.model, type="III")
#Aliased coefficients 


#Removing CCMP2464 altogether
lessdata<-subset(mydata3, Genotype !="CCMP2464")

No2464.model<-lm(r~Genotype*Temp*Round, data=lessdata)
qqp(resid(No2464.model), "norm")

logNo2464.model<-lm(logr~Genotype*Temp*Round, data=lessdata)
qqp(resid(logNo2464.model), "norm")
#Good
Anova(logNo2464.model, type="III")
#Yes, round is significant even without CCMP2464

#Boxcox
full.growth.model<-lm(r ~ Genotype*Temp*Round, data=mydata2)
step.growth.model<-stepAIC(full.growth.model, direction="both", trace = F)

boxcox<-boxcox(step.growth.model,lambda = seq(-5, 5, 1/1000),plotit = TRUE )

Selected.Power<-boxcox$x[boxcox$y==max(boxcox$y)]
Selected.Power
#0.18

mydata2$transf.r<-(mydata2$r)^0.18
transf.growth.model<-lm(transf.r~Genotype*Temp*Round, data = mydata2)
qqp(resid(transf.growth.model), "norm")
#I think that's the same as log, so I'll just stick with log


#Running stats on growth rate from growthcurver separated by round####
#MayJune (no CCMP2464)
MayJuneData<-mydata2%>%
  filter(Round=="May")%>%
  droplevels()

May.model<-lm(r~Genotype*Temp, data=MayJuneData)
qqp(resid(May.model), "norm")
#Normal, yay!
plot(May.model)
Anova(May.model, type="III")

#July
JulyData<-mydata%>%
  filter(Round=="July")%>%
  droplevels()
July.model<-lm(r~Genotype*Temp, data=JulyData)
qqp(resid(July.model), "norm")
#Not normal

JulyData$logr<-log(JulyData$r)
logJuly.model<-lm(logr~Genotype*Temp, data=JulyData)
qqp(resid(logJuly.model), "norm")
#Still not good enough

JulyData$loglogr<-log(JulyData$logr+3)
loglogJuly.model<-lm(loglogr~Genotype*Temp, data=JulyData)
qqp(resid(loglogJuly.model), "norm")
#Hm close

#BoxCox
full.model<-lm(r ~ Genotype*Temp, data=JulyData)
step.model<-stepAIC(full.model, direction="both", trace = F)

boxcox<-boxcox(step.model,lambda = seq(-5, 5, 1/1000),plotit = TRUE )

Selected.Power<-boxcox$x[boxcox$y==max(boxcox$y)]
Selected.Power
#0.15

JulyData$transf.r<-(JulyData$r)^0.15
transf.model<-lm(transf.r~Genotype*Temp, data = JulyData)
qqp(resid(transf.model), "norm")
#Worse than double log

#Using double log
Anova(loglogJuly.model, type="III")

#Graphing growthcurver max growthrate####
mydata<-read.csv("Data/GrowthcurverData_r.csv")
mydata$Temp<-as.factor(mydata$Temp)
mydata$Flask<-as.factor(mydata$Flask)
View(mydata)
#going to remove CCMP2464 from MayJune
mydata2<-mydata[-c(13:24),]
View(mydata2)

Summary2 <- mydata2 %>%
  group_by(Genotype, Temp, Round) %>%
  summarize(mean=mean(r, na.rm=TRUE), SE=sd(r, na.rm=TRUE)/sqrt(length(na.omit(r))))
View(Summary2)
Summary2$Round <- factor(Summary2$Round,levels = c("MayJune", "July"))

Growthcurver.r.Graph<-ggplot(Summary2, aes(x=Genotype, y=mean, fill=factor(Temp), group=factor(Temp)))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(axis.text.x=element_text(color="black", size=12), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=16), axis.title.y = element_text(face="bold", color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Genotype", y="Excel Exponential", fill="Temperature")+#labels the x and y axes
  facet_wrap(  ~ Round)+
  scale_fill_manual(values = wes_palette("Moonrise3"))+
  ggtitle("Exponential Growth Rates from growthcruver package")
Growthcurver.r.Graph
Growthcurver.r.Graph+ggsave("GrowthCurverGraph_growthrates.pdf", width=10, height=6.19, dpi=300, unit="in")

#Bargraph of just July growthcurver####
JulyData <- mydata %>%
  filter(Round == "July") %>%
  droplevels
JulyData

JulySummary <- JulyData %>%
  group_by(Genotype, Temp) %>%
  summarize(mean=mean(r, na.rm=TRUE), SE=sd(r, na.rm=TRUE)/sqrt(length(na.omit(r))))
head(JulySummary)

July.growthcurver.r.graph<-ggplot(JulySummary, aes(x=Genotype, y=mean, fill=factor(Temp), group=factor(Temp)))+  #basic plot
  theme_bw()+ #Removes grey background
  scale_y_continuous(expand=c(0,0), limits=c(0, 1))+
  theme(plot.title = element_text(face = "bold", size=16), axis.text.x=element_text(color="black", size=12), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(color="black", size=16), axis.title.y = element_text(color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Symbiont Strain", y="Max Growth Rate", fill="Temperature")+#labels the x and y axes
  scale_fill_manual(values = c("skyblue3", "darkgoldenrod2", "brown3"), labels=c("26°C", "30°C", "32°C"))+
  ggtitle("Maximum Growth Rate of Symbionts in Culture")
July.growthcurver.r.graph
July.growthcurver.r.graph+ggsave("Graphs/Growth/July.growthcurver.r.png", width=8, height=5)

#Boxplot of growthercurver max growth rate####
mydata<-read.csv("Data/GrowthcurverData_r.csv")
mydata$Temp<-as.factor(mydata$Temp)
mydata$Flask<-as.factor(mydata$Flask)
View(mydata)
#going to remove CCMP2464 from MayJune
mydata2<-mydata[-c(13:24),]
View(mydata2)
#Reordering round so MayJune comes up first
mydata2$Round<-factor(mydata2$Round, levels = c("MayJune", "July"))

pal=pnw_palette("Starfish",3, type = "discrete")
pal=wes_palette("Moonrise3", 3, type = c("discrete"))
scale_fill_manual(values = wes_palette("Moonrise3"), labels=c("26°C", "30°C", "32°C"))
pal
colors(pal)
#blue: #79CFDB
#green: #859A51
#pink: #DFADE1

boxplot<-mydata2%>%
  ggplot(aes(x=Genotype, y=r, fill=Temp))+
  geom_boxplot()+
  theme_bw()+
  geom_jitter(color="black", size=1, alpha=0.7)+
  scale_x_discrete(name = "Genotype") +
  scale_y_continuous(name = "Max growth rate (r)")+
  ggtitle("Maximum Growth Rate (from growthcurver package)")+
  theme(plot.title = element_text(hjust=0.5, face="bold"), 
        axis.text.x=element_text(size=10), 
        axis.text.y=element_text(size=10), 
        axis.title.y = element_text(face="bold", size=12), 
        axis.title.x = element_text(face="bold", size=12))+
  labs(fill="Temperature")+
  scale_fill_manual(values = c("#79CFDB", "#859A51", "#DFADE1"), labels=c("26°C", "30°C","32°C"))+
  facet_wrap(  ~ Round)
boxplot
boxplot+ggsave("Graphs/Growth/Growthcurver.growthrate.boxplot.png", width=10, height=5)

violinplot<-mydata2%>%
  ggplot(aes(x=Genotype, y=r, fill=Temp))+
  geom_violin()+
  facet_wrap( ~Round)
violinplot

data %>%
  ggplot( aes(x=name, y=value, fill=name)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("A boxplot with jitter") +
  xlab("")


#Graphing values I got using excel to calculate exponential growth####
#exponenant from exponential equation
mydata<-read.csv("Data/GrowthcurverData_r.csv")
mydata$Temp<-as.factor(mydata$Temp)
mydata$Flask<-as.factor(mydata$Flask)

Summary3 <- mydata %>%
  group_by(Genotype, Temp, Round) %>%
  summarize(mean=mean(ExcelExp, na.rm=TRUE), SE=sd(ExcelExp, na.rm=TRUE)/sqrt(length(na.omit(ExcelExp))))

View(Summary3)
Summary3$Round <- factor(Summary3$Round,levels = c("MayJune", "July"))

ExcelExpGraph<-ggplot(Summary3, aes(x=Genotype, y=mean, fill=factor(Temp), group=factor(Temp)))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(axis.text.x=element_text(color="black", size=12), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=16), axis.title.y = element_text(face="bold", color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Genotype", y="Excel Exponential", fill="Temperature")+#labels the x and y axes
  facet_wrap(  ~ Round)+
  scale_fill_manual(values = wes_palette("Moonrise3"))+
  ggtitle("Exponential Growth Rates calculated from Excel")
ExcelExpGraph
ExcelExpGraph+ggsave("ExpGrowthfromExcel.pdf", width=10, height=6.19, dpi=300, unit="in")


#Stats on Excel exp growth values####
#Clear the environment
rm(list=ls())
mydata<-read.csv("Data/GrowthcurverData_r.csv")
mydata$Temp<-as.factor(mydata$Temp)
mydata$Flask<-as.factor(mydata$Flask)

#Remove CCMP2464 MayJune
mydata2<-mydata[-c(13:24),]

excel.model<-lm(ExcelExp~Genotype*Temp*Round, data=mydata2)
qqp(resid(excel.model), "norm")
#Wow, it's normal
anova(excel.model)
summary(excel.model)

#

#Per capita growth curves from hand calculated growth rates#####

#Clear the environment
rm(list=ls())
#Load growth data
mydata<-read.csv("GrowthDataCombined_r.csv")
View(mydata)
mydata$Temp<-as.factor(mydata$Temp)

KB8Growth32F1<-ggplot(subset(mydata, Genotype == "KB8" & Temp=="32" & Flask=="1"), aes(x=Densityx10000, y=PerCapitaGrowthRate, color=Round))+
  geom_point()+
  theme_classic()+
  ggtitle("Per Capita Growth Curve KB8 - 32*")+
  geom_quantile(formula=y~x)+
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)
KB8Growth32F1
#"Missing data" is the last data point for each trial, since there is no per capita growth 
# rate for the last point. 
# y-int is r (highest growth rate), x-int is K (carrying capacity)
#For KB8, 32*, Flask 1, July: r= 2.38, K=82.64(x10,000)
#For KB8, 32*, Flask 1, MayJune: r=1.03, K=92.79(x10,000)

#Gonna see what the growthcurves package gives me

CCMP2458PerCap26F1<-ggplot(subset(mydata, Genotype == "CCMP2458" & Temp=="26" & Flask=="1"), aes(x=Densityx10000, y=PerCapitaGrowthRate, color=Round))+
  geom_point()+
  theme_classic()+
  ggtitle("Per Capita Growth Curve CCMP2458 - 26*")+
  geom_quantile(formula=y~x)+
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)
CCMP2458PerCap26F1
#For CCMP2458, 26*, Flask 1, July: r= 1.34, K=57.26(x10,000)
#For KB8, 32*, Flask 1, MayJune: r=1.12, K=146.6(x10,000)
#Growthrates package for MayJune gives me this output
#y0          y0_lm     mumax       lag 
#2.5555556 2.2217903 0.3080364 0.4543499 



#Calculating logistic growth rates day 1-15#####
#New strategy
#Looking at all of the graphs, they are all in logistic growth from day 5-15, so I'm 
#going to calculate a line from that, and use that as growth rate

#Clear the environment
rm(list=ls())
#Load growth data
mydata<-read.csv("GrowthDataCombined_r.csv")
View(mydata)
mydata$Temp<-as.factor(mydata$Temp)


CCMP2458<-ggplot(subset(mydata, Genotype == "CCMP2458" & Temp=="26" & Flask=="1" & Day > 4 & Day < 16), aes(x=Densityx10000, y=PerCapitaGrowthRate, color=Round))+
  geom_point()+
  theme_classic()+
  ggtitle("Per Capita Growth Curve CCMP2458 - 26*")+
  geom_quantile(formula=y~x)+
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)
CCMP2458

#Graph of 2458 @ 26* Flask 1, across BOTH trials, for days 5-15
CCMP2458<-ggplot(subset(mydata, Genotype == "CCMP2458" & Temp =="26" & Flask =="1" & Day > 4 & Day < 16), aes(x=Day, y=Densityx10000, color=Temp))+
  geom_point()+
  theme_classic()+
  theme(plot.title = element_text(face = "bold", size=16), axis.text.x=element_text(color="black", size=13), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=14), axis.title.y = element_text(face="bold", color="black", size=14),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), legend.position = "none") +
  ggtitle("CCMP2458 26 deg flaks 1")+
  labs(x="Day", y="Density (10,000 cells/mL)")+
  geom_smooth(method="lm")+
  geom_quantile(formula=y~x)+
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)
CCMP2458

ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}
fit1 <- lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "CCMP2458" & Temp =="26" & Flask =="1" & Day < 16))
ggplotRegression(fit1)
#Slope=6.8501, Intercept = -27.1, Adj R2 = 0.61124
#Both methods give me the same values!! Hooray!!
#Second method is less code, so I'm going to do that. 

#CCMP2458, 26*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "CCMP2458" & Temp =="26" & Flask =="1" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "CCMP2458" & Temp =="26" & Flask =="2" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "CCMP2458" & Temp =="26" & Flask =="3" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "CCMP2458" & Temp =="26" & Flask =="4" & Day < 16)))

#CCMP2458, 30*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "CCMP2458" & Temp =="30" & Flask =="1" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "CCMP2458" & Temp =="30" & Flask =="2" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "CCMP2458" & Temp =="30" & Flask =="3" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "CCMP2458" & Temp =="30" & Flask =="4" & Day < 16)))

#CCMP2458, 32*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "CCMP2458" & Temp =="32" & Flask =="1" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "CCMP2458" & Temp =="32" & Flask =="2" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "CCMP2458" & Temp =="32" & Flask =="3" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "CCMP2458" & Temp =="32" & Flask =="4" & Day < 16)))


#CCMP2464, 26*, flask 1-4 (July only!)
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "CCMP2464" & Temp =="26" & Flask =="1" & Day < 16 & Round == "July")))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "CCMP2464" & Temp =="26" & Flask =="2" & Day < 16 & Round == "July")))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "CCMP2464" & Temp =="26" & Flask =="3" & Day < 16 & Round == "July")))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "CCMP2464" & Temp =="26" & Flask =="4" & Day < 16 & Round == "July")))

#CCMP2464, 30*, flask 1-4 (July only!)
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "CCMP2464" & Temp =="30" & Flask =="1" & Day < 16 & Round == "July")))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "CCMP2464" & Temp =="30" & Flask =="2" & Day < 16 & Round == "July")))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "CCMP2464" & Temp =="30" & Flask =="3" & Day < 16 & Round == "July")))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "CCMP2464" & Temp =="30" & Flask =="4" & Day < 16 & Round == "July")))

#CCMP2464, 32*, flask 1-4 (July only!)
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "CCMP2464" & Temp =="32" & Flask =="1" & Day < 16 & Round == "July")))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "CCMP2464" & Temp =="32" & Flask =="2" & Day < 16 & Round == "July")))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "CCMP2464" & Temp =="32" & Flask =="3" & Day < 16 & Round == "July")))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "CCMP2464" & Temp =="32" & Flask =="4" & Day < 16 & Round == "July")))


#FLCass, 26*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "FLCass" & Temp =="26" & Flask =="1" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "FLCass" & Temp =="26" & Flask =="2" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "FLCass" & Temp =="26" & Flask =="3" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "FLCass" & Temp =="26" & Flask =="4" & Day < 16)))

#FLCass, 30*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "FLCass" & Temp =="30" & Flask =="1" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "FLCass" & Temp =="30" & Flask =="2" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "FLCass" & Temp =="30" & Flask =="3" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "FLCass" & Temp =="30" & Flask =="4" & Day < 16)))

#FLCass, 32*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "FLCass" & Temp =="32" & Flask =="1" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "FLCass" & Temp =="32" & Flask =="2" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "FLCass" & Temp =="32" & Flask =="3" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "FLCass" & Temp =="32" & Flask =="4" & Day < 16)))


#KB8, 26*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "KB8" & Temp =="26" & Flask =="1" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "KB8" & Temp =="26" & Flask =="2" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "KB8" & Temp =="26" & Flask =="3" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "KB8" & Temp =="26" & Flask =="4" & Day < 16)))

#KB8, 30*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "KB8" & Temp =="30" & Flask =="1" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "KB8" & Temp =="30" & Flask =="2" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "KB8" & Temp =="30" & Flask =="3" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "KB8" & Temp =="30" & Flask =="4" & Day < 16)))

#KB8, 32*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "KB8" & Temp =="32" & Flask =="1" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "KB8" & Temp =="32" & Flask =="2" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "KB8" & Temp =="32" & Flask =="3" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "KB8" & Temp =="32" & Flask =="4" & Day < 16)))


#RT362, 26*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "RT362" & Temp =="26" & Flask =="1" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "RT362" & Temp =="26" & Flask =="2" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "RT362" & Temp =="26" & Flask =="3" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "RT362" & Temp =="26" & Flask =="4" & Day < 16)))

#RT362, 30*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "RT362" & Temp =="30" & Flask =="1" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "RT362" & Temp =="30" & Flask =="2" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "RT362" & Temp =="30" & Flask =="3" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "RT362" & Temp =="30" & Flask =="4" & Day < 16)))

#RT362, 32*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "RT362" & Temp =="32" & Flask =="1" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "RT362" & Temp =="32" & Flask =="2" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "RT362" & Temp =="32" & Flask =="3" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(mydata, Genotype == "RT362" & Temp =="32" & Flask =="4" & Day < 16)))

#Same as above but separated by round#####
#Clear the environment
rm(list=ls())
#Load growth data
mydata<-read.csv("Data/GrowthDataCombined_r.csv")
View(mydata)
mydata$Temp<-as.factor(mydata$Temp)

MayJuneData <- subset(mydata, Round == "MayJune")
JulyData<- subset(mydata, Round =="July")


plot(Densityx10000~Day, data=mydata)

plot(Densityx10000~Day, data = subset(MayJuneData, Genotype == "CCMP2458" & Temp =="26" & Flask =="1" & Day > 4 & Day < 16))
mod <- lm(Densityx10000~Day, data = subset(MayJuneData, Genotype == "CCMP2458" & Temp =="26" & Flask =="1" & Day > 4 & Day < 16))
summary(mod)
mod
abline(lm(Densityx10000~Day, data = subset(MayJuneData, Genotype == "CCMP2458" & Temp =="26" & Flask =="1" & Day > 4 & Day < 16)))



ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}

#MayJune data, day 1-15#####
#CCMP2458, 26*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "CCMP2458" & Temp =="26" & Flask =="1" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "CCMP2458" & Temp =="26" & Flask =="2" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "CCMP2458" & Temp =="26" & Flask =="3" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "CCMP2458" & Temp =="26" & Flask =="4" & Day < 16)))

#CCMP2458, 30*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "CCMP2458" & Temp =="30" & Flask =="1" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "CCMP2458" & Temp =="30" & Flask =="2" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "CCMP2458" & Temp =="30" & Flask =="3" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "CCMP2458" & Temp =="30" & Flask =="4" & Day < 16)))

#CCMP2458, 32*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "CCMP2458" & Temp =="32" & Flask =="1" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "CCMP2458" & Temp =="32" & Flask =="2" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "CCMP2458" & Temp =="32" & Flask =="3" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "CCMP2458" & Temp =="32" & Flask =="4" & Day < 16)))

#No CCMP2464 for MayJune

#FLCass, 26*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "FLCass" & Temp =="26" & Flask =="1" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "FLCass" & Temp =="26" & Flask =="2" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "FLCass" & Temp =="26" & Flask =="3" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "FLCass" & Temp =="26" & Flask =="4" & Day < 16)))

#FLCass, 30*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "FLCass" & Temp =="30" & Flask =="1" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "FLCass" & Temp =="30" & Flask =="2" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "FLCass" & Temp =="30" & Flask =="3" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "FLCass" & Temp =="30" & Flask =="4" & Day < 16)))

#FLCass, 32*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "FLCass" & Temp =="32" & Flask =="1" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "FLCass" & Temp =="32" & Flask =="2" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "FLCass" & Temp =="32" & Flask =="3" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "FLCass" & Temp =="32" & Flask =="4" & Day < 16)))


#KB8, 26*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "KB8" & Temp =="26" & Flask =="1" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "KB8" & Temp =="26" & Flask =="2" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "KB8" & Temp =="26" & Flask =="3" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "KB8" & Temp =="26" & Flask =="4" & Day < 16)))

#KB8, 30*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "KB8" & Temp =="30" & Flask =="1" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "KB8" & Temp =="30" & Flask =="2" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "KB8" & Temp =="30" & Flask =="3" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "KB8" & Temp =="30" & Flask =="4" & Day < 16)))

#KB8, 32*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "KB8" & Temp =="32" & Flask =="1" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "KB8" & Temp =="32" & Flask =="2" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "KB8" & Temp =="32" & Flask =="3" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "KB8" & Temp =="32" & Flask =="4" & Day < 16)))


#RT362, 26*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "RT362" & Temp =="26" & Flask =="1" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "RT362" & Temp =="26" & Flask =="2" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "RT362" & Temp =="26" & Flask =="3" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "RT362" & Temp =="26" & Flask =="4" & Day < 16)))

#RT362, 30*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "RT362" & Temp =="30" & Flask =="1" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "RT362" & Temp =="30" & Flask =="2" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "RT362" & Temp =="30" & Flask =="3" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "RT362" & Temp =="30" & Flask =="4" & Day < 16)))

#RT362, 32*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "RT362" & Temp =="32" & Flask =="1" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "RT362" & Temp =="32" & Flask =="2" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "RT362" & Temp =="32" & Flask =="3" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "RT362" & Temp =="32" & Flask =="4" & Day < 16)))


#July data, day 1-15#####
#CCMP2458, 26*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "CCMP2458" & Temp =="26" & Flask =="1" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "CCMP2458" & Temp =="26" & Flask =="2" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "CCMP2458" & Temp =="26" & Flask =="3" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "CCMP2458" & Temp =="26" & Flask =="4" & Day < 16)))

#CCMP2458, 30*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "CCMP2458" & Temp =="30" & Flask =="1" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "CCMP2458" & Temp =="30" & Flask =="2" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "CCMP2458" & Temp =="30" & Flask =="3" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "CCMP2458" & Temp =="30" & Flask =="4" & Day < 16)))

#CCMP2458, 32*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "CCMP2458" & Temp =="32" & Flask =="1" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "CCMP2458" & Temp =="32" & Flask =="2" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "CCMP2458" & Temp =="32" & Flask =="3" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "CCMP2458" & Temp =="32" & Flask =="4" & Day < 16)))


#CCMP2464, 26*, flask 1-4 (July only!)
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "CCMP2464" & Temp =="26" & Flask =="1" & Day < 16 & Round == "July")))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "CCMP2464" & Temp =="26" & Flask =="2" & Day < 16 & Round == "July")))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "CCMP2464" & Temp =="26" & Flask =="3" & Day < 16 & Round == "July")))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "CCMP2464" & Temp =="26" & Flask =="4" & Day < 16 & Round == "July")))

#CCMP2464, 30*, flask 1-4 (July only!)
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "CCMP2464" & Temp =="30" & Flask =="1" & Day < 16 & Round == "July")))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "CCMP2464" & Temp =="30" & Flask =="2" & Day < 16 & Round == "July")))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "CCMP2464" & Temp =="30" & Flask =="3" & Day < 16 & Round == "July")))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "CCMP2464" & Temp =="30" & Flask =="4" & Day < 16 & Round == "July")))

#CCMP2464, 32*, flask 1-4 (July only!)
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "CCMP2464" & Temp =="32" & Flask =="1" & Day < 16 & Round == "July")))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "CCMP2464" & Temp =="32" & Flask =="2" & Day < 16 & Round == "July")))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "CCMP2464" & Temp =="32" & Flask =="3" & Day < 16 & Round == "July")))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "CCMP2464" & Temp =="32" & Flask =="4" & Day < 16 & Round == "July")))


#FLCass, 26*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "FLCass" & Temp =="26" & Flask =="1" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "FLCass" & Temp =="26" & Flask =="2" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "FLCass" & Temp =="26" & Flask =="3" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "FLCass" & Temp =="26" & Flask =="4" & Day < 16)))

#FLCass, 30*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "FLCass" & Temp =="30" & Flask =="1" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "FLCass" & Temp =="30" & Flask =="2" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "FLCass" & Temp =="30" & Flask =="3" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "FLCass" & Temp =="30" & Flask =="4" & Day < 16)))

#FLCass, 32*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "FLCass" & Temp =="32" & Flask =="1" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "FLCass" & Temp =="32" & Flask =="2" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "FLCass" & Temp =="32" & Flask =="3" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "FLCass" & Temp =="32" & Flask =="4" & Day < 16)))


#KB8, 26*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "KB8" & Temp =="26" & Flask =="1" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "KB8" & Temp =="26" & Flask =="2" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "KB8" & Temp =="26" & Flask =="3" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "KB8" & Temp =="26" & Flask =="4" & Day < 16)))

#KB8, 30*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "KB8" & Temp =="30" & Flask =="1" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "KB8" & Temp =="30" & Flask =="2" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "KB8" & Temp =="30" & Flask =="3" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "KB8" & Temp =="30" & Flask =="4" & Day < 16)))

#KB8, 32*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "KB8" & Temp =="32" & Flask =="1" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "KB8" & Temp =="32" & Flask =="2" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "KB8" & Temp =="32" & Flask =="3" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "KB8" & Temp =="32" & Flask =="4" & Day < 16)))


#RT362, 26*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "RT362" & Temp =="26" & Flask =="1" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "RT362" & Temp =="26" & Flask =="2" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "RT362" & Temp =="26" & Flask =="3" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "RT362" & Temp =="26" & Flask =="4" & Day < 16)))

#RT362, 30*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "RT362" & Temp =="30" & Flask =="1" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "RT362" & Temp =="30" & Flask =="2" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "RT362" & Temp =="30" & Flask =="3" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "RT362" & Temp =="30" & Flask =="4" & Day < 16)))

#RT362, 32*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "RT362" & Temp =="32" & Flask =="1" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "RT362" & Temp =="32" & Flask =="2" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "RT362" & Temp =="32" & Flask =="3" & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "RT362" & Temp =="32" & Flask =="4" & Day < 16)))


#Slope from day 5-15 (logistic growth)####
#Clear the environment
rm(list=ls())
#Load growth data
mydata<-read.csv("Data/GrowthDataCombined_r.csv")
View(mydata)
mydata$Temp<-as.factor(mydata$Temp)

MayJuneData <- subset(mydata, Round == "MayJune")
JulyData<- subset(mydata, Round =="July")

ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}

#MayJune data, day 5-15#####
#CCMP2458, 26*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "CCMP2458" & Temp =="26" & Flask =="1" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "CCMP2458" & Temp =="26" & Flask =="2" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "CCMP2458" & Temp =="26" & Flask =="3" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "CCMP2458" & Temp =="26" & Flask =="4" & Day > 4 & Day < 16)))

#CCMP2458, 30*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "CCMP2458" & Temp =="30" & Flask =="1" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "CCMP2458" & Temp =="30" & Flask =="2" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "CCMP2458" & Temp =="30" & Flask =="3" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "CCMP2458" & Temp =="30" & Flask =="4" & Day > 4 & Day < 16)))

#CCMP2458, 32*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "CCMP2458" & Temp =="32" & Flask =="1" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "CCMP2458" & Temp =="32" & Flask =="2" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "CCMP2458" & Temp =="32" & Flask =="3" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "CCMP2458" & Temp =="32" & Flask =="4" & Day > 4 & Day < 16)))

#No CCMP2464 for MayJune

#FLCass, 26*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "FLCass" & Temp =="26" & Flask =="1" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "FLCass" & Temp =="26" & Flask =="2" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "FLCass" & Temp =="26" & Flask =="3" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "FLCass" & Temp =="26" & Flask =="4" & Day > 4 & Day < 16)))

#FLCass, 30*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "FLCass" & Temp =="30" & Flask =="1" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "FLCass" & Temp =="30" & Flask =="2" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "FLCass" & Temp =="30" & Flask =="3" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "FLCass" & Temp =="30" & Flask =="4" & Day > 4 & Day < 16)))

#FLCass, 32*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "FLCass" & Temp =="32" & Flask =="1" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "FLCass" & Temp =="32" & Flask =="2" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "FLCass" & Temp =="32" & Flask =="3" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "FLCass" & Temp =="32" & Flask =="4" & Day > 4 & Day < 16)))


#KB8, 26*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "KB8" & Temp =="26" & Flask =="1" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "KB8" & Temp =="26" & Flask =="2" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "KB8" & Temp =="26" & Flask =="3" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "KB8" & Temp =="26" & Flask =="4" & Day > 4 & Day < 16)))

#KB8, 30*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "KB8" & Temp =="30" & Flask =="1" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "KB8" & Temp =="30" & Flask =="2" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "KB8" & Temp =="30" & Flask =="3" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "KB8" & Temp =="30" & Flask =="4" & Day > 4 & Day < 16)))

#KB8, 32*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "KB8" & Temp =="32" & Flask =="1" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "KB8" & Temp =="32" & Flask =="2" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "KB8" & Temp =="32" & Flask =="3" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "KB8" & Temp =="32" & Flask =="4" & Day > 4 & Day < 16)))


#RT362, 26*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "RT362" & Temp =="26" & Flask =="1" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "RT362" & Temp =="26" & Flask =="2" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "RT362" & Temp =="26" & Flask =="3" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "RT362" & Temp =="26" & Flask =="4" & Day > 4 & Day < 16)))

#RT362, 30*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "RT362" & Temp =="30" & Flask =="1" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "RT362" & Temp =="30" & Flask =="2" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "RT362" & Temp =="30" & Flask =="3" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "RT362" & Temp =="30" & Flask =="4" & Day > 4 & Day < 16)))

#RT362, 32*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "RT362" & Temp =="32" & Flask =="1" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "RT362" & Temp =="32" & Flask =="2" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "RT362" & Temp =="32" & Flask =="3" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(MayJuneData, Genotype == "RT362" & Temp =="32" & Flask =="4" & Day > 4 & Day < 16)))

#July data, day 5-15#####
#CCMP2458, 26*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "CCMP2458" & Temp =="26" & Flask =="1" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "CCMP2458" & Temp =="26" & Flask =="2" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "CCMP2458" & Temp =="26" & Flask =="3" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "CCMP2458" & Temp =="26" & Flask =="4" & Day > 4 & Day < 16)))

#CCMP2458, 30*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "CCMP2458" & Temp =="30" & Flask =="1" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "CCMP2458" & Temp =="30" & Flask =="2" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "CCMP2458" & Temp =="30" & Flask =="3" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "CCMP2458" & Temp =="30" & Flask =="4" & Day > 4 & Day < 16)))

#CCMP2458, 32*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "CCMP2458" & Temp =="32" & Flask =="1" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "CCMP2458" & Temp =="32" & Flask =="2" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "CCMP2458" & Temp =="32" & Flask =="3" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "CCMP2458" & Temp =="32" & Flask =="4" & Day > 4 & Day < 16)))


#CCMP2464, 26*, flask 1-4 
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "CCMP2464" & Temp =="26" & Flask =="1" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "CCMP2464" & Temp =="26" & Flask =="2" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "CCMP2464" & Temp =="26" & Flask =="3" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "CCMP2464" & Temp =="26" & Flask =="4" & Day > 4 & Day < 16)))

#CCMP2464, 30*, flask 1-4 (July only!)
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "CCMP2464" & Temp =="30" & Flask =="1" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "CCMP2464" & Temp =="30" & Flask =="2" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "CCMP2464" & Temp =="30" & Flask =="3" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "CCMP2464" & Temp =="30" & Flask =="4" & Day > 4 & Day < 16)))

#CCMP2464, 32*, flask 1-4 (July only!)
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "CCMP2464" & Temp =="32" & Flask =="1" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "CCMP2464" & Temp =="32" & Flask =="2" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "CCMP2464" & Temp =="32" & Flask =="3" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "CCMP2464" & Temp =="32" & Flask =="4" & Day > 4 & Day < 16)))


#FLCass, 26*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "FLCass" & Temp =="26" & Flask =="1" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "FLCass" & Temp =="26" & Flask =="2" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "FLCass" & Temp =="26" & Flask =="3" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "FLCass" & Temp =="26" & Flask =="4" & Day > 4 & Day < 16)))

#FLCass, 30*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "FLCass" & Temp =="30" & Flask =="1" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "FLCass" & Temp =="30" & Flask =="2" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "FLCass" & Temp =="30" & Flask =="3" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "FLCass" & Temp =="30" & Flask =="4" & Day > 4 & Day < 16)))

#FLCass, 32*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "FLCass" & Temp =="32" & Flask =="1" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "FLCass" & Temp =="32" & Flask =="2" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "FLCass" & Temp =="32" & Flask =="3" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "FLCass" & Temp =="32" & Flask =="4" & Day > 4 & Day < 16)))


#KB8, 26*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "KB8" & Temp =="26" & Flask =="1" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "KB8" & Temp =="26" & Flask =="2" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "KB8" & Temp =="26" & Flask =="3" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "KB8" & Temp =="26" & Flask =="4" & Day > 4 & Day < 16)))

#KB8, 30*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "KB8" & Temp =="30" & Flask =="1" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "KB8" & Temp =="30" & Flask =="2" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "KB8" & Temp =="30" & Flask =="3" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "KB8" & Temp =="30" & Flask =="4" & Day > 4 & Day < 16)))

#KB8, 32*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "KB8" & Temp =="32" & Flask =="1" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "KB8" & Temp =="32" & Flask =="2" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "KB8" & Temp =="32" & Flask =="3" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "KB8" & Temp =="32" & Flask =="4" & Day > 4 & Day < 16)))


#RT362, 26*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "RT362" & Temp =="26" & Flask =="1" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "RT362" & Temp =="26" & Flask =="2" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "RT362" & Temp =="26" & Flask =="3" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "RT362" & Temp =="26" & Flask =="4" & Day > 4 & Day < 16)))

#RT362, 30*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "RT362" & Temp =="30" & Flask =="1" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "RT362" & Temp =="30" & Flask =="2" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "RT362" & Temp =="30" & Flask =="3" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "RT362" & Temp =="30" & Flask =="4" & Day > 4 & Day < 16)))

#RT362, 32*, flask 1-4
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "RT362" & Temp =="32" & Flask =="1" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "RT362" & Temp =="32" & Flask =="2" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "RT362" & Temp =="32" & Flask =="3" & Day > 4 & Day < 16)))
ggplotRegression(lm(Densityx10000 ~ Day, data = subset(JulyData, Genotype == "RT362" & Temp =="32" & Flask =="4" & Day > 4 & Day < 16)))


#Created new data with all of those values####
#Clear the environment
rm(list=ls())
#Load growth data
mydata<-read.csv("Data/CultureSlopes_Days1and5to15.csv")
mydata$Temperature<-as.factor(mydata$Temperature)
mydata$Flask<-as.factor(mydata$Flask)
View(mydata)
#Remove combined, since I need to see it between rounds
mydata<-subset(mydata, Round != "Combined")
#This actually removed combined from the data
mydata$Round <- factor(mydata$Round,levels = c("MayJune", "July"))

#Make a model
#Genotype(fixed) and Temperature(fixed) on Slope (ie: growth rate, NOT PER CAPITA, problem?)
model1<-lm(Slope.5.to.15~Genotype*Temperature*Round, data=mydata)
qqp(resid(model1), "norm")
plot(model1)
#So normal, wow

anova(model1)
#Everything is very significant. 
summary(model1)

#Check if removing CCMP2464 altogether changes output. 
noCCMP2464<-subset(mydata, Genotype != "CCMP2464")
noCCMP2464$Genotype <- factor(noCCMP2464$Genotype,levels = c("CCMP2458", "FLCass", "KB8", "RT362"))
#Genotype(fixed) and Temperature(fixed) on Slope (ie: growth rate, NOT PER CAPITA, problem?)
model2<-lm(Slope.5.to.15~Genotype*Temperature*Round, data=noCCMP2464)
qqp(resid(model2), "norm")
plot(model2)
anova(model2)
#Nope, everything still significant

#Barplot of slopes
Summary <- mydata %>%
  group_by(Genotype, Temperature, Round) %>%
  summarize(mean=mean(Slope.5.to.15, na.rm=TRUE), SE=sd(Slope.5.to.15, na.rm=TRUE)/sqrt(length(na.omit(Slope.5.to.15))))
Summary

SlopesGraph<-ggplot(Summary, aes(x=Genotype, y=mean, fill=factor(Temperature), group=factor(Temperature)))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(legend.position = "none", plot.title = element_text(face = "bold", size=18), axis.text.x=element_text(color="black", size=13), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=12), axis.title.y = element_text(color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  scale_y_continuous(expand=c(0,0), limits=c(0, 13))+
  geom_bar(stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Genotype", y="Exponential Phase Growth Rate (10,000 cells/mL/day)", fill="Temperature")+  #labels the x and y axes
  scale_fill_manual(values = wes_palette("Moonrise3"), labels=c("26°C", "30°C", "32°C"))+
  ggtitle("Linear Growth Rates day 5-15")+
  facet_wrap(  ~ Round)
SlopesGraph
SlopesGraph+ggsave("Graphs/Growth/SlopesDay5to15.png",width=10, height=5 )

#July growth rates day 1-15####
#Clear the environment
rm(list=ls())
#Load growth data
mydata<-read.csv("Data/ExponentialGrowthRates_Day1to15.csv")
mydata$Temperature<-as.factor(mydata$Temperature)
JulyData<-subset(mydata, Round == "July")
JulyData<-subset(JulyData, Slope != "NA")

#Make a model
#Genotype(fixed) and Temperature(fixed) on Slope (ie: growth rate, NOT PER CAPITA, problem?)
model1<-lm(Slope~Genotype*Temperature, data=JulyData)
qqp(resid(model1), "norm")
#Normal

TukeyHSD(aov(model1))

#Normal. Dope.
plot(model1)
anova(model1)
#Very significant interaction between temp and geno. Cool beans. 
summary(model1)

#Barplot of slopes
Summary <- JulyData %>%
  group_by(Genotype, Temperature) %>%
  summarize(mean=mean(Slope, na.rm=TRUE), SE=sd(Slope, na.rm=TRUE)/sqrt(length(na.omit(Slope))))
Summary

SlopesGraph<-ggplot(Summary, aes(x=Genotype, y=mean, fill=factor(Temperature), group=factor(Temperature)))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(plot.title = element_text(face = "bold", size=18), axis.text.x=element_text(color="black", size=13), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=12), axis.title.y = element_text(color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  scale_y_continuous(expand=c(0,0), limits=c(0, 10))+
  geom_bar(stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  labs(x="Genotype", y="Exponential Phase Growth Rate (10,000 cells/mL/day)", fill="Temperature")+  #labels the x and y axes
  scale_fill_manual(values = c("skyblue3", "darkgoldenrod2", "brown3"), labels=c("26°C", "30°C", "32°C"))+
  ggtitle("Linear Growth Rates day 1-15 (July only)")
SlopesGraph



#ANCOVA#####

#Clear the environment
rm(list=ls())
#Load growth data
mydata<-read.csv("GrowthDataCombined_r.csv")
mydata$Temp<-as.factor(mydata$Temp)
mydata$Flask<-as.factor(mydata$Flask)

#New data frame with just data between days 5-15
mydata2<-subset(mydata, Day > 4 & Day < 16)
View(mydata2)
#I need to remove all of the NA's. So CCMP2464 for MayJune and FLCass Flask 1.
noNAdata<-subset(mydata2, !is.na(Densityx10000))
#CCMP2464 for just one round is a little tricker to remove. 
toBeRemoved<-which(noNAdata$Genotype=="CCMP2464" & noNAdata$Round == "MayJune")
noNAdata2<-noNAdata[-toBeRemoved,]

#Starting with a very simple model, just to wrap my mind around all of the interactions
geno.model<-lm(Densityx10000 ~ Genotype*Day, data=noNAdata2)
qqp(resid(geno.model), "norm")
#Normal
plot(Densityx10000~Day, data=noNAdata2)
#I guess that's linear

Anova(geno.model, type="II")
#Sig relationship between geno and day. So the density changes with day differently depending on the genotype
#Graph it
predBF<-predict(geno.model) #Gets the predicted values from the regression lines in the ANCOVA
graph.data<-cbind(noNAdata2, predBF) #attaches those predictions to the dataset

library(ggplot2)
ggplot(data=graph.data, aes(Day, Densityx10000, color=Genotype)) +
  theme_bw()+
  theme(legend.title=element_text(colour="black", size=14), axis.text.x=element_text(face="bold", color="black", size=16), axis.text.y=element_text(face="bold", color="black", size=13), axis.title.x = element_text(color="black", size=18, face="bold"), axis.title.y = element_text(color="black", size=18, face="bold"),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_point() + geom_line(aes(y=predBF), size=1) + 
  labs(x="Day", y="Density x10,000", fill="Genotype") 

#Now add in temperature
geno.temp.model<-lm(Densityx10000~Genotype*Temp*Day, data=noNAdata2)
qqp(resid(geno.temp.model), "norm")
#Still normal
Anova(geno.temp.model, type="II")
#Again, everything is signficant.
#Graph it
predBF<-predict(geno.temp.model) #Gets the predicted values from the regression lines in the ANCOVA
graph.data<-cbind(noNAdata2, predBF) #attaches those predictions to the dataset

library(ggplot2)
ggplot(data=graph.data, aes(Day, Densityx10000, color=Genotype, shape=Temp)) +
  theme_bw()+
  theme(legend.title=element_text(colour="black", size=14), axis.text.x=element_text(face="bold", color="black", size=16), axis.text.y=element_text(face="bold", color="black", size=13), axis.title.x = element_text(color="black", size=18, face="bold"), axis.title.y = element_text(color="black", size=18, face="bold"),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_point() + geom_line(aes(y=predBF), size=1) + 
  labs(x="Day", y="Density x10,000", fill="Genotype", shape="Temp") 
#Useless because it's too busy. But whatever. 

#Now the full model
full.model<-lm(Densityx10000 ~ Genotype * Temp * Round * Day, data=noNAdata2)
qqp(resid(full.model), "norm")
#Hm, not very normal now

noNAdata2$logDensity<-log(noNAdata2$Densityx10000)
log.full.model<-lm(logDensity ~ Genotype * Round * Temp * Day, data=noNAdata2)
qqp(resid(log.full.model), "norm")
#that's better. 
#Rest of assumptions for logged data
plot(logDensity~Day, data=noNAdata2)
#Well, now there doesn't really look to be a relationship at all. 
#Run model
Anova(log.full.model, type="II")
#Almost everything is sig. 
#Round*Temp*Day -- the relationship between density and day varies between rounds and temp
#Geno*Round*Day --
#But not the four way. So round does not affect the interaction between Geno*Temp?
#Also, not Geno*Temp*Day -- 
summary(log.full.model)

#ANCOVA just July#####

#Clear the environment
rm(list=ls())
#Load growth data
mydata<-read.csv("GrowthDataCombined_r.csv")
mydata$Temp<-as.factor(mydata$Temp)

#Subsetting just July data and removing the spilled FLCass flask
JulyData<-subset(mydata, Round == "July")
JulyData<-subset(JulyData, Method != "NA")
View(JulyData)

#New data frame with just data between days 5-15
mydata2<-subset(JulyData, Day > 4 & Day < 16)
View(mydata2)

model1<-lm(Densityx10000 ~ Genotype * Temp * Day, data=mydata2)
#Make sure that the relationship with covariate is linear
plot(Densityx10000~Day, data=mydata2)
#looks linear to me

plot(model1)
qqp(resid(model1), "norm")
#Not very normal. 

mydata2$logDensity<-log(mydata2$Densityx10000)
model2<-lm(logDensity ~ Genotype * Temp * Day, data=mydata2)
logresid<-resid(model2)
qqp(resid(model2), "norm")
#Perfect. 

#Rest of assumptions for logged data
plot(model2)
plot(logDensity~Day, data=mydata2)
#Run model
anova(model2, type="II")#Won't run, but I need to run it as type II cuz it's unbalanced

#Graphing
predBF<-predict(model2) #Gets the predicted values from the regression lines in the ANCOVA
graph.data<-cbind(mydata3, predBF) #attaches those predictions to the dataset

library(ggplot2)
ggplot(data=graph.data, aes(logT, logBF, color=species)) +
  theme_bw()+
  theme(legend.title=element_text(colour="black", size=14), axis.text.x=element_text(face="bold", color="black", size=16), axis.text.y=element_text(face="bold", color="black", size=13), axis.title.x = element_text(color="black", size=18, face="bold"), axis.title.y = element_text(color="black", size=18, face="bold"),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_point() + geom_line(aes(y=predBF)) + 
  labs(x="Log Thickness", y="Log Break Force", fill="Species")+ #Fill=two colors for species
  scale_color_manual(values=c("steelblue", "salmon"), name="Species", labels=c("C. cripus", "M. stellatus")) #Edits legend for not a bargraph


#Are growthcruver values and Exp excel values correlated?####
mydata<-read.csv("Data/GrowthcurverData_r.csv")
mydata$Temp<-as.factor(mydata$Temp)
mydata$Flask<-as.factor(mydata$Flask)
View(mydata)
mydata2<-mydata[-c(13:24),]

plot(ExcelExp~r, data=mydata2)
lineplot<-ggplot(mydata2, aes(x=ExcelExp, y=r, color=Genotype, shape=Temp))+  #basic plot
  theme_bw()+ #Removes grey background
  theme(axis.text.x=element_text(color="black", size=12), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(face="bold", color="black", size=16), axis.title.y = element_text(face="bold", color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_point(size=3)+ 
  labs(x="Genotype", y="k", fill="Genotype", shape="Temp")+
  scale_fill_manual(values = wes_palette("Royal2"))
lineplot
lineplot+ggsave("Graphs/Correlation.growthcurver.ExcelExp.png",width=10, height=5)
