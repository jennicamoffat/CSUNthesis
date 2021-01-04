#Symbiodinium microadriaticum growth curves
#Created by Jennica Moffat Oct 11, 2020
#Just growthcurvercode (stats and graphs) originally from Thesis_GrowthCurves_Cultures code
#load libraries
library(tidyverse)
library(car)
library(PNWColors)
library(reshape2)
library(growthcurver)
library(purrr)
library(lme4)
library(lmerTest)
library("lmtest")
library(emmeans)

#Clear the environment
rm(list=ls())
#
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

#Need to rename "Day" column to "time" for the package. 
names(MayJuneData)[1]<-"time"

# Now, we'll use Growthcurver to summarize the growth curve data for the entire
# plate using the default background correction method ("min").
gc_out <- SummarizeGrowthByPlate(MayJuneData, plot_fit = TRUE, plot_file="MayJuneGrowthValues.pdf")
head(gc_out)
View(gc_out)
#CCMP data are the only data that can't be fit, cuz it didn't grow.
#Separating sample ID into three columns
May.gc <- gc_out %>%
  separate(sample, into = c("Genotype", "Flask", "Temp"), sep = "_")

#Export to excel file
library("xlsx")
write.xlsx(May.gc, file = "GrowthcurverData.updated.xlsx",
           sheetName = "May", append = FALSE)

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

July.gc <- gc_out2 %>%
  separate(sample, into = c("Genotype", "Flask", "Temp"), sep = "_")

#Export to same excel file
write.xlsx(July.gc, file = "GrowthcurverData.updated.xlsx",
           sheetName = "July", append = TRUE)
#It's on two sheets, so I combined them into one sheet in excel and converted to CSV. Called "GrowthcurverData_r.csv"
#Redid with updated version of growth curver, nothing changed (phew). So the updated and not updated datasheets are exactly the same. 

#Sigma from growthcurver#####
rm(list=ls())
mydata<-read.csv("Data/GrowthcurverData_r.csv")
mydata$Temp<-as.factor(mydata$Temp)
mydata$Flask<-as.factor(mydata$Flask)

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

#Bargraphs of r #####
rm(list=ls())
mydata<-read.csv("Data/GrowthcurverData_r.csv")
mydata<-mydata%>%
  mutate_if(is.character,as.factor)
mydata$Temp<-as.factor(mydata$Temp)

#Change name of MayJune to just May
mydata<-mydata %>%
  mutate(Round = as.character(Round),
         Round = if_else(Round == 'MayJune', 'May', Round),
         Round = as.factor(Round))

#going to remove CCMP2464 altogether, because  I can't include in stats with Round (aliased coefficients=not equal replication)
lessdata<-mydata %>%
  filter(Genotype!="CCMP2464")%>%
  droplevels()

Summary2 <- lessdata %>%
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
  geom_bar(color="black", stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  scale_fill_manual(values=pal, labels = c("26°C", "30°C", "32°C"))+
  scale_y_continuous(expand=c(0,0), limits=c(0,1))+
  facet_wrap(  ~ Round)
rGraph.final
rGraph.final+ggsave("Graphs/FinalGraphs/culture_growth_bar.png", width=8, height=5)

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
  geom_bar(color="black", stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  scale_fill_manual(values=pal, labels = c("26°C", "30°C", "32°C"))+
  scale_y_continuous(expand=c(0,0), limits=c(0,1))+
  facet_wrap(  ~ Round, labeller = labeller(Round = 
                                              c("May" = "Round 1",
                                                "July" = "Round 2")))
rGraph.final.all
rGraph.final.all+ggsave("Graphs/FinalGraphs/culture_growth_CCMP2464.png", width=8, height=5)

#Boxplot of r ####
rm(list=ls())
mydata<-read.csv("Data/GrowthcurverData_r.csv")
mydata<-mydata%>%
  mutate_if(is.character,as.factor)
mydata$Temp<-as.factor(mydata$Temp)
#Change name of MayJune to just May
mydata<-mydata %>%
  mutate(Round = as.character(Round),
         Round = if_else(Round == 'MayJune', 'May', Round),
         Round = as.factor(Round))
#going to remove CCMP2464 altogether, because  I can't include in stats with Round (aliased coefficients=not equal replication)
lessdata<-mydata %>%
  filter(Genotype!="CCMP2464")%>%
  droplevels()
#Putting CCMP2464 back in for July
mydata3<-mydata[-c(13:24),]
summary(mydata3)
#Reordering so May comes first
mydata3$Round <- factor(mydata3$Round,levels = c("May", "July"))

boxplot.r<-mydata3%>%
  ggplot(aes(x=Genotype, y=r, fill=Temp))+
  geom_boxplot()+
  theme_bw()+
  geom_point(pch=21, position=position_jitterdodge(jitter.width=0.1), size=1)+
  theme(plot.title = element_text(face = "bold", size=16),
        axis.text.x=element_text(color="black", size=11, angle = 30, hjust=1), 
        axis.text.y=element_text(color="black", size=12), 
        axis.title.x = element_text(color="black", size=16), 
        axis.title.y = element_text(color="black", size=16),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_fill_manual(values = c("#ac8eab", "#f2cec7", "#c67b6f"), labels=c("26°C", "30°C","32°C"))+
  scale_x_discrete(name = "Symbiont Genotype") +
  scale_y_continuous(name = "Max growth rate (r)")+
  labs(fill="Temperature")+
  facet_wrap(  ~ Round, labeller = labeller(Round = 
                                              c("May" = "Round 1",
                                                "July" = "Round 2")))
boxplot.r+ggsave("Graphs/FinalGraphs/culture_growthrate_box.png", width=8, height=5)

#No jitter
boxplot.r.nojitter<-mydata3%>%
  ggplot(aes(x=Genotype, y=r, fill=Temp))+
  geom_boxplot()+
  theme_bw()+
  theme(plot.title = element_text(face = "bold", size=16),
        axis.text.x=element_text(color="black", size=11, angle=30, hjust=1), 
        axis.text.y=element_text(color="black", size=12), 
        axis.title.x = element_text(color="black", size=16), 
        axis.title.y = element_text(color="black", size=16),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_fill_manual(values = c("#ac8eab", "#f2cec7", "#c67b6f"), labels=c("26°C", "30°C","32°C"))+
  scale_x_discrete(name = "Symbiont Genotype") +
  scale_y_continuous(name = "Max growth rate (r)")+
  labs(fill="Temperature")+
  facet_wrap(  ~ Round, labeller = labeller(Round = 
                                              c("May" = "Round 1",
                                                "July" = "Round 2")))
boxplot.r.nojitter+ggsave("Graphs/FinalGraphs/culture_growthrate_box_nojitter.png", width=8, height=5)


#Stats for r####
rm(list=ls())
mydata<-read.csv("Data/GrowthcurverData_r.csv")
mydata$Temp<-as.factor(mydata$Temp)
mydata$Flask<-as.factor(mydata$Flask)
mydata3<-mydata[-c(13:24),]

model1<-lm(r~Genotype*Temp*Round, data=mydata3)
model1res<-resid(model1)
qqp(model1res, "norm")

mydata3$logr<-log(mydata3$r)

logr.model<-lm(logr~Round*Genotype*Temp, data=mydata3)
qqp(resid(logr.model), "norm")
#Pretty close

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

model2<-lm(logr~Genotype*Temp, data=lessdata)
anova(logNo2464.model, model2)
lrtest(logNo2464.model, model2)


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


#Stats for r separated by round####
rm(list=ls())
mydata<-read.csv("Data/GrowthcurverData_r.csv")
mydata$Temp<-as.factor(mydata$Temp)
mydata$Flask<-as.factor(mydata$Flask)

#Change name of MayJune to just May
mydata<-mydata %>%
  mutate(Round = as.character(Round),
         Round = if_else(Round == 'MayJune', 'May', Round),
         Round = as.factor(Round))

#MayJune (no CCMP2464)
mydata3<-mydata[-c(13:24),]
MayJuneData<-mydata3%>%
  filter(Round=="May")%>%
  droplevels()

May.model<-lm(r~Genotype*Temp, data=MayJuneData)
qqp(resid(May.model), "norm")
#Normal, yay!
plot(May.model)
Anova(May.model, type="III")
summary(May.model)

#emmeans
emm.may = emmeans(May.model, specs= pairwise~Genotype:Temp)
emm.may$emmeans
#emmean is mean from model 
#SE are calculated from the model as well

#Summary of raw data
SummaryMay <- MayJuneData %>%
  group_by(Genotype, Temp) %>%
  summarize(mean=mean(r, na.rm=TRUE), SE=sd(r, na.rm=TRUE)/sqrt(length(na.omit(r))))

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

#july summary
SummaryJuly <- JulyData %>%
  group_by(Genotype, Temp) %>%
  summarize(mean=mean(r, na.rm=TRUE), SE=sd(r, na.rm=TRUE)/sqrt(length(na.omit(r))))
#July model summary
#emmeans
emm.july = emmeans(loglogJuly.model, specs= pairwise~Genotype:Temp)
emm.july$emmeans
#emmean is mean from model (so double logged)
#SE are calculated from the model as well

#Bargraph of model data####
rm(list=ls())
mydata<-read.csv("Data/GrowthcurverData_r.csv")
mydata$Temp<-as.factor(mydata$Temp)
mydata$Flask<-as.factor(mydata$Flask)

#Change name of MayJune to just May
mydata<-mydata %>%
  mutate(Round = as.character(Round),
         Round = if_else(Round == 'MayJune', 'May', Round),
         Round = as.factor(Round))
#May (no CCMP2464)
data.noMay2464<-mydata[-c(13:24),]
#May data, just may, no CCMP2464
MayData<-data.noMay2464%>%
  filter(Round=="May")%>%
  droplevels()
#May model
May.model<-lm(r~Genotype*Temp, data=MayData)
#May emmeans
emm.may = emmeans(May.model, specs= pairwise~Genotype:Temp)
May.emmeans<-as.data.frame(emm.may$emmeans)
May.emmeans<-May.emmeans %>% add_column(Round="May")

pal<-c("#ac8eab", "#f2cec7", "#c67b6f")
growthrate.may.emmeans<-ggplot (May.emmeans, aes(x=Genotype, y=emmean, fill=factor(Temp), group=factor(Temp)))+  #basic plot
  theme_bw()+ #Removes grey background
  labs(x="Symbiont Genotype", y="Maximum growth rate (r)", fill="Temperature")+#labels the x and y axes
  theme(axis.text.x=element_text(color="black", size=11, angle = 30, hjust=1), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(color="black", size=16), axis.title.y = element_text(color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(color="black", stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=upper.CL, ymin=lower.CL), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  scale_fill_manual(values=pal, labels = c("26°C", "30°C", "32°C"))+
  scale_y_continuous(expand=c(0,0), limits=c(0,1))+
  facet_wrap(  ~ Round)
growthrate.may.emmeans+ggsave("Graphs/FinalGraphs/culture_growth_May_emmeans_bar.png", width=5.5, height=5)


#July data
JulyData<-mydata%>%
  filter(Round=="July")%>%
  droplevels()
#July model
JulyData$logr<-log(JulyData$r)
JulyData$loglogr<-log(JulyData$logr+3)
loglogJuly.model<-lm(loglogr~Genotype*Temp, data=JulyData)
#July emmeans
emm.july = emmeans(loglogJuly.model, specs= pairwise~Genotype:Temp)
July.emmeans<-as.data.frame(emm.july$emmeans)
July.emmeans<-July.emmeans %>% add_column(Round = "July")

#Means from data
Summary <- data.noMay2464 %>%
  group_by(Genotype, Temp, Round) %>%
  summarize(mean=mean(r, na.rm=TRUE))

#Joining datasets
MayJuly.emmeans<-full_join(May.emmeans, July.emmeans, copy = FALSE)
full.data<-full_join(MayJuly.emmeans, Summary, copy=FALSE)

graph.summary<-full.data%>%
  group_by(Genotype, Temp, Round)%>%
  summarize(mean=mean, upperCI=upper.CL, lowerCI=lower.CL, SE=SE)
graph.summary$Round <- factor(graph.summary$Round,levels = c("May", "July"))

pal<-c("#ac8eab", "#f2cec7", "#c67b6f")
growthrate.emmeans<-ggplot (graph.summary, aes(x=Genotype, y=mean, fill=factor(Temp), group=factor(Temp)))+  #basic plot
  theme_bw()+ #Removes grey background
  labs(x="Symbiont Genotype", y="Maximum growth rate (r)", fill="Temperature")+#labels the x and y axes
  theme(axis.text.x=element_text(color="black", size=11, angle = 30, hjust=1), axis.text.y=element_text(color="black", size=12), axis.title.x = element_text(color="black", size=16), axis.title.y = element_text(color="black", size=16),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_bar(color="black", stat="identity", position="dodge", size=0.6) + #determines the bar width
  geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), stat="identity", position=position_dodge(width=0.9), width=0.1)+  #adds error bars
  scale_fill_manual(values=pal, labels = c("26°C", "30°C", "32°C"))+
  scale_y_continuous(expand=c(0,0), limits=c(0,1))+
  facet_wrap(  ~ Round)
growthrate.emmeans

#Growth rate graphs####
rm(list=ls())
mydata<-read.csv("GrowthDataCombined_r.csv")
mydata$Temp<-as.factor(mydata$Temp)

#Change name of MayJune to just May
mydata<-mydata %>%
  mutate(Round = as.character(Round),
         Round = if_else(Round == 'MayJune', 'May', Round),
         Round = as.factor(Round))
#averaging count data
mean.counts<-mydata%>%
  group_by(Genotype, Temp, Round, Day)%>%
  summarize(avg=mean(Densityx10000, na.rm=TRUE), SE=sd(Densityx10000, na.rm=TRUE)/sqrt(length(na.omit(Densityx10000))))
mean.counts$upper<-mean.counts$avg+mean.counts$SE
mean.counts$lower<-mean.counts$avg-mean.counts$SE

#removing ccmp2464 may
May.means<-mean.counts%>%
  filter(Round=="May")
May.means.no2464<-May.means%>%
  filter(Genotype!="CCMP2464")

July.means<-mean.counts%>%
  filter(Round=="July")
#Joining back together
mean.counts.noMay2464<-full_join(May.means.no2464, July.means, copy = FALSE)
#Reordering so May is first in legend
mean.counts.noMay2464$Round <- factor(mean.counts.noMay2464$Round,levels = c("May", "July"))

pal<-c("#ac8eab", "#F7B8AC", "#c67b6f")
#SE ribbon
avg.growth.SEribbon<-ggplot(mean.counts.noMay2464, aes(x=Day, y=avg, color=Temp, shape=Round))+
  geom_line(size=1) +
  geom_point()+
  geom_ribbon(aes(x=Day, ymin=lower, ymax=upper), linetype=1, alpha=0.1)+
  xlab("Day")+
  ylab("Density (10,000 cells/mL)")+
  ggtitle("Growth Curves")+
  scale_x_continuous(breaks = c(0,10,20))+
  theme_minimal()+
  scale_color_manual(values=pal)+
  scale_shape_manual(values=c(1, 16))+
  theme(plot.title = element_text(hjust = 0.5, size = 14))+
  facet_wrap(~Genotype)
avg.growth.SEribbon+ggsave("Graphs/FinalGraphs/growth_curves_SEribbon.png", width=8, height=5)

#Just SE bars
avg.growth.SEbars<-ggplot(mean.counts.noMay2464, aes(x=Day, y=avg, color=Temp, shape=Round))+
  geom_line(size=0.75) +
  geom_point()+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2)+
  xlab("Day")+
  ylab("Density (10,000 cells/mL)")+
  ggtitle("Growth Curves")+
  scale_x_continuous(breaks = c(0,10,20))+
  theme_minimal()+
  scale_color_manual(values=pal)+
  scale_shape_manual(values=c(1, 16))+
  theme(plot.title = element_text(hjust = 0.5, size = 14))+
  facet_wrap(~Genotype)
avg.growth.SEbars+ggsave("Graphs/FinalGraphs/growth_curves_SEbars.png", width=8, height=5)

#No SE
avg.growth.noSE<-ggplot(mean.counts.noMay2464, aes(x=Day, y=avg, color=Temp, shape=Round))+
  geom_line(size=0.75) +
  geom_point()+
  xlab("Day")+
  ylab("Density (10,000 cells/mL)")+
  ggtitle("Growth Curves")+
  scale_x_continuous(breaks = c(0,10,20))+
  theme_minimal()+
  scale_color_manual(values=pal)+
  scale_shape_manual(values=c(1, 16))+
  theme(plot.title = element_text(hjust = 0.5, size = 14))+
  facet_wrap(~Genotype)
avg.growth.noSE+ggsave("Graphs/FinalGraphs/growth_curves_noSE.png", width=8, height=5)

#
#Bargraph of just July growthcurver (old graph formatting)####
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

