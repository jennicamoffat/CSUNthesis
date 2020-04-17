## RSP/PSN slope analysis
# Chelsea Brisson
# Sept 7, 2019

rsp1<-read.csv("/Users/ChelseaB/My Documents/THESIS PROJECT/EXP Native (Summer 2019)/PSN RSP/26 degrees/slopes.csv")

#data in csv should be: col 1 = times, col 2...i individual samples
# in my csv I had 22 individuals


attach(rsp1)


m<-array(rsp1[,2:23])
colnames(m)<-colnames(rsp1[,2:23])
t<-Time.Min.
m
t


##### OMFG I WROTE A WORKING CODE ##########

z<-for (i in 1:22) {
  afun<-function(i){
    x<-lm(m[,i]~t)
    xx<-x$coefficients
    return(xx[2]) 
  } 
  print(afun(i))
}


###### output as vector ########
output <- vector("double", ncol(m))
for (i in 1:22) {
  afun<-function(i){
    x<-lm(m[,i]~t)
    xx<-x$coefficients
    return(xx[2]) 
  } 
  output[[i]]<-print(afun(i))
}
output

output<-as.vector(output)

####### add output as slopes column ########
samples<-matrix(0, nrow = 23, ncol = 2)
samples[,1]<-colnames(rsp1)
samples<-samples[-c(1),]

samples[,2]<-output
samples
