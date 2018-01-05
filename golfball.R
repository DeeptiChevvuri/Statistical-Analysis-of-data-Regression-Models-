golfdata<- read.table("G:/MIS/Sem 2/Stat 526/Module 8/Homework 8/golfball.csv",header=T,sep=",",quote="\"")
#Comparing Means
bR<-boxplot(golfdata$number~golfdata$ball.type,main="number of drives needed to chip or crack each ball Versus Ball Type",ylab="number of drives",xlab="Ball type")
#fitting a model
mod1<-lm(golfdata$number~golfdata$ball.type,data=golfdata)
summary(mod1)
