#(a)Import cleaningoriginal.csv
cleaningOriginal<-read.table("G:/MIS/Sem 2/Stat 526/Homework 2/CleaningOriginal.csv",header=T,sep=",",quote="")

# Relational Plot between number of occupied rooms and number of crew 
plot(cleaningOriginal$NumberOfRooms,cleaningOriginal$NumberOfCrews,main="Variation of number of Crew with the number of occupied rooms", xlab="Number of Occupied Rooms", ylab="Number of crew members", pch=20)

#(b) estimating the corelation coef
cor(cleaningOriginal$NumberOfRooms,cleaningOriginal$NumberOfCrews)

#(c) placing both the means on graph to predict the effect of occupied rooms=50 and crew members=10
abline(h=mean(cleaningOriginal$NumberOfCrews),v=mean(cleaningOriginal$NumberOfRooms),col=c("blue","red"),lty=2)
legend("bottomright",c("Mean of # of Crew","Mean of # Occ. Rooms"),lty=c(2,2),col=c("blue","red"))
points(55,10,col="red",pch=20)

#(d) fitting pour model
fit.lm<-lm(NumberOfCrews~NumberOfRooms,data=cleaningOriginal)

fit.lm

#(e)
abline(fit.lm)


#(f) error in prediction
fit.lm$resid[26]

#(j) predict the number of crew required for 35 occupied rooms
predict(fit.lm,newdata=data.frame(NumberOfRooms=35))


# .95 prediction interval
predict(fit.lm,newdata=data.frame(NumberOfRooms=35),interval="prediction", level=0.95)

#(m)xlim=c(10,150)
predict(fit.lm,newdata=data.frame(NumberOfRooms=35),interval="confidence", level=0.95)
predict(fit.lm,newdata=data.frame(NumberOfRooms=35),interval="confidence", level=0.75)



#n)95% confidence
predict(fit.lm,newdata=data.frame(NumberOfRooms=92),interval="confidence", level=0.95)


#o)

newsetofrooms<-seq(5,80,1)


mean.predict<-predict(fit.lm,newdata=data.frame(NumberOfRooms=newsetofrooms),interval="confidence")
#new observation or prediction
obs.predict<-predict(fit.lm,newdata=data.frame(NumberOfRooms=newsetofrooms),interval="prediction")
# putting these predictions on graph
lines(newsetofrooms,mean.predict[,2],col="red",lty=2)
lines(newsetofrooms,mean.predict[,3],col="red",lty=2)

lines(newsetofrooms,obs.predict[,2],col="blue",lty=3)
lines(newsetofrooms,obs.predict[,3],col="blue",lty=3)



legend("topleft",c("predictions","confidence"),lty=c(3,2),col=c("blue","red"))


#q)checking linearity and constant variance
plot(fit.lm$fitted,fit.lm$resid,pch=20,xlab="Predicted",ylab="residuals")
abline(h=0)
# check normality
hist(fit.lm$resid,xlab="Residuals")
qqPlot(fit.lm$resid, ylab="Residuals")
