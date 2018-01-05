#Import respiratory.csv
respiratoryDS<-read.table("G:/MIS/Sem 2/Stat 526/Homework 3/respiratory.csv",header=T,sep=",",quote="\"")

#a) scattered plot between age and respiratory rate
plot(respiratoryDS$Age..Months.,respiratoryDS$Respiratory.Rate,xlab="Ages in Months",ylab="Respiratory Rate",pch=20,main="Variation in Respiratory Rate wrt Age in Months")

#b) fit a linear model to the data, place the fitted model on graph, then get the summary of the fitted model
fit.lm<-lm(Respiratory.Rate~Age..Months., data=respiratoryDS)
abline(fit.lm)
summary(fit.lm)
coef(fit.lm)

# c)???	Residual vs Prediction Plot (Checking Linearity and Constant Variance)
plot(fit.lm$fitted,fit.lm$resid,pch=20,xlab="Predicted",ylab="residuals")
abline(h=0)

hist(respiratoryDS$Age..Months., xlab="Ages in Months",main="Histogram of Ages in Months")
hist(respiratoryDS$Respiratory.Rate,xlab="Respiratpory Rate",main="Histogram of Respiratory Rate")
qqPlot(fit.lm$resid, ylab="Residuals")


#e)create new variable whichstores the log transformation of y var
lnRrate<-log(respiratoryDS$Respiratory.Rate)
#bind this new variable to the data set
respiratoryDS<-cbind(respiratoryDS,lnRrate)
#b) fit a log-linear model to the data, then get the summary of the fitted model
fit.loglm<-lm(lnRrate~Age..Months., data=respiratoryDS)
summary(fit.loglm)

#f)95% prediction interval for the respiratory rate for a child who is 5 months old.
predict(fit.loglm,newdata=data.frame(Age..Months.=5),interval="prediction")

#2 Importing datatsets
library(openintro)
data(textbooks)
head(textbooks)
# a) Log-log transformations
lnuclaNew<-log(textbooks$uclaNew)
lnamazaNew<-log(textbooks$amazNew)
#not to edit the original data set
textbookscopy<-textbooks
textbookscopy<-cbind(textbookscopy,lnuclaNew,lnamazaNew)
#fit a log-log model to the data, then get the summary of the fitted model
fit.loglogm<-lm(lnuclaNew~lnamazaNew, data=textbookscopy)
summary(fit.loglogm)


#b) Residual vs Prediction Plot (Checking Linearity and Constant Variance)
plot(fit.loglogm$fitted,fit.loglogm$resid,pch=20,xlab="Predicted",ylab="residuals")
abline(h=0)
# check normality
hist(fit.loglogm$resid,xlab="Residuals")
qqPlot(fit.loglogm$resid, ylab="Residuals")



#c)
plot(lnuclaNew~lnamazaNew, data=textbookscopy)
abline(fit.loglogm)
log(100)
predict(fit.loglogm,newdata=data.frame(lnamazaNew=4.60517))


#d) prediction intervat for ULCA book price
predict(fit.loglogm,newdata=data.frame(lnamazaNew=4.60517),interval="prediction")
