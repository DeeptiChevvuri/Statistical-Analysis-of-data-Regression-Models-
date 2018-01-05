#Import respiratory.csv
mallSales<-read.table("G:/MIS/Sem 2/Stat 526/week4/Homework 4/mall_sales.csv",header=T,sep=",",quote="\"")
# Create new column filled with default colour
mallSales$Colour="black"
# Set new column values to appropriate colours
mallSales$Colour[mallSales$Competitors==1]="red"
mallSales$Colour[mallSales$Competitors==2]="blue"
mallSales$Colour[mallSales$Competitors==3]="green"
mallSales$Colour[mallSales$Competitors==4]="yellow"
mallSales$Colour[mallSales$Competitors==5]="purple"
mallSales$Colour[mallSales$Competitors==6]="orange"
# Plot all points at once, using newly generated colours
#a) scattered plot between age and respiratory rate
plot(mallSales$Income,mallSales$Sales,col=mallSales$Colour,xlab="median income in thousands of dollars",ylab="sales per sq/ft",pch=15,main="Variation in Respiratory Rate wrt Age in Months")
legend("bottomright",c("No C.","1 C.","2 C.","3 C.","4 C.","5 C.","6 C."),col=c("black","red","blue","green","yellow","purple","orange"),pch=15,ncol=2)

mod1<-lm(Sales~Income+Competitors,data=mallSales)
summary(mod1)
#c
plot(fitted(mod1),residuals(mod1),xlab="Predicted Sales",ylab="Residuals")
abline(h=0)
#Can also use Plot

plot(mod1)
#d
anova(mod1)
mnsq <- anova(mod1)[["Sum Sq"]] # A vector of length 3
MSE = anova(mod1)[3]# Mean square for error.
354918+66190

#h
mean(mallSales$Income)
mean(mallSales$Income)
predict(mod1,data.frame(Income=mean(mallSales$Income), Competitors=0),interval="confidence",level=.95)
-24.1650+ 1.998972*6.3899
-24.1650- 1.998972*6.3899

#n
predict(mod1,data.frame(Income=86, Competitors=5),interval="confidence")
predict(mod1,data.frame(Income=72, Competitors=2),interval="confidence")
predict(mod1,data.frame(Income=79, Competitors=3),interval="confidence")
predict(mod1,data.frame(Income=86, Competitors=5),interval="prediction")
predict(mod1,data.frame(Income=72, Competitors=2),interval="prediction")
predict(mod1,data.frame(Income=79, Competitors=3),interval="prediction")

#m
predict(mod1,data.frame(Income=86, Competitors=5))
predict(mod1,data.frame(Income=72, Competitors=2))
predict(mod1,data.frame(Income=79, Competitors=3))



#2 e
abs(qt(c(.025, .975), df=29))
abs(qt(0.05/2, 96))

3.934+1.984984*0.1259
3.934-1.984984*0.1259
abs(qt(c(.025, .975), df=62))  
