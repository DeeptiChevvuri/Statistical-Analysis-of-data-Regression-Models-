#1
wine<- read.table("C:/Users/Colin/Documents/Stat526_Anna/wine.csv", header=T, sep = ",", quote="\"")
mod1<-lm(Price~Rating,data=wine)
summary(mod1)
plot(mod1$fitted,mod1$resid, main= "Residual Plot", xlab= "Predicted", ylab= "Residuals", abline(h=0))
plot(wine$Rating,wine$Price,xlab="Rating",ylab="Price",main="Scatterplot of Rating vs. Price")
mod2<-lm(Price~Rating+I(Rating^2),data=wine)
summary(mod2)
qqPlot(mod2$resid, main= "QQ Plot", xlab= "Quantiles", ylab= "Residuals")
plot(mod2$fitted,mod2$resid, main= "Residual Plot", xlab= "Predicted", ylab= "Residuals", abline(h=0))

#If you want to put a fitted curve on top of your scatterplot
ratingvalues <- seq(85,96, 0.1)
p.price <- predict(mod1,data.frame(Rating=ratingvalues))
lines(ratingvalues, p.price, col = "darkgreen", lwd = 2)

#2
conv <- read.table("C:/Users/Colin/Documents/Stat526_Anna/convenience.csv", header=T, sep = ",", quote="\"")
plot_colors <- c("green","blue") 
plot_shapes <-c(2, 3) 
plot(conv$VolumeGallons, conv$SalesDollars, col=plot_colors[conv$Site], pch=plot_shapes[conv$Site], xlab="Gallons of Gas Sold", ylab="Convenience Store Sales", main="Convenience Store Sales vs. Gallons of Gas Sold") 
legend("topleft", c("Site 1", "Site 2"), col=plot_colors, pch=plot_shapes)
mod3<-lm(SalesDollars~VolumeGallons+factor(Site),data=conv)
summary(mod3)
mod4<-lm(SalesDollars~VolumeGallons+factor(Site)+VolumeGallons*factor(Site),data=conv)
summary(mod4)
