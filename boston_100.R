
library(car)

# we read the data
houses <- read.csv("Boston_100.csv", header=TRUE)

# we fit the model and display results (Questions 1, 2, 3 and 4)
model <- lm(tax ~ zn + indus + radial, data = houses)
anova(model)
summary(model)
vif(model)

# we save some constants that will be useful later
n <- length(houses[,1])
k <- length(model$coefficients) - 1

# Question 5
sort(abs(summary(model)$residuals/summary(model)$sigma), decreasing = TRUE)[1:5]

# Question 6
sort(hatvalues(model), decreasing = TRUE)[1:5]

# Question 7
plot(hatvalues(model), summary(model)$residuals/summary(model)$sigma,
     ylim=c(-4,4), pch = 16, xlab = "Leverage", ylab = "Standardized Residuals")
h_ii_cutoff <- 2*(k+1)/n
abline(v=h_ii_cutoff, lty = 3)
abline(h=-3, lty = 3)
abline(h=3, lty = 3)

# Question 8
sort(cooks.distance(model), decreasing = TRUE)[1:5]

# Question 9
model2 <- lm(tax ~ zn + indus + radial, data = houses[-11,])
anova(model2)
summary(model2)
vif(model2)