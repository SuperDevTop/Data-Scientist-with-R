x <- c(141, 134, 178, 156, 108, 116, 119, 143, 162, 130)  
y <- c(62, 85, 56, 21, 47, 17, 76, 92, 62, 58)  
relationship_model<- lm(y~x)  
# print(summary(relationship_model)) 
z <- data <- read.csv(file = 'test.csv')
# print(z[, 8])
z <- subset(z, select = -Description) 
z <- z[order(z$Ave_Rating), ]  # arrange data in ascending order according to ave_rating
plot(z[, 7])
fit <- kmeans(z[, 7], 5, nstart=25) 
print(fit$cluster)
print(z)

# Giving a name to the chart file.  
# png(file = "linear_regression.png")  
# Plotting the chart.  
# plot(y,x,col = "red",main = "Height and Weight Regression",abline(lm(x~y)),cex = 1.3,pch = 16,xlab = "Weight in Kg",ylab = "Height in cm")  
# Saving the file.  
# dev.off() 
