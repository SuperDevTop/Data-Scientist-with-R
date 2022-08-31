
library("jsonlite")

data <- read.csv(file = 'beer_data_set.csv')
data <- subset(data, select = -Description)  # Remove column "Description"

# styles <- c("Altbier", "Barleywine - American", "Barleywine - English", "Bitter - English Extra Special / Strong Bitter (ESB)",
#             "Bitter - English", "Bière de Champagne / Bière Brut", "Blonde Ale - American",
#             "Blonde Ale - Belgian",
#             "Bock - Doppelbock",
#             "Bock - Eisbock",
#             "Bock - Maibock",
#             "Bock - Traditional",
#             "Bock - Weizenbock",
#             "Braggot",
#             "Brett Beer",
#             "")
# print(data)

arranged_data <- data[order(data$Ave_Rating), ]  # arrange data in ascending order according to ave_rating

# barplot(1:10, main="My Graph", xlab="The x-axis", ylab="The y axis")
# par(mfrow = c(2, 1))

# plot( arranged_data$AStringency, main="Astringency attribute", xlab = "Rating", ylab="AStringency", col="blue", cex=.08)
plot( arranged_data$ABV, main="ABV attribute", ylab="ABV", xlab = "Rating", col="red", cex=.08)
plot( arranged_data$Body, main="Body attribute", xlab = "Rating",ylab="Body", col="green", cex=.08)
plot( arranged_data$Alcohol, main="Alcohol attribute", xlab = "Rating", ylab="Alcohol", col="red", cex=.08)
plot( arranged_data$Bitter, main="Bitter attribute", xlab = "Rating", ylab="Bitter", col="blue", cex=.08)
plot( arranged_data$Sweet, main="Sweet attribute", xlab = "Rating", ylab="Sweet", col="green", cex=.08)
plot( arranged_data$Sour, main="Sour attribute", xlab = "Rating", ylab="Sour", col="red", cex=.08)
plot( arranged_data$Salty, main="Salty attribute", xlab = "Rating", ylab="Salty", col="blue", cex=.08)
plot( arranged_data$Fruits, main="Fruits attribute", xlab = "Rating", ylab="Fruits", col="green", cex=.08)
plot( arranged_data$Hoppy, main="Hoppy attribute", xlab = "Rating",ylab="Hoppy", col="red", cex=.08)
plot( arranged_data$Spices, main="Spices attribute", xlab = "Rating", ylab="Spices", col="blue", cex=.08)
plot( arranged_data$Malty, main="Malty attribute", xlab = "Rating", ylab="Malty", col="green", cex=.08)


# Two variables analyze
plot(arranged_data$ABV, arranged_data$Alcohol, xlab = "ABV", ylab="Alcohol", cex=.08)
plot(arranged_data$ABV, arranged_data$Fruits, xlab = "ABV", ylab="Fruits", cex=.08)
plot(arranged_data$ABV, arranged_data$Sour, xlab = "ABV", ylab="Sour", cex=.08)
plot(arranged_data$ABV, arranged_data$Spices, xlab = "ABV", ylab="Spices", cex=.08)
plot(arranged_data$Alcohol, arranged_data$Fruits, xlab = "Alcohol", ylab="Fruits", cex=.08)
plot(arranged_data$Alcohol, arranged_data$Sour, xlab = "Alcohol", ylab="Sour", cex=.08)
plot(arranged_data$Alcohol, arranged_data$Spices, xlab = "Alcohol", ylab="Spices", cex=.08)
plot(arranged_data$Fruits, arranged_data$Sour, xlab = "Fruits", ylab="Sour", cex=.08)
plot(arranged_data$Fruits, arranged_data$Spices, xlab = "Fruits", ylab="Spices", cex=.08)
plot(arranged_data$Sour, arranged_data$Spices, xlab = "Sour", ylab="Spices", cex=.08)

# K-means clustering
png(file="k_mean.png")
plot(head(arranged_data[, 7], n=20))
dev.off()
fit <- kmeans(head(arranged_data[, 7], n = 20), 5, nstart=25) 

plot(fit$cluster)
points(fit$centers, col = 1:5, pch = 8)

# print(fit$cluster)
# print(fit$centers)

# Linear Regression
x = arranged_data$Ave_Rating
y = arranged_data$ABV
linear_regression_model <- lm(y ~ x)
print(summary(linear_regression_model)) 

png(file = "linear_regression.png")  

plot(y, x, col = "red", main = "ABV and Ave Rating Regression", abline(lm(x~y)),cex = 0.3,pch = 16,xlab = "Ave Rating", ylab = "ABV")  
dev.off() 

# print(ncol(data)) # Count of cols
# print(nrow(data)) # Count of rows
# min_pro <- min(data$ABV) # Minimum value of ABV attribute  














