
data("iris")

model <- lm(Sepal.Length~Sepal.Width, data=iris)
summary(model)

new_data <- data.frame(Sepal.Width = c(4.0))
predicted_SL <- predict(model,new_data)

print(paste("new sepal length",round(predicted_SL,2)))



plot(iris$Sepal.Width,iris$Sepal.Length,col="blue",pch=16)
abline(model,col="red",lwd=2)

plot(fitted(model), residuals(model), main = "Residuals vs Fitted Values",      
xlab = "Fitted Values", ylab = "Residuals", col = "purple", pch = 16) 
abline(h = 0, col = "blue", lwd = 2)

qqnorm(model$residuals, main = "Q-Q Plot of Residuals") 
qqline(model$residuals, col = "red", lwd = 2) 

boxplot(iris$Sepal.Length, iris$Sepal.Width,
names = c("Sepal Length", "Sepal Width"),main = "Box Plot of Sepal Dimensions", ylab = "Values",col = c("lightblue", "lightgreen")) 