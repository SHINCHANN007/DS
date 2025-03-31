data(mtcars)  
model_mtcars <- lm(mpg ~ hp, data = mtcars)  
summary(model_mtcars) 
new_data <- data.frame(hp = c(100, 150, 200))  
predicted_mpg <- predict(model_mtcars, new_data)  
print(predicted_mpg)  

plot(mtcars$hp, mtcars$mpg, main = "Horsepower vs MPG", xlab = "Horsepower", ylab =  "MPG", pch = 16, col = "blue") 
abline(model_mtcars, col = "red") 


plot(model_mtcars$fitted.values, model_mtcars$residuals, main = "Residuals vs Fitted  
Model", xlab = "Fitted Values", ylab = "Residuals", pch = 16, col = "red") 
abline(h = 0, col = "blue") 

qqnorm(model_mtcars$residuals, main = "QQ Plot of Residuals")  
qqline(model_mtcars$residuals, col = "red")  

boxplot(mtcars$mpg, mtcars$hp, names = c("MPG", "Horsepower"), 
        main = "Box Plot of MPG and Horsepower",  ylab = "Values", col = c("lightblue", "lightgreen"))  