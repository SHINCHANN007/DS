
home_size <- c(1400, 1300, 1200, 950, 900, 1000, 1300, 850, 1100)  
selling_price <- c(70, 62, 65, 45, 40, 53, 68, 40, 55) 
data <- data.frame(home_size, selling_price)  

model <- lm(selling_price ~ home_size, data = data)  
summary(model)  

new_home <- data.frame(home_size = 1500) 
predicted_price <- predict(model, new_home)  
 
print(paste("Predicted Selling Price for 1500 sq ft home:", round(predicted_price, 2), "lakh Rs.")) 

plot(home_size, selling_price, pch = 16, col = "blue",main = "Home Size vs Selling Price",xlab = "Home Size (sq ft)", ylab = "Selling Price (lakh Rs.)") 
abline(model, col = "red", lwd = 2)

#Boxplot  
boxplot(home_size, selling_price, names = c("Home Size", "Selling Price"),main = "Box Plot of Home Size and 
Selling Price",col = c("lightblue", "lightgreen"), ylab = "Values")  

# Q-Q Plot  
qqnorm(residuals(model), main = "Q-Q Plot of Residuals") 
qqline(residuals(model), col = "red", lwd = 2)  

# Residuals vs Fitted 
plot(fitted(model), residuals(model), 
     main = "Residuals vs Fitted",
     xlab = "Fitted Values",
     ylab = "Residuals",pch = 16,col = "purple") 
abline(h = 0,col = "grey", lwd = 2) 