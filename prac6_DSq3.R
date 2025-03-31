# Load the dataset 
data(Titanic) 
# Convert the table to a data frame 
data <- as.data.frame(Titanic) 
# Fit the logistic regression model 
model <- glm(Survived ~ Class + Sex + Age, family = binomial, data = data) 
# View the summary of the model 
summary(model)

# Install and load the required packages 
#install.packages("ROCR") 
library(ROCR) 
# Fit the logistic regression model 
model <- glm(Survived ~ Class + Sex + Age, family = binomial, data = data) 
# Make predictions on the dataset 
predictions <- predict(model, type = "response") 
# Create a prediction object for ROCR 
prediction_objects <- prediction(predictions, data$Survived) 

# Create an ROC curve object 
roc_object <- performance(prediction_objects, measure = "tpr", x.measure = "fpr") 
# Plot the ROC curve 
plot(roc_object, main = "ROC Curve", col = "blue", lwd = 2) 
# Add labels and a legend to the plot 
legend("bottomright", legend =  paste("AUC =", round(performance(prediction_objects, measure = "auc") @y.values[[1]], 2)), col = "blue", lwd = 2) 