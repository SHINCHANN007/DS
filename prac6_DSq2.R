# Install and load required packages
#install.packages("dplyr")
#install.packages("caTools")
#install.packages("ROCR")
library(dplyr)
library(caTools)
library(ROCR)

# Examine the dataset
summary(mtcars)

# Split data into training (80%) and testing (20%) sets
set.seed(123) # For reproducibility
split <- sample.split(mtcars$vs, SplitRatio = 0.8)
train_reg <- subset(mtcars, split == "TRUE")
test_reg <- subset(mtcars, split == "FALSE")

# Train logistic regression model
# Predicting engine type (vs) using weight (wt) and displacement (disp)
logistic_model <- glm(vs ~ wt + disp, data = train_reg, family = "binomial")
summary(logistic_model)

# Make predictions on test set (THIS WAS MISSING)
predict_reg <- predict(logistic_model, test_reg, type = "response")

# Convert probabilities to binary predictions (0 or 1)
predict_reg <- ifelse(predict_reg > 0.5, 1, 0)

# Evaluate model with confusion matrix
conf_matrix <- table(test_reg$vs, predict_reg)
print(conf_matrix)

# Calculate accuracy
accuracy <- mean(predict_reg == test_reg$vs)
print(paste('Accuracy =', accuracy))

# ROC-AUC analysis
ROCPred <- prediction(predict(logistic_model, test_reg, type = "response"), test_reg$vs)
ROCPer <- performance(ROCPred, "tpr", "fpr")

# Calculate AUC
auc <- performance(ROCPred, "auc")@y.values[[1]]

# Plot ROC curve
plot(ROCPer, colorize = TRUE, 
     print.cutoffs.at = seq(0.1, 0.9, by = 0.1),
     main = "ROC CURVE")
abline(a = 0, b = 1) # Add diagonal reference line
legend("bottomright", legend = paste("AUC =", round(auc, 4)), 
       col = "blue", lty = 1)