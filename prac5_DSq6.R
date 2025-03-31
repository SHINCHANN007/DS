library(ggplot2)

# Load data (interactive file selection)
heart_data <- read.csv(file.choose(), header = TRUE)  
head(heart_data)  # Check the first few rows

# Fit a linear model
model <- lm(heart_disease ~ biking + smoking, data = heart_data)
summary(model)  # View model summary

# Predict heart disease for new data
new_data <- data.frame(biking = 50, smoking = 10)  # Example values
predicted_heart_disease <- predict(model, newdata = new_data)

# Print prediction
cat("Predicted Heart Disease: ", predicted_heart_disease, "\n")

ggplot(heart_data, aes(x = biking, y = heart_disease)) +
  geom_point(aes(color = heart_disease), size = 3) +
  labs(
    title = "Scatter Plot: Biking vs Heart Disease",
    x = "Biking",
    y = "Heart Disease"
  ) +
  scale_color_gradient(low = "blue", high = "red") +
  theme_minimal()
ggplot(heart_data, aes(x = smoking, y = heart_disease)) +
  geom_point(aes(color = heart_disease), size = 3) +
  labs(
    title = "Scatter Plot: Smoking vs Heart Disease",
    x = "Smoking",
    y = "Heart Disease"
  ) +
  scale_color_gradient(low = "blue", high = "red") +
  theme_minimal()

boxplot(
  heart_data$heart_disease ~ cut(heart_data$biking, breaks = 4),
  main = "Box Plot: Heart Disease by Biking Levels",
  xlab = "Biking Levels",
  ylab = "Heart Disease",
  col = c("lightblue", "lightgreen", "lightpink", "lightyellow"),
  names = c("Low", "Medium", "High", "Very High")
)

boxplot(
  heart_data$heart_disease ~ cut(heart_data$smoking, breaks = 4),
  main = "Box Plot: Heart Disease by Smoking Levels",
  xlab = "Smoking Levels",
  ylab = "Heart Disease",
  col = c("lightblue", "lightgreen", "lightpink", "lightyellow"),
  names = c("Low", "Medium", "High", "Very High")
)

qqnorm(model$residuals, main = "QQ Plot of Residuals")
qqline(model$residuals, col = "red")

ggplot(
  data.frame(Fitted = model$fitted.values, Residuals = model$residuals),
  aes(x = Fitted, y = Residuals)
) +
  geom_point(color = "darkgreen") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Residuals vs Fitted Values",
    x = "Fitted Values",
    y = "Residuals"
  ) +
  theme_minimal()