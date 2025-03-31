# Load data (replace with actual path)
Student <- read.csv("path/to/your/file.csv", header = TRUE, sep = ",")

# Exploratory Data Analysis
str(Student)  # Check structure
summary(Student)  # Summary statistics

# Filter students with Tmarks > 600
High_marks <- subset(Student, Tmarks > 600)
print(High_marks)

# Filter SYCS students with Tmarks > 600 and select first two columns
Filter_data <- subset(Student, Tmarks > 600 & class == "SYCS")
result <- Filter_data[, c("Rollno", "Name")]  # Use column names for clarity
print(result)

# Sort by Tmarks (ascending)
sorted_data <- Student[order(Student$Tmarks), ]

# Check for missing values in Tmarks
missing_marks <- Student[is.na(Student$Tmarks), ]
if (nrow(missing_marks) == 0) {
  print("No missing values in Tmarks.")
} else {
  print(missing_marks)
}

# Plotting
library(ggplot2)

# Scatter plot: Study Hours vs. Total Marks
ggplot(Student, aes(x = study_hours, y = Tmarks)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # Add regression line
  labs(title = "Study Hours vs. Total Marks", x = "Study Hours", y = "Total Marks") +
  theme_minimal()

# Boxplot of Total Marks (grouped by class)
ggplot(Student, aes(x = class, y = Tmarks)) +
  geom_boxplot() +
  labs(title = "Distribution of Total Marks by Class", y = "Total Marks") +
  theme_minimal()
