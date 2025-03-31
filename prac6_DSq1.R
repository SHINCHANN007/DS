# Load dataset from CSV file
loan <- read.csv(file.choose(), header = TRUE, sep = ",")

# Examine data structure
head(loan)       # First 6 rows
summary(loan)    # Statistical summary
str(loan)        # Variable types and structure

# Convert AGE to categorical if needed
loan$AGE <- as.factor(loan$AGE)
str(loan)        # Verify conversion
names(loan)      # List all variables

# Full logistic regression model
model1 <- glm(DEFAULTER ~ ., family = binomial, data = loan)
summary(model1)
# Null model (intercept only)
null <- glm(DEFAULTER ~ 1, family = binomial, data = loan)

# Likelihood ratio test
anova(null, model1, test = "Chisq")

# Add predicted probabilities to dataset
loan$predprob <- round(fitted(model1), 2)
head(loan)      # View predictions

# Prepare for ROC analysis
pred <- predict(model1, loan, type = "response")
loan$DEFAULTER <- as.numeric(as.factor(loan$DEFAULTER)) - 1  # Convert to 0/1

# ROC Curve components
library(ROCR)
rocrpred <- prediction(pred, loan$DEFAULTER)
rocrperf <- performance(rocrpred, "tpr", "fpr")

# Plot ROC curve
plot(rocrperf, colorize = TRUE, 
     print.cutoffs.at = seq(0.1, 1, by = 0.1))

# Calculate AUC
auc <- performance(rocrpred, "auc")@y.values[[1]]
print(auc)

# Log-odds coefficients
print(coef(model1))

# Odds ratios (more interpretable)
print(exp(coef(model1)))
