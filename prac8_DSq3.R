# =============================================
# PCA and PCR on USArrests Dataset
# =============================================

# 1. LOAD AND INSPECT DATA
data(USArrests)          # Load dataset
head(USArrests)          # Show first 6 rows
summary(USArrests)       # Statistical summary

# 2. PRINCIPAL COMPONENT ANALYSIS (PCA)
mypr_USArrests <- prcomp(USArrests, scale=TRUE)  # PCA with scaling

# 3. VISUALIZE ORIGINAL VS SCALED DATA
plot(USArrests$Murder, USArrests$Rape, main = "Original Data")  # Raw data plot
plot(scale(USArrests$Murder), scale(USArrests$Rape), main = "Scaled Data")  # Standardized plot

# 4. EXAMINE PCA RESULTS
mypr_USArrests            # Show PCA rotation matrix
summary(mypr_USArrests)   # Variance explained by components
plot(mypr_USArrests, type = "l")  # Scree plot of variances

# 5. PCA BI-PLOT VISUALIZATION
biplot(mypr_USArrests, scale=0)  # Shows variables and states

# 6. INSPECT PCA STRUCTURE
str(mypr_USArrests)       # View PCA object structure
mypr_USArrests$x          # View principal component scores

# 7. CREATE ENHANCED DATASET WITH PCs
USArrests2 <- cbind(USArrests, mypr_USArrests$x[,1:2])  # Add first 2 PCs
head(USArrests2)          # View augmented dataset

# 8. CHECK CORRELATIONS
cor(USArrests, USArrests2[,5:6])  # Correlate original vars with PCs

# 9. PRINCIPAL COMPONENT REGRESSION (PCR)
library(pls)  # Load PLS/PCR package
pcmodel_USArrests <- pcr(Murder~., ncomp=3, data=USArrests, scale=TRUE)  # Fit PCR model

# 10. MAKE PREDICTIONS
USArrests$pred <- predict(pcmodel_USArrests, USArrests, ncomp=2)  # Predict with 2 PCs
head(USArrests)  # View actual vs predicted murder rates

# 11. MODEL EVALUATION (ADDED)
# Calculate prediction error
RMSE <- sqrt(mean((USArrests$Murder - USArrests$pred)^2))
print(paste("RMSE:", round(RMSE, 3)))

# Calculate R-squared
rsq <- cor(USArrests$Murder, USArrests$pred)^2
print(paste("R-squared:", round(rsq, 3)))