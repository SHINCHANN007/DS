# =============================================
# PCA and PCR on mtcars dataset - Clean Version
# =============================================

# 1. LOAD AND INSPECT DATA
data(mtcars)          # Load dataset
head(mtcars)          # Show first 6 rows
summary(mtcars)       # Statistical summary

# 2. PRINCIPAL COMPONENT ANALYSIS (PCA)
mypr_mtcars <- prcomp(mtcars, scale=TRUE)  # PCA with scaling

# 3. VISUALIZE ORIGINAL VS SCALED DATA
plot(mtcars$mpg, mtcars$hp, main = "Original Data")  # Raw data plot
plot(scale(mtcars$mpg), scale(mtcars$hp), main = "Scaled Data")  # Standardized plot

# 4. EXAMINE PCA RESULTS
mypr_mtcars            # Show PCA rotation matrix
summary(mypr_mtcars)   # Variance explained by components
plot(mypr_mtcars, type = "l")  # Scree plot of variances

# 5. PCA BI-PLOT VISUALIZATION
biplot(mypr_mtcars, scale=0)  # Shows variables and observations

# 6. INSPECT PCA STRUCTURE
str(mypr_mtcars)       # View PCA object structure
mypr_mtcars$x          # View principal component scores

# 7. CREATE ENHANCED DATASET WITH PCs
mtcars2 <- cbind(mtcars, mypr_mtcars$x[,1:2])  # Add first 2 PCs
head(mtcars2)          # View augmented dataset

# 8. CHECK CORRELATIONS
cor(mtcars[,-1], mtcars2[, 12:13])  # Correlate original vars with PCs

# 9. PRINCIPAL COMPONENT REGRESSION (PCR)
library(pls)  # Load PLS/PCR package
pcmodel_mtcars <- pcr(mpg~., ncomp=3, data=mtcars, scale=TRUE)  # Fit PCR model

# 10. MAKE PREDICTIONS
mtcars$pred <- predict(pcmodel_mtcars, mtcars, ncomp=2)  # Predict with 2 PCs
head(mtcars)  # View actual vs predicted