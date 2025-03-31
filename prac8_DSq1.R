data("iris")  # Load built-in dataset
head(iris)    # View first 6 rows
summary(iris) # Statistical summary
str(iris)     # Check structure

# Perform PCA (scale=TRUE for standardization)
mypr <- prcomp(iris[,-5], scale=TRUE)

# View PCA results
print(mypr)       # Shows rotation matrix
summary(mypr)     # Variance explained

# Scree plot (variance by component)
plot(mypr, type="l", main="Scree Plot")

# Biplot (observations + variables)
biplot(mypr, scale=0, cex=0.7)

# Plot PC scores
plot(mypr$x[,1], mypr$x[,2], 
     col=iris$Species,
     pch=16,
     xlab="PC1", ylab="PC2")
legend("topright", legend=levels(iris$Species),
       col=1:3, pch=16)

# Add first two PCs to original data
iris2 <- cbind(iris, mypr$x[,1:2])
head(iris2)

# Correlation between original vars and PCs
cor(iris[,-5], iris2[,6:7])

library(pls)

# Convert Species to numeric for modeling
iris$Species_num <- as.numeric(iris$Species)

# Fit PCR model (3 components)
pcmodel <- pcr(Sepal.Length ~ Species_num + Sepal.Width + 
                 Petal.Length + Petal.Width,
               ncomp=3, 
               data=iris,
               scale=TRUE,
               validation="CV")

# Model summary
summary(pcmodel)

# Cross-validation plot
validationplot(pcmodel, legendpos="topright")

# Predict using 2 principal components
iris$pred <- predict(pcmodel, ncomp=2, newdata=iris)[,,1]

# View predictions vs actual
head(iris[,c("Sepal.Length","pred")])

# Calculate RMSE
rmse <- sqrt(mean((iris$Sepal.Length - iris$pred)^2))
print(paste("RMSE:", round(rmse,3)))

# Regression coefficients
coef(pcmodel)

# Variance explained
explvar(pcmodel)

# Score plot
plot(pcmodel, plottype="scores", comps=1:2)