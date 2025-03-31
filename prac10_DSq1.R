# Install required packages (run once)
install.packages(c("rpart", "rpart.plot", "tree", "party", "caret", "e1071"))

# Load all libraries
library(rpart)
library(rpart.plot)
library(tree)
library(party)
library(caret)
library(e1071)

# Load and prepare data
mydata <- data.frame(iris)
attach(mydata)

# 1. Basic Decision Tree Models (without train-test split)
## rpart model
model_rpart <- rpart(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
                     data = mydata, method = "class")
rpart.plot(model_rpart, type = 3, extra = 101, fallen.leaves = TRUE)

## tree model
model_tree <- tree(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
                   data = mydata)
plot(model_tree)
text(model_tree, all = TRUE, cex = 0.6)

## ctree model
model_ctree <- ctree(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
                     data = mydata)
plot(model_ctree)

# 2. Models with modified parameters
## tree with control parameters
model_tree_controlled <- tree(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
                              data = mydata,
                              control = tree.control(nobs = nrow(mydata), mincut = 10))
plot(model_tree_controlled)
text(model_tree_controlled, all = TRUE, cex = 0.6)

## ctree with control parameters
model_ctree_controlled <- ctree(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
                                data = mydata,
                                controls = ctree_control(maxdepth = 2))
plot(model_ctree_controlled)

# 3. Train-Test Split and Model Evaluation
set.seed(123)
train_index <- createDataPartition(mydata$Species, p = 0.7, list = FALSE)
train_data <- mydata[train_index, ]
test_data <- mydata[-train_index, ]

## rpart model evaluation
model_rpart_train <- rpart(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
                           data = train_data, method = "class")
pred_rpart <- predict(model_rpart_train, test_data, type = "class")
conf_matrix_rpart <- confusionMatrix(pred_rpart, test_data$Species)

## tree model evaluation
model_tree_train <- tree(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
                         data = train_data)
pred_tree <- predict(model_tree_train, test_data, type = "class")
conf_matrix_tree <- confusionMatrix(pred_tree, test_data$Species)

## ctree model evaluation
model_ctree_train <- ctree(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
                           data = train_data)
pred_ctree <- predict(model_ctree_train, test_data)
conf_matrix_ctree <- confusionMatrix(pred_ctree, test_data$Species)

# 4. Results Comparison
## Print confusion matrices
print(conf_matrix_rpart)
print(conf_matrix_tree)
print(conf_matrix_ctree)

## Create accuracy comparison
acc_rpart <- conf_matrix_rpart$overall["Accuracy"]
acc_tree <- conf_matrix_tree$overall["Accuracy"]
acc_ctree <- conf_matrix_ctree$overall["Accuracy"]

acc_res <- data.frame(
  Model = c("rpart", "tree", "ctree"),
  Accuracy = c(acc_rpart, acc_tree, acc_ctree)
)

print(acc_res)

## Visualize accuracy comparison
barplot(acc_res$Accuracy,
        names.arg = acc_res$Model,
        col = c("red", "blue", "green"),
        main = "Decision Tree Model Accuracy",
        ylab = "Accuracy",
        ylim = c(0, 1))