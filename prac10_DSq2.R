# Install required packages (run once)
install.packages(c("caret", "party", "rpart", "tree", "e1071"))

# Load all libraries
library(caret)
library(party)
library(rpart)
library(tree)
library(e1071)

# Load and prepare data
mydata <- data.frame(mtcars)
mydata$cyl <- as.factor(mydata$cyl)
str(mydata)

# Train-Test Split
set.seed(123)
train_index <- createDataPartition(mydata$cyl, p = 0.7, list = FALSE)
train_data <- mydata[train_index, ]
test_data <- mydata[-train_index, ]

# 1. Basic Decision Tree Models
## rpart model
model_rpart <- rpart(cyl ~ mpg + hp + wt + disp, 
                     data = train_data, 
                     method = "class")
plot(model_rpart)
text(model_rpart, use.n = TRUE, all = TRUE, cex = 0.8)
pred_rpart <- predict(model_rpart, test_data, type = "class")
conf_matrix_rpart <- confusionMatrix(pred_rpart, test_data$cyl)
print(conf_matrix_rpart)

## tree model
model_tree <- tree(cyl ~ mpg + hp + wt + disp, 
                   data = train_data)
plot(model_tree)
text(model_tree, all = TRUE, cex = 0.6)
pred_tree <- predict(model_tree, test_data, type = "class")
conf_matrix_tree <- confusionMatrix(pred_tree, test_data$cyl)
print(conf_matrix_tree)

## ctree model
model_ctree <- ctree(cyl ~ mpg + hp + wt + disp, 
                     data = train_data)
plot(model_ctree)
pred_ctree <- predict(model_ctree, test_data)
conf_matrix_ctree <- confusionMatrix(pred_ctree, test_data$cyl)
print(conf_matrix_ctree)

# 2. Tuned Decision Tree Models
## Tuned rpart model (max depth = 3)
model_rpart_tuned <- rpart(cyl ~ mpg + hp + wt + disp, 
                           data = train_data,
                           method = "class",
                           control = rpart.control(maxdepth = 3))
pred_rpart_tuned <- predict(model_rpart_tuned, test_data, type = "class")
conf_matrix_rpart_tuned <- confusionMatrix(pred_rpart_tuned, test_data$cyl)
print(conf_matrix_rpart_tuned)

## Tuned tree model
model_tree_tuned <- tree(cyl ~ mpg + hp + wt + disp, 
                         data = train_data)
pred_tree_tuned <- predict(model_tree_tuned, test_data, type = "class")
conf_matrix_tree_tuned <- confusionMatrix(pred_tree_tuned, test_data$cyl)
print(conf_matrix_tree_tuned)

## Tuned ctree model
model_ctree_tuned <- ctree(cyl ~ mpg + hp + wt + disp, 
                           data = train_data)
pred_ctree_tuned <- predict(model_ctree_tuned, test_data)
conf_matrix_ctree_tuned <- confusionMatrix(pred_ctree_tuned, test_data$cyl)
print(conf_matrix_ctree_tuned)

# 3. Cross-Validated Model
set.seed(123)
train_control <- trainControl(method = "cv", number = 10)
cv_model_rpart <- train(cyl ~ mpg + hp + wt + disp, 
                        data = train_data, 
                        method = "rpart",
                        trControl = train_control)
pred_cv_rpart <- predict(cv_model_rpart, test_data)
conf_matrix_cv_rpart <- confusionMatrix(pred_cv_rpart, test_data$cyl)
print(conf_matrix_cv_rpart)

# 4. Results Comparison
accuracy_results <- data.frame(
  Model = c("rpart", "tree", "ctree", 
            "rpart (Tuned)", "tree (Tuned)", "ctree (Tuned)"),
  Accuracy = c(conf_matrix_rpart$overall["Accuracy"],
               conf_matrix_tree$overall["Accuracy"],
               conf_matrix_ctree$overall["Accuracy"],
               conf_matrix_rpart_tuned$overall["Accuracy"],
               conf_matrix_tree_tuned$overall["Accuracy"],
               conf_matrix_ctree_tuned$overall["Accuracy"])
)
print(accuracy_results)

# 5. Visualization
barplot(accuracy_results$Accuracy, 
        names.arg = accuracy_results$Model,
        col = rainbow(6), 
        main = "Decision Tree Model Accuracy (mtcars)",
        ylab = "Accuracy", 
        ylim = c(0, 1))