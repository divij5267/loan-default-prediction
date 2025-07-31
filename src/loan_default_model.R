loan <- read.csv("accepted_2007_to_2018Q4.csv")
View(loan)
# This sets the seed for R's random number generator to ensure reproducibility.
set.seed(123) 

#It randomly selects 1/100* the number of rows
part_size <- nrow(loan) / 100
sample_indices <- sample(1:nrow(loan), size = part_size)

# retain only the randomly selected rows
loan <- loan[sample_indices, ]

nrow(loan)

#This removes columns that contain more than 50% missing values
loan <- loan[, colSums(is.na(loan)) < 0.5 * nrow(loan)]

#all the numeric columns in the loan dataset
numeric_columns <- sapply(loan, is.numeric) 

#replaces any missing values (NA) in the numeric columns with the median value of that column
loan[, numeric_columns] <- lapply(loan[, numeric_columns], function(x) {
  ifelse(is.na(x), median(x, na.rm = TRUE), x)
})

# identifies all the categorical (factor) columns in the loan dataset
categorical_columns <- sapply(loan, is.factor)  
loan[, categorical_columns] <- lapply(loan[, categorical_columns], function(x) {
  ifelse(is.na(x), as.character(stats::mode(x)), x)
})

#remove all rows where the loan_status column contains missing values (NA)
loan <- loan[!is.na(loan$loan_status), ]


# Assign 1 for defaulted and 0 for not defaulted
loan$defaulted <- ifelse(loan$loan_status %in% c("Charged Off", "Default", "Does not meet the credit policy. Status: Charged Off"), 1, 0)


#drop unessary columns
loan <- loan[, !names(loan) %in% c("pymnt_plan", "url","desc","zip_code","verification_status_joint")]

####################################
#screen out 20 variables according from the correlation 


# Select only numeric variables, including the target variable "defaulted".
numeric_columns <- sapply(loan, is.numeric)
numeric_data <- loan[, numeric_columns]

# Compute the correlation matrix, excluding missing values.
cor_matrix <- cor(numeric_data, use = "complete.obs")

# 
head(cor_matrix)

# Extract variables correlated with "defaulted" and sort by absolute value.
cor_with_defaulted <- cor_matrix["defaulted", ]
# 排除目标变量自身的相关性，并按绝对值排序
cor_with_defaulted <- sort(abs(cor_with_defaulted), decreasing = TRUE)

# Select the top 20 most correlated variables, excluding "defaulted" itself.
top_20_vars <- names(cor_with_defaulted)[2:21]  
print(top_20_vars)

top_20_vars <- c("annual_inc", "fico_range_high", "fico_range_low", "recoveries", "total_pymnt",
                 "total_rec_prncp", "out_prncp", "last_pymnt_amnt", "bc_util", "bc_open_to_buy",
                 "inq_last_6mths", "percent_bc_gt_75", "num_tl_op_past_12m", "total_rec_late_fee",
                 "int_rate", "out_prncp_inv", "collection_recovery_fee", "acc_open_past_24mths","loan_amnt")

# install
if (!"car" %in% installed.packages()) {
  install.packages("car")
}


library(car)

# install
if (!"corrplot" %in% installed.packages()) {
  install.packages("corrplot")
}


library(corrplot)


# Ensure the target variable "defaulted" is included in the dataset.
selected_columns <- c(top_20_vars, "defaulted")
filtered_data <- loan[, selected_columns]
a<-filtered_data

# 2. Compute and visualize the correlation matrix.
cor_matrix <- cor(filtered_data, use = "complete.obs")
corrplot(cor_matrix, method = "color", addCoef.col = "black", tl.cex = 0.7)

# 3. Build a linear model to compute VIF.

lm_model <- lm(defaulted ~ ., data = filtered_data)

# 4. calculate VIF
vif_values <- vif(lm_model)
print(vif_values)

# 5. Remove variables with VIF > 10.
threshold <- 10
high_vif_vars <- names(vif_values[vif_values > threshold])

cat("remove：", high_vif_vars, "\n")

# 6. retain VIF that is smaller than 10 的变量
filtered_data_reduced <- filtered_data[, !(names(filtered_data) %in% high_vif_vars)]

# 7. Check the new VIF values to ensure multicollinearity has been reduced.
lm_model_reduced <- lm(defaulted ~ ., data = filtered_data_reduced)
vif_values_reduced <- vif(lm_model_reduced)
print(vif_values_reduced)

cat("left variables：", names(filtered_data_reduced), "\n")


top_vars <- c("annual_inc", "last_pymnt_amnt", "bc_util", "bc_open_to_buy",
              "inq_last_6mths", "percent_bc_gt_75", "num_tl_op_past_12m", "total_rec_late_fee",
              "int_rate", "acc_open_past_24mths","loan_amnt")


selected_columns <- c(top_vars, "defaulted")

# create a new one containing all of them
final_loan <- loan[, selected_columns]
View(final_loan)


####################################
### Random Forest
library(ggplot2)
library(lattice)
library(caret)

# set random seeds
set.seed(123)

# Split the data into training and test sets
trainIndex <- createDataPartition(final_loan$defaulted, p = 0.8, list = FALSE)
trainData <- final_loan[trainIndex, ]
testData <- final_loan[-trainIndex, ]

# install
if (!"randomForest" %in% installed.packages()) {
  install.packages("randomForest")
}
library(randomForest)

# random forest model
rf_model <- randomForest(as.factor(defaulted) ~ ., data = trainData, importance = TRUE)

print(rf_model)

summary(rf_model)


#### ROC
# intall
if (!"pROC" %in% installed.packages()) {
  install.packages("pROC")
}
library(pROC)

# Extract the default probabilities (probability of Default = 1) from the prediction results.
predictions_prob <- predictions_rf_prob[, 2]  

# ROC curve
roc_curve <- roc(testData$defaulted, predictions_prob)

# print AUC
cat("AUC:", auc(roc_curve), "\n")

# draw ROC curve
plot(
  roc_curve,
  col = "blue",
  lwd = 2,
  main = "ROC Curve for Random Forest Model"
)

# add abline
abline(a = 0, b = 1, lty = 2, col = "gray")


# print optimal information
optimal_coords <- coords(roc_curve, "best", ret = c("threshold", "sensitivity", "specificity"))
cat("Optimal Threshold Details:\n")
print(optimal_coords)

# Mark the optimal point on the ROC curve
points(
  coords(roc_curve, "best", ret = c("1-specificity", "sensitivity")),
  col = "red", pch = 19
)




# Perform probability predictions using the test set
predictions_rf_prob <- predict(rf_model, newdata = testData, type = "prob")
predictions_rf_prob
# View the predicted default probabilities
head(predictions_rf_prob)

# Make binary classifications based on the new threshold (0.145)
predicted_classes_rf <- ifelse(predictions_rf_prob[, 2] > 0.145, 1, 0)
predicted_classes_rf
# Convert the prediction results to a factor type to ensure consistency with the target variable
predicted_classes_rf <- factor(predicted_classes_rf, levels = c(0, 1))

# Calculate the confusion matrix and model accuracy
confusion_matrix <- confusionMatrix(predicted_classes_rf, as.factor(testData$defaulted))

print(confusion_matrix)


# Calculate feature importance.
importance_values <- importance(rf_model)  # 使用随机森林模型中的 importance 函数
print(importance_values)  # 打印特征重要性得分

# Visualize feature importance: Mean Decrease Accuracy and Mean Decrease Gini.
par(mfrow = c(1, 2))  # 将图像分为两列显示


varImpPlot(rf_model, type = 1, main = "Mean Decrease Accuracy")

varImpPlot(rf_model, type = 2, main = "Mean Decrease Gini")


#################################
#lasso
trainData$defaulted <- as.factor(trainData$defaulted)

# Prepare independent variables (feature matrix)
x <- model.matrix(defaulted ~ ., data = trainData)[, -1]  # 去掉截距
y <- trainData$defaulted  # 目标变量（因子）


set.seed(123)

# Perform Lasso regression and use cross-validation to select the optimal lambda value.
install.packages("glmnet")
library(glmnet)
lasso_model <- cv.glmnet(x, y, family = "binomial", alpha = 1)

# best lambda
best_lambda <- lasso_model$lambda.min
print(best_lambda)

# "Plot the relationship between lambda values and error for the Lasso model
plot(lasso_model)

# Extract features with non-zero coefficients.
lasso_coeff <- coef(lasso_model, s = "lambda.min")
important_vars <- rownames(lasso_coeff)[lasso_coeff[, 1] != 0]  # 筛选出非零系数的特征
important_vars <- important_vars[important_vars != "(Intercept)"]  # 去掉截距
print(important_vars)

# Build a logistic regression model using the selected features
selected_formula <- as.formula(paste("defaulted ~", paste(important_vars, collapse = " + ")))
log_model <- glm(selected_formula, data = trainData, family = "binomial")

summary(log_model)


#################################################### 
### 10-fold validation part for random forest and lasso, and the model of CART is after the part of lasso

#random forest

# Set the cross-validation parameters."
control <- trainControl(method = "cv", number = 10)

# Use the train() function to train the random forest model with 10-fold cross-validation
set.seed(123)
rf_cv_model <- train(
  as.factor(defaulted) ~ .,
  data = final_loan,
  method = "rf",
  trControl = control
)


print(rf_cv_model)




#lasso 10-fold

library(caret)
library(randomForest)

# Set the 10-fold cross-validation control
control_rf <- trainControl(method = "cv", number = 10)

# Train the Random Forest model and perform cross-validation.
set.seed(123)
rf_cv_model <- train(
  as.factor(defaulted) ~ ., 
  data = final_loan, 
  method = "rf", 
  trControl = control_rf
)

print(rf_cv_model)

cat("Random Forest 10-Fold CV Accuracy:", max(rf_cv_model$results$Accuracy), "\n")

# Ensure the target variable is of factor type.
trainData$defaulted <- as.factor(trainData$defaulted)


x <- model.matrix(defaulted ~ ., data = trainData)[, -1]
y <- trainData$defaulted

set.seed(123)

# Lasso，choose 10-fold CV get the best lambda
lasso_model <- cv.glmnet(x, y, family = "binomial", alpha = 1, nfolds = 10)

# print lambda 
best_lambda <- lasso_model$lambda.min
cat("Lasso Best Lambda:", best_lambda, "\n")

# Extract the features with non-zero coefficients
lasso_coeff <- coef(lasso_model, s = "lambda.min")
important_vars <- rownames(lasso_coeff)[lasso_coeff[, 1] != 0]
important_vars <- important_vars[important_vars != "(Intercept)"]

# Build a logistic regression model using the filtered features
selected_formula <- as.formula(paste("defaulted ~", paste(important_vars, collapse = " + ")))
log_model <- glm(selected_formula, data = trainData, family = "binomial")


summary(log_model)

# Make predictions on the test set and calculate the accuracy.
pred <- predict(log_model, newdata = testData, type = "response")
pred_class <- ifelse(pred > 0.5, 1, 0)
confusion_matrix <- confusionMatrix(factor(pred_class), as.factor(testData$defaulted))
print(confusion_matrix)
cat("Lasso Logistic Regression Accuracy:", confusion_matrix$overall["Accuracy"], "\n")


#CART

library(rpart)       
library(rpart.plot) 


final_loan$defaulted <- as.factor(final_loan$defaulted)

# use rpart() to build CART model
set.seed(123)  
cart_model <- rpart(
  defaulted ~ .,       
  data = final_loan,   
  method = "class",     
  control = rpart.control(cp = 0.01)  
)


print(cart_model)

# visualize CART 
rpart.plot(cart_model)


# CART 10-fold
library(caret)
library(rpart)
library(rpart.plot)  


final_loan$defaulted <- as.factor(final_loan$defaulted)
levels(final_loan$defaulted)

# if the class names do not meet R's variable name requirements, we rename them
levels(final_loan$defaulted) <- make.names(levels(final_loan$defaulted))

print(levels(final_loan$defaulted))


set.seed(123)

# Create control parameters for 10-fold cross-validation
control <- trainControl(
  method = "cv",   
  number = 10,      # 10 fold
  savePredictions = "final", 
  classProbs = TRUE            
)

# train CART model，and do 10-Fold Cross-Validation
cart_model <- train(
  defaulted ~ .,               
  data = final_loan,             
  method = "rpart",              
  trControl = control,           
  metric = "Accuracy",           
  tuneLength = 10                
)

print(cart_model)


rpart.plot(cart_model$finalModel)

# print best accuracy
best_accuracy <- max(cart_model$results$Accuracy)
cat("CART model 10-Fold CV accuracy：", round(best_accuracy * 100, 2), "%\n")



#profit for client

# 1. calculate default_loss = loan_amnt - recoveries + collection_recovery_fee
a$default_loss <- a$loan_amnt - a$recoveries + a$collection_recovery_fee

# 2. Use the trained random forest model to predict default probabilities for dataset a
predictions_rf_prob <- predict(rf_model, newdata = a, type = "prob")

# 3. Add the predicted probabilities to the dataset.
a$no_default_prob <- predictions_rf_prob[, 1]  # not defaulted
a$default_prob <- predictions_rf_prob[, 2]     # defaulted

# 4. calculate profit for each client
a$profit <- a$no_default_prob * (a$loan_amnt * a$int_rate / 100) - 
  a$default_prob * a$default_loss

# 5. View the profit information for the first few customers
head(a[, c("loan_amnt", "int_rate", "default_loss", "no_default_prob", 
           "default_prob", "profit")])
export_data<-a[, c("loan_amnt", "int_rate", "default_loss", "no_default_prob", 
                   "default_prob", "profit")]
export_data

# export csv
write.csv(export_data, "final_profits.csv", row.names = FALSE)


# content
head(export_data)
