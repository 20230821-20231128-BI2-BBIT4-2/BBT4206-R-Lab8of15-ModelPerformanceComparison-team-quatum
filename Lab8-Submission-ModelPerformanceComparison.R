# STEP 1. Install and Load the Required Packages ----
## mlbench ----
if (require("mlbench")) {
  require("mlbench")
} else {
  install.packages("mlbench", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## caret ----
if (require("caret")) {
  require("caret")
} else {
  install.packages("caret", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## kernlab ----
if (require("kernlab")) {
  require("kernlab")
} else {
  install.packages("kernlab", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## randomForest ----
if (require("randomForest")) {
  require("randomForest")
} else {
  install.packages("randomForest", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

data("BostonHousing")


##Train the Models ----

train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

### Linear Regression 
set.seed(7)
housing_model_lm <- train(medv ~ ., data = BostonHousing,
                          method = "lm", trControl = train_control)

### CART ----
set.seed(7)
housing_model_cart <- train(medv ~ ., data = BostonHousing,
                            method = "rpart", trControl = train_control)

### KNN ----
set.seed(7)
housing_model_knn <- train(medv ~ ., data = BostonHousing,
                           method = "knn", trControl = train_control)

### SVM ----
set.seed(7)
housing_model_svm <- train(medv ~ ., data = BostonHousing,
                           method = "svmRadial", trControl = train_control)

### Random Forest ----
set.seed(7)
housing_model_rf <- train(medv ~ ., data = BostonHousing,
                          method = "rf", trControl = train_control)

## 3.b. Call the `resamples` Function ----
results <- resamples(list(LM = housing_model_lm, CART = housing_model_cart,
                          KNN = housing_model_knn, SVM = housing_model_svm,
                          RF = housing_model_rf))

# STEP 4. Display the Results ----

## 1. Table Summary ----
summary(results)

## 2. Box and Whisker Plot ----
scales <- list(x = list(relation = "free"), y = list(relation = "free"))
bwplot(results, scales = scales)

## 3. Dot Plots ----
scales <- list(x = list(relation = "free"), y = list(relation = "free"))
dotplot(results, scales = scales)

## 4. Scatter Plot Matrix ----
splom(results)

## 5. Pairwise xyPlots ----
xyplot(results, models = c("LDA", "SVM"))

# or
xyplot(results, models = c("SVM", "CART"))

## 6. Statistical Significance Tests ----
diffs <- diff(results)
summary(diffs)
