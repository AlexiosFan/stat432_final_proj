
library(caret)
library(gbm)

set.seed(432)
load("MyData.RData")

train_scores <- MyTrain_RF[, 1]
test_scores <- MyTest_RF[, 1]

train_features <- MyTrain_RF[, -1]
test_features <- MyTest_RF[, -1]

train_matrix <- data.matrix(train_features)
test_matrix <- data.matrix(test_features)

cv_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

gbm_grid <- expand.grid(interaction.depth = c(1, 3, 5),
                        n.trees = c(50, 100, 150),
                        shrinkage = c(0.01, 0.1),
                        n.minobsinnode = 10)

gbm_model <- train(x = train_matrix, y = train_scores,
                   method = "gbm",
                   trControl = cv_control,
                   verbose = FALSE,
                   tuneGrid = gbm_grid)

print(gbm_model)

gbm_predictions <- predict(gbm_model, newdata = test_matrix)

gbm_rmse <- sqrt(mean((gbm_predictions - test_scores)^2))
gbm_r2 <- 1 - sum((gbm_predictions - test_scores)^2) / sum((test_scores - mean(test_scores))^2)

cat("GBM Regression RMSE:", gbm_rmse, "\n")
cat("GBM Regression R-squared:", gbm_r2, "\n")