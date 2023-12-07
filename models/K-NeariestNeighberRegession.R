library(caret)
library(glmnet)

set.seed(432)
load("MyData.RData")

train_scores <- MyTrain_pca[, 1]
test_scores <- MyTest_pca[, 1]

train_features <- MyTrain_pca[, -1]
test_features <- MyTest_pca[, -1]

train_matrix <- data.matrix(train_features)
test_matrix <- data.matrix(test_features)

cv_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

tune_grid <- expand.grid(k = 1:20)

set.seed(432)
knn_reg <- train(x = train_matrix, y = train_scores,
                 method = "knn",
                 tuneGrid = tune_grid,
                 trControl = cv_control)

print(knn_reg)

knn_predictions <- predict(knn_reg, newx = test_matrix)

mse_knn <- mean((knn_predictions - test_scores)^2)
r2_knn <- 1 - mse_knn / var(test_scores)

png("knn_performance.png", width = 800, height = 800)
plot(knn_reg$results$k, knn_reg$results$RMSE, type = "b", pch = 19, xlab = "Number of Neighbors (k)", ylab = "RMSE")
dev.off()

cat("KNN Regression MSE:", mse_knn, "R-squared:", r2_knn, "\n")

#second tuning
train_scaled <- scale(train_matrix)
test_scaled <- scale(test_matrix)

knn_grid <- expand.grid(k = seq(1, 30, 1)) # Testing a wider range of k values
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3, search = "grid")

set.seed(432)
knn_tuned <- train(x = train_scaled, y = train_scores, method = "knn", tuneGrid = knn_grid, trControl = ctrl, 
                   preProcess = "scale", weights = "distance")

print(knn_tuned)

knn_predictions_test <- predict(knn_tuned, newx = test_matrix)

rmse_test <- sqrt(mean((knn_predictions_test - test_scores)^2))

r2_test <- 1 - (sum((knn_predictions_test - test_scores)^2) / sum((test_scores - mean(test_scores))^2))

mae_test <- mean(abs(knn_predictions_test - test_scores))


cat("Test RMSE:", rmse_test, "\n")
cat("Test R-squared:", r2_test, "\n")
cat("Test MAE:", mae_test, "\n")