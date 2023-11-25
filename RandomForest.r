library(randomForest)

set.seed(432)
# Load MyData.RData
load("MyData.RData")

# Assuming train_1 is the loaded data
# Discard the id
train_1 <- train_1[, -1]
# Convert all other columns to numerics
train_1 <- apply(train_1, 2, as.numeric)

# Split the data: 80% for training and 20% for testing
train_size <- floor(0.8 * nrow(train_1))
train_indices <- sample(seq_len(nrow(train_1)), size = train_size)
train <- train_1[train_indices, ]
test <- train_1[-train_indices, ]

# a grid for mtry and nodesize
# mtry: 1, 5, 10, 15, 25, 50
# nodesize: 1, 5, 10, 15, 25, 50

mtrys <- c(1, 3, 5, 8, 10, 12, 15, 20, 25, 40,  50)
nodesizes <- c(1, 3, 5, 8, 10, 12, 15, 20, 25, 40,  50)

# grid for accuracy and accuracy with +- 0.5 error
accuracies <- rep(0, length(mtrys) * length(nodesizes))
accuracies_with_tolerace <- rep(0, length(mtrys) * length(nodesizes))

for (i in (1:length(mtrys))) {
    for (j in  (1:length(nodesizes))) {
        # Fit the random forest model on the training data
        # Using only 'train' for training
        rf <- randomForest(x = train[, -1], y = as.factor(train[, 1]), type = "classification", 
            importance = TRUE, mtry = mtrys[i], nodesize = nodesizes[j])

        # Predict the classes on the test data
        pred <- predict(rf, newdata = test[, -1])

        # Calculate the confusion matrix
        confusion_matrix <- table(Predicted = pred, Actual = as.factor(test[, 1]))
        # Calculate the accuracy
        accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
        # add the accuracy to the grid
        accuracies[i * (length(mtrys) - 1) + j] <- accuracy

        # allow for +- 0.5 error 
        sum_subdiag <- sum(confusion_matrix[cbind(2:nrow(confusion_matrix), 1:(ncol(confusion_matrix)-1))])
        sum_superdiag <- sum(confusion_matrix[cbind(1:(nrow(confusion_matrix)-1), 2:ncol(confusion_matrix))])

        accuracy_with_tolerace <- (sum(diag(confusion_matrix)) + sum_subdiag + sum_superdiag) / sum(confusion_matrix)
        accuracies_with_tolerace[i * (length(mtrys) - 1) + j] <- accuracy_with_tolerace
    }
}

# plot the accuracies and accuracies with +- 0.5 error
accuracy_mat = matrix(accuracies, nrow = length(mtrys), ncol = length(nodesizes))
accuracy_with_tolerace_mat = matrix(accuracies_with_tolerace, nrow = length(mtrys), ncol = length(nodesizes))

for (i in 1:length(mtrys)) {
    for (j in 1:length(nodesizes)) {
        accuracy_mat[i, j] <- accuracies[i * (length(mtrys) - 1) + j]
        accuracy_with_tolerace_mat[i, j] <- accuracies_with_tolerace[i * (length(mtrys) - 1) + j]    
    }
}

# the contour plot of the accuracies and accuracies with +- 0.5 error
png("contour_plot_accuracy.png", width = 800, height = 800)
filled.contour(mtrys, nodesizes, accuracy_mat, color.palette = heat.colors, 
 plot.title = title(main = "Accuracy", 
                                  xlab = "mtry", ylab = "nodesize"))
dev.off()
png("contour_plot_accuracy_with_tolerance.png", width = 800, height = 800)
filled.contour(mtrys, nodesizes, accuracy_with_tolerace_mat, color.palette = heat.colors, 
 plot.title = title(main = "Accuracy with tolerance", 
                                  xlab = "mtry", ylab = "nodesize"))
dev.off()

importance(rf)
