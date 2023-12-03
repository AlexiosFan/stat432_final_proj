library(randomForest)

set.seed(432)
# Load MyData.RData
load("MyData.RData")

head(MyTrain)
head(MyTrain_RF)
MyTrain <- t(apply(MyTrain_RF, 1, as.numeric))
MyTest <- t(apply(MyTest_RF, 1, as.numeric))


mtrys <- c(1, 3, 5, 8, 10, 12, 15, 20, 25, 40,  50)
nodesizes <- c(1, 3, 5, 8, 10, 12, 15, 20, 25, 40,  50)

# grid for accuracy and accuracy with +- 0.5 error
accuracies <- rep(0, length(mtrys) * length(nodesizes))
accuracies_with_tolerace <- rep(0, length(mtrys) * length(nodesizes))

for (i in (1:length(mtrys))) {
    for (j in  (1:length(nodesizes))) {
        # Fit the random forest model on the training data
        # Using only 'train' for training
        rf <- randomForest(x = MyTrain[, -1], y = as.factor(MyTrain[, 1]), type = "classification", 
            importance = TRUE, mtry = mtrys[i], nodesize = nodesizes[j])

        # Predict the classes on the test data
        pred <- predict(rf, newdata = MyTest[, -1])

        # Calculate the confusion matrix
        confusion_matrix <- table(Predicted = pred, Actual = as.factor(MyTest[, 1]))
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


