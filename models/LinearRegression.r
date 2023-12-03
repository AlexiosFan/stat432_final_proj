library(glmnet)

set.seed(432)
# Load MyData.RData
load("MyData.RData")

# Assuming train_1 is the loaded data
# Discard the id
train <- t(apply(MyTrain_pca, 1, as.numeric))
test <- t(apply(MyTest_pca, 1, as.numeric))

# a vector for alpha from 0 to 1 with 0.01 increments
alphas <- seq(0, 1, 0.01)
r2s <- rep(0, length(alphas))

for (i in 1:length(alphas)) {
    # fit a ridge regression using lm.ridge
    simple_ridge <- cv.glmnet(x = data.matrix(train[, -1]), y = train[, 1], nfold = 10, alpha = alphas[i])
    # predict the values on the test data
    pred <- predict(simple_ridge, newx = test[, -1], s = "lambda.min")
    # calculate the MSE
    mse <- mean((pred - test[, 1])^2)
    # r square
    r2 <- 1 - mse / var(test[, 1])
    # add the r square to the vector
    r2s[i] <- r2
}

# plot the r square
png("elastic_net.png", width = 800, height = 800)
plot(alphas, r2s, type = "l", xlab = "alpha", ylab = "R square")
dev.off()

# tuning lambda
lambda = exp(seq(-5, 5, 0.05))
nsim = 100
r2sum = rep(0, length(lambda))
for (i in 1:nsim){
    ridge <- cv.glmnet(x = data.matrix(train[, -1]), y = train[, 1], nfold = 10, alpha = 0, lambda = lambda)
    pred <- predict(ridge, newx = test[, -1], lambda)
    for (j in 1:length(lambda)){
        mse <- mean((pred[, j] - test[, 1])^2)
        r2 <- 1 - mse / var(test[, 1])
        r2sum[j] <- r2sum[j] + r2
    }
}


# plot the r square
plot(lambda, r2sum / nsim, type = "l", xlab = "lambda", ylab = "R square")



png("lambda.png", width = 800, height = 800)
plot(lambda, r2sum / nsim, type = "l", xlab = "lambda", ylab = "R square")
dev.off()
