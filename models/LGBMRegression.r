library(lightgbm)

set.seed(432)
# Load MyData.RData
load("MyData.RData")


# Convert all other columns to numerics
MyTrain <- t(apply(MyTrain_RF, 1, as.numeric))
MyTest <- t(apply(MyTest_RF, 1, as.numeric))


dtrain = lgb.Dataset(MyTrain[, -1], label = MyTrain[, 1])
dtest = lgb.Dataset.create.valid(dtrain, MyTest[, -1], label = MyTest[, 1])
 
# validataion data
valids = list(test = dtest)

# a 3 x 3 x 3 grid for nrounds, learning rate and num leaves
nrounds = c(10, 20, 30)
learning_rate = c(0.1, 0.01, 0.001)
num_leaves = c(10, 20, 30)

rsquare = rep(0, 27)

for (i in (1:length(nrounds))) {
  for (j in (1:length(learning_rate))) {
    for (k in (1:length(num_leaves))) {
      # define parameters
      params = list(
      objective = "regression"
      , metric = "l2"
      , min_data = 1L
      , nrounds = nrounds[i]
      , learning_rate = learning_rate[j]
      , num_leaves = num_leaves[k]
      )

      # train model 
      model = lgb.train(
      params = params
      , data = dtrain
      , valids = valids
      )

      pred <- predict(model, MyTest[, -1])

      mse <- mean((pred - MyTest[, 1])^2)
      r2 <- 1 - mse / var(MyTest[, 1])
      rsquare[(i - 1) * 9 + (j - 1) * 3 + k] <- r2
    }
  }
}

# plot the rsquare vector
png("rquare_lightbgm.png", width = 800, height = 800)
plot(rsquare, xlab = "(nrounds, learning_rate, num_leaves)", ylab = "r2", type = "h")
dev.off()

