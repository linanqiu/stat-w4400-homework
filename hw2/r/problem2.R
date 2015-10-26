library(ggplot2)

source("./fakedata.R")

# problem 2.1 classify
classify = function(S, z) {
  y = sign(S %*% z)
  return(y)
}

# problem 2.2 perceptron training algorithm
alpha = function(k) {
  return(1/k)
}

cost = function(S, y, z) {
  class = classify(S, z)
  return(t(0.5 * abs(class - y)) %*% (S %*% z))
}

diff = function(f, y) {
  diff = f * y
  diff[diff == 1] = 0
  diff[diff == -1] = 1
  return(diff)
}

perceptrain = function(S, y) {
  z = runif(dim(S)[2], 0, 1)
  z_history = matrix(z, nrow=1, ncol=dim(S)[2])
  
  k = 1
  
  while(cost(S, y, z) != 0) {
    f = classify(S, z)
    print(z)
    z = z - alpha(k) * as.vector(t(diff(f, y) * (-y)) %*% S)
    print(z)
    k = k + 1
    z_history = rbind(z_history, z)
  }
  result = list(z = z, z_history = z_history)
  return(result)
}

result = perceptrain(data$S, data$y)
# sanity check
hyperplane = c(0, 1)
c = 1
data = fakedata(c(hyperplane, -c), 100)
classify(data$S, result$z) - classify(data$S, c(hyperplane, -c))

# problem 1.3
z = runif(3, 0, 1)
train = fakedata(z, 100)
train_results = perceptrain(train$S, train$y)

test = fakedata(z, 100)
test_classify = classify(test$S, train_results$z)
test_accuracy = test_classify - classify(test$S, z)
sum(test_accuracy == 0) / length(test_accuracy)

# problem 1.4
## test
intercept = - train_results$z[3] / train_results$z[2]
slope = - train_results$z[1] / train_results$z[2]
df_test = as.data.frame(test)
plot = ggplot() + geom_point(data = df_test, aes(S.1, S.2, colour=factor(y))) + geom_abline(slope = slope, intercept = intercept)
plot

## train
df_train = as.data.frame(train)
intercepts = - train_results$z_history[, 3] / train_results$z_history[, 2]
slopes = - train_results$z_history[, 1] / train_results$z_history[, 2]
history_colours = seq(1, length(slopes))
plot = ggplot() + geom_point(data = df_train, aes(S.1, S.2, colour=factor(y))) + geom_abline(slope = slopes, intercept = intercepts, alpha=history_colours/length(history_colours))
plot
