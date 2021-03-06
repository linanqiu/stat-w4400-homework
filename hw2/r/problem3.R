library(e1071)
library(rpart)

data = read.csv('./uspsdata/uspsdata.txt', header=FALSE, sep="\t")
colnames(data) = seq(1, 100)
labels = read.csv('./uspsdata/uspscl.txt', header=FALSE, sep="\t")

colnames(labels) = "labels"

data = as.matrix(data)
data = cbind(data, labels)
data = data.frame(data)
data$labels = as.factor(data$labels)

# split data into test and train
index = 1:nrow(data)
testindex = sample(index, trunc(length(index) / 5))
testset = data[testindex,]
trainset = data[-testindex,]

# train linear SVM with soft margin
library(ggplot2)
library(scales)
tuned_linear = tune.svm(labels ~ ., kernel = "linear", data = trainset, cost = 2^(-20:2))
ggplot(data=tuned_linear$performances) + geom_line(aes(x=cost, y=error)) + scale_x_continuous(trans=log2_trans(), breaks=trans_breaks("log2", function(x) 2^x), labels=trans_format("log2", math_format(2^.x)))

tuned_rbf = tune.svm(labels ~ ., data = trainset, cost = 2^(-20:20), gamma = 10^(-5:1))
ggplot(data=tuned_rbf$performances) + geom_line(aes(x=cost, y=error, colour=factor(gamma))) + scale_x_continuous(trans=log2_trans(), breaks=trans_breaks("log2", function(x) 2^x), labels=trans_format("log2", math_format(2^.x)))

linear_model = svm(labels ~ ., data=data, method="C-classification", kernel="linear", cost=tuned_linear$best.parameter$cost[1])
linear_predict = predict(linear_model, testset[, -ncol(data)])
classAgreement(table(pred = linear_predict, true = testset[, ncol(data)]))

rbf_model = svm(labels ~ ., data=data, method="C-classification", kernel="radial", cost=tuned_rbf$best.parameter$cost[1], gamma=tuned_rbf$best.parameter$gamma[1])
rbf_predict = predict(rbf_model, testset[, -ncol(data)])
classAgreement(table(pred = rbf_predict, true = testset[, ncol(data)]))
