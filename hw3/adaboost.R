source('./stump.R')

labels = read.csv('./uspsdata/uspscl.txt', header=FALSE, sep='\t')
data = read.csv('./uspsdata/uspsdata.txt', header=FALSE, sep='\t')
# labels = as.factor(as.matrix(labels))
data = as.matrix(data)

index = 1:nrow(data)
testindex = 1:(nrow(data)/5)
testdata = data[testindex,]
traindata = data[-testindex,]
testlabels = labels[testindex,]
trainlabels = labels[-testindex,]

adaboost = function(X, y, B, K=5) {
  w = rep(1/nrow(X), nrow(X))
  classifiers = list(rep(list(), B))
  alphas = vector(length=B)
  
  errors_train = vector(length=B)
  
  interval = trunc(nrow(X)/K)
  
  print(paste('Interval ', interval))
  
  for(b in 1:B) {
    print(paste('Iteration ', b))
    classifiers_crossfold = list(rep(list(), K))
    errs_crossfold = vector(length=K)
    for(k in 1:K) {
      Xvalid = X[k:(k+interval),]
      Xtrain = X[-(k:(k+interval)),]
      yvalid = y[k:(k+interval)]
      ytrain = y[-(k:(k+interval))]
      wvalid = w[k:(k+interval)]
      wtrain = w[-(k:(k+interval))]
      
      # fit classifer
      pars_crossfold = train(Xtrain, wtrain, ytrain)
      classifiers_crossfold[[k]] = pars_crossfold
      
      predicted_crossfold = classify(Xvalid, pars_crossfold)
      incorrects_crossfold = unlist(abs(predicted_crossfold - yvalid))
      err_crossfold = (sum(incorrects_crossfold)/length(incorrects_crossfold))
      errs_crossfold[k] = err_crossfold
    }
    
    par_select = which.min(errs_crossfold)
    pars = classifiers_crossfold[[par_select]]
    classifiers[[b]] = pars
    
    # find error
    predicted = classify(X, pars)
    incorrects = abs(predicted - y)/2
    err = (sum(w*incorrects)/sum(w))
    
    # alpha
    alpha = log((1-err)/err)
    alphas[b] = alpha
    
    errors_train[b] = evaluate(b, X, y, alphas[1:b], classifiers)
    
    
    # update w
    w = w * exp(alpha * incorrects)
  }
  return(list(allPars=classifiers, alphas=alphas, errors_train=errors_train))
}

evaluate = function(i, data, labels, alphas, allPars) {
  predicted = agg_class(data, alphas, allPars, i)
  results = abs(predicted - labels)/2
  error = sum(results) / length(results)
  return(error)
}

B = 1000
c = adaboost(traindata, trainlabels, B)
results = sapply(1:B, evaluate, data=testdata, labels=testlabels, alphas=c$alphas, allPars=c$allPars)

library(ggplot2)
library(reshape)
df = list(index=1:B, errors_train=c$errors_train, errors_test=results)
df = as.data.frame(df)
melted = melt(df, id=c('index'))
plot = ggplot(data=subset(melted, index<500)) + geom_line(aes(x=index, y=value, color=factor(variable))) + labs(title="Errors", x="Iterations", y="Errors")
plot
