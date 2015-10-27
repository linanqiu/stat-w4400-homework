# weak learner training routine
train = function(X, w, y) {
  dimensions = ncol(X)
  
  # determine optimal theta for each dimension
  thetas = matrix(nrow = dimensions)
  costs = matrix(nrow = dimensions)
  ms = matrix(nrow = dimensions)
  for(i in 1 : dimensions) {
    x = X[,i]
    optimal = optimal_theta(x, y)    
    thetas[i] = optimal$theta
    costs[i] = optimal$cost
    ms[i] = optimal$m
  }
  
  # select dimension for which cost term is minimal
  j = which.min(costs)
  theta = thetas[j]
  m = ms[j]
  return(list(j=j, theta=theta, m=m, weighted_cost=costs[j]))
}

weighted_split_cost = function(theta, m, x, w, y) {
  predicted = classify_column(theta=theta, x=x, m=m)
  incorrects = w * abs(predicted - y)/2
  return(sum(incorrects) / sum(w))
}

optimal_theta = function(x, y) {
  
  theta = runif(1, min(x), max(x))
  m = 1
  cost = split_cost(theta, m, x, y)
  
  if(cost > 0.5) {
    m = -1
    cost = split_cost(theta, m, x, y)
  }
  
  return(list(cost=cost, theta=theta, m=m))
  
  #   m_pos = optimize(split_cost, c(min(x), max(x)), m=1, x=x, y=y)
  #   m_neg = optimize(split_cost, c(min(x), max(x)), m=-1, x=x, y=y)
  #   
  #   cost_pos = m_pos$objective
  #   cost_neg = m_neg$objective
  #   
  #   if(cost_pos < cost_neg) {
  #     return(list(cost=cost_pos, theta=m_pos$minimum, m=1))
  #   } else {
  #     return(list(cost=cost_neg, theta=m_neg$minimum, m=-1))
  #   }
}

classify_column = function(theta, x, m) {
  predicted = m*(x-theta)
  predicted[predicted < 0] = -1
  predicted[predicted >= 0] = 1
  return(predicted)
}

split_cost = function(theta, m, x, y) {
  predicted = classify_column(theta=theta, x=x, m=m)
  incorrects = sum(abs(predicted - y)/2)
  return (incorrects / length(x))
}

# weak learner classification routine
classify = function(X, pars) {
  x = X[,pars$j]
  predicted = classify_column(theta=pars$theta, x=x, m=pars$m)
  return(predicted)
}

# aggregated classifier
agg_class = function(X, alphas, allPars, n=length(alpha)) {
  results = rep(0, nrow(X))
  for(i in 1 : n) {
    classify_results = classify(X, allPars[[i]])
    results = results + alphas[i] * classify_results
  }
  results[results > 0] = 1
  results[results < 0] = -1
  return(results)
}
