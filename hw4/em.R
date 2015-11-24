calculate_phi = function(i, H, t, k) {
  return(exp(sum(H[i,] * log(t[k,]))))
}

calculate_a = function(i, c, k, phi) {
  value = c[k] * phi[i, k] / sum(c * phi[i,])
  if(is.nan(value)) {
    return(0)
  } else {
    return(value)
  }
}

MultinomialEM = function(H, K, tau) {
  H = H + 0.01
  n = nrow(H)
  centroids = sample(c(1:n), size=K)
  
  # normalized centroids
  t = t(apply(H[centroids,], 1, function(row) { row / sum(row) }))
  
  # assignment probabilities
  a = matrix(0, ncol=K, nrow=n)
  phi = matrix(0, ncol=K, nrow=n)
  
  # mixture weights
  c = rep(1/K, times=K)
  
  delta = 100
  
  while(delta > tau) {
    a_old = a
    for(k in 1:K) {
      
      # E-step
      phi[,k] = sapply(c(1:n), calculate_phi, H=H, t=t, k=k)
      a[,k] = sapply(c(1:n), calculate_a, c=c, k=k, phi=phi)
      #for(i in 1:n) {
      #  phi[i, k] = exp(sum(H[i,] * log(t[k,])))
      #  a[i, k] = c[k] * phi[i, k] / sum(c * phi[i,])
      #}
      #a[is.nan(a)] = 0
      
      #print(head(phi))
      #print(head(a))
            
      # M-step
      c[k] = sum(a[,k]) / n
      b_k = a[,k] %*% H
      t[k,] = b_k / sum(b_k)
    }
    delta = norm(a_old - a, "O")
    print(delta)
  }
  # hard assignment
  m = sapply(c(1:n), function(i) { which.max(a[i,])})
  return(m)
}


