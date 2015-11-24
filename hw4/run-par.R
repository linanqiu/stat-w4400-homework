library(parallel)
cl = makeCluster(3)
clusterEvalQ(cl, source('em.R'))

K = c(3:5)
ms = parSapply(cl=cl, K, function(K) {
  H = matrix(readBin('histograms.bin', 'double', 640000), 40000, 16)
  MultinomialEM(H=H, K=K, tau=0.1)
})
matrix = as.matrix(ms)

for(k in K) {
  m_matrix = matrix(matrix[,k-K[1]+1], nrow=200, ncol=200)
  pdf(file=paste("plot", k, ".pdf", sep=""))
  image(m_matrix, col=gray.colors(k))
  dev.off()
}

stopCluster(cl)
