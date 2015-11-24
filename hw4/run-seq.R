source("em.R")

H = matrix(readBin('histograms.bin', 'double', 640000), 40000, 16)

K = c(3:5)

ms = sapply(K, function(K) {MultinomialEM(H=H, K=K, tau=0.1)})
matrix = as.matrix(ms)

for(k in K) {
  m_matrix = matrix(matrix[,k-K[1]+1], nrow=200, ncol=200)
  pdf(file=paste("plot", k, ".pdf", sep=""))
  image(m_matrix, col=gray.colors(k))
  dev.off()
}