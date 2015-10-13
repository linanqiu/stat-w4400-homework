dist = function(v) {
  return(sqrt(sum(v^2)))
}

vh = matrix(c(1/sqrt(2), 1/sqrt(2)), nrow=2)
c = 1/(2 * sqrt(2))
x1 = matrix(c(-3, 0), nrow=2)
x2 = matrix(c(1/2, 1/2), nrow=2)

sign(crossprod(x1, vh) - c)
sign(crossprod(x2, vh) - c)
