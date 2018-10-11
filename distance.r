library(stats)
library(lsa)

# clear everything
rm(list=ls())

euclidean.dist <- function(x) {
  sqrt(sum((x[1,] - x[2,]) ^ 2))
} 

manhattan.dist <- function(x) {
  sum(abs(x[1,] - x[2,]))
}

#https://en.wikipedia.org/wiki/Canberra_distance
canberra.dist <- function(x) {
  p = x[1,]
  q = x[2,]
  sum(abs(p-q)/(abs(p) + abs(q)))
}

# AKA chessboard distance
chebyshev.dist <- function(x) {
  max(abs(x[1,] - x[2,]))
}

minkowski.dist <- function(x) {
  if (length(x[1,])==1) return (manhattan.dist(x))
  else if (length(x[1,])==2) return (euclidean.dist(x))
  else return(chebyshev.dist(x)) 
  # Not a good result when x is between 2 and infinity
}

cosine.sim <- function(p) {
  X = p[1,]
  Y = p[2,]
  return( sum(X*Y)/sqrt(sum(X^2)*sum(Y^2)) )
}  

# https://en.wikipedia.org/wiki/Sample_mean_and_covariance
covariance <- function(p) {
  X = p[1,]
  Y = p[2,]
  # substract from each element the mean of the vector
  x.deviation = X - (sum(X) / length(X))
  y.deviation = Y - (sum(Y) / length(Y))
  sum(x.deviation * y.deviation) / (length(x) -1)
}


x <- c(1,2,3,8)
y <- c(2,3,4,1)
p <- rbind(x, y)
test <- p[2,2]

e <- euclidean.dist(p)
e.p <- stats::dist(p, method="euclidean")
m <- manhattan.dist(p)
m.p <- stats::dist(p, method="manhattan")
mink <- minkowski.dist(p)
mink.p <- stats::dist(p, method="minkowski")
c <- cosine.sim(p)
c.s <- lsa::cosine(p[1,], p[2,])
can <- canberra.dist(p)
can.p <- stats::dist(p, method="canberra")
mah.p <-  stats::mahalanobis(x, center, cov, inverted = FALSE)
cova <- covariance(p) 
cova.p <- cov(p[1,], p[2,])

