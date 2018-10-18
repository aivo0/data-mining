mydistfun<- function(element1,element2,metricf){
  # this function returns the distace between the element1 and element2
  # according to the metricf
  
  dimensions=length(element1)
  sqd<-matrix(,dimensions,1)
  #print(dimensions)
  #print(element1)
  if (metricf=="euclidean"){
  #euclidean
  for(i in seq(along=element1)){
    
    sqd[i]<-(element1[i]-element2[i])^2
  }
  dist<-sqrt(colSums(sqd))
  
  }
  if (metricf=="manhattan"){
  #manhattan
  for(i in seq(along=element1)){
    sqd[i]<-abs(element1[i]-element2[i])
  }
  dist<-colSums(sqd)
  
  }
  
  if(metricf=="Chebyshev"){
    for(i in seq(along=element1)){
      sqd[i]<-abs(element1[i]-element2[i])
    }
    dist<-max(sqd)
  }
  
  if(metricf=="Canberra"){
    dist <- canberra.dist(element1, element2)
  }
  
  if(metricf=="cosine"){
    dist <- cosine.sim(element1, element2)
  }
  return(dist)
}

euclidean.dist <- function(x) {
  if (class(x) == "matrix") {
    sqrt(sum((x[1,] - x[2,]) ^ 2))
  }
  else sqrt(sum(x ^ 2))
}

manhattan.dist <- function(element1, element2) {
  sum(abs(element1 - element2))
}

#https://en.wikipedia.org/wiki/Canberra_distance
canberra.dist <- function(p, q) {
  sum(abs(p-q)/(abs(p) + abs(q)))
}

# AKA chessboard distance
chebyshev.dist <- function(x) {
  max(abs(x[1,] - x[2,]))
}

# https://en.wikipedia.org/wiki/Mahalanobis_distance
# C is the covariance matrix. 
# solve helps handle cases where matrix multiplication can result in vector or matrix
mahalanobis.dist <- function(element1, element2, c){
    # Using solve for inversing and matrix multiplication https://stackoverflow.com/questions/11995832/inverse-of-matrix-in-r
    distance <- sqrt(t(element1-element2) %*% solve(c) %*%(element1-element2))
    return(distance)
}

# Minkowski distance depends on the p value. It returns Manhattan and Eucledian distance for 
# values 1 or 2, respectively. If p is Inf, meaning infinite, Chebyshev distance is returned.
# For all other cases Minkowski formula is used.
minkowski.dist <- function(element1, element2, p) {
  if (p==1) return (mydistfun(element1, element2,"manhattan"))
  else if (p==2) return (mydistfun(element1, element2, "euclidean"))
  else if (p==Inf) return(mydistfun(element1, element2, "Chebyshev")) 
  # formula from https://en.wikipedia.org/wiki/Minkowski_distance
  else return ( (sum((abs(element1 - element2))^p))^(1/p) )
}

cosine.sim <- function(p, q) {
  return( sum(p*q)/sqrt(sum(p^2)*sum(q^2)) )
} 

covariance <- function(p) {
  X = p[1,]
  Y = p[2,]
  # translate X so as to get the result to have a mean of 0
  x.no.mean = X - (sum(X) / length(X))
  y.no.mean = Y - (sum(Y) / length(Y))
  x.no.mean * y.no.mean / (length(x) -1)
}
