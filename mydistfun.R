mydistfun<- function(element1,element2,metricf){
  # this function returns the distace between the element1 and element2
  # according to the metricf
  
  dimensions=length(element1)
  sqd<-matrix(,dimensions,1)
  #print(dimensions)
  #print(element1)
  if (metricf=="eucledean"){
  #eucledean
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
  
  
  return(dist)
}
  