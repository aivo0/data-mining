# clear everything
rm(list=ls())
# load the data
load(file="C:/Users/Sven/Puu/Teaching-Files/DataMining/Practice2/kNN_data1.RData")

# find dimensions of the data
datasize=dim(x)

#NB! The code contains few errors made with a purpose. 
# YOUR GOAL IS TO CORRECT THE ERRORS!!!

# counters for two sets
k_train<-0
k_valid<-0
# create the arrays to store training and validation data
x_train<-matrix(,1000,3)
x_valid<-matrix(,500,3)

# split the data into two sets
for(i in seq(along=x[,1])){
  if (i%%3==0){
    k_valid<-k_valid+1
    x_valid[k_valid,]<-x[i,]
  }
  else{
    k_train<-k_train+1
    x_train[k_train,]<-x[i,]
  }
}


# lets plot 
#for (i in seq(along=x_train[,1])){
#  a<-switch(x_train[i,3], "red","green","blue")
#  b<-x_train[i,1:2]
#  plot(x_train[i,1],x_train[i,2],col=a,type="p",xlim=c(-10,20),ylim=c(-10,20))
#  par(new=TRUE)
#}

# perform classification
library(class)
knn_res<-(knn(x_train[,1:2], x_valid[,1:2], x_train[,3], k = 7,l=4, prob = TRUE))
predicted_classes<-as.numeric(knn_res)

# now we may try to plot the results
for(i in seq(along=x_valid[,1])){
  a<-switch(x_valid[i,3], "red","green","blue")
  b<-switch(predicted_classes[i], 'red','green','blue')
  plot(x_valid[i,1],x_valid[i,2],col=a,type="p",cex=2,xlim=c(-10,20),ylim=c(-10,20))
  par(new=TRUE)
  plot(x_valid[i,1],x_valid[i,2],col=b,type="p",pch=8,xlim=c(-10,20),ylim=c(-10,20))
  par(new=TRUE)
}