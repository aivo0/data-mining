# clustering and different distance functions. 
# clear everything
rm(list=ls())

library(shotGroups)
# load the data
load(file="C:/Users/Sven/Puu/Teaching-Files/Datamining/kNN_data1.RData")
# NB!!!!!!!!   variable x will appear in the workspace. Ignore third column

#split the data in proportion 70/30 for training and validation purposes.
sample_size <- floor(0.7 * nrow(x))

set.seed(123) # seed is necessary to make it reproducible
train_ind <- sample(seq_len(nrow(x)), size = sample_size)

train_set <- x[train_ind, ]
test <- x[-train_ind, ]
train <- train_set[,1:2] # the data we used was initially prepared for the classificatrion example please remove third column

results <- kmeans(train,3)
idx = results[["cluster"]]

for (i in seq(along=idx)){
  a<-switch(idx[i],"red","green","blue") 
  plot(train[i,1],train[i,2], col=a,type="p",xlim=c(-10,20),ylim=c(-10,20))
  par(new=TRUE)
  }

# select cluster Nr 1 (you may select any other cluster)
cluster_1 = train[idx == 1,]

cov_cluster_1 = cov(cluster_1)


#DrawEllipse(results$centers[1,1], results$centers[1,2], a = sqrt(eigen_values[1]), b = sqrt(eigen_values[2]), angle = theta,  border = NULL, col = "r")
drawEllipse(results$centers[1,], cov_cluster_1, radius=3, nv = 100, axes = FALSE, fg = par('fg'), bg = NA, colCtr = "red", lty = par('lty'), lwd = par('lwd'), pch = par('pch'), cex = par('cex'))




