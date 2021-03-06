library(ggplot2)
source("/home/aivo/kool/R/mydistfun.R")

# this is autogenerated test data
load(file="/home/aivo/kool/R/kNN_data2.RData")

# Make random sampling repeatable
set.seed(100)
x <- cbind(x, y)
# this is because of input data
k <- 3
# standardize inputs, in case their scale is different, with a function Xs = (X-min) / (max-min)
x1 <- x[,1:2]
xs <- (x1-min(x1)) / (max(x1)-min(x1))
# attach back the labels
xs <- cbind(xs, x[,3])

# split data to 80 / 20 
training_size <- floor(0.8 * nrow(xs))
train_idx <- sample(seq_len(nrow(xs)), size = training_size)
# also remove 3rd column from test set, which has correct labels
train <- xs[train_idx, ]
test <- xs[-train_idx, 1:2]
test_labels <- xs[-train_idx, 3]
# convert into data frames
train <- data.frame("x" = train[, 1], "y" = train[, 2], "label" = train[, 3])
test <- data.frame("x" = test[, 1], "y" = test[, 2])

my.knn <- function(x.i, all, k) {
  distances <- c()
  categories <- c()
  # ensure it's numeric
  x.i <- as.numeric(x.i)
  # calculate the distance between the new data point and all the training data points
  for (j in 1:nrow(all)) {
    x.j <- as.numeric(all[j,])
    # remove the last column for distance calculation, which holds labels
    #distances[j] <- mydistfun(x.i, x.j[1:(length(x.j) - 1)], "manhattan")
    #distances[j] <- mydistfun(x.i, x.j[1:(length(x.j) - 1)], "euclidean")
    distances[j] <- mydistfun(x.i, x.j[1:(length(x.j) - 1)], "Chebyshev")
    #distances[j] <- mydistfun(x.i, x.j[1:(length(x.j) - 1)], "cosine")
    #distances[j] <- mydistfun(x.i, x.j[1:(length(x.j) - 1)], "Canberra")
    categories[j] <- all[j,]$label
  }
  # convert to data frame and order by distance 
  # assume positive k and filter k least values from the ordered list
  top <- data.frame(distances,categories)[order(distances),][1:k,]
  # treat categories as factor and summarize how many of each label is in the k distances
  # then sort decreasingly and select the first element and return the factor's name
  return(names(sort(summary(as.factor(top$categories)), decreasing=TRUE)[1]))
}

colors <- c("#00CC00", "#FF3399", "#9933FF")

# create a scatterplot to display relationsships between continous variables
plot <- ggplot(data = train, aes(x = x, y = y))
plot <- plot + geom_point(data = train[train$label == 1,], color = colors[1])
plot <- plot + geom_point(data = train[train$label == 2,], color = colors[2])
plot <- plot + geom_point(data = train[train$label== 3,], color = colors[3])

# calculate knn for each point in the test data set
corrects <- 0
for (i in 1:nrow(test)) {
  p <- test[i,]
  p$label <- as.numeric(my.knn(x.i = p, all = train, k = k))
  # also check right away if the classifications was correct
  if (p$label == test_labels[i]) {corrects <- corrects + 1}
  plot <- plot + geom_point(shape = 21, size = 3, data = p, color = "black", fill = colors[p$label])
}

print(plot)

accuracy <- corrects / nrow(test) * 100

# 3 dim - accuracy with manhattan distance: 99.0%
# 3 dim - accuracy with euclidean distance: 99.0%
# 3 dim - accuracy with Chebyshev distance: 99.0%
# 9 dim - accuracy with manhattan distance: 99.0%
# 9 dim - accuracy with euclidean distance: 99.0%
# 9 dim - accuracy with Chebyshev distance: 99.5%
# 9 dim - accuracy with cosine distance: 98.0%
# 9 dim - accuracy with Canberra distance: 100%
