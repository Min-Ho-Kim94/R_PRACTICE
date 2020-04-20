# kmeans 공부

library(tidyverse)
library(caret)

set.seed(1712)
inTrain <- createDataPartition(y=iris$Species, p=0.7, list=F)
training <- iris[inTrain,]
testing <- iris[-inTrain,]

# scaling
training.data <- scale(training[,-5])
summary(training.data)

iris.kmeans <- kmeans(training.data[,-5], centers = 3, iter.max=10000, nstart=1100)
iris.kmeans$centers

training$cluster <- as.factor(iris.kmeans$cluster)
qplot(Petal.Width, Petal.Length, colour=cluster, data=training)

table(training$Species, training$cluster)


# 군집 개수 판단
wssplot <- function(data, nc = 15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data, 2, var))
  for(i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i, nstart=10000, iter.max = 1000000)$withinss) #with-in cluster sum of squares
  } 
  plot(1:nc, wss, type='b', xlab="# of clusters", ylab='within groups sum of squares')
}

wssplot(training.data)



# NA exploring
NAs <- function(data){
  aa <- sapply(data, function(x) sum(is.na(x)))
  return(sort(aa[which(aa>0)], decreasing = T))
}

NAs(data)
