require(tidyverse)
require(ISLR)
require(MASS)
require(boot)

#The second component of delta is the average mean-squared error that you obtain from doing K-fold CV, 
#but with a bias correction. How this is achieved is, initially, the residual sum of squares (RSS) is computed 
#based on the GLM predicted values and the actual response values for the entire data set. 
#As you're going through the K folds, you generate a training model, and then you compute the RSS 
#between the entire data set of y-values (not just the training set) and the predicted values from the training model. 
#These resulting RSS values are then subtracted from the initial RSS. After you're done going through your K folds, 
#you will have subtracted K values from the initial RSS. This is the second component of delta.

#################################### ch3 linear regression ####################################
MASS::Boston

fix(Boston)
names(Boston)
lm.fit=lm(medv~lstat, Boston)
lm.fit
summary(lm.fit)
confint(lm.fit)

predict(lm.fit, data.frame(lstat=c(5,10,15)), interval = c("confidence"))
predict(lm.fit, data.frame(lstat=c(5,10,15)), interval = c("prediction"))

with(Boston, plot(lstat, medv))
abline(lm.fit)
abline(lm.fit, lwd=3, col="red")

with(Boston, plot(lstat, medv, col="red"))
with(Boston, plot(lstat, medv, pch=20))
with(Boston, plot(lstat, medv, pch="+"))
plot(1:20, 1:20,pch=1:20)

par(mfrow=c(2,2))
plot(lm.fit)

plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

#################################### ch8 tree-based methods ####################################
library(ISLR)
library(tree)

#> str(Carseats)
#'data.frame':	400 obs. of  12 variables:
#  $ Sales      : num  9.5 11.22 10.06 7.4 4.15 ...
#$ CompPrice  : num  138 111 113 117 141 124 115 136 132 132 ...
#$ Income     : num  73 48 35 100 64 113 105 81 110 113 ...
#$ Advertising: num  11 16 10 4 3 13 0 15 0 0 ...
#$ Population : num  276 260 269 466 340 501 45 425 108 131 ...
#$ Price      : num  120 83 80 97 128 72 108 120 124 124 ...
#$ ShelveLoc  : Factor w/ 3 levels "Bad","Good","Medium": 1 2 3 3 1 1 3 2 3 3 ...
#$ Age        : num  42 65 59 55 38 78 71 67 76 76 ...
#$ Education  : num  17 10 12 14 13 16 15 10 10 17 ...
#$ Urban      : Factor w/ 2 levels "No","Yes": 2 2 2 2 2 1 2 2 1 1 ...
#$ US         : Factor w/ 2 levels "No","Yes": 2 2 2 2 1 2 1 2 1 2 ...
#$ High       : Factor w/ 2 levels "No","Yes": 2 2 2 1 1 2 1 2 1 1 ...

attach(Carseats)
High=ifelse(Sales<=8,"No","Yes")
Carseats=data.frame(Carseats, High)
tree.carseats=tree(High~.-Sales, Carseats)
summary(tree.carseats)

plot(tree.carseats)
text(tree.carseats, pretty = 0)

tree.carseats



# train:test=50:50
set.seed(2)
train=sample(1:nrow(Carseats),200)
Carseats.test=Carseats[-train,]
High.test=High[-train]
tree.carseats=tree(High~.-Sales, Carseats, subset=train)
tree.pred=predict(tree.carseats, Carseats.test, type="class")
table(tree.pred, High.test)
(104+50)/200


# pruning
set.seed(3)
cv.carseats=cv.tree(tree.carseats, FUN=prune.misclass)
cv.carseats=cv.tree(tree.carseats)
names(cv.carseats)
cv.carseats

par(mfrow=c(1,2))
plot(cv.carseats$size, cv.carseats$dev, type = "b")
plot(cv.carseats$k, cv.carseats$dev, type = "b")

prune.carseats=prune.misclass(tree.carseats, best=9)
plot(prune.carseats)
text(prune.carseats, pretty=0)


















