## load libraries
library(ggplot2)
library(ggfortify)
library(GGally)
library(e1071)
library(class)
library(psych)
library(readr)

setwd("C:/Users/liggea/Documents/GitHub/data-analytics-s26/Lab 5")
wine <- read_csv("wine.data", col_names = FALSE)
names(wine) <- c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins","Color Intensity","Hue","Od280/od315 of diluted wines","Proline")
head(wine)
wine$Type <- as.factor(wine$Type)

set.seed(2026)

N <- nrow(wine)
train.indexes <- sample(N,0.7*N)

train <- wine[train.indexes,]
test <- wine[-train.indexes,]

ggpairs(train, aes(color = Type))

# first model

tune = tune.svm(Type ~ Alcohol + Flavanoids, data = train, kernel = 'linear', cost = c(0.1, 1, 10, 100))
tune # C = 0.1

svm.mod1 <- svm(Type ~ Alcohol + Flavanoids, data = train, kernel = 'linear', scale = TRUE, cost = 0.1) # this combination seemed to cleanly separate variables in the ggpairs plot above
svm.mod1
plot(svm.mod1, data = train, formula = Alcohol~Flavanoids, svSymbol = "x", dataSymbol = "o")

make.grid = function(X, n = 75) {
  grange = apply(X, 2, range)
  X1 = seq(from = grange[1,1], to = grange[2,1], length = n)
  X2 = seq(from = grange[1,2], to = grange[2,2], length = n)
  expand.grid(Alcohol = X1, Flavanoids = X2)
}

X <- train[,c("Alcohol","Flavanoids")]
Y <- train$Type

xgrid = make.grid(X)
ygrid = predict(svm.mod1, xgrid)
cols <- c("1"="red", "2"="blue", "3"="green")

plot(xgrid, col = cols[as.character(ygrid)], pch = 20, cex = .2)
points(X, col = cols[as.character(Y)], pch = 19)
points(X[svm.mod1$index,], pch = 5, cex = 2)

### predict for test data
test.pred <- predict(svm.mod1, test)

cm = as.matrix(table(Actual = test$Type, Predicted = test.pred))
cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diagv = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

accuracy <- sum(diagv)/n
accuracy

recall = diagv / rowsums 
precision = diagv / colsums
f1 = 2 * precision * recall / (precision + recall) 

svm.mod1.res <- data.frame(precision, recall, f1)
svm.mod1.res

# second model w/ radial kernel

tune = tune.svm(Type ~ Alcohol + Flavanoids, data = train, kernel = 'radial', cost = c(0.1, 1, 10, 100), gamma = c(0.01, 0.1, 1, 10))
tune # 0.1 gamma, 100 cost

svm.mod2 <- svm(Type ~ Alcohol + Flavanoids, data = train, kernel = 'radial', scale = TRUE, cost = 100, gamma = 0.1)
svm.mod2
plot(svm.mod2, data = train, formula = Alcohol~Flavanoids, svSymbol = "x", dataSymbol = "o")

make.grid = function(X, n = 75) {
  grange = apply(X, 2, range)
  X1 = seq(from = grange[1,1], to = grange[2,1], length = n)
  X2 = seq(from = grange[1,2], to = grange[2,2], length = n)
  expand.grid(Alcohol = X1, Flavanoids = X2)
}

X <- train[,c("Alcohol","Flavanoids")]
Y <- train$Type

xgrid = make.grid(X)
ygrid = predict(svm.mod2, xgrid)
cols <- c("1"="red", "2"="blue", "3"="green")

plot(xgrid, col = cols[as.character(ygrid)], pch = 20, cex = .2)
points(X, col = cols[as.character(Y)], pch = 19)
points(X[svm.mod1$index,], pch = 5, cex = 2)

### predict for test data
test.pred <- predict(svm.mod2, test)

cm = as.matrix(table(Actual = test$Type, Predicted = test.pred))
cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diagv = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

accuracy <- sum(diagv)/n
accuracy

recall = diagv / rowsums 
precision = diagv / colsums
f1 = 2 * precision * recall / (precision + recall) 

svm.mod2.res <- data.frame(precision, recall, f1)
svm.mod2.res

# radial model performs slightly better

# alternate method: kNN
k <- 5
knn.pred = knn(train=train, test=test, cl = train$Type, k = k)

cm = as.matrix(table(Actual = test$Type, Predicted = knn.pred))
cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diagv = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

accuracy <- sum(diagv)/n
accuracy

recall = diagv / rowsums 
precision = diagv / colsums
f1 = 2 * precision * recall / (precision + recall) 

knn.res <- data.frame(precision, recall, f1)
knn.res

# The kNN model is a bit worse at predicting type 1 but far worse at predicting types 2 and 3
# Precision, recall and f1 are all lower for types 2 and 3 in kNN, particularly recall for type 3
# This makes sense as types 2 and 3 have a lot of overlap