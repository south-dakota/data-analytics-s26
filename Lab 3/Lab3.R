install.packages("class")
library("class")
install.packages("cluster")
library("cluster")
install.packages("factoextra")
library("factoextra")

# part 1: kNN

# read dataset
abalone.data <- read.table("C:/Users/liggea/Documents/GitHub/data-analytics-s26/Lab 3/abalone/abalone.data", header=FALSE, sep=',')
colnames(abalone.data) = c("sex", "length", "diameter", "height", "whole_weight", "shucked_weight", "viscera_weight", "shell_weight", "rings")

## add new column age.group with 3 values based on the number of rings 
abalone.data$age.group <- cut(abalone.data$rings, br=c(0,8,11,35), labels = c("young", 'adult', 'old'))

set.seed(2026)

train.index <- sample(1:nrow(abalone.data), 0.7 * nrow(abalone.data))
training <- abalone.data[train.index, ]
testing <- abalone.data[-train.index, ]

uno = c("length", "diameter", "height", "rings")
dos = c("whole_weight", "shucked_weight", "viscera_weight", "shell_weight")

normalize <- function(training, testing, featureset) {
  training.scaled <- scale(training[, featureset])
  testing.scaled <- scale(testing[, featureset], center=attr(training.scaled, "scaled:center"), scale=attr(training.scaled, "scaled:scale"))
  list(training=training.scaled, test=testing.scaled)
}

set1 <- normalize(training, testing, uno)
set2 <- normalize(training, testing, dos)

k <- 3

# evaluate 1
knn.predicted1 <- knn(train = set1$training,
                      test  = set1$test,
                      cl    = training$age.group,
                      k     = k)

table(knn.predicted1, testing$age.group,
      dnn = c("predicted","actual"))


# evaluate 2
knn.predicted2 <- knn(train = set2$training,
                      test  = set2$test,
                      cl    = training$age.group,
                      k     = k)

table(knn.predicted2, testing$age.group,
      dnn = c("predicted","actual"))

# 1 is clearly better, find an ideal k value
for (k in 1:10) {
  knn.predicted1 <- knn(train = set1$training,
                        test  = set1$test,
                        cl    = training$age.group,
                        k     = k)
  correct <- sum(knn.predicted1 == testing$age.group)
  total <- length(testing$age.group)
  
  print(k)
  print(table(knn.predicted1, testing$age.group,
        dnn = c("predicted","actual")))
  print(round(correct / total, 4))
}
# highest accuracy is 0.9968 with k = 1. wasn't expecting that

# part 2: k-means/PAM
cluster.data <- scale(abalone.data[, c("length", "diameter", "height", "whole_weight", "shucked_weight", "viscera_weight", "shell_weight", "rings")])

# k-means first
k.best <- 0
width.best <- 0
for (k in 2:10) {
  kmeans.result = kmeans(cluster.data, centers = k, nstart = 25)
  sil <- silhouette(kmeans.result$cluster, dist(cluster.data))
  width = mean(sil[, 3])
  if(width > width.best) {
    k.best <- k
    width.best <- width
  }
}
print(k.best)
kmeans.result <- kmeans(cluster.data, centers = k.best, nstart = 25)
sil <- silhouette(kmeans.result$cluster, dist(cluster.data))
fviz_silhouette(sil)

# PAM
k.best <- 0
width.best <- 0
for (k in 2:10) {
  pam.result = pam(cluster.data, k = k)
  width <- pam.result$silinfo$avg.width
  if(width > width.best) {
    k.best <- k
    width.best <- width
  }
}
print(k.best)
pam.result = pam(cluster.data, k = k.best)
fviz_silhouette(pam.result)
