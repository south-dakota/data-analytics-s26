##########################################
### Principal Component Analysis (PCA) ###
##########################################

## load libraries
library(ggplot2)
library(ggfortify)
library(GGally)
library(e1071)
library(class)
library(psych)
library(readr)

## set working directory so that files can be referenced without the full path
setwd("C:/Users/liggea/Documents/GitHub/data-analytics-s26/Lab 4")

## read dataset
wine <- read_csv("wine.data", col_names = FALSE)

## set column names
names(wine) <- c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins","Color Intensity","Hue","Od280/od315 of diluted wines","Proline")

## inspect data frame
head(wine)

## change the data type of the "Type" column from character to factor
####
# Factors look like regular strings (characters) but with factors R knows 
# that the column is a categorical variable with finite possible values
# e.g. "Type" in the Wine dataset can only be 1, 2, or 3
####

wine$Type <- as.factor(wine$Type)


## visualize variables
pairs.panels(wine[,-1],gap = 0,bg = c("red", "yellow", "blue")[wine$Type],pch=21)

ggpairs(wine, ggplot2::aes(colour = Type))

###

X <- wine[,-1]
Y <- wine$Type

#######
# PCA #
#######
Xc <- scale(as.matrix(X),center=T,scale=F)
pcs <- princomp(Xc)

pc_scores <- as.data.frame(pcs$scores)
pc_scores$type <- Y
ggplot(pc_scores,aes(x=Comp.1, y=Comp.2, color=type)) + 
  geom_point(size=3) +
  labs(x="PC1",y="PC2")

pc1_loadings <- pcs$loadings[,1]
sort(abs(pc1_loadings),decreasing=TRUE) |> head()
# By far the greatest contributor to PC1 is proline with a loading value of 0.9998
# Magnesium is a very distant second with a loading value of 0.0179
# Everything else is less than 0.005

pc2_loadings <- pcs$loadings[,2]
sort(abs(pc2_loadings),decreasing=TRUE) |> head()
# PC2 is very magnesium-based

pc3_loadings <- pcs$loadings[,3]
sort(abs(pc3_loadings),decreasing=TRUE) |> head()
# Alcalinity of ash and color intensity?
# Use proline, magnesium, ash alcalinity and color intensity as the 4 variables

# 4-variable subset model
wine_subset <- wine[, c("Type","Proline", "Magnesium", "Alcalinity of ash", "Color Intensity")]
X <- scale(wine_subset[,-1])

vars_model = knn(train = X, test = X, cl = wine_subset$Type, k = 5)
cm <- table(Predicted = vars_model, Actual = wine_subset$Type)
precision <- diag(cm) / rowSums(cm)
recall <- diag(cm) / colSums(cm)
f1 <- 2 * (precision * recall) / (precision + recall)
cm
precision
recall
f1

# PCs model
pcs_data <- as.data.frame(pcs$scores[,1:2])
pcs_data$Type <- Y

X <- pcs_data[,1:2]
pcs_model = knn(train = X, test = X, cl = pcs_data$Type, k = 5)
cm <- table(Predicted = pcs_model, Actual = wine_subset$Type)
precision <- diag(cm) / rowSums(cm)
recall <- diag(cm) / colSums(cm)
f1 <- 2 * (precision * recall) / (precision + recall)
cm
precision
recall
f1

pcs_data$Predicted <- pcs_model

# Variables model seems to perform better than the PCs model
# Precision/recall/f1 values are similar for type 1, but the PCs model is significantly worse for types 2 and 3
# This makes sense as the types overlap significantly in the PC1/PC2 graph