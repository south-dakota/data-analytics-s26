library("ggplot2")
library("readr")

## read dataset
NY_House_Dataset <- read_csv("C:/Users/liggea/Documents/GitHub/data-analytics-s26/Lab 2/NY-House-Dataset.csv")

dataset <- NY_House_Dataset

## filter data
dataset <- dataset[dataset$PRICE<195000000,]
dataset <- dataset[dataset$PROPERTYSQFT!=2184.207862,]
dataset <- dataset[dataset$BEDS+dataset$BATH<25,]

## model 1: log10(PROPERTYSQFT) vs log10(PRICE)
model1 <- lm(log10(PRICE)~log10(PROPERTYSQFT), data = dataset)

## print model output
summary(model1)

## scatter plot
ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

ggplot(model1, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0)

## model 2: BEDS + BATH vs log10(PRICE)
model2 <- lm(log10(PRICE)~(BEDS + BATH), data = dataset)

## print model output
summary(model2)

ggplot(dataset, aes(x = BEDS + BATH, y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="blue")

ggplot(model2, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0)

## model 3: BEDS + BATH + log10(PROPERTYSQFT) vs log10(PRICE)
model3 <- lm(log10(PRICE)~(BEDS + BATH + log10(PROPERTYSQFT)), data = dataset)

## print model output
summary(model3)

ggplot(dataset, aes(x = BEDS + BATH + log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="green")

ggplot(model3, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0)

## Unsurprisingly, the one that takes all three into account has the highest R^2
## Surprisingly, however, it is not by much. Model 3's R^2 is only higher than model 1's by a little less than 0.03
### THE END ###

