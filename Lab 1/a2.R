library(readr)
library(EnvStats)
library(nortest)

# set working directory (relative path)
setwd("C:/Users/theen/OneDrive/Documents/GitHub/data-analytics-s26/Lab 1")

# read data
epi.data <- read_csv("epi_results_2024_pop_gdp.csv")

# view dataframe
View(epi.data)

# grab variables
ECO <- epi.data$ECO.new
BDH <- epi.data$BDH.new

# summaries
summary(ECO)
summary(BDH)

# boxplots
boxplot(ECO, BDH, names = c("ECO","BDH"))

# histograms
v <- seq(20., 85., 5)
hist(ECO, v, prob=TRUE)
curve(dnorm(x, mean(ECO), sd(ECO)),add=TRUE,col="blue",lwd=2)

w <- seq(0, 100, 5)
hist(BDH, w, prob=TRUE)
curve(dnorm(x, mean(BDH), sd(BDH)),add=TRUE,col="blue",lwd=2)

# ecdfs
plot(ecdf(ECO), do.points=FALSE, verticals=TRUE) 
plot(ecdf(BDH), do.points=FALSE, verticals=TRUE) 

## qqs vs normal
qqnorm(ECO); qqline(ECO, col="blue")
qqnorm(BDH); qqline(BDH, col="blue")

## qq vs each other
qqplot(ECO, BDH, xlab = "Q-Q plot for ECO and BDH")

# normality tests
shapiro.test(ECO)
ad.test(ECO)

shapiro.test(BDH)
ad.test(BDH)

# identical distribution test
ks.test(ECO, BDH)