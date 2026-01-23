library(readr)
library(EnvStats)
library(nortest)

# set working directory (relative path)
setwd("C:/Users/liggea/Documents/ata danalytics/Lab 1")

# read data
epi.data <- read_csv("epi_results_2024_pop_gdp.csv")

# view dataframe
View(epi.data)