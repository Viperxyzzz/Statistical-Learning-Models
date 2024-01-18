library(ggplot2)
library(cowplot)

url <- "https://raw.githubusercontent.com/StatQuest/logistic_regression_demo/master/processed.cleveland.data"

data <- read.csv(url, header=FALSE)

