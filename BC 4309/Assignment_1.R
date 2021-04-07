# ========================================================================================================
# Purpose:      Rscript for Neuralnet demo.
# Author:       Neumann
# DOC:          15-03-2018
# Topics:       Comparing neuralnet with traditional backprog vs nnet
# Data Source:  infert in base package datasets
#=========================================================================================================

setwd(getwd())

data <- read.csv("Diabetes.csv") # load dataset from R base package
output <- as.factor(data$Class)
View(output)
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

data <- as.data.frame(lapply(data[0:8], min_max_norm))

data$output <- output

# nnet ------------------------------------------------------------------------
library(nnet)
# Imprtant to check/set all categorical varibles are factors so that the correct default algo in nnet will be applied automatically;
# From nnet documentation: If the response in formula is a factor, an appropriate classification network is constructed; this has one output and entropy fit if the number of levels is two, ...

# Override the default maxit = 100 to be the same as neuralnet (10,000), and 2 hidden nodes.
m4 <- nnet(data$output~., data=data, size=2, maxit = 10000)
m4$value  # error = 111.28
m4$fitted.values  # output for each obs in trainset
m4$wts  # final Weights. But hard to see between which nodes. Use summary() to see weights in context
summary(m4)  # No native plot function in nnet to view network
cat('Trainset Confusion Matrix with nnet (1 hidden layer, 2 hidden nodes, algo = backprog):')
View(data$output)
length(data$output)
length(predict(m4, type = 'class'))
table(data$output, predict(m4, type = 'class'))  # nnet has predict function, unlike neuralnet.
