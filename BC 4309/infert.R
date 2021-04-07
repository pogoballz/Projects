# ========================================================================================================
# Purpose:      Rscript for Neuralnet demo.
# Author:       Neumann
# DOC:          15-03-2018
# Topics:       Comparing neuralnet with traditional backprog vs nnet
# Data Source:  infert in base package datasets
#=========================================================================================================

setwd(getwd())

data(infert)  # load dataset from R base package
help(infert)  # view documentation of the dataset infert
# Parity = Num of Births.
# Target variable is case. case = 1 (infertile) or 0 (not infertile).

library(neuralnet)
set.seed(2014)  # for random starting weights

infert$age1 <- (infert$age - min(infert$age))/
  (max(infert$age)-min(infert$age))
infert$parity1 <- (infert$parity - min(infert$parity))/(max(infert$parity)-min(infert$parity))

# Neuralnet package cannot handle factors, unlike nnet. Thus need to manually create dummy variables in order to use neuralnet.
infert$induced1 <- ifelse(infert$induced==1, 1, 0)
infert$induced2 <- ifelse(infert$induced==2, 1, 0)
infert$spontaneous1 <- ifelse(infert$spontaneous==1, 1, 0)
infert$spontaneous2 <- ifelse(infert$spontaneous==2, 1, 0)

# Neural Network comprising 1 hidden layer with 2 hidden nodes and using traditional backprop instead of (default) resilient backprog with backtracking.
m3 <- neuralnet(case~age1+parity1+induced1+induced2+spontaneous1+spontaneous2, 
data=infert, hidden=2,err.fct="ce", linear.output=FALSE, algorithm = 'backprop', learningrate = 0.01)
## Stop adjusting weights when all gradients of the error function were smaller than 0.01 (the default threshold).

par(mfrow=c(1,1))
plot(m3)

m3$net.result  # predicted outputs
m3$result.matrix  # summary. Error = 114.59
m3$startweights
m3$weights
# The generalized weight is defined as the contribution of the ith input variable to the log-odds:
m3$generalized.weights
## Easier to view GW as plots instead

par(mfrow=c(2,3))
gwplot(m3,selected.covariate="age1", min=-2.5, max=5)
gwplot(m3,selected.covariate="parity1", min=-2.5, max=5)
gwplot(m3,selected.covariate="induced1", min=-2.5, max=5)
gwplot(m3,selected.covariate="induced2", min=-2.5, max=5)
gwplot(m3,selected.covariate="spontaneous1", min=-2.5, max=5)
gwplot(m3,selected.covariate="spontaneous2", min=-2.5, max=5)
## Age is now a significant factor. Shows importance of scaling.

# Display inputs with model outputs
out <- cbind(m3$covariate, m3$net.result[[1]])
dimnames(out) <- list(NULL, c("age1","parity1","induced1", "induced2", "spontaneous1", "spontaneous2", "nn-output"))
head(out)  # shows first 6 rows of data.

pred.m3 <- ifelse(unlist(m3$net.result) > 0.5, 1, 0)
cat('Trainset Confusion Matrix with neuralnet (1 hidden layer, 2 hidden nodes, algo = backprog):')
table(infert$case, pred.m3)

# nnet ------------------------------------------------------------------------
library(nnet)
# Imprtant to check/set all categorical varibles are factors so that the correct default algo in nnet will be applied automatically;
# From nnet documentation: If the response in formula is a factor, an appropriate classification network is constructed; this has one output and entropy fit if the number of levels is two, ...
infert$case <- factor(infert$case)
infert$spontaneous <- factor(infert$spontaneous)
infert$induced <- factor(infert$induced)

# Override the default maxit = 100 to be the same as neuralnet (10,000), and 2 hidden nodes.
m4 <- nnet(case~age1+parity1+induced+spontaneous, data=infert, size=2, maxit = 10000)
m4$value  # error = 111.28
m4$fitted.values  # output for each obs in trainset
m4$wts  # final Weights. But hard to see between which nodes. Use summary() to see weights in context
summary(m4)  # No native plot function in nnet to view network
cat('Trainset Confusion Matrix with nnet (1 hidden layer, 2 hidden nodes, algo = backprog):')
table(infert$case, predict(m4, type = 'class'))  # nnet has predict function, unlike neuralnet.
