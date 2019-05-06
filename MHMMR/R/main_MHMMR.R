# Segmentation of multivariate time series with a Multiple Hidden Markov Model Regression (MHMMR).
#
#
# Multiple Hidden Markov Model Regression (HMMR) for the segmentation of multivariate time series
# with regime changes. The model assumes that the time series is
# governed by a sequence of hidden discrete regimes/states, where each
# regime/state has multivariate Gaussian regressors emission densities.
# The model parameters are estimated by MLE via the EM algorithm
#
# Devoloped and written by Faicel Chamroukhi
#
## Please cite the following papers for this code:
#
#
# @article{Chamroukhi-FDA-2018,
#  	Journal = {Wiley Interdisciplinary Reviews: Data Mining and Knowledge Discovery},
#  	Author = {Faicel Chamroukhi and Hien D. Nguyen},
#  	Note = {DOI: 10.1002/widm.1298.},
#  	Volume = {},
#  	Title = {Model-Based Clustering and Classification of Functional Data},
#  	Year = {2019},
#  	Month = {to appear},
#  	url =  {https://chamroukhi.com/papers/MBCC-FDA.pdf}
# }
#
# @article{Chamroukhi-MHMMR-2013,
# 	Author = {Trabelsi, D. and Mohammed, S. and Chamroukhi, F. and Oukhellou, L. and Amirat, Y.},
# 	Journal = {IEEE Transactions on Automation Science and Engineering},
# 	Number = {10},
# 	Pages = {829--335},
# 	Title = {An unsupervised approach for automatic activity recognition based on Hidden Markov Model Regression},
# 	Volume = {3},
# 	Year = {2013},
# 	url  ={https://chamroukhi.com/papers/Chamroukhi-MHMMR-IeeeTase.pdf}
# 	}
#
#
# (c) Faicel Chamroukhi (since 2010).
###########################################################################

rm(list = ls())
source("R/enums.R")
source("R/FData.R")
source("R/ModelMHMMR.R")
source("R/ModelLearner.R")

# Building matrices for regression

# Toy multivariate time series with regime changes
# Y = cbind(c(rnorm(100, mean = 0), rnorm(120, mean = 7), rnorm(200, mean = 4), rnorm(100, mean = -1), rnorm(150, mean = 3.5)),
#            c(rnorm(100, mean = 1), rnorm(120, mean = 5), rnorm(200, mean = 6), rnorm(100, mean = -2), rnorm(150, mean = 2)),
#            c(rnorm(100, mean = -2), rnorm(120, mean = 10), rnorm(200, mean = 8), rnorm(100, mean = 0), rnorm(150, mean = 5)))
# X = matrix(seq(from = 0, to = 1, length.out = nrow(Y)), nrow = 1)

# Toy time series with regime changes
load("data/simulatedTimeSeries.RData")

# Some real time series with regime changes
load("data/realTimeSeries.RData")

fData <- FData$new()
fData$setData(X, Y)


K <- 5 # Number of regimes (states)
p <- 3 # Dimension of beta (order of the polynomial regressors)
variance_type <- variance_types$hetereskedastic

modelMHMMR <- ModelMHMMR(fData, K, p, variance_type)

n_tries <- 1
max_iter <- 1500
threshold <- 1e-6
verbose <- TRUE

solution <- EM(modelMHMMR, n_tries, max_iter, threshold, verbose)

solution$plot()
