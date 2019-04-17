source("R/enums.R")
source("R/mk_stochastic.R")

ParamMHMMR <- setRefClass(
  "ParamMHMMR",
  fields = list(
    prior = "matrix",
    trans_mat = "matrix",
    beta = "array",
    sigma = "array",
    mask = "matrix"
  ),
  methods = list(
    init_mhmmr = function(modelMHMMR, phi, try_algo = 1) {
      # function mhmmr =  init_mhmmr(X, y, K, type_variance, EM_try)
      # init_mhmmr initialize the parameters of a Multivriate Hidden Markov Model
      # Regression (MHMMR) model
      #
      # Inputs :
      #
      #           X: [nx(p+1)] regression desing matrix
      #           y: [nxd] multivariate time series
      #           K : Number of polynomial regression components (regimes)
      #          	type_variance: hoskedastoc or heteroskedastic
      #           EM_try: number of the current EM run
      #
      # Outputs :
      #
      #         mhmmr: the initial MHMMR model. a structure composed of:
      #
      #         prior: [Kx1]: prior(k) = Pr(z_1=k), k=1...K
      #         trans_mat: [KxK], trans_mat(\ell,k) = Pr(z_t = k|z_{t-1}=\ell)
      #         reg_param: the paramters of the regressors:
      #                 betak: regression coefficients
      #                 sigma2k (or sigma2) : the covariance matrices(s). sigma2k(k) = cov[y(t)|z(t)=k]
      #         and some stats: like the the posterior probs, the loglikelihood,
      #         etc
      #
      # Faicel Chamroukhi, first version in November 2008
      ################################################################################

      # Initialization taking into account the constraint:

      # Initialization of the transition matrix
      maskM <- 0.5 * diag(modelMHMMR$K) # mask of order 1

      for (k in 1:(modelMHMMR$K - 1)) {
        ind <- which(maskM[k, ] != 0)
        maskM[k, ind + 1] <- 0.5
      }
      trans_mat <<- maskM
      mask <<- maskM

      # Initialization of the initial distribution
      prior <<- matrix(c(1, rep(0, modelMHMMR$K - 1)))

      # Initialization of regression coefficients and variances
      init_mhmmr_regressors(phi, modelMHMMR, try_algo)

    },

    init_mhmmr_regressors = function (phi, modelMHMMR, try_algo = 1) {

      if (try_algo == 1) { # Uniform segmentation into K contiguous segments, and then a regression

        zi <- round(modelMHMMR$n / modelMHMMR$K) - 1

        s <- 0 # If homoskedastic
        for (k in 1:modelMHMMR$K) {
          yk <- modelMHMMR$Y[((k - 1) * zi + 1):(k * zi), ]
          Xk <- as.matrix(phi[((k - 1) * zi + 1):(k * zi), ])

          beta[, , k] <<- solve(t(Xk) %*% Xk + (10 ^ -4) * diag(modelMHMMR$p + 1)) %*% t(Xk) %*% yk # regress(yk,Xk); # for a use in octave, where regress doesnt exist

          muk <- Xk %*% beta[, , k]
          sk <- t(yk - muk) %*% (yk - muk)
          if (modelMHMMR$variance_type == variance_types$homoskedastic) {
            s <- (s + sk)
            sigma <<- s / modelMHMMR$n
          }
          else {
            sigma[, , k] <<- sk / length(yk)
          }
        }
      }
      else{ # Random segmentation into contiguous segments, and then a regression

        Lmin <- modelMHMMR$p + 1 + 1 # Minimum length of a segment
        tk_init <- rep(0, modelMHMMR$K)
        tk_init <- t(tk_init)
        tk_init[1] <- 0
        K_1 <- modelMHMMR$K
        for (k in 2:modelMHMMR$K) {
          K_1 <- K_1 - 1
          temp <- seq(tk_init[k - 1] + Lmin, modelMHMMR$n - K_1 * Lmin)
          ind <- sample(1:length(temp), length(temp))
          tk_init[k] <- temp[ind[1]]
        }
        tk_init[K + 1] <- modelMHMMR$n

        s <- 0
        for (k in 1:modelMHMMR$K) {
          i <- tk_init[k] + 1
          j <- tk_init[k + 1]
          yk <- modelMHMMR$Y[i:j, ]
          Xk <- phi[i:j, ]
          beta[, , k] <<- solve(t(Xk) %*% Xk + 1e-4 * diag(modelMHMMR$p + 1)) %*% t(Xk) %*% yk #regress(yk,Xk); # for a use in octave, where regress doesnt exist
          muk <- Xk %*% beta[, , k]
          sk <- t(yk - muk) %*% (yk - muk)

          if (modelMHMMR$variance_type == variance_types$homoskedastic) {
            s <- s + sk
            sigma[1] <<- s / modelMHMMR$n

          }
          else{
            sigma[, , k] <<- sk / length(yk)
          }
        }
      }
    },

    MStep = function(modelMHMMR, statMHMMR, phi) {
      # Updates of the Markov chain parameters
      # Initial states prob: P(Z_1 = k)
      prior <<- matrix(normalize(statMHMMR$tau_tk[1, ])$M)

      # Transition matrix: P(Zt=i|Zt-1=j) (A_{k\ell})
      trans_mat <<- mk_stochastic(apply(statMHMMR$xi_tkl, c(2, 3), sum))

      # For segmental HMMR: p(z_t = k| z_{t-1} = \ell) = zero if k<\ell (no back) of if k >= \ell+2 (no jumps)
      trans_mat <<- mk_stochastic(mask * trans_mat)
      # Update of the regressors (reg coefficients betak and the variance(s) sigma2k)

      s <- 0 # If homoskedastic
      for (k in 1:modelMHMMR$K) {
        weights <- statMHMMR$tau_tk[, k]

        nk <- sum(weights) # Expected cardinal number of state k
        Xk <- phi * (sqrt(weights) %*% matrix(1, 1, modelMHMMR$p + 1)) # [n*(p+1)]
        yk <- modelMHMMR$Y * (sqrt(weights) %*% ones(1, modelMHMMR$m)) # dimension :(nxd).*(nxd) = (nxd)

        # Regression coefficients
        lambda <- 1e-5 # If a bayesian prior on the beta's
        bk <- (solve(t(Xk) %*% Xk + lambda * diag(modelMHMMR$p + 1)) %*% t(Xk)) %*% yk
        beta[, , k] <<- bk

        # Variance(s)
        z <- (modelMHMMR$Y - phi %*% bk) * (sqrt(weights) %*% ones(1, modelMHMMR$m))
        sk <- t(z) %*% z
        if (modelMHMMR$variance_type == variance_types$homoskedastic) {
          s <- (s + sk)
          sigma <<- s / modelMHMMR$n
        }
        else{
          sigma[, , k] <<- sk / nk + lambda * diag(x = 1, modelMHMMR$m)
        }
      }

    }
  )
)

ParamMHMMR <- function(modelMHMMR) {
  prior <- matrix(NA, ncol = modelMHMMR$K - 1)
  trans_mat <- matrix(NA, modelMHMMR$K, modelMHMMR$K)
  beta <- array(NA, dim = c(modelMHMMR$p + 1, modelMHMMR$m, modelMHMMR$K))
  if (modelMHMMR$variance_type == variance_types$homoskedastic) {
    sigma <- matrix(NA, modelMHMMR$m, modelMHMMR$m)
  }
  else{
    sigma <- array(NA, dim = c(modelMHMMR$m, modelMHMMR$m, modelMHMMR$K))
  }
  mask <- matrix(NA, modelMHMMR$K, modelMHMMR$K)
  new("ParamMHMMR", prior = prior, trans_mat = trans_mat, beta = beta, sigma = sigma, mask = mask)
}
