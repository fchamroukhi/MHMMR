#' @importFrom MASS ginv
#' @export
ParamMHMMR <- setRefClass(
  "ParamMHMMR",
  fields = list(
    fData = "FData",
    phi = "matrix",

    K = "numeric", # Number of regimes
    p = "numeric", # Dimension of beta (order of polynomial regression)
    variance_type = "numeric",
    nu = "numeric", # Degree of freedom

    prior = "matrix",
    trans_mat = "matrix",
    beta = "array",
    sigma = "array",
    mask = "matrix"
  ),
  methods = list(

    initialize = function(fData = FData(numeric(1), matrix(1)), K = 2, p = 2, variance_type = 1) {

      fData <<- fData

      phi <<- designmatrix(x = fData$X, p = p)$XBeta

      K <<- K
      p <<- p
      variance_type <<- variance_type

      if (variance_type == variance_types$homoskedastic) {
        nu <<- K - 1 + K * (K - 1) + K * (p + 1) * fData$m + fData$m * (fData$m + 1) / 2
      }
      else{
        nu <<- K - 1 + K * (K - 1) + K * (p + 1) * fData$m + K * fData$m * (fData$m + 1) / 2
      }

      prior <<- matrix(NA, ncol = K - 1)
      trans_mat <<- matrix(NA, K, K)
      beta <<- array(NA, dim = c(p + 1, fData$m, K))
      if (variance_type == variance_types$homoskedastic) {
        sigma <<- matrix(NA, fData$m, fData$m)
      }
      else{
        sigma <<- array(NA, dim = c(fData$m, fData$m, K))
      }
      mask <<- matrix(NA, K, K)

    },

    initMhmmr = function(try_algo = 1) {
      # function mhmmr =  initMhmmr(X, y, K, type_variance, EM_try)
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
      ################################################################################

      # Initialization taking into account the constraint:

      # Initialization of the transition matrix
      maskM <- 0.5 * diag(K) # Mask of order 1

      for (k in 1:(K - 1)) {
        ind <- which(maskM[k, ] != 0)
        maskM[k, ind + 1] <- 0.5
      }
      trans_mat <<- maskM
      mask <<- maskM

      # Initialization of the initial distribution
      prior <<- matrix(c(1, rep(0, K - 1)))

      # Initialization of regression coefficients and variances
      initMhmmrRegressors(try_algo)

    },

    initMhmmrRegressors = function(try_algo = 1) {

      if (try_algo == 1) { # Uniform segmentation into K contiguous segments, and then a regression

        zi <- round(fData$n / K) - 1

        s <- 0 # If homoskedastic
        for (k in 1:K) {
          yk <- fData$Y[((k - 1) * zi + 1):(k * zi), ]
          Xk <- as.matrix(phi[((k - 1) * zi + 1):(k * zi), ])

          beta[, , k] <<- solve(t(Xk) %*% Xk + (10 ^ -4) * diag(p + 1)) %*% t(Xk) %*% yk # regress(yk,Xk); # for a use in octave, where regress doesnt exist

          muk <- Xk %*% beta[, , k]
          sk <- t(yk - muk) %*% (yk - muk)
          if (variance_type == variance_types$homoskedastic) {
            s <- (s + sk)
            sigma <<- s / fData$n
          }
          else {
            sigma[, , k] <<- sk / length(yk)
          }
        }
      }
      else{# Random segmentation into contiguous segments, and then a regression

        Lmin <- p + 1 + 1 # Minimum length of a segment
        tk_init <- rep(0, K)
        tk_init <- t(tk_init)
        tk_init[1] <- 0
        K_1 <- K
        for (k in 2:K) {
          K_1 <- K_1 - 1
          temp <- seq(tk_init[k - 1] + Lmin, fData$n - K_1 * Lmin)
          ind <- sample(1:length(temp), length(temp))
          tk_init[k] <- temp[ind[1]]
        }
        tk_init[K + 1] <- fData$n

        s <- 0
        for (k in 1:K) {
          i <- tk_init[k] + 1
          j <- tk_init[k + 1]
          yk <- fData$Y[i:j, ]
          Xk <- phi[i:j, ]
          beta[, , k] <<- solve(t(Xk) %*% Xk + 1e-4 * diag(p + 1)) %*% t(Xk) %*% yk #regress(yk,Xk); # for a use in octave, where regress doesnt exist
          muk <- Xk %*% beta[, , k]
          sk <- t(yk - muk) %*% (yk - muk)

          if (variance_type == variance_types$homoskedastic) {
            s <- s + sk
            sigma[1] <<- s / fData$n

          }
          else{
            sigma[, , k] <<- sk / length(yk)
          }
        }
      }
    },

    MStep = function(statMHMMR) {
      # Updates of the Markov chain parameters
      # Initial states prob: P(Z_1 = k)
      prior <<- matrix(normalize(statMHMMR$tau_tk[1, ])$M)

      # Transition matrix: P(Zt=i|Zt-1=j) (A_{k\ell})
      trans_mat <<- mkStochastic(apply(statMHMMR$xi_tkl, c(1, 2), sum))

      # For segmental HMMR: p(z_t = k| z_{t-1} = \ell) = zero if k<\ell (no back) of if k >= \ell+2 (no jumps)
      trans_mat <<- mkStochastic(mask * trans_mat)
      # Update of the regressors (reg coefficients betak and the variance(s) sigma2k)

      s <- 0 # If homoskedastic
      for (k in 1:K) {
        weights <- statMHMMR$tau_tk[, k]

        nk <- sum(weights) # Expected cardinal number of state k
        Xk <- phi * (sqrt(weights) %*% matrix(1, 1, p + 1)) # [n*(p+1)]
        yk <- fData$Y * (sqrt(weights) %*% ones(1, fData$m)) # dimension :(nxd).*(nxd) = (nxd)

        # Regression coefficients
        lambda <- 1e-5 # If a bayesian prior on the beta's


        # bk <- (solve(t(Xk) %*% Xk + lambda * diag(p + 1)) %*% t(Xk)) %*% yk
        bk <- (ginv(t(Xk) %*% Xk) %*% t(Xk)) %*% yk

        beta[, , k] <<- bk

        # Variance(s)
        z <- (fData$Y - phi %*% bk) * (sqrt(weights) %*% ones(1, fData$m))
        sk <- t(z) %*% z
        if (variance_type == variance_types$homoskedastic) {
          s <- (s + sk)
          sigma <<- s / fData$n
        }
        else{
          sigma[, , k] <<- sk / nk + lambda * diag(x = 1, fData$m)
        }
      }

    }
  )
)
