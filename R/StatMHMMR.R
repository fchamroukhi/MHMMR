#' @export
StatMHMMR <- setRefClass(
  "StatMHMMR",
  fields = list(
    tau_tk = "matrix", # tau_tk: smoothing probs: [nxK], tau_tk(t,k) = Pr(z_i=k | y1...yn)
    alpha_tk = "matrix", # alpha_tk: [nxK], forwards probs: Pr(y1...yt,zt=k)
    beta_tk = "matrix", # beta_tk: [nxK], backwards probs: Pr(yt+1...yn|zt=k)
    xi_tkl = "array", # xi_tkl: [(n-1)xKxK], joint post probs : xi_tk\elll(t,k,\ell)  = Pr(z_t=k, z_{t-1}=\ell | Y) t =2,..,n
    f_tk = "matrix", # f_tk: [nxK] f(yt|zt=k)
    log_f_tk = "matrix", # log_f_tk: [nxK] log(f(yt|zt=k))
    loglik = "numeric", # loglik: log-likelihood at convergence
    stored_loglik = "list", # stored_loglik: stored log-likelihood values during EM
    cputime = "numeric", # cputime: for the best run
    klas = "matrix", # klas: [nx1 double]
    z_ik = "matrix", # z_ik: [nxK]
    state_probs = "matrix", # state_probs: [nxK]
    BIC = "numeric", # BIC
    AIC = "numeric", # AIC
    regressors = "array", # regressors: [nxK]
    predict_prob = "matrix", # predict_prob: [nxK]: Pr(zt=k|y1...y_{t-1})
    predicted = "matrix", # predicted: [nx1]
    filter_prob = "matrix", # filter_prob: [nxK]: Pr(zt=k|y1...y_t)
    filtered = "matrix", # filtered: [nx1]
    smoothed_regressors = "array", # smoothed_regressors: [nxK]
    smoothed = "matrix" # smoothed: [nx1]
  ),
  methods = list(

    initialize = function(paramMHMMR = ParamMHMMR(fData = FData(numeric(1), matrix(1)), K = 2, p = 2, variance_type = 1)) {

      tau_tk <<- matrix(NA, paramMHMMR$fData$n, paramMHMMR$K) # tau_tk: smoothing probs: [nxK], tau_tk(t,k) = Pr(z_i=k | y1...yn)
      alpha_tk <<- matrix(NA, paramMHMMR$fData$n, ncol = paramMHMMR$K) # alpha_tk: [nxK], forwards probs: Pr(y1...yt,zt=k)
      beta_tk <<- matrix(NA, paramMHMMR$fData$n, paramMHMMR$K) # beta_tk: [nxK], backwards probs: Pr(yt+1...yn|zt=k)
      xi_tkl <<- array(NA, c(paramMHMMR$K, paramMHMMR$K, paramMHMMR$fData$n - 1)) # xi_tkl: [(n-1)xKxK], joint post probs : xi_tk\elll(t,k,\ell)  = Pr(z_t=k, z_{t-1}=\ell | Y) t =2,..,n
      f_tk <<- matrix(NA, paramMHMMR$fData$n, paramMHMMR$K) # f_tk: [nxK] f(yt|zt=k)
      log_f_tk <<- matrix(NA, paramMHMMR$fData$n, paramMHMMR$K) # log_f_tk: [nxK] log(f(yt|zt=k))
      loglik <<- -Inf # loglik: log-likelihood at convergence
      stored_loglik <<- list() # stored_loglik: stored log-likelihood values during EM
      cputime <<- Inf # cputime: for the best run
      klas <<- matrix(NA, paramMHMMR$fData$n, 1) # klas: [nx1 double]
      z_ik <<- matrix(NA, paramMHMMR$fData$n, paramMHMMR$K) # z_ik: [nxK]
      state_probs <<- matrix(NA, paramMHMMR$fData$n, paramMHMMR$K) # state_probs: [nxK]
      BIC <<- -Inf # BIC
      AIC <<- -Inf # AIC
      regressors <<- array(NA, dim = c(paramMHMMR$fData$n, paramMHMMR$fData$m, paramMHMMR$K)) # regressors: [nxxK]
      predict_prob <<- matrix(NA, paramMHMMR$fData$n, paramMHMMR$K) # predict_prob: [nxK]: Pr(zt=k|y1...y_{t-1})
      predicted <<- matrix(NA, paramMHMMR$fData$n, paramMHMMR$fData$m) # predicted: [nx1]
      filter_prob <<- matrix(NA, paramMHMMR$fData$n, paramMHMMR$K) # filter_prob: [nxK]: Pr(zt=k|y1...y_t)
      filtered <<- matrix(NA, paramMHMMR$fData$n, paramMHMMR$fData$m) # filtered: [nx1]
      smoothed_regressors <<- array(NA, dim = c(paramMHMMR$fData$n, paramMHMMR$fData$m, paramMHMMR$K)) # smoothed_regressors: [nxK]
      smoothed <<- matrix(NA, paramMHMMR$fData$n, paramMHMMR$fData$m) # smoothed: [nx1]


    },

    MAP = function() {
      N <- nrow(tau_tk)
      K <- ncol(tau_tk)
      ikmax <- max.col(tau_tk)
      ikmax <- matrix(ikmax, ncol = 1)
      z_ik <<- ikmax %*% ones(1, K) == ones(N, 1) %*% (1:K) # Partition_MAP
      klas <<- ones(N, 1)
      for (k in 1:K) {
        klas[z_ik[, k] == 1] <<- k
      }
    },

    computeLikelihood = function(paramMHMMR) {
      fb <- forwardsBackwards(paramMHMMR$prior, paramMHMMR$trans_mat, t(f_tk))
      loglik <<- fb$loglik

    },

    computeStats = function(paramMHMMR, cputime_total) {
      cputime <<- mean(cputime_total)

      # State sequence prob p(z_1,...,z_n;\pi,A)
      state_probs <<- hmmProcess(paramMHMMR$prior, paramMHMMR$trans_mat, paramMHMMR$fData$n)

      # BIC, AIC, ICL
      BIC <<- loglik - paramMHMMR$nu * log(paramMHMMR$fData$n) / 2
      AIC <<- loglik - paramMHMMR$nu

      # # CL(theta) : Completed-data loglikelihood
      # sum_t_log_Pz_ftk = sum(hmmr.stats.Zik.*log(state_probs.*hmmr.stats.f_tk), 2);
      # comp_loglik = sum(sum_t_log_Pz_ftk(K:end));
      # hmmr.stats.comp_loglik = comp_loglik;
      # hmmr.stats.ICL = comp_loglik - (nu*log(m)/2);

      # Predicted, filtered, and smoothed time series
      for (k in 1:paramMHMMR$K) {
        regressors[, , k] <<- paramMHMMR$phi %*% paramMHMMR$beta[, , k]
      }

      # Prediction probabilities = Pr(z_t|y_1,...,y_{t-1})
      predict_prob[1, ] <<- paramMHMMR$prior # t=1 p (z_1)
      predict_prob[2:paramMHMMR$fData$n, ] <<- (alpha_tk[(1:(paramMHMMR$fData$n - 1)), ] %*% paramMHMMR$trans_mat) / (apply(alpha_tk[(1:(paramMHMMR$fData$n - 1)), ], 1, sum) %*% matrix(1, 1, paramMHMMR$K)) # t = 2,...,n

      # Predicted observations
      predictedk <- array(NA, dim = c(paramMHMMR$fData$n, paramMHMMR$fData$m, paramMHMMR$K))
      for (k in 1:paramMHMMR$K) {
        predictedk[, , k] <- (predict_prob[, k] %*% ones(1, paramMHMMR$fData$m)) * regressors[, , k] # Weighted by prediction probabilities
      }
      predicted <<- apply(predictedk, c(1, 2), sum)

      # Filtering probabilities = Pr(z_t|y_1,...,y_t)
      filter_prob <<- alpha_tk / (apply(alpha_tk, 1, sum) %*% matrix(1, 1, paramMHMMR$K)) # Normalize(alpha_tk,2);

      # Filetered observations
      filteredk <- array(NA, dim = c(paramMHMMR$fData$n, paramMHMMR$fData$m, paramMHMMR$K))
      for (k in 1:paramMHMMR$K) {
        filteredk[, , k] <- (filter_prob[, k] %*% ones(1, paramMHMMR$fData$m)) * regressors[, , k] # Weighted by filtering probabilities
      }
      filtered <<- apply(filteredk, c(1, 2), sum)

      # Smoothed observations
      # smoothed_regressors <<- (tau_tk %*% ones(1, paramMHMMR$fData$m)) * regressors
      smoothedk <- array(NA, dim = c(paramMHMMR$fData$n, paramMHMMR$fData$m, paramMHMMR$K))
      for (k in 1:paramMHMMR$K) {
        smoothedk[, , k] <- (tau_tk[, k] %*% ones(1, paramMHMMR$fData$m)) * regressors[, , k]
      }
      smoothed <<- apply(smoothedk, c(1, 2), sum)

    },

    EStep = function(paramMHMMR) {
      muk <- array(0, dim = c(paramMHMMR$fData$n, paramMHMMR$fData$m, paramMHMMR$K))

      # Observation likelihoods
      for (k in 1:paramMHMMR$K) {
        mk <- paramMHMMR$phi %*% paramMHMMR$beta[, , k] # The regressors means
        muk[, , k] <- mk

        if (paramMHMMR$variance_type == variance_types$homoskedastic) {
          sk <- paramMHMMR$sigma2
        }
        else{
          sk <- paramMHMMR$sigma2[, , k]
        }
        z <- (paramMHMMR$fData$Y - mk) %*% solve(sk) * (paramMHMMR$fData$Y - mk)
        mahalanobis <- rowSums(z)
        denom <- (2 * pi) ^ (paramMHMMR$fData$m / 2) * (det(sk)) ^ (1 / 2)
        log_f_tk[, k] <<- -ones(paramMHMMR$fData$n, 1) * log(denom) - 0.5 * mahalanobis

      }

      log_f_tk <<- pmin(log_f_tk, log(.Machine$double.xmax))
      log_f_tk <<- pmax(log_f_tk, log(.Machine$double.xmin))

      f_tk <<- exp(log_f_tk)

      fb <- forwardsBackwards(paramMHMMR$prior, paramMHMMR$trans_mat, t(f_tk))

      tau_tk <<- t(fb$tau_tk)
      xi_tkl <<- fb$xi_tkl
      alpha_tk <<- t(fb$alpha_tk)
      beta_tk <<- t(fb$beta_tk)
      loglik <<- fb$loglik

    }
  )
)
