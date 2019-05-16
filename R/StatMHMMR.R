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
    #           X: [nx(p+1)] regression design matrix
    #           nu: model complexity
    #           parameter_vector
  ),
  methods = list(
    MAP = function() {
      N <- nrow(tau_tk)
      K <- ncol(tau_tk)
      ikmax <- max.col(tau_tk)
      ikmax <- matrix(ikmax, ncol = 1)
      z_ik <<- ikmax %*% ones(1, K) == ones(N, 1) %*% (1:K) # partition_MAP
      klas <<- ones(N, 1)
      for (k in 1:K) {
        klas[z_ik[, k] == 1] <<- k
      }
    },
    # #######
    # # compute loglikelihood
    # #######
    computeLikelihood = function(paramMHMMR) {
      fb <- forwards_backwards(paramMHMMR$prior, paramMHMMR$trans_mat, t(f_tk))
      loglik <<- fb$loglik

    },
    #######
    # compute the final solution stats
    #######
    computeStats = function(modelMHMMR, paramMHMMR, phi, cputime_total) {
      cputime <<- mean(cputime_total)

      # ## sate sequence prob p(z_1,...,z_n;\pi,A)
      state_probs <<- hmm_process(paramMHMMR$prior, paramMHMMR$trans_mat, modelMHMMR$n)

      ### BIC, AIC, ICL
      BIC <<- loglik - modelMHMMR$nu * log(modelMHMMR$n) / 2
      AIC <<- loglik - modelMHMMR$nu

      # # CL(theta) : Completed-data loglikelihood
      # sum_t_log_Pz_ftk = sum(hmmr.stats.Zik.*log(state_probs.*hmmr.stats.f_tk), 2);
      # comp_loglik = sum(sum_t_log_Pz_ftk(K:end));
      # hmmr.stats.comp_loglik = comp_loglik;
      # hmmr.stats.ICL = comp_loglik - (nu*log(m)/2);

      ## Predicted, filtered, and smoothed time series
      for (k in 1:modelMHMMR$K) {
        regressors[, , k] <<- phi %*% paramMHMMR$beta[, , k]
      }

      # Prediction probabilities = Pr(z_t|y_1,...,y_{t-1})
      predict_prob[1, ] <<- paramMHMMR$prior # t=1 p (z_1)
      predict_prob[2:modelMHMMR$n, ] <<- (alpha_tk[(1:(modelMHMMR$n - 1)), ] %*% paramMHMMR$trans_mat) / (apply(alpha_tk[(1:(modelMHMMR$n - 1)), ], 1, sum) %*% matrix(1, 1, modelMHMMR$K)) # t = 2,...,n

      # Predicted observations
      predictedk <- array(NA, dim = c(modelMHMMR$n, modelMHMMR$m, modelMHMMR$K))
      for (k in 1:modelMHMMR$K) {
        predictedk[, , k] <- (predict_prob[, k] %*% ones(1, modelMHMMR$m)) * regressors[, , k] # Weighted by prediction probabilities
      }
      predicted <<- apply(predictedk, c(1, 2), sum)

      # Filtering probabilities = Pr(z_t|y_1,...,y_t)
      filter_prob <<- alpha_tk / (apply(alpha_tk, 1, sum) %*% matrix(1, 1, modelMHMMR$K)) # Normalize(alpha_tk,2);

      # Filetered observations
      filteredk <- array(NA, dim = c(modelMHMMR$n, modelMHMMR$m, modelMHMMR$K))
      for (k in 1:modelMHMMR$K) {
        filteredk[, , k] <- (filter_prob[, k] %*% ones(1, modelMHMMR$m)) * regressors[, , k] # Weighted by filtering probabilities
      }
      filtered <<- apply(filteredk, c(1, 2), sum)

      # Smoothed observations
      # smoothed_regressors <<- (tau_tk %*% ones(1, modelMHMMR$m)) * regressors
      smoothedk <- array(NA, dim = c(modelMHMMR$n, modelMHMMR$m, modelMHMMR$K))
      for (k in 1:modelMHMMR$K) {
        smoothedk[, , k] <- (tau_tk[, k] %*% ones(1, modelMHMMR$m)) * regressors[, , k]
      }
      smoothed <<- apply(smoothedk, c(1, 2), sum)

    },
    #######
    # EStep
    #######
    EStep = function(modelMHMMR, paramMHMMR, phi) {
      muk <- array(0, dim = c(modelMHMMR$n, modelMHMMR$m, modelMHMMR$K))

      # observation likelihoods
      for (k in 1:modelMHMMR$K) {
        mk <- phi %*% paramMHMMR$beta[, , k] # the regressors means
        muk[, , k] <- mk

        if (modelMHMMR$variance_type == variance_types$homoskedastic) {
          sk <- paramMHMMR$sigma
        }
        else{
          sk <- paramMHMMR$sigma[, , k]
        }
        z <- (modelMHMMR$Y - mk) %*% solve(sk) * (modelMHMMR$Y - mk)
        mahalanobis <- rowSums(z)
        denom <- (2 * pi) ^ (modelMHMMR$m / 2) * (det(sk)) ^ (1 / 2)
        log_f_tk[, k] <<- -ones(modelMHMMR$n, 1) * log(denom) - 0.5 * mahalanobis

      }

      log_f_tk <<- pmin(log_f_tk, log(.Machine$double.xmax))
      log_f_tk <<- pmax(log_f_tk, log(.Machine$double.xmin))

      f_tk <<- exp(log_f_tk)

      fb <- forwards_backwards(paramMHMMR$prior, paramMHMMR$trans_mat, t(f_tk))

      tau_tk <<- t(fb$tau_tk)
      xi_tkl <<- fb$xi_tkl
      alpha_tk <<- t(fb$alpha_tk)
      beta_tk <<- t(fb$beta_tk)
      loglik <<- fb$loglik

    }
  )
)


StatMHMMR <- function(modelMHMMR) {
  tau_tk <- matrix(NA, modelMHMMR$n, modelMHMMR$K) # tau_tk: smoothing probs: [nxK], tau_tk(t,k) = Pr(z_i=k | y1...yn)
  alpha_tk <- matrix(NA, modelMHMMR$n, ncol = modelMHMMR$K) # alpha_tk: [nxK], forwards probs: Pr(y1...yt,zt=k)
  beta_tk <- matrix(NA, modelMHMMR$n, modelMHMMR$K) # beta_tk: [nxK], backwards probs: Pr(yt+1...yn|zt=k)
  xi_tkl <- array(NA, c(modelMHMMR$K, modelMHMMR$K, modelMHMMR$n - 1)) # xi_tkl: [(n-1)xKxK], joint post probs : xi_tk\elll(t,k,\ell)  = Pr(z_t=k, z_{t-1}=\ell | Y) t =2,..,n
  f_tk <- matrix(NA, modelMHMMR$n, modelMHMMR$K) # f_tk: [nxK] f(yt|zt=k)
  log_f_tk <- matrix(NA, modelMHMMR$n, modelMHMMR$K) # log_f_tk: [nxK] log(f(yt|zt=k))
  loglik <- -Inf # loglik: log-likelihood at convergence
  stored_loglik <- list() # stored_loglik: stored log-likelihood values during EM
  cputime <- Inf # cputime: for the best run
  klas <- matrix(NA, modelMHMMR$n, 1) # klas: [nx1 double]
  z_ik <- matrix(NA, modelMHMMR$n, modelMHMMR$K) # z_ik: [nxK]
  state_probs <- matrix(NA, modelMHMMR$n, modelMHMMR$K) # state_probs: [nxK]
  BIC <- -Inf # BIC
  AIC <- -Inf # AIC
  regressors <- array(NA, dim = c(modelMHMMR$n, modelMHMMR$m, modelMHMMR$K)) # regressors: [nxxK]
  predict_prob <- matrix(NA, modelMHMMR$n, modelMHMMR$K) # predict_prob: [nxK]: Pr(zt=k|y1...y_{t-1})
  predicted <- matrix(NA, modelMHMMR$n, modelMHMMR$m) # predicted: [nx1]
  filter_prob <- matrix(NA, modelMHMMR$n, modelMHMMR$K) # filter_prob: [nxK]: Pr(zt=k|y1...y_t)
  filtered <- matrix(NA, modelMHMMR$n, modelMHMMR$m) # filtered: [nx1]
  smoothed_regressors <- array(NA, dim = c(modelMHMMR$n, modelMHMMR$m, modelMHMMR$K)) # smoothed_regressors: [nxK]
  smoothed <- matrix(NA, modelMHMMR$n, modelMHMMR$m) # smoothed: [nx1]

  new(
    "StatMHMMR",
    tau_tk = tau_tk,
    alpha_tk = alpha_tk,
    beta_tk = beta_tk,
    xi_tkl = xi_tkl,
    f_tk = f_tk,
    log_f_tk = log_f_tk,
    loglik = loglik,
    stored_loglik = stored_loglik,
    cputime = cputime,
    klas = klas,
    z_ik = z_ik,
    state_probs = state_probs,
    BIC = BIC,
    AIC = AIC,
    regressors = regressors,
    predict_prob = predict_prob,
    predicted = predicted,
    filter_prob = filter_prob,
    filtered = filtered,
    smoothed_regressors = smoothed_regressors,
    smoothed = smoothed
  )
}
