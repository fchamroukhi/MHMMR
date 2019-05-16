EM <- function(modelMHMMR, n_tries = 1, max_iter = 1500, threshold = 1e-6, verbose = FALSE) {
  # learn_mhmmr learn a Regression model with a Hidden Markov Process (MHMMR)
  # for modeling and segmentation of a time series with regime changes.
  # The learning is performed by the EM (Baum-Welch) algorithm.
  #
  #
  # Inputs :
  #
  #          (x,y) : a time series composed of m points : dim(y)=[m d]
  #                * Each curve is observed during the interval [0,T], i.e x =[t_1,...,t_m]
  #
  #           K : Number of polynomial regression components (regimes)
  #          	p : degree of the polynomials
  #
  # Outputs :
  #
  #         mhmmr: the estimated MHMMR model. a structure composed of:
  #
  #         prior: [Kx1]: prior(k) = Pr(z_1=k), k=1...K
  #         trans_mat: [KxK], trans_mat(\ell,k) = Pr(z_t = k|z_{t-1}=\ell)
  #         reg_param: the paramters of the regressors:
  #                 betak: regression coefficients
  #                 sigmak (or sigma2) : the variance(s)
  #         Stats:
  #           tau_tk: smoothing probs: [nxK], tau_tk(t,k) = Pr(z_i=k | y1...yn)
  #           alpha_tk: [nxK], forwards probs: Pr(y1...yt,zt=k)
  #           beta_tk: [nxK], backwards probs: Pr(yt+1...yn|zt=k)
  #           xi_tkl: [(n-1)xKxK], joint post probs : xi_tk\elll(t,k,\ell)  = Pr(z_t=k, z_{t-1}=\ell | Y) t =2,..,n
  #           X: [nx(p+1)] regression design matrix
  #           nu: model complexity
  #           parameter_vector
  #           f_tk: [nxK] f(yt|zt=k)
  #           log_f_tk: [nxK] log(f(yt|zt=k))
  #           loglik: log-likelihood at convergence
  #           stored_loglik: stored log-likelihood values during EM
  #           cputime: for the best run
  #           cputime_total: for all the EM runs
  #           klas: [nx1 double]
  #           Zik: [nxK]
  #           state_probs: [nxK]
  #           BIC: -2.1416e+03
  #           AIC: -2.0355e+03
  #           regressors: [nxK]
  #           predict_prob: [nxK]: Pr(zt=k|y1...y_{t-1})
  #           predicted: [nx1]
  #           filter_prob: [nxK]: Pr(zt=k|y1...y_t)
  #           filtered: [nx1]
  #           smoothed_regressors: [nxK]
  #           smoothed: [nx1]
  #
  #
  #Faicel Chamroukhi, sept 2008
  #
  ## Please cite the following papers for this code:
  #
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
  # 	url  = {https://chamroukhi.com/papers/Chamroukhi-MHMMR-IeeeTase.pdf}
  # 	}
  #
  ##########################################################################################

  phi <- designmatrix(x = modelMHMMR$X, p = modelMHMMR$p)$XBeta

  nb_good_try <- 0
  total_nb_try <- 0
  best_loglik <- -Inf
  cputime_total <- c()

  while (nb_good_try < n_tries) {
    start_time <- Sys.time()

    if (n_tries > 1) {
      print(paste("EM try n?", (nb_good_try + 1)))
    }
    total_nb_try <- total_nb_try + 1

    ## EM Initializaiton step
    ## Initialization of the Markov chain params, the regression coeffs, and the variance(s)
    param <- ParamMHMMR(modelMHMMR)
    param$init_mhmmr(modelMHMMR, phi, nb_good_try + 1)

    iter <- 0
    prev_loglik <- -Inf
    converged <- FALSE
    top <- 0

    stat <- StatMHMMR(modelMHMMR)

    while ((iter <= max_iter) && !converged) {

      ## E step : calculate tge tau_tk (p(Zt=k|y1...ym;theta)) and xi t_kl (and the log-likelihood) by
      #  forwards backwards (computes the alpha_tk et beta_tk)
      stat$EStep(modelMHMMR, param, phi)

      ## M step
      param$MStep(modelMHMMR, stat, phi)

      ## End of an EM iteration

      iter <-  iter + 1

      # test of convergence
      lambda <- 1e-5 # if a bayesian prior on the beta's
      stat$loglik <- stat$loglik + log(lambda)

      if (verbose) {
        print(paste('HMM_regression | EM   : Iteration :', iter, ' Log-likelihood : ', stat$loglik))
      }

      if ((prev_loglik - stat$loglik) > 1e-4) {
        top <- top + 1
        if (top == 10) {
          stop(print(paste('!!!!! The loglikelihood is decreasing from', prev_loglik, ' to ', stat$loglik)))
        }
      }

      converged <- (abs(stat$loglik - prev_loglik) / abs(prev_loglik) < threshold)
      if (is.na(converged)) {
        converged <- FALSE
      } # Basically for the first iteration when prev_loglik is Inf

      prev_loglik <- stat$loglik
      stat$stored_loglik[iter] <- stat$loglik

    } # END EM LOOP

    cputime_total[nb_good_try + 1] <- Sys.time() - start_time

    # at this point we have computed param and stat that contains all the information

    if (n_tries > 1) {
      print(paste('loglik_max = ', stat$loglik))
    }

    if (length(param$beta) != 0) {
      nb_good_try <- nb_good_try + 1
      total_nb_try <- 0

      if (stat$loglik > best_loglik) {

        statSolution <- stat$copy()
        paramSolution <- param$copy()

        best_loglik <- stat$loglik
      }
    }

    if (total_nb_try > 500) {
      stop(paste("can't obtain the requested number of classes"))
    }

  }

  if (n_tries > 1) {
    print(paste('best_loglik:  ', statSolution$loglik))
  }

  # Smoothing state sequences : argmax(smoothing probs), and corresponding binary allocations partition
  statSolution$MAP()

  # FINISH computation of statSolution
  statSolution$computeStats(modelMHMMR, paramSolution, phi, cputime_total)

  return(FittedMHMMR(modelMHMMR, paramSolution, statSolution))
}
