#' @export
ModelMHMMR <- setRefClass(
  "ModelMHMMR",
  fields = list(
    paramMHMMR = "ParamMHMMR",
    statMHMMR = "StatMHMMR"
  ),
  methods = list(
    plot = function(what = c("predicted", "filtered", "smoothed", "regressors")) {

      what <- match.arg(what, several.ok = TRUE)

      oldpar <- par()[c("mfrow", "mai", "mgp")]
      on.exit(par(oldpar), add = TRUE)

      yaxislim <- c(min(paramMHMMR$fData$Y) - 2 * mean(sqrt(apply(paramMHMMR$fData$Y, 2, var))), max(paramMHMMR$fData$Y) + 2 * mean(sqrt(apply(paramMHMMR$fData$Y, 2, var))))

      colorsvec <- rainbow(paramMHMMR$K)

      if (any(what == "predicted")) {
        # Predicted time series and predicted regime probabilities
        par(mfrow = c(2, 1), mai = c(0.6, 1, 0.5, 0.5), mgp = c(2, 1, 0))
        matplot(paramMHMMR$fData$X, paramMHMMR$fData$Y, type = "l", ylim = yaxislim, xlab = "x", ylab = "y", col = gray.colors(paramMHMMR$fData$m), lty = 1)
        title(main = "Original and predicted HMMR time series")

        for (d in 1:paramMHMMR$fData$m) {
            lines(paramMHMMR$fData$X, statMHMMR$predicted[, d], col = "red", lwd = 1.5)
        }

        # Prediction probabilities of the hidden process (segmentation)
        plot.default(paramMHMMR$fData$X, statMHMMR$predict_prob[, 1], type = "l", xlab = "x", ylab = expression('P(Z'[t] == k ~ '|' ~ list(y[1],..., y[t - 1]) ~ ')'), col = colorsvec[1], lwd = 1.5, main = "Prediction probabilities", ylim = c(0, 1))
        for (k in 2:paramMHMMR$K) {
          lines(paramMHMMR$fData$X, statMHMMR$predict_prob[, k], col = colorsvec[k], lwd = 1.5) # Post Probs: Pr(Z_{t}=k|y_1,\ldots,y_{t-1})
        }
      }

      if (any(what == "filtered")) {
        # Filtered time series and filtering regime probabilities
        par(mfrow = c(2, 1), mai = c(0.6, 1, 0.5, 0.5), mgp = c(2, 1, 0))
        matplot(paramMHMMR$fData$X, paramMHMMR$fData$Y, type = "l", ylim = yaxislim, xlab = "x", ylab = "y", col = gray.colors(paramMHMMR$fData$m), lty = 1)
        title(main = "Original and filtered HMMR time series")
        for (d in 1:paramMHMMR$fData$m) {
          lines(paramMHMMR$fData$X, statMHMMR$filtered[, d], col = "red", lwd = 1.5)
        }

        # Filtering probabilities of the hidden process (segmentation)
        plot.default(paramMHMMR$fData$X, statMHMMR$filter_prob[, 1], type = "l", xlab = "x", ylab = expression('P(Z'[t] == k ~ '|' ~ list(y[1],..., y[t]) ~ ')'), col = colorsvec[1], lwd = 1.5, main = "Filtering probabilities", ylim = c(0, 1))
        for (k in 2:paramMHMMR$K) {
          lines(paramMHMMR$fData$X, statMHMMR$filter_prob[, k], col = colorsvec[k], lwd = 1.5) # Post Probs: Pr(Z_{t}=k|y_1,\ldots,y_t)
        }
      }

      if (any(what == "regressors")) {
        # Data, regressors, and segmentation
        par(mfrow = c(2, 1), mai = c(0.6, 1, 0.5, 0.5), mgp = c(2, 1, 0))
        matplot(paramMHMMR$fData$X, paramMHMMR$fData$Y, type = "l", ylim = yaxislim, xlab = "x", ylab = "y", col = gray.colors(paramMHMMR$fData$m), lty = 1)
        title(main = "Time series, MHMMR regimes, and smoothing probabilites")
        for (k in 1:paramMHMMR$K) {
          model_k <- statMHMMR$regressors[, , k]

          index <- statMHMMR$klas == k
          active_model_k <- model_k[index, ] # prob_model_k >= prob);
          active_period_model_k <- paramMHMMR$fData$X[index] # prob_model_k >= prob);

          if (length(active_model_k) != 0) {
            for (d in 1:paramMHMMR$fData$m) {
              lines(paramMHMMR$fData$X, model_k[, d], col = colorsvec[k], lty = "dotted", lwd = 1)
              lines(active_period_model_k, active_model_k[, d], col = colorsvec[k], lwd = 1.5)
            }
          }
        }

        # Probablities of the hidden process (segmentation)
        plot.default(paramMHMMR$fData$X, statMHMMR$tau_tk[, 1], type = "l", xlab = "x", ylab = expression('P(Z'[t] == k ~ '|' ~ list(y[1],..., y[n]) ~ ')'), col = colorsvec[1], lwd = 1.5, main = "Smoothing probabilities", ylim = c(0, 1))
        if (paramMHMMR$K > 1) {
          for (k in 2:paramMHMMR$K) {
            lines(paramMHMMR$fData$X, statMHMMR$tau_tk[, k], col = colorsvec[k], lwd = 1.5) # Post Probs: Pr(Z_{t}=k|y_1,\ldots,y_n)
          }
        }
      }

      if (any(what == "smoothed")) {
        # Data, regression model, and segmentation
        par(mfrow = c(2, 1), mai = c(0.6, 1, 0.5, 0.5), mgp = c(2, 1, 0))
        matplot(paramMHMMR$fData$X, paramMHMMR$fData$Y, type = "l", ylim = yaxislim, xlab = "x", ylab = "y", col = gray.colors(paramMHMMR$fData$m), lty = 1)
        title(main = "Original, smoothed HMMR time series, and segmentation")
        for (d in 1:paramMHMMR$fData$m) {
          lines(paramMHMMR$fData$X, statMHMMR$smoothed[, d], col = "red" , lwd = 1.5)
        }

        # Transition time points
        tk <- which(diff(statMHMMR$klas) != 0)
        for (i in 1:length(tk)) {
          abline(v = paramMHMMR$fData$X[tk[i]], col = "red", lty = "dotted", lwd = 1.5)
        }

        # Probablities of the hidden process (segmentation)
        plot.default(paramMHMMR$fData$X, statMHMMR$klas, type = "l", xlab = "x", ylab = "Estimated class labels", col = "red", lwd = 1.5, yaxt = "n")
        axis(side = 2, at = 1:paramMHMMR$K)
      }
    },

    summary = function() {

      digits = getOption("digits")

      title <- paste("Fitted MHMMR model")
      txt <- paste(rep("-", min(nchar(title) + 4, getOption("width"))), collapse = "")

      # Title
      cat(txt)
      cat("\n")
      cat(title)
      cat("\n")
      cat(txt)

      cat("\n")
      cat("\n")
      cat(paste0("MHMMR model with K = ", paramMHMMR$K, ifelse(paramMHMMR$K > 1, " regimes", " regime")))
      cat("\n")
      cat("\n")

      tab <- data.frame("log-likelihood" = statMHMMR$loglik, "nu" = paramMHMMR$nu, "AIC" = statMHMMR$AIC,
                        "BIC" = statMHMMR$BIC, row.names = "", check.names = FALSE)
      print(tab, digits = digits)

      cat("\nClustering table:")
      print(table(statMHMMR$klas))

      cat("\n\n")

      txt <- paste(rep("-", min(nchar(title), getOption("width"))), collapse = "")

      for (k in 1:paramMHMMR$K) {
        cat(txt)
        cat("\nRegime ", k, " (K = ", k, "):\n", sep = "")

        cat("\nRegression coefficients:\n\n")
        if (paramMHMMR$p > 0) {
          row.names = c("1", sapply(1:paramMHMMR$p, function(x) paste0("X^", x)))
        } else {
          row.names = "1"
        }

        betas <- data.frame(paramMHMMR$beta[, , k], row.names = row.names)
        colnames(betas) <- sapply(1:paramMHMMR$fData$m, function(x) paste0("Beta(d = ", x, ")"))
        print(betas, digits = digits)

        if (paramMHMMR$variance_type == "heteroskedastic") {
          cat("\nCovariance matrix:\n")
          sigma2 <- data.frame(paramMHMMR$sigma2[, , k])
          colnames(sigma2) <- NULL
          print(sigma2, digits = digits, row.names = FALSE)
        }
      }

      if (paramMHMMR$variance_type == "homoskedastic") {
        cat("\n")
        txt <- paste(rep("-", min(nchar(title), getOption("width"))), collapse = "")
        cat(txt)
        cat("\nCommon covariance matrix:\n")
        cat(txt)
        sigma2 <- data.frame(paramMHMMR$sigma2)
        colnames(sigma2) <- NULL
        print(sigma2, digits = digits, row.names = FALSE)
      }

    }
  )
)
