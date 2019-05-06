FittedMHMMR <- setRefClass(
  "FittedMHMMR",
  fields = list(
    modelMHMMR = "ModelMHMMR",
    paramMHMMR = "ParamMHMMR",
    statMHMMR = "StatMHMMR"
  ),
  methods = list(
    plot = function() {

      oldpar <- par()[c("mfrow", "mai", "mgp")]
      on.exit(par(oldpar), add = TRUE)

      yaxislim <- c(min(modelMHMMR$Y) - 2 * mean(sqrt(apply(modelMHMMR$Y, 2, var))), max(modelMHMMR$Y) + 2 * mean(sqrt(apply(modelMHMMR$Y, 2, var))))

      # Predicted time series and predicted regime probabilities
      par(mfrow = c(2, 1), mai = c(0.6, 1, 0.5, 0.5), mgp = c(2, 1, 0))
      matplot(modelMHMMR$Y, type = "l", ylim = yaxislim, xlab = "x", ylab = "y", col = gray.colors(modelMHMMR$m), lty = 1)
      title(main = "Original and predicted HMMR time series")

      for (d in 1:modelMHMMR$m) {
          lines(statMHMMR$predicted[, d], col = "red", lwd = 1.5)
      }

      # Prediction probabilities of the hidden process (segmentation)
      colorsvec <- rainbow(modelMHMMR$K)
      plot.default(statMHMMR$predict_prob[, 1], type = "l", xlab = "x", ylab = expression('P(Z'[t] == k ~ '|' ~ list(y[1],..., y[t - 1]) ~ ')'), col = colorsvec[1], lwd = 1.5, main = "Prediction probabilities")
      for (k in 2:modelMHMMR$K) {
        lines(statMHMMR$predict_prob[, k], col = colorsvec[k], lwd = 1.5) # Post Probs: Pr(Z_{t}=k|y_1,\ldots,y_{t-1})
      }

      # Filtered time series and filtering regime probabilities
      par(mfrow = c(2, 1), mai = c(0.6, 1, 0.5, 0.5), mgp = c(2, 1, 0))
      matplot(modelMHMMR$Y, type = "l", ylim = yaxislim, xlab = "x", ylab = "y", col = gray.colors(modelMHMMR$m), lty = 1)
      title(main = "Original and filtered HMMR time series")
      for (d in 1:modelMHMMR$m) {
        lines(statMHMMR$filtered[, d], col = "red", lwd = 1.5)
      }

      # Filtering probabilities of the hidden process (segmentation)
      plot.default(statMHMMR$filter_prob[, 1], type = "l", xlab = "x", ylab = expression('P(Z'[t] == k ~ '|' ~ list(y[1],..., y[t]) ~ ')'), col = colorsvec[1], lwd = 1.5, main = "Filtering probabilities")
      for (k in 2:modelMHMMR$K) {
        lines(statMHMMR$filter_prob[, k], col = colorsvec[k], lwd = 1.5) # Post Probs: Pr(Z_{t}=k|y_1,\ldots,y_t)
      }

      # Data, regressors, and segmentation
      par(mfrow = c(2, 1), mai = c(0.6, 1, 0.5, 0.5), mgp = c(2, 1, 0))
      matplot(modelMHMMR$Y, type = "l", ylim = yaxislim, xlab = "x", ylab = "y", col = gray.colors(modelMHMMR$m), lty = 1)
      title(main = "Time series, MHMMR regimes, and smoothing probabilites")
      for (k in 1:modelMHMMR$K) {
        model_k <- statMHMMR$regressors[, , k]

        index <- statMHMMR$klas == k
        active_model_k <- model_k[index, ] # prob_model_k >= prob);
        active_period_model_k <- seq(1:modelMHMMR$n)[index] # prob_model_k >= prob);

        if (length(active_model_k) != 0) {
          for (d in 1:modelMHMMR$m) {
            lines(model_k[, d], col = colorsvec[k], lty = "dotted", lwd = 1)
            lines(active_period_model_k, active_model_k[, d], col = colorsvec[k], lwd = 1.5)
          }
        }
      }

      # Probablities of the hidden process (segmentation)
      plot.default(statMHMMR$tau_tk[, 1], type = "l", xlab = "x", ylab = expression('P(Z'[t] == k ~ '|' ~ list(y[1],..., y[n]) ~ ')'), col = colorsvec[1], lwd = 1.5, main = "Smoothing probabilities")
      if (modelMHMMR$K > 1) {
        for (k in 2:modelMHMMR$K) {
          lines(statMHMMR$tau_tk[, k], col = colorsvec[k], lwd = 1.5) # Post Probs: Pr(Z_{t}=k|y_1,\ldots,y_n)
        }
      }

      # Data, regression model, and segmentation
      par(mfrow = c(2, 1), mai = c(0.6, 1, 0.5, 0.5), mgp = c(2, 1, 0))
      matplot(modelMHMMR$Y, type = "l", ylim = yaxislim, xlab = "x", ylab = "y", col = gray.colors(modelMHMMR$m), lty = 1)
      title(main = "Original, smoothed HMMR time series, and segmentation")
      for (d in 1:modelMHMMR$m) {
        lines(statMHMMR$smoothed[, d], col = "red" , lwd = 1.5)
      }

      # Transition time points
      tk <- which(diff(statMHMMR$klas) != 0)
      for (i in 1:length(tk)) {
        abline(v = tk[i], col = "red", lty = "dotted", lwd = 1.5)
      }

      # Probablities of the hidden process (segmentation)
      plot.default(statMHMMR$klas, type = "l", xlab = "x", ylab = "Estimated class labels", col = "red", lwd = 1.5)
    }
  )
)

FittedMHMMR <- function(modelMHMMR, paramMHMMR, statMHMMR) {
  new("FittedMHMMR", modelMHMMR = modelMHMMR, paramMHMMR = paramMHMMR, statMHMMR = statMHMMR)
}
