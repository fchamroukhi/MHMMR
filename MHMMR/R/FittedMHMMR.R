FittedMHMMR <- setRefClass(
  "FittedMHMMR",
  fields = list(
    modelMHMMR = "ModelMHMMR",
    paramMHMMR = "ParamMHMMR",
    statMHMMR = "StatMHMMR"
  ),
  methods = list(
    plot = function() {
      yaxislim <- c(min(modelMHMMR$Y) - 2 * mean(sqrt(apply(modelMHMMR$Y, 2, var))), max(modelMHMMR$Y) + 2 * mean(sqrt(apply(modelMHMMR$Y, 2, var))))

      par(mfrow = c(2, 1), mai = c(0.5, 1, 0.5, 0.5), oma = c(0, 0, 2, 0))

      # Predicted time series and predicted regime probabilities
      matplot(modelMHMMR$Y, type = "l", lty = 1, col = gray.colors(modelMHMMR$m), ylab = "y", xlab = "")
      title(main = "Original and predicted HMMR time series")

      mtext("Time series, HMMR regimes, and process probabilites", outer = TRUE, cex = 1.4, font = 2)
      for (d in 1:modelMHMMR$m) {
          lines(statMHMMR$predicted[, d], lwd = 1, col = "red")
      }

      # Prediction probabilities of the hidden process (segmentation)
      colors <- rainbow(modelMHMMR$K)
      plot.default(statMHMMR$predict_prob[, 1], col = colors[1], type = "l", lwd = 1.5, main = "Prediction probabilities", ylab = "Prob")
      for (k in 2:modelMHMMR$K) {
        lines(statMHMMR$predict_prob[, k], col = colors[k])
      } # Post Probs: Pr(Z_{t}=k|y_1,\ldots,y_{t-1})

      # Filtered time series and filtering regime probabilities
      par(mfrow = c(2, 1), mai = c(0.5, 1, 0.5, 0.5), oma = c(0, 0, 2, 0))
      matplot(modelMHMMR$Y, type = "l", lty = 1, col = gray.colors(modelMHMMR$m), ylab = "y", xlab = "")
      title(main = "Original and filtered HMMR time series")
      for (d in 1:modelMHMMR$m) {
        lines(statMHMMR$filtered[, d], lwd = 1, col = "red")
      }

      # Filtering probabilities of the hidden process (segmentation)
      plot.default(statMHMMR$filter_prob[, 1], col = colors[1], type = "l", lwd = 1.5, main = "Filtering probabilities", xlab = "t", ylab = "Prob")
      for (k in 2:modelMHMMR$K) {
        lines(statMHMMR$filter_prob[, k], col = colors[k], type = "l", lwd = 1) #Post Probs: Pr(Z_{t}=k|y_1,\ldots,y_t)
      } # Post Probs: Pr(Z_{t}=k|y_1,\ldots,y_t)

      # Data, regressors, and segmentation
      par(mfrow = c(2, 1), mai = c(0.5, 1, 0.5, 0.5), oma = c(0, 0, 2, 0))
      matplot(modelMHMMR$Y, type = "l", lty = 1, col = gray.colors(modelMHMMR$m), ylab = "y", xlab = "")

      mtext("Time series, MHMMR regimes, and smoothing probabilites", outer = TRUE, cex = 1.4, font = 2)
      for (k in 1:modelMHMMR$K) {
        model_k <- statMHMMR$regressors[, , k]

        index <- statMHMMR$klas == k
        active_model_k <- model_k[index, ] #prob_model_k >= prob);
        active_period_model_k <- seq(1:modelMHMMR$n)[index]#prob_model_k >= prob);

        if (length(active_model_k) != 0) {
          for (d in 1:modelMHMMR$m) {
            lines(model_k[, d], col = colors[k], lty = "dotted", lwd = 1)
            lines(active_period_model_k, active_model_k[, d], col = colors[k], type = "l", lwd = 3)
          }
        }
      }

      # Probablities of the hidden process (segmentation)
      plot.default(statMHMMR$tau_tk[, 1], main = "Smoothing probabilities", xlab = "", ylab = "Prob", col = colors[1], type = "l", lwd = 1.5)
      if (modelMHMMR$K > 1) {
        for (k in 2:modelMHMMR$K) {
          lines(statMHMMR$tau_tk[, k], col = colors[k], type = "l", lwd = 1.5)
        }
      } # Post Probs: Pr(Z_{t}=k|y_1,\ldots,y_n)

      # Data, regression model, and segmentation
      par(mfrow = c(2, 1), mai = c(0.5, 1, 0.5, 0.5), oma = c(0, 0, 2, 0))
      matplot(modelMHMMR$Y, type = "l", lty = 1, col = gray.colors(modelMHMMR$m), ylab = "y", xlab = "")

      mtext("Original, smoothed HMMR time series, and segmentation", outer = TRUE, cex = 1.4, font = 2)
      for (d in 1:modelMHMMR$m) {
        lines(statMHMMR$smoothed[, d], lwd = 1, col = "red")
      }

      # Transition time points
      tk <- which(diff(statMHMMR$klas) != 0)
      for (i in 1:length(tk)) {
        abline(v = tk[i], lty = "dotted", lwd = 2, col = "red")
      }

      # Probablities of the hidden process (segmentation)
      plot.default(statMHMMR$klas, type = "l", lwd = 2, col = "red", xlab = "", ylab = "Estimated class labels")
    }
  )
)

FittedMHMMR <- function(modelMHMMR, paramMHMMR, statMHMMR) {
  new("FittedMHMMR", modelMHMMR = modelMHMMR, paramMHMMR = paramMHMMR, statMHMMR = statMHMMR)
}
