FittedMHMMR <- setRefClass(
  "FittedMHMMR",
  fields = list(
    modelMHMMR = "ModelMHMMR",
    paramMHMMR = "ParamMHMMR",
    statMHMMR = "StatMHMMR"
  ),
  methods = list(
    # plot = function() {
    #   yaxislim <- c(min(modelMHMMR$Y) - 2 * mean(sqrt(apply(modelMHMMR$Y, 2, var))), min(modelMHMMR$Y) + 2 * mean(sqrt(apply(modelMHMMR$Y, 2, var))))
    #
    #   par(mfrow = c(2, 1))
    #
    #   # Predicted time series and predicted regime probabilities
    #   matplot(modelMHMMR$Y, type = "l", lty = 1, col = gray.colors(modelMHMMR$m), ylab = "y", xlab = "")
    #   lines(statMHMMR$predicted, type = "l", lwd = 2, col = "red")
    #   title(main = "Original and predicted HMMR time series")
    #
    #   # Prediction probabilities of the hidden process (segmentation)
    #   colors <- rainbow(modelMHMMR$K)
    #   plot.default(statMHMMR$predict_prob[, 1], col = colors[1], type = "l", lwd = 1.5, main = "Prediction probabilities", ylab = "Prob")
    #   for (k in 2:modelMHMMR$K) {
    #     lines(statMHMMR$predict_prob[, k], col = colors[k])
    #   }
    #
    #   # Filtered time series and filtering regime probabilities
    #   par(mfrow = c(2, 1))
    #   plot.default(modelMHMMR$Y, type = "l", ylab = "y", xlab = "", ylim = yaxislim)#black
    #   title(main = "Original and filtered HMMR time series")
    #   lines(statMHMMR$filtered, col = "red", lwd = 2)
    #
    #   # Filtering probabilities of the hidden process (segmentation)
    #   plot.default(statMHMMR$filter_prob[, 1], col = colors[1], type = "l", lwd = 1.5, main = "Filtering probabilities", xlab = "t", ylab = "Prob")
    #   for (k in 2:modelMHMMR$K) {
    #     lines(statMHMMR$filter_prob[, k], col = colors[k], type = "l", lwd = 1.5) #Post Probs: Pr(Z_{t}=k|y_1,\ldots,y_t)
    #   }
    #
    #   # Data, regressors, and segmentation
    #   par(mfrow = c(2, 1))
    #   plot.default(modelMHMMR$Y, type = "l", ylab = "y", xlab = "", ylim = yaxislim)
    #   title(main = "Time series, HMMR regimes, and smoothing probabilites")
    #   for (k in 1:modelMHMMR$K) {
    #     model_k <- statMHMMR$regressors[, k]
    #     #prob_model_k = HMMR$param$piik[,k]
    #
    #     index <- statMHMMR$klas == k
    #     active_model_k <- model_k[index]#prob_model_k >= prob);
    #     active_period_model_k <- seq(1:modelMHMMR$m)[index]#prob_model_k >= prob);
    #
    #     if (length(active_model_k) != 0) {
    #       lines(model_k, col = colors[k], lty = "dotted", lwd = 1)
    #       lines(active_period_model_k, active_model_k, col = colors[k], type = "l", lwd = 3)
    #     }
    #   }
    #
    #   # Probablities of the hidden process (segmentation)
    #   plot.default(statMHMMR$tau_tk[, 1], main = "Smoothing probabilities", xlab = "", ylab = "Prob", col = colors[1], type = "l", lwd = 1.5)
    #   if (modelMHMMR$K > 1) {
    #     for (k in 2:modelMHMMR$K) {
    #       lines(statMHMMR$tau_tk[, k], col = colors[k], type = "l", lwd = 1.5)
    #     }
    #   }
    #   #Post Probs: Pr(Z_{t}=k|y_1,\ldots,y_n)
    #
    #   ## data, regression model, and segmentation
    #   par(mfrow = c(2, 1))
    #   plot.default(modelMHMMR$Y, type = "l", ylab = "y", xlab = "", ylim = yaxislim)
    #   title(main = "Original and smoothed HMMR time series, and segmentation")
    #   lines(statMHMMR$smoothed, col = "red", lwd = 2)
    #
    #   # Transition time points
    #   tk <- which(diff(statMHMMR$klas) != 0)
    #   for (i in 1:length(tk)) {
    #     abline(v = tk[i], lty = "dotted", lwd = 2, col = "red")
    #   }
    #
    #   # Probablities of the hidden process (segmentation)
    #   plot.default(statMHMMR$klas, type = "l", lwd = 2, col = "red", xlab = "", ylab = "Estimated class labels")
    # }
  )
)

FittedMHMMR <- function(modelMHMMR, paramMHMMR, statMHMMR) {
  new("FittedMHMMR", modelMHMMR = modelMHMMR, paramMHMMR = paramMHMMR, statMHMMR = statMHMMR)
}
