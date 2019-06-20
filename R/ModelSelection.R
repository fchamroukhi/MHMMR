#' @export
selectMHMMR <- function(X, Y, Kmin = 1, Kmax = 10, pmin = 0, pmax = 4, criterion = c("BIC", "AIC")) {

  criterion <- match.arg(criterion)

  vmhmmr <- Vectorize(function(K, p, X1 = X, Y1 = Y) emMHMMR(X = X1, Y = Y1, K, p),
                     vectorize.args = c("K", "p"))

  mhmmr <- outer(Kmin:Kmax, pmin:pmax, vmhmmr)

  if (criterion == "BIC") {
    results <- apply(mhmmr, 1:2, function(x) x[[1]]$statMHMMR$BIC)
  } else {
    results <- apply(mhmmr, 1:2, function(x) x[[1]]$statMHMMR$AIC)
  }
  rownames(results) <- sapply(Kmin:Kmax, function(x) paste0("(K = ", x, ")"))
  colnames(results) <- sapply(pmin:pmax, function(x) paste0("(p = ", x, ")"))


  selected <- mhmmr[which(results == max(results), arr.ind = T)][[1]]

  cat(paste0("The MHMMR model selected via the \"", criterion, "\" has K = ",
             selected$paramMHMMR$K, " regimes \n and the order of the ",
             "polynomial regression is p = ", selected$paramMHMMR$p, "."))
  cat("\n")
  cat(paste0("BIC = ", selected$statMHMMR$BIC, "\n"))
  cat(paste0("AIC = ", selected$statMHMMR$AIC, "\n"))

  return(selected)

}
