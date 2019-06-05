#' A Reference Class which represents functional data.
#'
#' FData is a reference class which represents general independent and
#' identically distributed (i.i.d.) functional objects. The data can be
#' ordered by time (functional time series). In the last case, the field `X`
#' represents the time.
#'
#' @field X numeric vector of length \emph{m}.
#' @field Y matrix of size \emph{\eqn{n x m}} representing \emph{n} functions
#' of `X` observed at points \eqn{1,\dots,m}.
#' @export
FData <- setRefClass(
  "FData",
  fields = list(
    X = "numeric", # Covariates
    Y = "matrix", # Response
    m = "numeric",
    n = "numeric",
    vecY = "matrix"
  )
)

FData <- function(X, Y) {

  Y <- as.matrix(Y)

  n <- nrow(Y)
  m <- ncol(Y)

  vecY <- matrix(t(Y), ncol = 1)

  if (n == 1) {
    Y <- t(Y)
  }

  fData <- new("FData", X = X, Y = Y, m = m, n = n, vecY = vecY)
}
