ModelMHMMR <- setRefClass(
  "ModelMHMMR",
  contains = "FData",
  # Define the fields
  fields = list(
    K = "numeric",
    # number of regimes
    p = "numeric",
    # dimension of beta (order of polynomial regression)
    variance_type = "numeric",
    nu = "numeric" # degree of freedom
  )
)

ModelMHMMR <- function(fData, K, p, variance_type) {
  if (variance_type == variance_types$homoskedastic) {
    nu <<- K - 1 + K * (K - 1) + K * (p + 1) * fData$m + fData$m * (fData$m + 1) / 2
  }
  else{
    nu <<- K - 1 + K * (K - 1) + K * (p + 1) * fData$m + K * fData$m * (fData$m + 1) / 2
  }

  new(
    "ModelMHMMR",
    Y = fData$Y,
    X = fData$X,
    m = fData$m,
    n = fData$n,
    K = K,
    p = p,
    variance_type = variance_type,
    nu = nu
  )
}
