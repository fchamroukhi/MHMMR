source("R/FData.R")
source("R/enums.R")

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
    nu <<- K - 1 + K * (K - 1) + K * (p + 1) + 1
  }
  else{
    nu <<- K - 1 + K * (K - 1) + K * (p + 1) + K
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
