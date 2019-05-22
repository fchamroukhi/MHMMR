mkStochastic <- function(M) {
  # mkStochastic Ensure the argument is a stochastic matrix, i.e., the sum over the last dimension is 1.
  # M = mkStochastic(M)
  #
  # If M is a vector, it will sum to 1.
  # If M is a matrix, each row will sum to 1.
  # If M is a 3D array, then sum_k M(i,j,k) = 1 for all i,j.

  # Set zeros to 1 before dividing
  # This is valid since S(j) = 0 iff M(i,j) = 0 for all j
  ###########################################################################################################
  if (is.vector(M) == TRUE) {
    # Vector
    M <- normalize(M)$M
  } else if (is.matrix(M) == TRUE) {
    # Matrix
    S <- apply(M, 1, sum)
    S <- S + (S == 0)
    norm <- matrix(rep(S, ncol(M)), nrow = length(S))
    M <- M / norm
  } else{
    # Multi-dimensional array
    ns <- dim(M)
    M <- matrix(M, prod(ns[1]:ns[length(ns) - 1]), ns[length(ns)])
    S <- apply(M, 1, sum)
    S <- S + (S == 0)
    norm <- matrix(rep(S, ncol(M)), nrow = length(S), ncol = ns[length(ns)])
    M <- M / norm
    M <- array(M, ns)
  }
  return(M)
}
