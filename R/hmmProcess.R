hmmProcess <- function(prior, trans_mat, n) {
  K <- length(prior)
  state_probs <- matrix(0, n, K)
  pz1 <- prior
  state_probs[1, ] <- pz1
  for (t in 2:n) {
    pzt <-  t(trans_mat) %*% state_probs[t - 1,]
    state_probs[t, ] <- pzt
  }
  return(state_probs)
}
