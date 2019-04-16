hmm_process <- function(prior, trans_mat, n) {
  # hmm_process calculates the distribution p(z_1,...,z_n;pi,A) of a markov chain (z_1,...z_n)
  # with prior prob \pi and transition matrix A
  #
  #
  #
  #
  #
  # Faicel Chamroukhi
  ######################################################################################################
  K <- length(prior)
  state_probs <- matrix(0, n, K)
  pz1 <- prior
  state_probs[1, ] <- pz1
  for (t in 2:n) {
    pzt <-  t(trans_mat) %*% state_probs[t - 1, ] #p(z_i = k ) = sum_l (p(z_i=k,z_{i-1}=l)) = sum_l (p(z_i=k|z_i-1=l))*p(z_{i-1}= l) = sum_l A_{lk}*p(z_{i-1})
    #    pzi = normalise(pzi)
    state_probs[t, ] <- pzt
  }
  return(state_probs)
}
