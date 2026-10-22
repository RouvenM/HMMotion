######################################################################################
#### Posterior HMM features per defender track.
#### Instead of summarising the most likely state (argmax) at each time point,
#### the features are expectations under the posterior distribution of the
#### state sequence given the observations, computed by a scaled
#### forward-backward pass. Sourced by 03_create_data_HMM.R.
######################################################################################

# delta:    initial distribution of the track (length N)
# Gamma:    N x N x n array; Gamma[,,t] is the transition from t-1 to t
#           (Gamma[,,1] is ignored), as used by LaMa::forward_g;
#           or a single N x N matrix for a homogeneous chain
# allprobs: n x N matrix of state-dependent densities
hmm_track_posterior <- function(delta, Gamma, allprobs){
  n <- nrow(allprobs)
  N <- ncol(allprobs)
  if(length(dim(Gamma)) == 2) Gamma <- array(Gamma, c(N, N, n))

  # scaled forward and backward variables
  alpha <- matrix(0, n, N)
  beta <- matrix(1, n, N)
  cs <- numeric(n)
  a <- delta * allprobs[1, ]
  cs[1] <- sum(a)
  alpha[1, ] <- a / cs[1]
  if(n > 1){
    for(t in 2:n){
      a <- as.vector(alpha[t-1, ] %*% Gamma[, , t]) * allprobs[t, ]
      cs[t] <- sum(a)
      alpha[t, ] <- a / cs[t]
    }
    for(t in (n-1):1){
      beta[t, ] <- as.vector(Gamma[, , t+1] %*% (allprobs[t+1, ] * beta[t+1, ])) / cs[t+1]
    }
  }

  # local state probabilities Pr(S_t = j | y)
  probs <- alpha * beta

  # Pr(S_{t-1} = S_t | y), from the diagonal of the pairwise posterior
  stay <- numeric(0)
  if(n > 1){
    stay <- sapply(2:n, \(t){
      sum(alpha[t-1, ] * diag(Gamma[, , t]) * allprobs[t, ] * beta[t, ]) / cs[t]
    })
  }

  # Pr(same state over the whole track | y) = sum_i Pr(S_1 = ... = S_n = i | y)
  logL <- sum(log(cs))
  log_diag <- if(n > 1) colSums(log(t(apply(Gamma[, , 2:n, drop = FALSE], 3, diag)))) else 0
  log_const <- log(delta) + colSums(log(allprobs)) + log_diag - logL
  p_const <- min(1, sum(exp(log_const)))

  occupancy <- colMeans(probs)
  occupancy <- occupancy[occupancy > 0]

  list(probs = probs,
       exp_switches = sum(1 - stay),               # expected number of assignment switches
       p_switch = 1 - p_const,                     # Pr(at least one switch)
       entropy = -sum(occupancy * log(occupancy)), # entropy of time-averaged state probabilities
       logL = logL)
}
