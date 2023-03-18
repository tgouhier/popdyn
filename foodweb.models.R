# ---- niche-model ----
# Niche food web model
niche <- function(S, C) {
  adj <- matrix(nrow=S, ncol=S, 0)
  niches <- runif(S) # Random niches n
  radius <- rbeta(S, 1, 1/(2*C)-1) * niches # Random radii r
  # Feed between [r/2, n)
  center <- runif(S, min = radius/2, max = niches)  
  for (i in 1:S) {
    consumed <- which(niches > (center[i] - radius[i]/2) & 
                        niches < (center[i] + radius[i]/2))
    adj[consumed, i] <- 1
  }
  return (adj)
}

# ---- random-model ----
# Random food web model
random <- function(S, C) {
  adj <- matrix(nrow=S, ncol=S, 0)
  rand <- matrix(nrow=S, ncol=S, runif(S*S))
  adj[rand < C] <- 1
  return (adj)
}

# ---- cascade-model ----
# Cascade food web model
cascade <- function(S, C = NULL) {
  adj <- matrix(nrow=S, ncol=S, 0)
  niches <- runif(S) # Random niches n
  if (is.null(C)) {
    prob <- 1
  }
  else {
    prob <- 2*C*S/(S-1)
  }
  for (i in 1:S) {
    consumed <- which(niches < niches[i] & runif(S) < prob)
    adj[consumed, i] <- 1
  }
  return (adj)
}
