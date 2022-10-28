library(tidyverse)

N <- 100000

get_theta_by_rand_endpoint <- function(N = 1) {
  a1 <- runif(N, 0, 2*pi)
  a2 <- runif(N, 0, 2*pi)
  
  adiff <- abs(a1 - a2)
  
  adiff[adiff > pi] <- 2 * pi - adiff[adiff > pi]
  
  return(adiff/2)
}

get_theta_by_rand_radial_points <- function(N) {
  # d <- runif(N, 0, radius)
  # theta <- acos(d / radius)
  
  # simplify to:
  
  theta <- acos(runif(N, 0, 1))
  return(theta)
}

get_theta_by_rand_midpoints <- function(N) {
  # d <- radius * sqrt(runif(N, 0, 1))
  # theta <- acos(d / radius)
  
  # simplify to:
  
  theta <- acos(sqrt(runif(N, 0, 1)))
  return(theta)
}

samples <- data.frame(
  radius = runif(N * 3, 1, 100),
  rand_method = as.factor(c(
    rep("A) Endpoints", N),
    rep("B) Radial points", N),
    rep("C) Chord midpoints", N)
  )),
  theta = c(
    get_theta_by_rand_endpoint(N), 
    get_theta_by_rand_radial_points(N),
    get_theta_by_rand_midpoints(N)
  )
)

results <- samples %>%
  group_by(rand_method) %>%
  summarize(
    chord_length = 2 * sin(theta) * radius,
    triangle_length = sqrt(3) * radius,
    A = chord_length > triangle_length,
    nA = cumsum(A),
    nS = ((row_number() - 1) %% N) + 1,
    PrA = nA / nS
  )

results$PrA[N] # 0.333
results$PrA[2*N] # 0.5
results$PrA[3*N] # 0.25

# For method A, Pr(ch_len > tr_len) is 0.333
# For method B, Pr(ch_len > tr_len) is 0.5
# For method C, Pr(ch_len > tr_len) is 0.25