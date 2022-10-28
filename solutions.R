library(tidyverse)

N <- 100000

get_theta_by_rand_endpoint <- function(N = 1) {
  a1 <- runif(N, 0, 2*pi)
  a2 <- runif(N, 0, 2*pi)
  
  adiff <- abs(a1 - a2)
  
  adiff[adiff > pi] <- 2 * pi - adiff[adiff > pi]
  
  return(adiff/2)
}

samples <- data.frame(
  radius = runif(N, 1, 100),
  theta = get_theta_by_rand_endpoint(N)
)

chord_length <- 2 * sin(samples$theta) * samples$radius

triangle_length <- sqrt(3) * samples$radius

mean(chord_length > triangle_length) # 0.333

# For method A, Pr(ch_len > tr_len) is 0.333