library(tidyverse)
library(ggplot2)

N <- 100000 # Number of random tests generated via each method

samples <- data.frame(
  radius = runif(N * 3, 1, 100),
  rand_method = as.factor(c(
    rep("A) Endpoints", N),
    rep("B) Radial points", N),
    rep("C) Chord midpoints", N)
  )),
  theta = c( # Code below to be explained in README.md
    runif(N, 0, pi / 2), # via. randomized endpoints
    acos(runif(N, 0, 1)), # via. randomized radial points
    acos(sqrt(runif(N, 0, 1))) # randomized chord midpoint positions
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

# Plot data
ggplot(results) +
  geom_line(
    aes(
      nS,
      PrA,
      col = rand_method
    ),
    alpha = 0.4, # Add a bit of transparency in case of line overlaps
    size = 0.8
  ) +
  scale_x_log10(
    limits = c(1, N),
    breaks = c(1, 10, 100, 1000, 10000, N),
    minor_breaks = NULL,
    labels = sprintf("10^%.0f", log10(c(1, 10, 100, 1000, 10000, N)))
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = round(c(
      0,
      results$PrA[N],
      results$PrA[2 * N],
      results$PrA[3 * N],
      1
    ), 2),
    minor_breaks = NULL
  ) +
  labs(
    col = "Randomizing method",
    x = "n(S)",
    y = "Pr(A) = n(A) / n(S)",
    title = "Principle of Indifference; Pr(A) = n(A) / n(S)",
    subtitle = "Plotted against n(S)"
  )