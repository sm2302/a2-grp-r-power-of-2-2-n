# SHORT DESCRIPTION:
# This (solutions.R) script contains these steps:
#   i  ) Generate a tibble of N number of random samples to be plotted
#   ii ) Plotting the convergence of Pr(A) wrt n(S) as a non-animated image

# NOTE: The elaboration for computing theta values is in the README

# README: https://github.com/sm2302/a2-grp-r-power-of-2-2-n#readme
#         [SHIFT + LEFT CLICK TO OPEN LINK]

# Ensure all required packages are installed------------------------------------
# Solution reference for method of bulk installation of packages: https://www.r-bloggers.com/2020/01/an-efficient-way-to-install-and-load-r-packages/
packages <- c("tidyverse", "ggplot2", "ggforce", "gganimate", "gifski")

installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

lapply(packages, library, character.only = TRUE) # Load the packages


# Initialize constants ---------------------------------------------------------

N <- 100000 # How many samples to generate

# Radius lengths need not be randomized; so all are standardized with radius 1.
triangle_length <- sqrt(3) # eqtri length corresponding to circle of radius 1


# Generate a dataframe of N number of samples to be plotted as a still image----

samples_df <- tibble(
  rand_method = as.factor(c(
    rep("A) Endpoints", N),
    rep("B) Radial points", N),
    rep("C) Midpoints", N)
  )),
  theta = c( # Code below explained in README
    runif(N, 0, pi / 2),       # via. randomized endpoints
    acos(runif(N, 0, 1)),      # via. randomized radial points
    acos(sqrt(runif(N, 0, 1))) # via. randomized midpoint positions
  )
) %>% # Then compute essential variables for computing Pr(A)
  group_by(rand_method) %>%
  mutate(
    chord_length = 2 * sin(theta), # multiplied by radius, but radius is 1
    A = chord_length > triangle_length,
    nA = cumsum(A),
    nS = row_number(),
    PrA = nA / nS
  )

# Plot the graph illustrating the convergence of Pr(A) as n(S) approaches N ----

theme_set(theme_grey())
poi_plot <- ggplot(samples_df) +
  geom_line(
    aes(
      x   = nS,
      y   = PrA,
      col = rand_method
    ),
    size = 0.8
  ) +
  # Transform x axis to log10 scale for better view of early and all stages
  scale_x_log10(
    limits = c(1, N),
    breaks = c(1, 10, 100, 1000, 10000, N),
    minor_breaks = NULL,
    labels = sprintf("10^%.0f", log10(c(1, 10, 100, 1000, 10000, N)))
  ) +
  # Add a break on approximated final Pr(A) values
  scale_y_continuous(
    limits = c(0, 1),
    breaks = round(c(0, 1, samples_df$PrA[samples_df$nS == N]), 2),
    minor_breaks = NULL
  ) +
  labs(
    col = "Randomizing method",
    x = "n(S)",
    y = "Pr(A) = n(A) / n(S)",
    title = "Principle of Indifference; Pr(A) = n(A) / n(S)",
    subtitle = "Plotted against n(S)"
  )

print(poi_plot)

ggsave("poi_plot.png", width = 2400, height = 1200, units = "px")
