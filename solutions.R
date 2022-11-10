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
N_anim <- 500 # How many out of N samples to include in animation rendering

# Radius lengths need not be randomized; so all are standardized with radius 1.
triangle_length <- sqrt(3) # eqtri length corresponding to circle of radius 1

y_offset <- 2.5 # How far apart the row of circles are drawn from each other

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


# Extend the dataframe to include variables used for drawing the chords --------
#   but only up to N_anim amount of samples (too many samples = slow rendering)

samples_anim_df <- samples_df[samples_df$nS <= N_anim, ] %>%
  mutate(
    # Compute variables for drawing chords via each method side by side
    direction_theta <- runif(N_anim, 0, 2 * pi),
    x_offset =
      ifelse(rand_method == "A) Endpoints", 0,
             ifelse(rand_method == "B) Radial points", 3, 6)
      ),
    # [Row I  ] Draw truly random chords
    rand_chord_x = cos(direction_theta) + x_offset,
    rand_chord_xend = cos(direction_theta + 2 * theta) + x_offset,
    rand_chord_y = sin(direction_theta),
    rand_chord_yend = sin(direction_theta + 2 * theta),
    # [Row II ] Alternative drawing:
    #   Relocate the chords such that all their midpoints lie on a straight line
    #   while maintaining their exact randomized lengths from "Row I"
    stacked_chord_x = cos(3 * pi / 2 - theta) + x_offset,
    stacked_chord_xend = cos(3 * pi / 2 + theta) + x_offset,
    stacked_chord_y = sin(3 * pi / 2 - theta) - y_offset,
    stacked_chord_yend = sin(3 * pi / 2 + theta) - y_offset
  ) %>%
  # [Row III] Better view of the chords stacked together, sorted by length
  arrange(rand_method, desc(chord_length)) %>%
  group_by(rand_method) %>%
  mutate(
    elevation = row_number() / (N_anim / 3)
  ) %>%
  # Revert rows' arrangement to their starting order
  arrange(rand_method, nS)


# Coordinates of three equilateral triangles to be plotted side by side
eqtri_row_df <- tibble(
  x    = c(outer(c(0, sqrt(3) / 2, -sqrt(3) / 2), c(0, 3, 6), "+")),
  y    = rep(c(1, -0.5, -0.5), 3),
  xend = c(outer(c(sqrt(3) / 2, -sqrt(3) / 2, 0), c(0, 3, 6), "+")),
  yend = rep(c(-0.5, -0.5, 1), 3)
)

# Coordinates of the above but replicated into two rows
eqtri_2rows_df <- tibble(
  x    = rep(eqtri_row_df$x, 2),
  y    = c(eqtri_row_df$y, eqtri_row_df$y - y_offset),
  xend = rep(eqtri_row_df$xend, 2),
  yend = c(eqtri_row_df$yend, eqtri_row_df$yend - y_offset),
)


# Generate the ggplot for visualizing Bertrand's Paradox -----------------------

theme_set(theme_void())

bp_anim <- ggplot() +
  # [Row I] Truly randomized chords
  geom_segment(
    data = samples_anim_df,
    aes(
      x = rand_chord_x,
      y = rand_chord_y,
      xend = rand_chord_xend,
      yend = rand_chord_yend,
      col = A
    ),
    size = 2
  ) +
  # [Row II] Same randomized chords but with standardized midpoint directions
  geom_segment(
    data = samples_anim_df,
    aes(
      x = stacked_chord_x,
      y = stacked_chord_y,
      xend = stacked_chord_xend,
      yend = stacked_chord_yend,
      col = A
    ),
    size = 2
  ) +
  # [Row III] Better view of chords i.e. stacked vertically, sorted by lengths
  geom_segment(
    data = samples_anim_df,
    aes(
      x = stacked_chord_x,
      xend = stacked_chord_xend,
      y = elevation - 7,
      yend = elevation - 7,
      col = A
    ),
    size = 2
  ) +
  # Plot the circles' outlines:
  ggforce::geom_circle(aes(x0 = c(0, 3, 6), y0 = 0, r = 1), col = "gray50") +
  ggforce::geom_circle(aes(x0 = c(0, 3, 6), y0 = -2.5, r = 1), col = "gray50") +
  # Plot their corresponding equilateral triangles:
  geom_segment(data = eqtri_2rows_df, aes(x = x, y = y, xend = xend, yend = yend))

print(bp_anim)