# SHORT DESCRIPTION: -----------------------------------------------------------
# This (solutions.R) script contains these steps:
#   i  ) Generate a tibble of N number of random samples
#        - rand_method - by which method the chords are randomized
#        - theta       - main variable differentiating each method
#        - Pr(A)       - Pr(lengths of a random chord > length of triangle)
#   ii ) Plotting the convergence of Pr(A) wrt n(S) as a non-animated image
#   iii) Extend the tibble for animation up to the "N_anim"th sample
#        - direction_theta - Random angle corresponding to the direction of
#                            the chord from circle's midpoint
#        - rand_chord    - collection of chords, generated using direction_theta
#        - stacked_chord - same as rand_chord, but with standardized direction
#        - elevation     - variable to be used for sorting chords by length
#   iv ) Render the animation
#   v  ) Print final results Pr(A) values in the console


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

# Constants for drawing
x_offset <- c(0, 3, 6) # Horizontal translation of some plot items
y_offset <- c(0, -2.5, -5.5, -7) # Vertical translation of some plot items
exact_PrA <- c(0.33, 0.5, 0.25)

# Generate a dataframe of N number of samples to be plotted as a still image----

samples_df <- tibble(
  rand_method = as.factor(c(
    rep("A) Endpoints", N),
    rep("B) Radial points", N),
    rep("C) Midpoints", N)
  )),
  theta = c( # Approach for computing theta below explained in README
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

# Pr(A) values by the Nth sample, and "N_anim"th sample, respectively
approx_PrA <- samples_df$PrA[samples_df$nS == N]
anim_PrA <- samples_df$PrA[samples_df$nS == N_anim]


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
  # Transform x axis to log10 scale for nice view of both earlier and later ends
  scale_x_log10(
    limits = c(1, N),
    breaks = c(1, 10, 100, 1000, 10000, N),
    minor_breaks = NULL,
    labels = sprintf("10^%.0f", log10(c(1, 10, 100, 1000, 10000, N)))
  ) +
  # Add a break on each approximated final Pr(A) value
  scale_y_continuous(
    limits = c(0, 1),
    breaks = round(c(0, 1, approx_PrA), 2),
    minor_breaks = NULL
  ) +
  labs(
    col      = "Randomizing method",
    x        = "n(S)",
    y        = "Pr(A) = n(A) / n(S)",
    title    = "Principle of Indifference; Pr(A) = n(A) / n(S)",
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
    rand_chord_x    = cos(direction_theta) + x_offset,
    rand_chord_y    = sin(direction_theta) + y_offset[1],
    rand_chord_xend = cos(direction_theta + 2 * theta) + x_offset,
    rand_chord_yend = sin(direction_theta + 2 * theta) + y_offset[1],
    # [Row II ] Alternative drawing:
    #   Relocate the chords such that all their midpoints lie on a straight line
    #   while maintaining their exact randomized lengths from "Row I"
    stacked_chord_x    = cos(3 * pi / 2 - theta) + x_offset,
    stacked_chord_y    = sin(3 * pi / 2 - theta) + y_offset[2],
    stacked_chord_xend = cos(3 * pi / 2 + theta) + x_offset,
    stacked_chord_yend = sin(3 * pi / 2 + theta) + y_offset[2]
  ) %>%
  # [Row III] Better view of the chords stacked together, sorted by length
  arrange(rand_method, desc(chord_length)) %>%
  group_by(rand_method) %>%
  mutate(
    elevation = row_number() / (N_anim / 3)
  ) %>%
  # Revert rows' arrangement to their starting order
  arrange(rand_method, nS)

# Coordinates of an equilateral triangles of a circle of radius 1 and center 0,0
eqtri_df <- tibble(
  x    = c(0, sqrt(3) / 2, -sqrt(3) / 2),
  y    = c(1, -0.5, -0.5),
  xend = c(sqrt(3) / 2, -sqrt(3) / 2, 0),
  yend = c(-0.5, -0.5, 1)
)

# Translation of the above triangle onto the six different circles' positions
eqtris_df <- tibble(
  x    = rep(outer(eqtri_df$x, x_offset, "+"), 2),
  y    = rep(outer(eqtri_df$y, y_offset[1:2], "+"), 3),
  xend = rep(outer(eqtri_df$xend, x_offset, "+"), 2),
  yend = rep(outer(eqtri_df$yend, y_offset[1:2], "+"), 3)
)


# Generate the ggplot for visualizing Bertrand's Paradox -----------------------

theme_set(theme_void())

bp_anim <- ggplot() +
  # [Row I] Truly randomized chords
  geom_segment(
    data = samples_anim_df,
    aes(
      x    = rand_chord_x,
      y    = rand_chord_y,
      xend = rand_chord_xend,
      yend = rand_chord_yend,
      col  = A
    )#,
    #size = 2
  ) +
  # [Row II] Same randomized chords but with standardized midpoint directions
  geom_segment(
    data = samples_anim_df,
    aes(
      x    = stacked_chord_x,
      y    = stacked_chord_y,
      xend = stacked_chord_xend,
      yend = stacked_chord_yend,
      col  = A
    )#,
    #size = 2
  ) +
  # [Row III] Better view of chords i.e. stacked vertically, sorted by lengths
  geom_segment(
    data = samples_anim_df,
    aes(
      x    = x_offset + 1 - chord_length,
      xend = x_offset + 1,
      y    = y_offset[4] + elevation,
      yend = y_offset[4] + elevation,
      col  = A
    )#,
    #size = 2
  ) +
  # Plot the six circles' outlines:
  ggforce::geom_circle(aes(
    x0 = rep(x_offset, 2),
    y0 = rep(y_offset[1:2], 3), r = 1
  )) +
  # Plot their corresponding equilateral triangles:
  geom_segment(data = eqtris_df, aes(x = x, y = y, xend = xend, yend = yend)) +
  # Dynamic text for Pr(A):
  geom_text(
    data = samples_anim_df,
    aes(
      y = 1.8, x = x_offset,
      label = sprintf(
        "\nPr(A) = %.00f / %.00f = %.02f",
        nA, nS, PrA
      )
    ),
    size = 8
  ) +
  # Plot Title:
  annotate(
    geom = "text", x = 2.65, y = 2.9,
    label = "Bertrand's Paradox Visualization", size = 23
  ) +
  # Note to viewer:
  annotate(
    geom = "text", x = 2.65, y = -7.75,
    label = sprintf(paste(
      "[Due to small sample size (%d), approximated Pr(A)'s are",
      "less likely to match their exact values]"
    ), N_anim), size = 7.5
  ) +
  # Randomizing Methods' Titles:
  annotate(
    geom = "text", x = x_offset, y = 2, label =
      c(
        "Random Endpoints",
        "Random Radial Points",
        "Random Midpoints"
      ), size = 11
  ) +
  # Visualization Titles:
  annotate(
    geom = "text", x = -1.7, y = y_offset[1:3], label =
      c(
        "Random\nDirections",
        "Standardized\nDirections",
        "Distribution"
      ), angle = 90, size = 8
  ) +
  # Axis & break labels for "Distribution" figures:
  annotate(geom = "text", x = x_offset - 1, y = -7.05, label = "2", angle = 90, size = 6, hjust = "right") +
  annotate(geom = "text", x = x_offset + 1, y = -7.05, label = "0", angle = 90, size = 6, hjust = "right") +
  annotate(geom = "text", x = x_offset + 1.05, y = -4, label = "1", angle = 90, size = 6, vjust = "top") +
  annotate(geom = "text", x = x_offset + 1.05, y = -7, label = "0", angle = 90, size = 6, vjust = "top") +
  annotate(geom = "text", x = x_offset - 1.05, y = -7 + 3 * exact_PrA, label = exact_PrA, angle = 90, size = 6, vjust = "bottom") +
  annotate(geom = "text", x = x_offset + 1.05, y = -7 + 3 * anim_PrA, label = sprintf("Pr(A) = %.2f", anim_PrA), angle = 90, size = 6, vjust = "top") +
  annotate(geom = "text", x = x_offset + 1 - sqrt(3), y = -7.05, label = "√3", angle = 90, size = 6, hjust = "right") +
  # Break lines for "Distribution" figures
  geom_segment(linetype = "dashed", aes(x = x_offset + 1 - sqrt(3), xend = x_offset + 1 - sqrt(3), y = -7, yend = -4)) +
  geom_segment(linetype = "dashed", aes(x = x_offset - 1, xend = x_offset + 1, y = -7 + 3 * exact_PrA, yend = -7 + 3 * exact_PrA)) +
  geom_segment(aes(x = c(1, 4, 7) - sqrt(3), xend = c(1, 4, 7), y = -7 + 3 * anim_PrA, yend = -7 + 3 * anim_PrA)) +
  # Fix aspect ratio & configure legend:
  coord_equal() +
  labs(
    col = "A = LENGTH(CHORD) > LENGTH(TRIANGLE)   "
  ) +
  theme(
    # Reference1: https://www.statology.org/ggplot2-legend-size/
    # Reference2: https://stackoverflow.com/questions/32826156/how-to-change-background-colour-of-legend-in-ggplot2
    legend.position   = "bottom",
    legend.margin     = margin(t = 0.5, b = 0.5, l = 0.5, r = 0.5, unit = "cm"),
    legend.background = element_rect(fill = "black"),
    legend.key.size   = unit(3, "cm"),
    legend.key.height = unit(1, "cm"),
    legend.key.width  = unit(1, "cm"),
    legend.title      = element_text(size = 24, color = "white"),
    legend.text       = element_text(size = 20, color = "white")
  ) +
  # gganimate related codes:
  transition_time(nS) + # animate along nS
  shadow_mark(size = 0.1) # keep previous frames (i.e. drawn chords) in frame

# Render visualization of Bertrand's Paradox
animate(bp_anim,
  nframes = N_anim + 50, fps = 25,
  width = 1000, height = 1250, end_pause = 50,
  renderer = gifski_renderer("bp_anim.gif")
)


# Print results in console------------------------------------------------------
cat(
  "Final values for Pr(A) = n(A) / n(S):\n",
  sprintf(
    "For method %-24s Pr(A) = %d/%d = %.2f\n",
    c("A: Random Endpoints,", "B: Random Radial Points,", "C: Random Midpoints,"),
    samples_df$nA[samples_df$nS == N],
    samples_df$nS[samples_df$nS == N],
    samples_df$PrA[samples_df$nS == N]
  ),
  sprintf(
    "\nPlot of Pr(A) against n(S) (%d samples) saved in \"poi_plot.png\"\n",
    N
  ),
  sprintf(
    "\nBertrand's Paradox visualization (up to %dth sample) saved in \"bp_anim.gif\"\n",
    N_anim
  )
)
