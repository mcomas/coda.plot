#!/usr/bin/env Rscript

# Deterministic coda.base logo based on an orthonormal ilr basis for S^3.
#
# e1 = (1, -1, 0) / sqrt(2)
# e2 = (1,  1, -2) / sqrt(6)
#
# Lines in (z1, z2) are mapped to the simplex with
# x = closure(exp(e1 * z1 + e2 * z2)). Their ternary projections are curves.

navy <- "#062C63"
teal <- "#05AFA8"
coral <- "#FF5548"
ivory <- "#FCFAF3"

args <- commandArgs(trailingOnly = TRUE)
version <- if (length(args) >= 1) args[[1]] else "v2"
basis_norm <- if (length(args) >= 2) as.numeric(args[[2]]) else 1
grid_step <- if (length(args) >= 3) as.numeric(args[[3]]) else 1
basis_lwd <- if (length(args) >= 4) as.numeric(args[[4]]) else 5.4
rotation_seed <- if (length(args) >= 5) {
  as.integer(args[[5]])
} else if (version %in% c("v4", "v5", "v7")) {
  20260819L
} else {
  NA_integer_
}
origin_cex <- if (length(args) >= 6) {
  as.numeric(args[[6]])
} else if (version %in% c("v3", "v4", "v5", "v7")) {
  0.65
} else {
  2.7
}

if (!is.finite(basis_norm) || basis_norm <= 0) {
  stop("basis_norm must be a positive finite number")
}
if (!is.finite(grid_step) || grid_step <= 0) {
  stop("grid_step must be a positive finite number")
}
if (!is.finite(basis_lwd) || basis_lwd <= 0) {
  stop("basis_lwd must be a positive finite number")
}
if (!is.finite(origin_cex) || origin_cex <= 0) {
  stop("origin_cex must be a positive finite number")
}
if (!grepl("^[A-Za-z0-9_-]+$", version)) {
  stop("version may only contain letters, numbers, underscores, and hyphens")
}

logo_dir <- file.path("assets", "logos")
dir.create(logo_dir, recursive = TRUE, showWarnings = FALSE)
svg_file <- file.path(logo_dir, paste0("coda.base_", version, ".svg"))
png_file <- file.path(logo_dir, paste0("coda.base_", version, ".png"))

canonical_basis <- cbind(
  c(1, -1, 0) / sqrt(2),
  c(1, 1, -2) / sqrt(6)
)

rotation_angle <- 0
if (!is.na(rotation_seed)) {
  set.seed(rotation_seed)
  rotation_angle <- stats::runif(1, 0, 2 * pi)
}

rotation <- matrix(
  c(cos(rotation_angle), sin(rotation_angle),
    -sin(rotation_angle), cos(rotation_angle)),
  nrow = 2
)
basis <- canonical_basis %*% rotation

ilr_inverse <- function(z1, z2) {
  clr <- outer(z1, basis[, 1]) + outer(z2, basis[, 2])
  clr <- clr - apply(clr, 1, max)
  x <- exp(clr)
  x / rowSums(x)
}

simplex_vertices <- rbind(
  c(270, 460),  # first part: lower left
  c(930, 460),  # second part: lower right
  c(600, 1000)  # third part: top
)

ternary_xy <- function(z1, z2) {
  ilr_inverse(z1, z2) %*% simplex_vertices
}

draw_curve <- function(z1, z2, colour, lwd, alpha = 1) {
  xy <- ternary_xy(z1, z2)
  lines(xy[, 1], xy[, 2], col = grDevices::adjustcolor(colour, alpha.f = alpha),
        lwd = lwd, lend = "round", ljoin = "round")
}

colour_with_alpha <- function(colour, alpha) {
  rgb <- grDevices::col2rgb(colour)[, 1] / 255
  grDevices::rgb(rgb[[1]], rgb[[2]], rgb[[3]], alpha = alpha)
}

radial_alpha <- function(distance, target_distance = 2,
                         target_alpha = 0.00015) {
  decay <- -log(target_alpha) / target_distance^2
  exp(-decay * distance^2)
}

draw_radially_faded_curve <- function(z1, z2, colour, lwd,
                                      fade_distance = 2,
                                      fade_alpha = 0.00015) {
  xy <- ternary_xy(z1, z2)
  radial_distance <- sqrt(z1^2 + z2^2)
  midpoint_distance <- (radial_distance[-1] + radial_distance[-length(radial_distance)]) / 2
  alpha <- radial_alpha(midpoint_distance, fade_distance, fade_alpha)

  rgb <- grDevices::col2rgb(colour)[, 1] / 255
  segment_colours <- grDevices::rgb(
    rgb[[1]], rgb[[2]], rgb[[3]], alpha = alpha
  )

  segments(
    xy[-nrow(xy), 1], xy[-nrow(xy), 2],
    xy[-1, 1], xy[-1, 2],
    col = segment_colours, lwd = lwd, lend = "round"
  )
}

draw_positive_arrow <- function(z1, z2, colour, arrow_length, lwd) {
  xy <- ternary_xy(z1, z2)
  n <- nrow(xy)
  arrows(
    xy[n - 1, 1], xy[n - 1, 2], xy[n, 1], xy[n, 2],
    col = colour, lwd = lwd, length = arrow_length, angle = 25, code = 2
  )
}

grDevices::svg(
  filename = svg_file,
  width = 10,
  height = 10,
  bg = "transparent",
  pointsize = 12,
  onefile = TRUE,
  family = "sans"
)

par(mar = rep(0, 4), xaxs = "i", yaxs = "i")
plot.new()
plot.window(c(0, 1200), c(0, 1200), asp = 1)

hexagon <- rbind(
  c(600, 1170), c(1080, 900), c(1080, 310),
  c(600, 40), c(120, 310), c(120, 900)
)
polygon(hexagon, col = ivory, border = navy, lwd = 24)

# Exact regularly spaced coordinate grid: z1 = k and z2 = k.
# Lines with larger fixed |coordinate| are progressively fainter.
grid_values <- seq(-6, 6, by = grid_step)
grid_range <- seq(-8, 8, length.out = 600)
grid_lwd <- if (grid_step < 1) 1.15 else 1.8

for (k in grid_values) {
  if (version %in% c("v5", "v7")) {
    # Pointwise radial fading based on the exact Aitchison distance
    # sqrt(z1^2 + z2^2). Opacity is 0.00015 at distance 2.
    draw_radially_faded_curve(
      rep(k, length(grid_range)), grid_range, teal, grid_lwd
    )
    draw_radially_faded_curve(
      grid_range, rep(k, length(grid_range)), coral, grid_lwd
    )
  } else {
    # Scaling opacity by the step keeps total visual density comparable when
    # the grid becomes finer.
    opacity <- min(0.34, 0.34 * grid_step) * exp(-0.34 * abs(k))
    draw_curve(rep(k, length(grid_range)), grid_range, teal, grid_lwd, opacity)
    draw_curve(grid_range, rep(k, length(grid_range)), coral, grid_lwd, opacity)
  }
}

# Version 7 adds a small reproducible sample of compositional observations.
# Points are generated in ilr space and mapped through the same rotated basis
# and inverse-ilr transformation as the grid.
if (identical(version, "v7")) {
  set.seed(707L)
  sample_z <- matrix(stats::rnorm(120, sd = 0.72), ncol = 2)
  sample_z <- sample_z[sqrt(rowSums(sample_z^2)) <= 1.65, , drop = FALSE]
  sample_z <- sample_z[seq_len(min(14, nrow(sample_z))), , drop = FALSE]
  sample_xy <- ternary_xy(sample_z[, 1], sample_z[, 2])
  sample_alpha <- 0.38 + 0.42 * radial_alpha(sqrt(rowSums(sample_z^2)))
  points(
    sample_xy[, 1], sample_xy[, 2],
    pch = 21,
    bg = colour_with_alpha(navy, sample_alpha),
    col = colour_with_alpha(navy, pmin(1, sample_alpha + 0.12)),
    lwd = 0.7,
    cex = 0.72
  )
}

# The two basis vectors start at the coordinate origin and end exactly at
# (basis_norm, 0) and (0, basis_norm). Hence each has Aitchison (ilr) length
# basis_norm. Their images are curved after inverse-ilr projection.
basis_t <- seq(0, basis_norm, length.out = 120)
arrow_length <- if (basis_norm < 0.5) 0.07 else 0.13
draw_curve(basis_t, rep(0, length(basis_t)), teal, basis_lwd)
draw_curve(rep(0, length(basis_t)), basis_t, coral, basis_lwd)
draw_positive_arrow(basis_t, rep(0, length(basis_t)), teal, arrow_length, basis_lwd)
draw_positive_arrow(rep(0, length(basis_t)), basis_t, coral, arrow_length, basis_lwd)

# Simplex boundary and the neutral composition (1/3, 1/3, 1/3).
# In v3 and v4, the boundary has the same visual weight as the basis vectors.
simplex_lwd <- if (version %in% c("v5", "v7")) {
  basis_lwd / 2
} else if (version %in% c("v3", "v4")) {
  basis_lwd
} else {
  11
}
polygon(simplex_vertices, border = navy, col = NA, lwd = simplex_lwd)
origin <- ternary_xy(0, 0)
points(origin[1, 1], origin[1, 2], pch = 21, bg = navy, col = navy, cex = origin_cex)

text(
  600, 245, "coda.base",
  col = navy, cex = 4.3, font = 2, family = "sans"
)

dev.off()

rsvg::rsvg_png(svg_file, png_file, width = 1400, height = 1400)

message("Created ", svg_file)
message("Created ", png_file)
message("Rotation seed: ", if (is.na(rotation_seed)) "none" else rotation_seed)
message("Rotation angle (radians): ", format(rotation_angle, digits = 10))
