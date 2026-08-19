#!/usr/bin/env Rscript

# Side-by-side comparison of the same ilr geometry in the simplex (left) and
# in its Euclidean coordinate plane (right).

args <- commandArgs(trailingOnly = TRUE)
version <- if (length(args) >= 1) args[[1]] else "v8"
if (!version %in% c("v8", "v9")) stop("version must be 'v8' or 'v9'")
designer_style <- identical(version, "v9")

navy <- "#062C63"
teal <- "#05AFA8"
coral <- "#FF5548"
ivory <- "#FCFAF3"

logo_dir <- file.path("assets", "logos")
dir.create(logo_dir, recursive = TRUE, showWarnings = FALSE)
svg_file <- file.path(logo_dir, paste0("coda.base_", version, ".svg"))
png_file <- file.path(logo_dir, paste0("coda.base_", version, ".png"))

basis_norm <- 0.4
grid_step <- 0.4
basis_lwd <- if (designer_style) 3.4 else 3.8
origin_cex <- if (designer_style) 0.5 else 0.55
rotation_seed <- 20260819L
sample_seed <- 707L

canonical_basis <- cbind(
  c(1, -1, 0) / sqrt(2),
  c(1, 1, -2) / sqrt(6)
)

set.seed(rotation_seed)
rotation_angle <- stats::runif(1, 0, 2 * pi)
rotation <- matrix(
  c(cos(rotation_angle), sin(rotation_angle),
    -sin(rotation_angle), cos(rotation_angle)),
  nrow = 2
)
basis <- canonical_basis %*% rotation

ilr_inverse <- function(z) {
  clr <- z %*% t(basis)
  clr <- clr - apply(clr, 1, max)
  x <- exp(clr)
  x / rowSums(x)
}

simplex_vertices <- if (designer_style) {
  rbind(c(220, 515), c(500, 515), c(360, 815))
} else {
  rbind(c(205, 525), c(535, 525), c(370, 850))
}

simplex_xy <- function(z) {
  ilr_inverse(z) %*% simplex_vertices
}

# Align the Euclidean origin with the barycentre of the ternary panel.
euclidean_center <- c(if (designer_style) 845 else 850, mean(simplex_vertices[, 2]))
euclidean_scale <- if (designer_style) 74 else 86
euclidean_xy <- function(z) {
  cbind(
    euclidean_center[[1]] + euclidean_scale * z[, 1],
    euclidean_center[[2]] + euclidean_scale * z[, 2]
  )
}

radial_alpha <- function(distance, target_distance = 2,
                         target_alpha = 0.00015) {
  decay <- -log(target_alpha) / target_distance^2
  exp(-decay * distance^2)
}

colour_with_alpha <- function(colour, alpha) {
  rgb <- grDevices::col2rgb(colour)[, 1] / 255
  grDevices::rgb(rgb[[1]], rgb[[2]], rgb[[3]], alpha = alpha)
}

draw_faded_curve <- function(z, projector, colour, lwd = 1) {
  xy <- projector(z)
  distance <- sqrt(rowSums(z^2))
  midpoint_distance <- (distance[-1] + distance[-length(distance)]) / 2
  segment_colours <- colour_with_alpha(colour, radial_alpha(midpoint_distance))
  segments(
    xy[-nrow(xy), 1], xy[-nrow(xy), 2],
    xy[-1, 1], xy[-1, 2],
    col = segment_colours, lwd = lwd, lend = "round"
  )
}

draw_basis_vector <- function(axis, projector, colour) {
  t <- seq(0, basis_norm, length.out = 120)
  z <- matrix(0, nrow = length(t), ncol = 2)
  z[, axis] <- t
  xy <- projector(z)
  lines(xy[, 1], xy[, 2], col = colour, lwd = basis_lwd,
        lend = "round", ljoin = "round")
  n <- nrow(xy)
  arrows(
    xy[n - 1, 1], xy[n - 1, 2], xy[n, 1], xy[n, 2],
    col = colour, lwd = basis_lwd, length = 0.06, angle = 25, code = 2
  )
}

set.seed(sample_seed)
sample_z <- matrix(stats::rnorm(120, sd = 0.72), ncol = 2)
sample_z <- sample_z[sqrt(rowSums(sample_z^2)) <= 1.65, , drop = FALSE]
sample_z <- sample_z[seq_len(min(14, nrow(sample_z))), , drop = FALSE]
sample_alpha <- 0.38 + 0.42 * radial_alpha(sqrt(rowSums(sample_z^2)))

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
polygon(hexagon, col = ivory, border = navy,
        lwd = if (designer_style) 19 else 24)

# Subtle colour fields group the two views without enclosing them in boxes.
if (designer_style) {
  symbols(
    c(mean(simplex_vertices[, 1]), euclidean_center[[1]]),
    c(mean(simplex_vertices[, 2]), euclidean_center[[2]]),
    circles = c(190, 190), inches = FALSE, add = TRUE,
    bg = c(
      grDevices::adjustcolor(teal, alpha.f = 0.035),
      grDevices::adjustcolor(coral, alpha.f = 0.028)
    ),
    fg = NA
  )
}

# The same exact 0.4-spaced ilr grid is passed through both projectors.
grid_values <- seq(-2.4, 2.4, by = grid_step)
grid_range <- seq(-3, 3, length.out = 360)
for (k in grid_values) {
  z1 <- cbind(rep(k, length(grid_range)), grid_range)
  z2 <- cbind(grid_range, rep(k, length(grid_range)))
  draw_faded_curve(z1, simplex_xy, teal)
  draw_faded_curve(z2, simplex_xy, coral)
  draw_faded_curve(z1, euclidean_xy, teal)
  draw_faded_curve(z2, euclidean_xy, coral)
}

# Ternary simplex boundary. The Euclidean view is intentionally frameless.
polygon(
  simplex_vertices,
  border = grDevices::adjustcolor(
    navy, alpha.f = if (designer_style) 0.52 else 0.62
  ),
  col = NA,
  lwd = if (designer_style) 1.15 else 1.2
)

# Same observations in both representations.
for (projector in list(simplex_xy, euclidean_xy)) {
  sample_xy <- projector(sample_z)
  points(
    sample_xy[, 1], sample_xy[, 2],
    pch = 21,
    bg = colour_with_alpha(navy, sample_alpha),
    col = colour_with_alpha(navy, pmin(1, sample_alpha + 0.12)),
    lwd = 0.65,
    cex = if (designer_style) 0.56 else 0.62
  )
}

# Same unit directions and norm in both representations.
for (projector in list(simplex_xy, euclidean_xy)) {
  draw_basis_vector(1, projector, teal)
  draw_basis_vector(2, projector, coral)
  origin <- projector(matrix(c(0, 0), nrow = 1))
  points(origin[1, 1], origin[1, 2], pch = 21, bg = navy, col = navy,
         cex = origin_cex)
}

# A compact transformation symbol. Version 9 uses a circular brand device
# that stays legible at favicon scale.
arrow_y <- euclidean_center[[2]]
if (designer_style) {
  symbols(605, arrow_y, circles = 26, inches = FALSE, add = TRUE,
          bg = navy, fg = NA)
  segments(593, arrow_y, 610, arrow_y, col = ivory, lwd = 2.8, lend = "round")
  polygon(
    rbind(c(607, arrow_y + 7), c(619, arrow_y), c(607, arrow_y - 7)),
    col = ivory, border = NA
  )
} else {
  segments(603, arrow_y, 647, arrow_y, col = navy, lwd = 3.2, lend = "round")
  polygon(
    rbind(c(642, arrow_y + 10), c(663, arrow_y), c(642, arrow_y - 10)),
    col = navy, border = NA
  )
}

text(
  600, if (designer_style) 238 else 245, "coda.base",
  col = navy, cex = if (designer_style) 4.05 else 4.3,
  font = 2, family = "sans"
)

dev.off()

rsvg::rsvg_png(svg_file, png_file, width = 1400, height = 1400)

message("Created ", svg_file)
message("Created ", png_file)
message("Rotation seed: ", rotation_seed)
message("Sample seed: ", sample_seed)
