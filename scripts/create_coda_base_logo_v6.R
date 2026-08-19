#!/usr/bin/env Rscript

# Deterministic coda.base v6 logo for the four-part simplex S^4.
# A rotated orthonormal ilr basis in R^3 is mapped into a tetrahedron with
# x = closure(exp(B %*% z)), then projected affinely to the drawing plane.

navy <- "#062C63"
teal <- "#05AFA8"
coral <- "#FF5548"
blue <- "#3D8FD1"
ivory <- "#FCFAF3"

logo_dir <- file.path("assets", "logos")
dir.create(logo_dir, recursive = TRUE, showWarnings = FALSE)
svg_file <- file.path(logo_dir, "coda.base_v6.svg")
png_file <- file.path(logo_dir, "coda.base_v6.png")

basis_norm <- 0.4
grid_step <- 0.4
basis_lwd <- 4.4
origin_cex <- 0.65
rotation_seed <- 20260819L

canonical_basis <- cbind(
  c(1, -1, 0, 0) / sqrt(2),
  c(1, 1, -2, 0) / sqrt(6),
  c(1, 1, 1, -3) / sqrt(12)
)

# A reproducible Haar-like orthogonal rotation obtained from a Gaussian QR
# decomposition. Correct the sign so the matrix is a proper rotation.
set.seed(rotation_seed)
random_matrix <- matrix(stats::rnorm(9), nrow = 3)
rotation <- qr.Q(qr(random_matrix))
if (det(rotation) < 0) rotation[, 1] <- -rotation[, 1]
basis <- canonical_basis %*% rotation

ilr_inverse <- function(z) {
  clr <- z %*% t(basis)
  clr <- clr - apply(clr, 1, max)
  x <- exp(clr)
  x / rowSums(x)
}

# Oblique projection of a tetrahedron. The fourth vertex lies visually behind
# the front triangular face; barycentric projection remains affine and exact.
tetra_vertices <- rbind(
  c(300, 450),  # part 1: front left
  c(900, 450),  # part 2: front right
  c(600, 1010), # part 3: top
  c(600, 610)   # part 4: rear/interior vertex
)

project_xy <- function(z) {
  ilr_inverse(z) %*% tetra_vertices
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

draw_faded_curve <- function(z, colour, lwd = 1.15) {
  xy <- project_xy(z)
  distance <- sqrt(rowSums(z^2))
  midpoint_distance <- (distance[-1] + distance[-length(distance)]) / 2
  segment_colours <- colour_with_alpha(colour, radial_alpha(midpoint_distance))
  segments(
    xy[-nrow(xy), 1], xy[-nrow(xy), 2],
    xy[-1, 1], xy[-1, 2],
    col = segment_colours, lwd = lwd, lend = "round"
  )
}

draw_basis_vector <- function(axis, colour) {
  t <- seq(0, basis_norm, length.out = 120)
  z <- matrix(0, nrow = length(t), ncol = 3)
  z[, axis] <- t
  xy <- project_xy(z)
  lines(xy[, 1], xy[, 2], col = colour, lwd = basis_lwd,
        lend = "round", ljoin = "round")
  n <- nrow(xy)
  arrows(
    xy[n - 1, 1], xy[n - 1, 2], xy[n, 1], xy[n, 2],
    col = colour, lwd = basis_lwd, length = 0.07, angle = 25, code = 2
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

# Exact three-dimensional coordinate lattice. Each curve varies one ilr
# coordinate while the other two remain on the 0.4-spaced grid. Only offsets
# within radius 1.6 are needed because the radial fade is negligible beyond 2.
grid_offsets <- seq(-1.6, 1.6, by = grid_step)
grid_range <- seq(-3, 3, length.out = 260)
axis_colours <- c(teal, coral, blue)

for (axis in 1:3) {
  fixed_axes <- setdiff(1:3, axis)
  for (a in grid_offsets) {
    for (b in grid_offsets) {
      if (sqrt(a^2 + b^2) > 1.65) next
      z <- matrix(0, nrow = length(grid_range), ncol = 3)
      z[, axis] <- grid_range
      z[, fixed_axes[[1]]] <- a
      z[, fixed_axes[[2]]] <- b
      draw_faded_curve(z, axis_colours[[axis]])
    }
  }
}

# Tetrahedral boundary. Rear edges are lighter to preserve depth without
# changing their geometry.
front_edges <- rbind(c(1, 2), c(2, 3), c(3, 1))
rear_edges <- rbind(c(1, 4), c(2, 4), c(3, 4))
for (edge in seq_len(nrow(rear_edges))) {
  ij <- rear_edges[edge, ]
  segments(
    tetra_vertices[ij[1], 1], tetra_vertices[ij[1], 2],
    tetra_vertices[ij[2], 1], tetra_vertices[ij[2], 2],
    col = grDevices::adjustcolor(navy, alpha.f = 0.48),
    lwd = basis_lwd, lty = 2
  )
}
for (edge in seq_len(nrow(front_edges))) {
  ij <- front_edges[edge, ]
  segments(
    tetra_vertices[ij[1], 1], tetra_vertices[ij[1], 2],
    tetra_vertices[ij[2], 1], tetra_vertices[ij[2], 2],
    col = navy, lwd = basis_lwd
  )
}

# Three orthonormal basis vectors of Aitchison norm 0.4.
for (axis in 1:3) draw_basis_vector(axis, axis_colours[[axis]])

origin <- project_xy(matrix(c(0, 0, 0), nrow = 1))
points(origin[1, 1], origin[1, 2], pch = 21, bg = navy, col = navy,
       cex = origin_cex)

text(
  600, 245, "coda.base",
  col = navy, cex = 4.3, font = 2, family = "sans"
)

dev.off()

rsvg::rsvg_png(svg_file, png_file, width = 1400, height = 1400)

message("Created ", svg_file)
message("Created ", png_file)
message("Rotation seed: ", rotation_seed)
message("det(rotation): ", format(det(rotation), digits = 10))
