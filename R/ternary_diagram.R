#' Ternary frame for compositional data (D = 3)
#'
#' Build a ternary plotting frame for compositional data with exactly three parts.
#' The returned object stores the geometric and log-ratio transformation machinery
#' needed to add multiple data layers consistently to the same ternary diagram.
#'
#' @param X A numeric matrix or data frame with exactly three columns.
#'   This data defines the reference frame used for centering/scaling in log-ratio space.
#' @param center Logical. If \code{TRUE}, center log-ratio coordinates using the mean
#'   coordinates of \code{X}. Default is \code{FALSE}.
#' @param scale Logical or numeric. If \code{FALSE}, no scaling is applied. If \code{TRUE},
#'   log-ratio coordinates are scaled by their empirical standard deviations. If a single
#'   positive numeric value is supplied, centered log-ratio coordinates are multiplied
#'   by that value.
#' @param labels Optional character vector of length 3 used as corner labels.
#'   If \code{NULL}, \code{colnames(X)} are used when available. Otherwise, temporary
#'   labels \code{c1}, \code{c2}, \code{c3} are used.
#'
#' @return An object of class \code{"ternary_frame"}.
#'
#' @export
ternary_frame <- function(X, center = FALSE, scale = FALSE, labels = NULL) {
  ternary_frame_impl(X, center = center, scale = scale, labels = labels,
                     labels_are_default = NULL)
}

# internal constructor
ternary_frame_impl <- function(X, center = FALSE, scale = FALSE, labels = NULL,
                               labels_are_default = NULL) {
  composition <- function(x, ...) suppressWarnings(coda.base::composition(x, ...))

  if (!is.matrix(X)) X <- as.matrix(X)
  storage.mode(X) <- "double"

  if (ncol(X) != 3) stop("three columns needed")
  if (any(!is.finite(X))) stop("X contains non-finite values")
  if (any(X <= 0)) {
    warning("X contains non-positive values; CoDa transforms may fail (consider zero replacement).")
  }

  if (!is.logical(center) || length(center) != 1 || is.na(center)) {
    stop("'center' must be TRUE or FALSE")
  }

  if (is.null(labels)) {
    labs <- colnames(X)
    if (is.null(labs)) {
      labs <- c("c1", "c2", "c3")
      labels_default <- TRUE
    } else {
      labels_default <- FALSE
    }
  } else {
    labs <- labels
    labels_default <- FALSE
  }

  if (!is.null(labels_are_default)) {
    labels_default <- isTRUE(labels_are_default)
  }

  if (length(labs) != 3) stop("'labels' must have length 3")

  to_ternary <- function(df) {
    df <- transform(
      df,
      .A = c1 / (c1 + c2 + c3),
      .B = c2 / (c1 + c2 + c3),
      .C = c3 / (c1 + c2 + c3)
    )
    transform(
      df,
      .x = .C + 0.5 * .A,
      .y = sqrt(3) / 2 * .A
    )
  }

  H_raw <- coda.base::coordinates(X)

  center_vec <- rep(0, ncol(H_raw))
  if (center) {
    center_vec <- colMeans(H_raw)
  }

  scale_mode <- "none"
  scale_vec <- rep(1, ncol(H_raw))
  scale_num <- 1

  if (is.logical(scale)) {
    if (length(scale) != 1 || is.na(scale)) {
      stop("'scale' must be a single TRUE/FALSE or a positive numeric value")
    }
    if (scale) {
      scale_mode <- "sd"
      scale_vec <- apply(H_raw, 2, stats::sd)
      scale_vec[!is.finite(scale_vec) | scale_vec == 0] <- 1
    }
  } else if (is.numeric(scale)) {
    if (length(scale) != 1 || !is.finite(scale) || scale <= 0) {
      stop("numeric 'scale' must be a single positive finite value")
    }
    scale_mode <- "numeric"
    scale_num <- scale
  } else {
    stop("'scale' must be FALSE, TRUE, or a positive numeric value")
  }

  apply_transform <- isTRUE(center) || !identical(scale, FALSE)

  transform_simplex <- function(Y) {
    if (!is.matrix(Y)) Y <- as.matrix(Y)
    storage.mode(Y) <- "double"

    if (ncol(Y) != 3) stop("three columns needed")
    if (any(!is.finite(Y))) stop("Y contains non-finite values")

    H <- coda.base::coordinates(Y)

    if (center) {
      H <- sweep(H, 2, center_vec, FUN = "-")
    }

    if (scale_mode == "sd") {
      H <- sweep(H, 2, scale_vec, FUN = "/")
    } else if (scale_mode == "numeric") {
      H <- H * scale_num
    }

    composition(H)
  }

  structure(
    list(
      X_ref = X,
      labels = unname(labs),
      labels_are_default = labels_default,
      center = center,
      scale = scale,
      scale_mode = scale_mode,
      scale_vec = scale_vec,
      scale_num = scale_num,
      center_vec = center_vec,
      apply_transform = apply_transform,
      to_ternary = to_ternary,
      transform_simplex = transform_simplex
    ),
    class = "ternary_frame"
  )
}

# internal helper
new_default_ternary_frame <- function() {
  X0 <- matrix(rep(1 / 3, 3), nrow = 1)
  colnames(X0) <- c("c1", "c2", "c3")
  ternary_frame_impl(
    X0,
    center = FALSE,
    scale = FALSE,
    labels = c("c1", "c2", "c3"),
    labels_are_default = TRUE
  )
}

# internal helper
set_ternary_frame <- function(p, frame) {
  attr(p, "ternary_frame") <- frame
  p
}

# internal helper
get_ternary_frame <- function(p) {
  frame <- attr(p, "ternary_frame")
  if (is.null(frame) || !inherits(frame, "ternary_frame")) {
    stop("The ggplot object does not contain a ternary_frame.")
  }
  frame
}

# internal helper
ternary_add_layer <- function(p, layer) {
  p$layers <- c(p$layers, list(layer))
  p
}

# internal helper
ternary_add_layers <- function(p, layers) {
  p$layers <- c(p$layers, layers)
  p
}

# internal helper
ternary_make_outline_layer <- function(...) {
  ggplot2::geom_path(
    data = data.frame(
      .x = c(0, 1, 0.5, 0),
      .y = c(0, 0, sqrt(3) / 2, 0)
    ),
    ggplot2::aes(.x, .y),
    inherit.aes = FALSE,
    ...
  )
}

# internal helper
ternary_make_label_layer <- function(labels, ...) {
  labs <- data.frame(
    lab = unname(labels),
    .x = c(0.5, 0, 1),
    .y = c(sqrt(3) / 2, 0, 0)
  )

  ggplot2::geom_text(
    data = labs,
    ggplot2::aes(.x, .y, label = lab),
    inherit.aes = FALSE,
    vjust = c(-0.4, 1.2, 1.2),
    ...
  )
}

# internal helper
infer_ternary_labels <- function(X) {
  cn <- colnames(X)
  if (is.null(cn) || length(cn) != 3) return(NULL)
  unname(cn)
}

# internal helper
ternary_refresh_label_layer <- function(p) {
  frame <- get_ternary_frame(p)

  if (!isTRUE(attr(p, "ternary_show_labels"))) {
    return(p)
  }

  idx <- attr(p, "ternary_label_layer_idx")
  if (!is.null(idx) && length(p$layers) >= idx) {
    p$layers <- p$layers[-idx]
  }

  p <- ternary_add_layer(p, ternary_make_label_layer(frame$labels))
  attr(p, "ternary_label_layer_idx") <- length(p$layers)
  p
}

# internal helper
ternary_maybe_adopt_labels <- function(p, X) {
  frame <- get_ternary_frame(p)

  if (!isTRUE(frame$labels_are_default)) {
    return(p)
  }

  new_labels <- infer_ternary_labels(X)
  if (is.null(new_labels)) {
    return(p)
  }

  frame$labels <- new_labels
  frame$labels_are_default <- FALSE
  p <- set_ternary_frame(p, frame)
  p <- ternary_refresh_label_layer(p)
  p
}

# internal helper
prepare_ternary_data <- function(frame, X, transform = TRUE, group = NULL) {
  if (!inherits(frame, "ternary_frame")) stop("'frame' must be a ternary_frame object")

  if (!is.matrix(X)) X <- as.matrix(X)
  storage.mode(X) <- "double"

  if (ncol(X) != 3) stop("three columns needed")
  if (any(!is.finite(X))) stop("X contains non-finite values")

  if (!is.null(group) && length(group) != nrow(X)) {
    stop("length(group) must equal nrow(X)")
  }

  X_use <- X
  if (isTRUE(transform) && frame$apply_transform) {
    X_use <- frame$transform_simplex(X)
  }

  df <- as.data.frame(X_use)
  names(df) <- c("c1", "c2", "c3")
  df <- frame$to_ternary(df)

  if (!is.null(group)) {
    df$group <- as.factor(group)
  }

  df
}

#' Transform compositional data into ternary plotting coordinates
#'
#' Convert a compositional dataset into ternary plotting coordinates under a given
#' \code{ternary_frame}.
#'
#' @param frame A \code{ternary_frame} object.
#' @param X A numeric matrix or data frame with exactly three columns.
#' @param transform Logical. If \code{TRUE}, apply the frame transformation before
#'   converting to ternary coordinates. Default is \code{TRUE}.
#' @param group Optional grouping variable of length \code{nrow(X)}.
#'
#' @return A data frame with compositional columns \code{c1}, \code{c2}, \code{c3},
#'   ternary coordinates \code{.x}, \code{.y}, and optionally \code{group}.
#'
#' @export
ternary_coords <- function(frame, X, transform = TRUE, group = NULL) {
  prepare_ternary_data(frame, X, transform = transform, group = group)
}

# internal helper
ternary_grid_data <- function(frame, ticks = seq(0.1, 0.9, 0.1), n = 300, eps = 1e-6) {
  if (!inherits(frame, "ternary_frame")) stop("'frame' must be a ternary_frame object")

  isolines_one <- function(var = c("A", "B", "C")) {
    var <- match.arg(var)

    out <- lapply(seq_along(ticks), function(k) {
      tk <- ticks[k]

      if (var == "A") {
        Bv <- seq(0, 1 - tk, length.out = n)
        Cv <- (1 - tk) - Bv
        Av <- rep(tk, n)
      } else if (var == "B") {
        Av <- seq(0, 1 - tk, length.out = n)
        Cv <- (1 - tk) - Av
        Bv <- rep(tk, n)
      } else {
        Av <- seq(0, 1 - tk, length.out = n)
        Bv <- (1 - tk) - Av
        Cv <- rep(tk, n)
      }

      M <- cbind(Av, Bv, Cv)
      colnames(M) <- c("c1", "c2", "c3")

      if (frame$apply_transform) {
        M <- M + eps
        M <- M / rowSums(M)
        M <- frame$transform_simplex(M)
      }

      df <- as.data.frame(M)
      names(df) <- c("c1", "c2", "c3")
      df <- frame$to_ternary(df)
      df$gid <- paste0(var, "_", k)
      df
    })

    do.call(rbind, out)
  }

  list(
    A = isolines_one("A"),
    B = isolines_one("B"),
    C = isolines_one("C")
  )
}

#' Create a base ternary plot
#'
#' Create the base ggplot object associated with a \code{ternary_frame}.
#'
#' @param frame Optional \code{ternary_frame} object. If \code{NULL}, a default
#'   ternary frame is created with no centering and no scaling.
#' @param show_grid Logical. If \code{TRUE}, draw the ternary grid.
#' @param show_outline Logical. If \code{TRUE}, draw the ternary triangle outline.
#' @param show_labels Logical. If \code{TRUE}, draw corner labels.
#' @param grid_ticks Numeric vector of grid levels.
#'
#' @return A \code{ggplot2} object with the \code{ternary_frame} attached as attribute.
#'
#' @export
ternary_plot <- function(frame = NULL,
                         show_grid = TRUE,
                         show_outline = TRUE,
                         show_labels = TRUE,
                         grid_ticks = seq(0.1, 0.9, 0.1)) {
  if (is.null(frame)) {
    frame <- new_default_ternary_frame()
  }

  if (!inherits(frame, "ternary_frame")) stop("'frame' must be a ternary_frame object")

  p <- ggplot2::ggplot()
  p <- set_ternary_frame(p, frame)
  attr(p, "ternary_show_labels") <- isTRUE(show_labels)
  attr(p, "ternary_label_layer_idx") <- NULL

  if (show_grid) {
    p <- add_ternary_grid(p, ticks = grid_ticks)
  }

  if (show_outline) {
    p <- ternary_add_layer(p, ternary_make_outline_layer())
  }

  if (show_labels) {
    p <- ternary_add_layer(p, ternary_make_label_layer(frame$labels))
    attr(p, "ternary_label_layer_idx") <- length(p$layers)
  }

  p <- p +
    ggplot2::coord_equal() +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.margin = ggplot2::margin(10, 10, 10, 10)
    )

  set_ternary_frame(p, frame)
}

#' Add a ternary grid layer
#'
#' @param p A \code{ggplot2} object created by \code{ternary_plot()}.
#' @param ticks Numeric vector of grid levels.
#' @param n Number of sampled points per grid line.
#' @param eps Small positive offset used before log-ratio transformation.
#' @param ... Further arguments passed to \code{ggplot2::geom_path()}.
#'
#' @return A \code{ggplot2} object.
#'
#' @export
add_ternary_grid <- function(p, ticks = seq(0.1, 0.9, 0.1), n = 300, eps = 1e-6, ...) {
  frame <- get_ternary_frame(p)
  g <- ternary_grid_data(frame, ticks = ticks, n = n, eps = eps)

  grid_alpha <- if (identical(frame$scale, FALSE)) 0.30 else 0.18
  grid_colour <- if (identical(frame$scale, FALSE)) "black" else "grey60"
  grid_linewidth <- if (identical(frame$scale, FALSE)) 0.30 else 0.22

  layers <- list(
    ggplot2::geom_path(
      data = g$A,
      ggplot2::aes(.x, .y, group = gid),
      inherit.aes = FALSE,
      alpha = grid_alpha,
      colour = grid_colour,
      linewidth = grid_linewidth,
      ...
    ),
    ggplot2::geom_path(
      data = g$B,
      ggplot2::aes(.x, .y, group = gid),
      inherit.aes = FALSE,
      alpha = grid_alpha,
      colour = grid_colour,
      linewidth = grid_linewidth,
      ...
    ),
    ggplot2::geom_path(
      data = g$C,
      ggplot2::aes(.x, .y, group = gid),
      inherit.aes = FALSE,
      alpha = grid_alpha,
      colour = grid_colour,
      linewidth = grid_linewidth,
      ...
    )
  )

  p <- ternary_add_layers(p, layers)
  set_ternary_frame(p, frame)
}

#' Add compositional points to a ternary plot
#'
#' @param p A \code{ggplot2} object created by \code{ternary_plot()}.
#' @param X A numeric matrix or data frame with exactly three columns.
#' @param group Optional grouping variable of length \code{nrow(X)}.
#' @param transform Logical. If \code{TRUE}, apply the frame transformation.
#' @param ... Further arguments passed to \code{ggplot2::geom_point()}.
#'
#' @return A \code{ggplot2} object.
#'
#' @export
add_ternary_points <- function(p, X, group = NULL, transform = TRUE, ...) {
  p <- ternary_maybe_adopt_labels(p, X)
  frame <- get_ternary_frame(p)

  df <- prepare_ternary_data(frame, X, transform = transform, group = group)

  layer <- if (!is.null(group)) {
    ggplot2::geom_point(
      data = df,
      ggplot2::aes(.x, .y, colour = group),
      inherit.aes = FALSE,
      ...
    )
  } else {
    ggplot2::geom_point(
      data = df,
      ggplot2::aes(.x, .y),
      inherit.aes = FALSE,
      ...
    )
  }

  p <- ternary_add_layer(p, layer)
  set_ternary_frame(p, frame)
}

#' Add a compositional path to a ternary plot
#'
#' @param p A \code{ggplot2} object created by \code{ternary_plot()}.
#' @param X A numeric matrix or data frame with exactly three columns.
#' @param group Optional grouping variable of length \code{nrow(X)} for multiple paths.
#' @param transform Logical. If \code{TRUE}, apply the frame transformation.
#' @param ... Further arguments passed to \code{ggplot2::geom_path()}.
#'
#' @return A \code{ggplot2} object.
#'
#' @export
add_ternary_path <- function(p, X, group = NULL, transform = TRUE, ...) {
  p <- ternary_maybe_adopt_labels(p, X)
  frame <- get_ternary_frame(p)

  df <- prepare_ternary_data(frame, X, transform = transform, group = group)

  layer <- if (!is.null(group)) {
    ggplot2::geom_path(
      data = df,
      ggplot2::aes(.x, .y, group = group),
      inherit.aes = FALSE,
      ...
    )
  } else {
    ggplot2::geom_path(
      data = df,
      ggplot2::aes(.x, .y),
      inherit.aes = FALSE,
      ...
    )
  }

  p <- ternary_add_layer(p, layer)
  set_ternary_frame(p, frame)
}

#' Add principal component paths to a ternary plot
#'
#' @param p A \code{ggplot2} object created by \code{ternary_plot()}.
#' @param X A numeric matrix or data frame with exactly three columns.
#' @param group Optional grouping variable of length \code{nrow(X)}. If supplied,
#'   PCs are computed separately by group.
#' @param pcs Integer vector indicating which principal components to draw.
#' @param basis An ilr basis. Default is \code{coda.base::ilr_basis(3)}.
#' @param n Number of sampled points per PC path.
#' @param eps Small positive threshold used to keep the path inside the simplex.
#' @param ... Further arguments passed to \code{ggplot2::geom_path()}.
#'
#' @return A \code{ggplot2} object.
#'
#' @export
add_ternary_pc <- function(p, X, group = NULL, pcs = 1:2,
                           basis = coda.base::ilr_basis(3),
                           n = 600, eps = 1e-3, ...) {
  p <- ternary_maybe_adopt_labels(p, X)
  frame <- get_ternary_frame(p)

  if (!is.matrix(X)) X <- as.matrix(X)
  storage.mode(X) <- "double"

  if (ncol(X) != 3) stop("three columns needed")
  if (!is.null(group) && length(group) != nrow(X)) {
    stop("length(group) must equal nrow(X)")
  }

  X_use <- if (frame$apply_transform) frame$transform_simplex(X) else X
  H <- coda.base::coordinates(X_use, basis)

  pc_paths_one <- function(H_use, group_value = NULL) {
    eig <- eigen(stats::cov(H_use))
    mu <- colMeans(H_use)

    inside_simplex <- function(h) {
      x <- coda.base::composition(matrix(h, nrow = 1), basis)
      all(is.finite(x)) && min(x) > eps && max(x) < 1 - eps
    }

    find_limit <- function(v, i, dir = 1, t0 = 3, grow = 2, max_expand = 25, bisect = 40) {
      v <- sqrt(eig$values[i]) * v

      t_in <- 0
      t_out <- t0
      ok_out <- inside_simplex(mu + dir * t_out * v)

      k <- 0
      while (ok_out && k < max_expand) {
        t_in <- t_out
        t_out <- t_out * grow
        ok_out <- inside_simplex(mu + dir * t_out * v)
        k <- k + 1
      }

      if (ok_out) return(t_out)

      lo <- t_in
      hi <- t_out
      for (j in seq_len(bisect)) {
        mid <- (lo + hi) / 2
        if (inside_simplex(mu + dir * mid * v)) {
          lo <- mid
        } else {
          hi <- mid
        }
      }
      lo
    }

    mk_path <- function(i) {
      v <- eig$vectors[, i]
      t_pos <- find_limit(v, i = i, dir = +1)
      t_neg <- find_limit(v, i = i, dir = -1)
      tgrid <- seq(-t_neg, t_pos, length.out = n)

      Hline <- sweep(outer(tgrid, sqrt(eig$values[i]) * v), 2, mu, `+`)
      Xline <- coda.base::composition(Hline, basis)

      df <- as.data.frame(Xline)
      names(df) <- c("c1", "c2", "c3")
      df <- frame$to_ternary(df)
      df$pc <- paste0("PC", i)
      if (!is.null(group_value)) df$group <- group_value
      df
    }

    do.call(rbind, lapply(pcs, mk_path))
  }

  layer <- if (!is.null(group)) {
    group <- as.factor(group)

    pcs_df <- do.call(rbind, lapply(levels(group), function(gk) {
      pc_paths_one(H[group == gk, , drop = FALSE], group_value = gk)
    }))

    ggplot2::geom_path(
      data = pcs_df,
      ggplot2::aes(.x, .y, colour = group, linetype = pc),
      inherit.aes = FALSE,
      ...
    )
  } else {
    pcs_df <- pc_paths_one(H)

    ggplot2::geom_path(
      data = pcs_df,
      ggplot2::aes(.x, .y, linetype = pc),
      inherit.aes = FALSE,
      ...
    )
  }

  p <- ternary_add_layer(p, layer)
  set_ternary_frame(p, frame)
}

#' Ternary diagram for compositional data (D = 3)
#'
#' Create a ternary diagram from compositional data with exactly three parts.
#' Optionally center and/or scale the data in log-ratio coordinates, color points by
#' group, and overlay the first two principal component directions computed in
#' \emph{ilr} coordinates.
#'
#' This function is kept as a convenient wrapper around the modular ternary API:
#' \code{ternary_frame()}, \code{ternary_plot()}, \code{add_ternary_points()}, and
#' \code{add_ternary_pc()}.
#'
#' @param X A numeric matrix or data frame with exactly three columns (the parts of the composition).
#'   Values should be positive. Column names (if present) are used as corner labels.
#' @param group Optional. A factor or character vector of length \code{nrow(X)} used to color points by group.
#' @param center Logical. If \code{TRUE}, center the log-ratio coordinates before plotting. Default is \code{FALSE}.
#' @param scale Logical or numeric. If \code{FALSE}, no scaling is applied. If \code{TRUE},
#'   log-ratio coordinates are scaled by their empirical standard deviations. If a single
#'   positive numeric value is supplied, centered log-ratio coordinates are multiplied
#'   by that value, so values larger than 1 increase visual spread and values between 0 and 1 shrink it.
#' @param show_pc Logical. If \code{TRUE}, overlay the first two principal component directions computed on
#'   log-ratio coordinates (recommended: \emph{ilr}). Default is \code{FALSE}.
#'
#' @return A \code{ggplot2} object.
#'
#' @seealso \code{\link{ternary_frame}}, \code{\link{ternary_plot}},
#'   \code{\link{add_ternary_points}}, \code{\link{add_ternary_pc}}
#'
#' @examples
#' X <- milk_cows[, 5:7]
#' group <- milk_cows$group
#'
#' ternary_diagram(X, group = group)
#' ternary_diagram(X, group = group, center = TRUE, scale = TRUE)
#' ternary_diagram(X, group = group, center = TRUE, scale = 1.5)
#' ternary_diagram(X, show_pc = TRUE)
#'
#' @export
ternary_diagram <- function(X, group = NULL,
                            center = FALSE, scale = FALSE,
                            show_pc = FALSE) {
  p <- ternary_plot(
    ternary_frame(X, center = center, scale = scale)
  )

  p <- add_ternary_points(p, X = X, group = group)

  if (show_pc) {
    p <- add_ternary_pc(p, X = X, group = group)
    p <- p +
      ggplot2::scale_linetype_manual(values = c("dashed", "dotted")) +
      ggplot2::labs(linetype = "")

    if (!is.null(group)) {
      p <- p +
        ggplot2::labs(colour = "") +
        ggplot2::guides(
          colour = ggplot2::guide_legend(order = 1),
          linetype = ggplot2::guide_legend(order = 2)
        )
    }
  } else if (!is.null(group)) {
    p <- p + ggplot2::labs(colour = "")
  }

  p
}
