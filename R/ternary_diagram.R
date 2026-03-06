#' Ternary diagram for compositional data (D = 3)
#'
#' Create a ternary diagram from compositional data with exactly three parts.
#' Optionally center and/or scale the data in log-ratio coordinates, color points by
#' group, and overlay the first two principal component directions computed in
#' \emph{ilr} coordinates.
#'
#' @param X A numeric matrix or data frame with exactly three columns (the parts of the composition).
#'   Values should be positive. Column names (if present) are used as corner labels.
#' @param group Optional. A factor or character vector of length \code{nrow(X)} used to color points by group.
#' @param center Logical. If \code{TRUE}, center the log-ratio coordinates before plotting. Default is \code{FALSE}.
#' @param scale Logical. If \code{TRUE}, scale log-ratio coordinates to unit variance before plotting. Default is \code{FALSE}.
#' @param show_pc Logical. If \code{TRUE}, overlay the first two principal component directions computed on
#'   log-ratio coordinates (recommended: \emph{ilr}). Default is \code{FALSE}.
#'
#' @details
#' \strong{Input requirements.} \code{X} must have exactly three columns. For log-ratio methods,
#' parts should be strictly positive. If \code{X} contains zeros or non-positive values, log-ratio
#' transforms may fail; consider applying a zero-replacement strategy before calling this function.
#'
#' \strong{Centering/scaling.} When \code{center} and/or \code{scale} are enabled, the function
#' transforms \code{X} to log-ratio coordinates (via \code{coda.base::coordinates()}), applies
#' centering/scaling in Euclidean space, and maps back to the simplex using
#' \code{coda.base::composition()}.
#'
#' \strong{Principal components.} If \code{show_pc = TRUE}, the function computes principal components
#' in log-ratio space (recommended: \emph{ilr} basis for \eqn{D = 3}) and draws the first two PC
#' directions back in the simplex as curves on the ternary diagram. When \code{group} is provided,
#' PCs are computed and drawn separately for each group.
#'
#' @return A \code{ggplot2} object.
#'
#' @seealso \code{\link[coda.base]{composition}}, \code{\link[coda.base]{coordinates}},
#'   \code{\link[coda.base]{ilr_basis}}
#'
#' @examples
#' # Example with grouping
#' X <- milk_cows[, 5:7]
#' group <- milk_cows$group
#' ternary_diagram(X, group = group)
#'
#' # With centering/scaling and principal components
#' ternary_diagram(X, group = group, center = TRUE, scale = TRUE, show_pc = TRUE)
#'
#' # Without grouping
#' ternary_diagram(X, show_pc = TRUE)
#'
#' @export
ternary_diagram <- function(X, group = NULL,
                            center = FALSE, scale = FALSE,
                            show_pc = FALSE){

  composition <- function(x, ...) suppressWarnings(coda.base::composition(x, ...))

  if(!is.matrix(X)) X <- as.matrix(X)
  if(ncol(X) != 3) stop("three columns needed")
  if(any(!is.finite(X))) stop("X contains non-finite values")
  if(any(X <= 0)) warning("X contains non-positive values; CoDa transforms may fail (consider zero replacement).")

  GROUPED <- !is.null(group)
  if(GROUPED){
    if(length(group) != nrow(X)) stop("length(group) must equal nrow(X)")
    group <- as.factor(group)
  }

  xyz_labs <- colnames(X)
  if(is.null(xyz_labs)) xyz_labs <- c("c1","c2","c3")

  # center/scale in (Euclidean) coordinates, then back to composition
  if(center || scale){
    X <- composition(scale(coda.base::coordinates(X), center = center, scale = scale))
  }

  dplot <- as.data.frame(matrix(X, ncol = 3))
  names(dplot) <- c("c1","c2","c3")

  to_ternary <- function(df){
    df <- transform(df,
                    .A = c1/(c1+c2+c3),
                    .B = c2/(c1+c2+c3),
                    .C = c3/(c1+c2+c3)
    )
    transform(df,
              .x = .C + 0.5 * .A,
              .y = sqrt(3)/2 * .A
    )
  }
  dplot <- to_ternary(dplot)
  if(GROUPED) dplot$group <- group

  geom_ternary_outline <- function(...) {
    ggplot2::geom_path(
      data = data.frame(.x = c(0, 1, 0.5, 0),
                        .y = c(0, 0, sqrt(3)/2, 0)),
      ggplot2::aes(.x, .y), inherit.aes = FALSE, ...
    )
  }

  ternary_isolines_df <- function(var = c("A","B","C"), ticks = seq(0.1, 0.9, 0.1), n = 120) {
    var <- match.arg(var)
    out <- lapply(seq_along(ticks), function(k) {
      tk <- ticks[k]
      if (var == "A") {
        Bv <- seq(0, 1 - tk, length.out = n); Cv <- (1 - tk) - Bv; Av <- rep(tk, n)
      } else if (var == "B") {
        Av <- seq(0, 1 - tk, length.out = n); Cv <- (1 - tk) - Av; Bv <- rep(tk, n)
      } else {
        Av <- seq(0, 1 - tk, length.out = n); Bv <- (1 - tk) - Av; Cv <- rep(tk, n)
      }
      df <- to_ternary(data.frame(c1 = Av, c2 = Bv, c3 = Cv))
      df$gid <- paste0(var, "_", k)   # group id (avoids 'tick' NSE NOTE)
      df
    })
    do.call(rbind, out)
  }

  geom_ternary_grid <- function(ticks = seq(0.1, 0.9, 0.1), alpha = 0.3, ...) {
    gA <- ternary_isolines_df("A", ticks)
    gB <- ternary_isolines_df("B", ticks)
    gC <- ternary_isolines_df("C", ticks)
    list(
      ggplot2::geom_path(data = gA, ggplot2::aes(.x, .y, group = gid), inherit.aes = FALSE, alpha = alpha, ...),
      ggplot2::geom_path(data = gB, ggplot2::aes(.x, .y, group = gid), inherit.aes = FALSE, alpha = alpha, ...),
      ggplot2::geom_path(data = gC, ggplot2::aes(.x, .y, group = gid), inherit.aes = FALSE, alpha = alpha, ...)
    )
  }

  geom_ternary_corner_labels <- function(labels, ...) {
    labs <- data.frame(
      lab = unname(labels),
      .x  = c(0.5, 0, 1),
      .y  = c(sqrt(3)/2, 0, 0)
    )
    ggplot2::geom_text(
      data = labs, ggplot2::aes(.x, .y, label = lab),
      inherit.aes = FALSE, vjust = c(-0.4, 1.2, 1.2), ...
    )
  }

  theme_ternary <- function() {
    ggplot2::theme_void() + ggplot2::theme(plot.margin = ggplot2::margin(10,10,10,10))
  }

  p <- ggplot2::ggplot(dplot) +
    geom_ternary_grid() +
    geom_ternary_outline() +
    geom_ternary_corner_labels(xyz_labs) +
    ggplot2::coord_equal() +
    theme_ternary()

  if(GROUPED){
    p <- p + ggplot2::geom_point(ggplot2::aes(.x, .y, col = group)) + ggplot2::labs(color = "")
  } else {
    p <- p + ggplot2::geom_point(ggplot2::aes(.x, .y))
  }

  if(show_pc){
    # PCA in ilr coordinates (recommended)
    B <- coda.base::ilr_basis(3)
    H <- coda.base::coordinates(X, B)

    add_pc_paths <- function(H_use, group_value = NULL, eps = 1e-3, n = 600){
      eig <- eigen(stats::cov(H_use))
      mu  <- colMeans(H_use)

      # Checks whether a point in ilr-space maps to a composition safely inside the simplex
      inside_simplex <- function(h){
        x <- coda.base::composition(matrix(h, nrow = 1), B)
        all(is.finite(x)) && min(x) > eps && max(x) < 1 - eps
      }

      # Find maximum |t| in direction 'dir' (+1 or -1) such that composition stays inside (min part > eps)
      find_limit <- function(v, i, dir = 1, t0 = 3, grow = 2, max_expand = 25, bisect = 40){
        v <- sqrt(eig$values[i]) * v

        t_in  <- 0
        t_out <- t0
        ok_out <- inside_simplex(mu + dir * t_out * v)

        k <- 0
        while(ok_out && k < max_expand){
          t_in <- t_out
          t_out <- t_out * grow
          ok_out <- inside_simplex(mu + dir * t_out * v)
          k <- k + 1
        }

        if(ok_out) return(t_out)

        lo <- t_in
        hi <- t_out
        for(j in seq_len(bisect)){
          mid <- (lo + hi)/2
          if(inside_simplex(mu + dir * mid * v)){
            lo <- mid
          } else {
            hi <- mid
          }
        }
        lo
      }

      mk_path <- function(i){
        v <- eig$vectors[, i]

        t_pos <- find_limit(v, i = i, dir = +1)
        t_neg <- find_limit(v, i = i, dir = -1)

        tgrid <- seq(-t_neg, t_pos, length.out = n)

        Hline <- sweep(outer(tgrid, sqrt(eig$values[i]) * v), 2, mu, `+`)
        Xline <- coda.base::composition(Hline, B)

        df <- as.data.frame(Xline); names(df) <- c("c1","c2","c3")
        df <- to_ternary(df)
        if(!is.null(group_value)) df$group <- group_value
        df
      }

      list(pc1 = mk_path(1), pc2 = mk_path(2))
    }

    if(GROUPED){
      for(gk in levels(group)){
        Hk <- H[group == gk, , drop = FALSE]
        pcs <- add_pc_paths(Hk, gk)
        p <- p +
          ggplot2::geom_path(data = pcs$pc1, ggplot2::aes(.x, .y, col = group, linetype = "Prin.Comp.1")) +
          ggplot2::geom_path(data = pcs$pc2, ggplot2::aes(.x, .y, col = group, linetype = "Prin.Comp.2"))
      }
      p <- p +
        ggplot2::scale_linetype_manual(values = c("dashed", "dotted")) +
        ggplot2::labs(col = "", linetype = "") +
        ggplot2::guides(color = ggplot2::guide_legend(order = 1),
                        linetype = ggplot2::guide_legend(order = 2))
    } else {
      pcs <- add_pc_paths(H)
      p <- p +
        ggplot2::geom_path(data = pcs$pc1, ggplot2::aes(.x, .y, linetype = "Prin.Comp.1")) +
        ggplot2::geom_path(data = pcs$pc2, ggplot2::aes(.x, .y, linetype = "Prin.Comp.2")) +
        ggplot2::scale_linetype_manual(values = c("dashed", "dotted")) +
        ggplot2::labs(linetype = "")
    }
  }

  p
}
