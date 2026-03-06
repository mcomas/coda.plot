#' Geometric Mean Barplot for Compositional Data
#'
#' Generates a barplot based on the geometric mean of compositional parts. Optionally,
#' it can compare groups, display the parts on the x-axis, overlay boxplots, or use centered log-ratio (clr) transformation.
#'
#' @param X A numeric matrix or data frame representing compositional data.
#'   Each row is an observation and each column is a part (must be strictly positive).
#' @param group A factor or character vector indicating group membership for each observation.
#'   Must have length \code{nrow(X)}.
#' @param x_show_parts Logical. If \code{TRUE}, the x-axis displays parts instead of group labels. Default is \code{TRUE}.
#' @param include_boxplot Logical. If \code{TRUE}, a boxplot is overlaid on top of the barplot. Default is \code{FALSE}.
#' @param clr_scale Logical. If \code{TRUE}, the data are transformed to clr coordinates before computing means. Default is \code{FALSE}.
#'
#' @return A \code{ggplot2} object representing the geometric mean barplot.
#'
#' @details
#' For each part, the function computes (within each group) the mean of either \code{log(X)} (default)
#' or \code{clr(X)} (\code{clr_scale = TRUE}), and subtracts the overall mean across all observations.
#' Therefore, bars represent deviations from the overall (global) mean on the chosen scale.
#' Overlaying a boxplot can help visualize within-group variability.
#'
#' @examples
#' set.seed(1)
#' X <- matrix(runif(30, 1, 10), ncol = 3)
#' colnames(X) <- c("A", "B", "C")
#' group <- rep(c("G1", "G2"), each = 5)
#' geometric_mean_barplot(X, group, include_boxplot = TRUE)
#' geometric_mean_barplot(X, group, clr_scale = TRUE)
#'
#' @export
geometric_mean_barplot <- function(X, group,
                                   x_show_parts = TRUE,
                                   include_boxplot = FALSE,
                                   clr_scale = FALSE){

  TITLE <- "Geometric mean bar plot"
  XLAB  <- ""
  YLAB  <- "ln(group gmean) - ln(overall gmean)"
  if(isTRUE(clr_scale)) YLAB <- "clr(group gmean) - clr(overall gmean)"

  dplot <- geometric_mean_data(X, group = group, clr_scale = clr_scale)

  p <- ggplot2::ggplot() + ggplot2::geom_hline(yintercept = 0)

  if(isTRUE(x_show_parts)){
    p <- p +
      ggplot2::stat_summary(
        data = dplot,
        ggplot2::aes(x = part, y = lx.diff, fill = group),
        fun = "mean",
        geom = "bar",
        colour = "black",
        position = ggplot2::position_dodge(width = 0.9),
        alpha = 0.8
      )

    if(isTRUE(include_boxplot)){
      p <- p +
        ggplot2::geom_boxplot(
          data = dplot,
          ggplot2::aes(x = part, y = lx.diff, fill = group),
          alpha = 0.3,
          position = ggplot2::position_dodge(width = 0.9),
          width = 0.25,
          show.legend = FALSE,
          outlier.size = 0.6
        )
    }

    p <- p +
      ggplot2::facet_wrap(~part, nrow = 1, drop = TRUE, scales = "free_x", strip.position = "bottom") +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        panel.grid.minor.x = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank()
      )

  } else {

    p <- p +
      ggplot2::stat_summary(
        data = dplot,
        ggplot2::aes(x = group, y = lx.diff, fill = part),
        fun = "mean",
        geom = "bar",
        colour = "black",
        position = ggplot2::position_dodge(width = 0.9),
        alpha = 0.8
      )

    if(isTRUE(include_boxplot)){
      p <- p +
        ggplot2::geom_boxplot(
          data = dplot,
          ggplot2::aes(x = group, y = lx.diff, fill = part),
          alpha = 0.3,
          position = ggplot2::position_dodge(width = 0.9),
          width = 0.25,
          show.legend = FALSE,
          outlier.size = 0.6
        )
    }

    p <- p +
      ggplot2::facet_wrap(~group, nrow = 1, scales = "free_x") +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        panel.grid.minor.x = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank()
      )
  }

  p + ggplot2::labs(title = TITLE, x = XLAB, y = YLAB, fill = "")
}


geometric_mean_data <- function(X, group, clr_scale = FALSE){
  if(missing(group) || is.null(group)) stop("group is required.")
  if(!is.matrix(X)) X <- as.matrix(X)
  if(any(!is.finite(X))) stop("X contains non-finite values.")
  if(any(X <= 0)) stop("X must be strictly positive.")
  if(is.null(colnames(X))) colnames(X) <- paste0("p", seq_len(ncol(X)))

  n <- nrow(X)
  if(length(group) != n) stop("length(group) must equal nrow(X).")
  if(any(is.na(group))) stop("group contains NA values.")
  group <- as.factor(group)

  rs <- rowSums(X)
  if(any(rs <= 0)) stop("Row sums must be positive.")
  X <- X / rs

  if(isTRUE(clr_scale)){
    LX <- coda.base::coordinates(X, "clr")
    LX <- matrix(LX, ncol = ncol(X), dimnames = list(NULL, colnames(X)))
  } else {
    LX <- log(X)
  }

  dplot <- do.call(rbind, lapply(seq_len(ncol(LX)), function(j){
    part <- colnames(LX)[j]
    data.frame(
      group = group,
      part  = part,
      lx.diff = LX[, j] - mean(LX[, j]),
      stringsAsFactors = FALSE
    )
  }))

  dplot$part <- factor(dplot$part, levels = colnames(X))
  dplot
}
