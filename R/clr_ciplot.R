#' Compositional CLR Biplot
#'
#' Generates a centered log-ratio (CLR) biplot for compositional data.
#'
#' @param X A matrix or data frame containing compositional data (strictly positive).
#' @param group Optional factor/character used to color the observations.
#' @param biplot_type Character string specifying the type of biplot. Either
#'   \code{"covariance"} (default) or \code{"form"}.
#' @param alpha Optional numeric in [0,1]. If provided, overrides \code{biplot_type}.
#'   \itemize{
#'     \item \code{alpha = 0}: covariance biplot
#'     \item \code{alpha = 1}: form biplot
#'   }
#' @param shape_group Optional factor/character used to assign shapes to observations.
#' @param return_data Logical. If TRUE, returns a list with data frames for observations,
#'   variables, and the ggplot object.
#' @param repel Logical. If TRUE (default), use ggrepel for variable labels when available.
#' @param repel_force Numeric. Repulsion force passed to \code{ggrepel::geom_text_repel()}.
#' @param repel_max_overlaps Numeric. Maximum overlaps allowed (ggrepel).
#'
#' @return A \code{ggplot2} object. If \code{return_data = TRUE}, a list with elements
#'   \code{obs}, \code{vars}, and \code{plot}.
#' @examples
#' # Basic example (no groups)
#' set.seed(1)
#' X <- matrix(runif(120, 0.1, 10), ncol = 6)
#' colnames(X) <- paste0("p", 1:6)
#' clr_biplot(X)
#'
#' # Grouped example (color)
#' grp <- factor(sample(c("A", "B"), nrow(X), replace = TRUE))
#' clr_biplot(X, group = grp)
#'
#' # Color + shape
#' shp <- factor(sample(c("S1", "S2", "S3"), nrow(X), replace = TRUE))
#' clr_biplot(X, group = grp, shape_group = shp)
#'
#' # Form biplot (alpha = 1) with repelled variable labels (requires ggrepel)
#' clr_biplot(X, group = grp, biplot_type = "form", repel = TRUE)
#'
#' # Covariance biplot (alpha = 0) and custom repel settings
#' clr_biplot(X, group = grp, alpha = 0, repel = TRUE, repel_force = 1.5, repel_max_overlaps = 30)
#' @export
clr_biplot <- function(X, group = NULL, biplot_type = "covariance",
                       alpha = NULL, shape_group = NULL,
                       return_data = FALSE,
                       repel = TRUE,
                       repel_force = 1,
                       repel_max_overlaps = Inf){

  # alpha / type handling
  if(is.null(alpha)){
    if(!biplot_type %in% c("form", "covariance")){
      stop("Select biplot_type between 'covariance' and 'form'.")
    }
    alpha <- if(biplot_type == "form") 1 else 0
  } else {
    if(!is.numeric(alpha) || length(alpha) != 1 || is.na(alpha) || alpha < 0 || alpha > 1){
      stop("alpha must be a single numeric value in [0,1].")
    }
  }

  if(!is.matrix(X)) X <- as.matrix(X)
  if(any(!is.finite(X))) stop("X contains non-finite values.")
  if(any(X <= 0)) stop("X must be strictly positive (required for CLR).")

  if(is.null(colnames(X))) colnames(X) <- paste0("c", seq_len(ncol(X)))

  n <- nrow(X)
  if(!is.null(group) && length(group) != n) stop("length(group) must equal nrow(X).")
  if(!is.null(shape_group) && length(shape_group) != n) stop("length(shape_group) must equal nrow(X).")

  if(!is.null(group)) group <- as.factor(group)
  if(!is.null(shape_group)) shape_group <- as.factor(shape_group)

  # CLR coordinates and SVD
  CLR  <- coda.base::coordinates(X, "clr")
  CLR0 <- base::scale(CLR, center = TRUE, scale = FALSE)

  SVD  <- base::svd(CLR0)
  PEXP <- 100 * (SVD$d^2) / sum(SVD$d^2)

  # Biplot factors
  FX <- SVD$u %*% diag(SVD$d^alpha)
  GX <- SVD$v %*% diag((SVD$d)^(1 - alpha))

  I1 <- 1
  I2 <- 2
  FX <- FX[, c(I1, I2), drop = FALSE]
  GX <- GX[, c(I1, I2), drop = FALSE]
  colnames(FX) <- c("x", "y")
  colnames(GX) <- c("x", "y")

  # Safe rescaling to comparable ranges
  sF <- max(abs(FX)); if(!is.finite(sF) || sF == 0) sF <- 1
  sG <- max(abs(GX)); if(!is.finite(sG) || sG == 0) sG <- 1
  FX <- FX / sF
  GX <- GX / sG

  dF <- as.data.frame(FX)
  dG <- as.data.frame(GX)
  dG$text <- sprintf("clr(%s)", colnames(X))

  # Axis ranges / breaks
  xr <- range(dF$x, dG$x)
  yr <- range(dF$y, dG$y)
  xyrange <- range(c(dF$x, dF$y, dG$x, dG$y))
  xybreaks <- pretty(xyrange, 10)

  # Base plot + points
  p <- ggplot2::ggplot()

  # points (no rlang dependency)
  if(is.null(group) && is.null(shape_group)){
    p <- p + ggplot2::geom_point(data = dF, ggplot2::aes(x = x, y = y))
  } else if(!is.null(group) && is.null(shape_group)){
    dF$col <- group
    p <- p + ggplot2::geom_point(data = dF, ggplot2::aes(x = x, y = y, colour = col))
  } else if(is.null(group) && !is.null(shape_group)){
    dF$shape <- shape_group
    p <- p + ggplot2::geom_point(data = dF, ggplot2::aes(x = x, y = y, shape = shape))
  } else {
    dF$col <- group
    dF$shape <- shape_group
    p <- p + ggplot2::geom_point(data = dF, ggplot2::aes(x = x, y = y, colour = col, shape = shape))
  }

  # Variable arrows
  p <- p + ggplot2::geom_segment(
    data = dG,
    ggplot2::aes(x = 0, xend = x, y = 0, yend = y)
  )

  # Variable labels (ggrepel when available)
  use_repel <- isTRUE(repel) && requireNamespace("ggrepel", quietly = TRUE)

  if(use_repel){
    p <- p + ggrepel::geom_text_repel(
      data = dG,
      mapping = ggplot2::aes(x = x, y = y, label = text),
      fontface = "bold",
      force = repel_force,
      max.overlaps = repel_max_overlaps,
      box.padding = 0.25,
      point.padding = 0.15,
      min.segment.length = 0
    )
  } else {
    p <- p + ggplot2::geom_text(
      data = dG,
      ggplot2::aes(x = x, y = y, label = text),
      fontface = "bold"
    )
  }

  p <- p +
    ggplot2::geom_hline(yintercept = 0, linetype = "dotted") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dotted") +
    ggplot2::scale_x_continuous(
      limits = mean(xr) + 1.2 * (xr - mean(xr)),
      breaks = xybreaks
    ) +
    ggplot2::scale_y_continuous(
      limits = mean(yr) + 1.2 * (yr - mean(yr)),
      breaks = xybreaks
    ) +
    ggplot2::coord_equal() +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    ) +
    ggplot2::labs(colour = "", shape = "")

  # Axis labels: only for form biplot (alpha = 1)
  if(alpha == 1){
    p <- p + ggplot2::labs(
      x = sprintf("PC%d (%0.1f%%)", I1, PEXP[I1]),
      y = sprintf("PC%d (%0.1f%%)", I2, PEXP[I2])
    )
  } else {
    p <- p + ggplot2::labs(x = "", y = "")
  }

  if(return_data){
    return(list(obs = dF, vars = dG, plot = p))
  }
  p
}
