#' Compositional CLR Biplot
#'
#' Generates a centered log-ratio (CLR) biplot for compositional data.
#'
#' @param X A matrix or data frame containing compositional data.
#' @param group factor used to color the observations.
#' @param biplot_type Character string specifying the type of biplot. Either `"covariance"` (default) or `"form"`.
#' @param alpha Optional numeric value between 0 and 1. If provided, this overrides \code{biplot_type}. Controls the type of biplot:
#'   \itemize{
#'     \item 0 = covariance biplot
#'     \item 1 = form biplot
#'   }
#' @param shape_group Optional factor used to assign shapes to the observations.
#'
#' @return A \code{ggplot} object displaying the biplot.
#' @export
clr_biplot = function(X, group = NULL, biplot_type = 'covariance',
                      alpha = NULL, shape_group = NULL){
  if(is.null(alpha)){
    if(biplot_type == 'form' | biplot_type == 'covariance'){
      if(biplot_type == 'form'){
        alpha = 1
      }
      if(biplot_type == 'covariance'){
        alpha = 0
      }
    }else{
      stop("Select between covariance and form biplot_type")
    }
  }
  if(!is.matrix(X)){
    X = as.matrix(X)
  }
  CLR = coda.base::coordinates(X, 'clr')
  CLR0 = base::scale(CLR, scale = FALSE)
  SVD = svd(CLR0)
  PEXP = 100*SVD$d^2/sum(SVD$d^2)
  # CLR0[1:5,1:3]
  # (SVD$u %*% diag(SVD$d) %*% t(SVD$v))[1:5,1:3]

  # alpha = 1  # form clr-biplot
  # alpha = 0  # covariance clr-biplot
  FX = SVD$u %*% diag(SVD$d^alpha)
  GX = SVD$v %*% diag((SVD$d)^(1-alpha))

  I1 = 1
  I2 = 2

  FX = FX[,c(I1,I2)]
  GX = GX[,c(I1,I2)]

  colnames(FX) = c('x', 'y')
  colnames(GX) = c('x', 'y')

  FX = FX/max(abs(FX))
  GX = GX/max(abs(GX))

  xyrange = range(FX,GX)
  xr = range(FX[,1],GX[,1])
  yr = range(FX[,2],GX[,2])
  xybreaks = pretty(xyrange, 10)

  dF = as.data.frame(FX)
  dG = as.data.frame(GX)
  dG$text = sprintf("clr(%s)", colnames(X))
  if(is.null(group) & is.null(shape_group)){
    p = ggplot() +
      geom_point(data = dF, aes(x = x, y = y))
  }else{
    if(!is.null(group) & !is.null(shape_group)){
      dF$col = group
      dF$shape = shape_group
      p = ggplot() +
        geom_point(data = dF, aes(x = x, y = y, col = col, shape = shape))
    }else{
      if(!is.null(group)){
        dF$col = group
        p = ggplot() +
          geom_point(data = dF, aes(x = x, y = y, col = col))
      }
      if(!is.null(shape_group)){
        dF$shape = shape_group
        p = ggplot() +
          geom_point(data = dF, aes(x = x, y = y, shape = shape))
      }
    }
  }
  p = p +
    geom_segment(data = dG, aes(x = 0, xend = x, y = 0, yend = y), col = 'red') +
    geom_text(data=dG, aes(x = x, y = y, label = text), fontface = 'bold') +
    geom_hline(yintercept = 0, linetype = 'dotted') + geom_vline(xintercept = 0, linetype = 'dotted') +
    scale_x_continuous(limits = mean(xr) + 1.2*(xr - mean(xr)), breaks = xybreaks) +
    scale_y_continuous(limits = mean(yr) + 1.2*(yr - mean(yr)), breaks = xybreaks) +
    coord_equal() +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(), panel.grid.minor = element_blank())
  if(alpha == 1){
    p = p +
      labs(x = sprintf("PC%d (%0.1f%%)", I1, PEXP[I1]), y = sprintf("PC%d (%0.1f%%)", I2, PEXP[I2]))
  }else{
    p = p +
      labs(x = "", y = "")
  }
  p +
    labs(col = "", shape = "")

}
