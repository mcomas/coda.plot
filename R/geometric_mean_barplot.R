#' Geometric Mean Barplot for Compositional Data
#'
#' Generates a barplot based on the geometric mean of compositional parts. Optionally,
#' it can compare groups, display the parts on the x-axis, overlay boxplots, or use centered log-ratio (clr) transformation.
#'
#' @param X A numeric matrix or data frame representing compositional data.
#'          Each row is an observation and each column is a part (must be strictly positive).
#' @param group An optional factor or character vector indicating group membership for each observation.
#' @param x_show_parts Logical. If \code{TRUE}, the x-axis displays parts instead of group labels. Default is \code{TRUE}.
#' @param include_boxplot Logical. If \code{TRUE}, a boxplot is overlaid on top of the barplot. Default is \code{FALSE}.
#' @param clr_scale Logical. If \code{TRUE}, the data are transformed to clr coordinates before computing geometric means. Default is \code{FALSE}.
#'
#' @return A '\code{ggplot2}' object representing the geometric mean barplot.
#'
#' @details
#' The function computes geometric means for each compositional part, optionally stratified by groups.
#' If \code{clr_scale = TRUE}, the data are transformed using the centered log-ratio transformation before computing means.
#' Overlaying a boxplot can help visualize within-group variability.
#'
#' @examples
#' # Example with simulated compositional data
#' X = matrix(runif(30, 1, 10), ncol = 3)
#' colnames(X) = c("A", "B", "C")
#' group = rep(c("G1", "G2"), each = 5)
#' geometric_mean_barplot(X, group, include_boxplot = TRUE)
#'
#' @export
geometric_mean_barplot = function(X, group,
                                  x_show_parts = TRUE,
                                  include_boxplot = FALSE,
                                  clr_scale = FALSE){

  TITLE = 'Geometric mean bar plot'
  XLAB = ''
  YLAB = 'ln(group gmean)-ln(overall gmean)'
  GROUP_LABEL = ""

  if(clr_scale) YLAB = 'clr(group gmean)-clr(overall gmean)'

  dplot = geometric_mean_data(X, group, clr_scale)
  if(x_show_parts){
    p = ggplot() +
      geom_hline(yintercept = 0) +
      geom_bar(data = dplot, aes(x = part, y = lx.diff, fill = group),
               col = 'black', position = position_dodge(), stat = 'summary', fun = 'mean', alpha = 0.8)
    if(include_boxplot){
      p = p +
        geom_boxplot(data = dplot, aes(x = part, y = lx.diff, fill = group),
                     alpha = 0.3, position = position_dodge(0.9), width = 0.25, show.legend = FALSE)
    }
    p = p +
      facet_wrap(~part, nrow = 1, drop = TRUE, scales = 'free_x', strip.position='bottom') +
      theme_minimal() +
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            axis.text.x = element_blank())
  }else{
    p = ggplot() +
      geom_hline(yintercept = 0) +
      geom_bar(data = dplot, aes(x = group, y = lx.diff, fill = part),
               col = 'black', position = position_dodge(), stat = 'summary', fun = 'mean', alpha = 0.8)
    if(include_boxplot){
      p = p +
        geom_boxplot(data = dplot, aes(x = group, y = lx.diff, fill = part),
                     alpha = 0.3, position = position_dodge(0.9), width = 0.25, show.legend = FALSE)
    }
    p = p +
      facet_wrap(~group, nrow = 1, scales = 'free_x') +
      theme_minimal() +
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            axis.text.x = element_blank())
  }
  p +
    labs(title = TITLE, x = XLAB, y = YLAB, fill = GROUP_LABEL)
}

geometric_mean_data = function(X, group, clr_scale){
  if(!is.matrix(X)){
    X = as.matrix(X)
  }
  X = X/rowSums(X)
  if(clr_scale){
    LX = matrix(coordinates(X, 'clr'), ncol = ncol(X),
                dimnames = list(NULL, colnames(X)))
  }else{
    LX = log(X)
  }

  # LX.mean = apply(LX, 2, function(lx) tapply(lx, group, mean) - mean(lx))
  l_dplot = lapply(colnames(LX), function(part){
    data.frame(
      'group' = group,
      'part' = part,
      'lx.diff' = LX[,part] - mean(LX[,part]),
      stringsAsFactors = FALSE
    )
  })
  dplot = do.call(rbind, l_dplot)
  dplot$part = factor(dplot$part, levels = colnames(X))
  return(dplot)

}
