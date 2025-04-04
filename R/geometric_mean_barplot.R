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
#' Geometric Mean Barplot
#'
#' Creates a barplot of the geometric mean of compositional parts.
#'
#' @param X A matrix or data frame representing compositional data.
#' @param group An optional factor indicating groups for comparison.
#' @param group_label A character string used as the label for the grouping variable in the legend.
#' @param title A character string specifying the plot title.
#' @param xlab A character string specifying the label for the x-axis.
#' @param ylab A character string specifying the label for the y-axis.
#' @param x_show_parts Logical. If \code{TRUE}, the x-axis shows parts to visualize the relative contributions within the composition.
#' @param clr_scale Should the clr coordinates be compared?
#'
#' @return A \code{ggplot} object representing the barplot.
#' @export

geometric_mean_barplot = function(X, group, group_label = "",
                                  title = 'Geometric mean bar plot',
                                  xlab = '',
                                  ylab = 'ln(group gmean)-ln(overall gmean)',
                                  x_show_parts = TRUE,
                                  clr_scale = FALSE){
  dplot = geometric_mean_data(X, group, clr_scale)
  if(clr_scale) ylab = 'clr(group gmean)-clr(overall gmean)'
  if(x_show_parts){
    p = ggplot() +
      geom_hline(yintercept = 0) +
      geom_bar(data = dplot, aes(x = part, y = lx.diff, fill = group),
               col = 'black', position = position_dodge(), stat = 'summary', fun = 'mean', alpha = 0.8) +
      facet_wrap(~part, nrow = 1, drop = TRUE, scales = 'free_x', strip.position='bottom') +
      theme_minimal() +
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            axis.text.x = element_blank()) +
      labs(title = title, x = xlab, y = ylab, fill = group_label)
  }else{
    p = ggplot() +
      geom_hline(yintercept = 0) +
      geom_bar(data = dplot, aes(x = group, y = lx.diff, fill = part),
               col = 'black', position = position_dodge(), stat = 'summary', fun = 'mean', alpha = 0.8) +
      facet_wrap(~group, nrow = 1, scales = 'free_x') +
      theme_minimal() +
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            axis.text.x = element_blank()) +
      labs(title = title, x = xlab, y = ylab, fill = group_label)
  }
  p
}
