geometric_mean_dplot = function(X, group){
  if(!is.matrix(X)){
    X = as.matrix(X)
  }
  X = X/rowSums(X)
  LX = log(X)
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
#' @export
geometric_mean_barplot = function(X, group, group_label = "", type = 'bar',
                                   title = 'Geometric mean bar plot', xlab = '', ylab = 'ln(group mean)-ln(overall mean)'){
  dplot = geometric_mean_dplot(X, group)
  p = ggplot() +
    geom_hline(yintercept = 0) +
    geom_bar(data = dplot, aes(x = part, y = lx.diff, fill = group), width = 0.6,
             col = 'black', position = position_dodge(width = 0.6), stat = 'summary', fun = 'mean', alpha = 0.8)
  # p
  # if(type == 'box'){
  #   p = p +
  #     geom_boxplot(data=dplot, aes(x = variable, y = value-gmean, fill = group),
  #                  width = 0.5)
  # }
  # if(type == 'bar'){
  #   p = p +
  #     geom_bar(aes(x = variable, y = mean, fill = group), width = 0.75,
  #              col = NA, position = position_dodge(), stat = 'identity')
  # }
  # if(type == 'line'){
  #   W = 0
  #   p = p +
  #     geom_point(aes(x = variable, y = mean, col = group, group = group),
  #                position = position_dodge(width=W)) +
  #     geom_line(aes(x = variable, y = mean, col = group, group = group),
  #               linetype = 'dotted')
  # }
  p +
    facet_wrap(~part, nrow = 1, drop = TRUE, scales = 'free_x') +
    theme_minimal() +
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.text.x = element_blank()) +
    labs(title = title, x = xlab, y = ylab, fill = group_label)
}
