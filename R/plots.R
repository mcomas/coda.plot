geometric_mean_dplot = function(X, group){
  LOG = log(as.data.table(X))
  LOG$group = group
  dlong = melt(LOG, measure.vars = 1:ncol(X))
  all_mean = dlong[,.(gmean=mean(value, na.rm = TRUE)), .(variable)]
  dplot = merge(dlong, all_mean)
  return(dplot)

}
#' @export
geometric_mean_bar_plot = function(X, group, group_label = "", type = 'bar',
                                   title = 'Geometric mean bar plot', xlab = '', ylab = ''){
  dplot = geometric_mean_dplot(X, group)
  dplot_mean = dplot[,.(mean=mean(value-gmean, na.rm = TRUE),
                        se=sd(value-gmean)/sqrt(length(value))), .(group, variable)]
  p = ggplot(data=dplot_mean) +
    geom_hline(yintercept = 0)
  if(type == 'box'){
    p = p +
      geom_boxplot(data=dplot, aes(x = variable, y = value-gmean, fill = group),
                   width = 0.5)
  }
  if(type == 'bar'){
    p = p +
      geom_bar(aes(x = variable, y = mean, fill = group), width = 0.75,
               col = NA, position = position_dodge(), stat = 'identity')
  }
  if(type == 'line'){
    W = 0
    p = p +
      geom_point(aes(x = variable, y = mean, col = group, group = group),
                 position = position_dodge(width=W)) +
      geom_line(aes(x = variable, y = mean, col = group, group = group),
                linetype = 'dotted')
  }
  p +
    theme_minimal() +
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank()) +
    labs(title = title, x = xlab, y = ylab, fill = group_label)
}


geometric_mean_line_plot = function(X, group, group_label = "", se = FALSE,
                                    title = 'Geometric mean plot',
                                       xlab = '', ylab = ''){
  dplot = geometric_mean_dplot(X, group)

  W = 0.3
  p = ggplot(data=dplot) +
    geom_point(aes(x = variable, y = mean, col = group, group = group),
               position = position_dodge(width=W))
  p
  if(se){
    p = p +
      geom_errorbar(aes(x = variable, ymin = mean - 1.96*se, ymax = mean + 1.96*se, col = group),
                    position = position_dodge(width=W), width=0.2)
  }
  p +
    geom_line(aes(x = variable, y = mean, col = group, group = group),
              linetype = 'dotted') +
    geom_hline(yintercept = 0) +
    theme_minimal() +
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank()) +
    labs(title = title, x = xlab, y = ylab, col = group_label)
}

#' @export
biplot_form = function(X, group = NULL, group_label = "", ipc = 1:2){
  ivar = c(1,1+ipc)
  PC = coordinates(X, 'pc')
  DATA = setnames(as.data.table(scale(PC, scale = FALSE))[,..ipc], c('x','y'))
  if(!is.null(group)){
    G = data.table(group = group)
  }
  max.radius = max(abs(DATA))
  VARS.length_all = (ncol(X) - 1) / ncol(X)
  VARS = setnames(as.data.table(basis(PC), keep.rownames = TRUE)[,..ivar], c('rn', 'x', 'y'))
  VARS.long = melt(VARS, id.vars = 'rn')
  VARS.length_max = max(VARS.long[,.(length = sqrt(sum(value^2))),.(rn)][,-1])

  VARS.arrow = dcast(VARS.long[,.(variable=variable, value=max.radius*VARS.length_max*value/VARS.length_all),.(rn)],
                      rn~variable, value.var = 'value')
  VARS.radius = dcast(VARS.long[,.(variable=variable, value=max.radius*value/sqrt(sum(value^2))),.(rn)],
                      rn~variable, value.var = 'value')

  pl_ = ggplot() +
    # geom_hline(yintercept = 0, linetype = 'dashed', alpha = 0.3) +
    # geom_vline(xintercept = 0, linetype = 'dashed', alpha = 0.3) +
    geom_segment(data=VARS.radius, aes(x = 0, y = 0, xend = x, yend = y), linetype = 'dotted', size = 0.5,
                 col = '#db0bb2') + #arrow = arrow(length = unit(0.03, "npc")),
    geom_segment(data=VARS.arrow, aes(x = 0, y = 0, xend = x, yend = y), size = 1,
                 col = '#db0bb2')
  if(is.null(group)){
    pl_ = pl_ + geom_point(data=DATA, aes(x = x, y = y),  col = 'blue', alpha = 0.5)
  }else{
    pl_ = pl_ + geom_point(data=cbind(G,DATA), aes(x = x, y = y, col = group),  alpha = 0.5)
  }
  pl_  +
    # geom_point(aes(x = 0, y = 0), size = 6, shape = 18, color = 'blue') +
    geom_text(data=VARS.radius, aes(x = x, y = y, label = rn), fontface = 'bold', check_overlap = TRUE) +
    theme_minimal() + coord_equal(xlim = c(-max.radius, max.radius), ylim = c(-max.radius, max.radius)) +
    labs(x = sprintf("PC %d", ipc[1]), y = sprintf("PC %d", ipc[2]), col = group_label)

}

