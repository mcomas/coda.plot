
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
clr_biplot = function(X, group = NULL, alpha = 1){
  CLR = coordinates(X, 'clr')
  SVD = svd(scale(CLR, scale = FALSE))

  DATA = with(SVD, u %*% diag(d^alpha))
  VARS = with(SVD, diag(d^(1-alpha)) %*% v)

  D2 = as.data.table(DATA)
  V2 = as.data.table(VARS)

  # D2_min = min(D2)
  # D2_max = max(D2)
  # V2_min = min(V2)
  # V2_max = max(V2)

  V2[,rn := sprintf('clr(%s)', names(X))]

  ipc = 1:2
  ipc_rn = c(ipc, ncol(V2))
  V2.long = melt(V2[,..ipc_rn], id.vars = 'rn')
  V2.length = V2.long[,.(length = sqrt(sum(value^2))),.(rn)]
  V2.length_max = max(V2.length[,-1])

  D2.long = melt(D2[,..ipc][,id:=1:nrow(D2)], id.vars = 'id')
  D2.length = D2.long[,.(length = sqrt(sum(value^2))),.(id)]
  D2.length_max = max(D2.length[,-1])

  max.radius = D2.length_max

  V2.arrow = dcast(V2.long[,.(variable=variable, value=max.radius*value/V2.length_max),.(rn)],
                   rn~variable, value.var = 'value')
  V2.radius = dcast(V2.long[,.(variable=variable, value=max.radius*value/sqrt(sum(value^2))),.(rn)],
                    rn~variable, value.var = 'value')

  # min_ = min(D2,V2)
  # max_ = max(D2,V2)
  if(!is.null(group)){
    G = data.table(group = group)
  }

  pl_ = ggplot()

  if(is.null(group)){
    pl_ = pl_ + geom_point(data=D2, aes(x = V1, y = V2))
  }else{
    pl_ = pl_ + geom_point(data=cbind(G,D2), aes(x = V1, y = V2, col = group))
  }
  pl_  +
    geom_segment(data=V2.radius, aes(x = 0, y = 0, xend = V1, yend = V2), color = 'blue', linetype = 'dotted', size = 0.5) + #arrow = arrow(length = unit(0.03, "npc")),
    geom_segment(data=V2.arrow, aes(x = 0, y = 0, xend = V1, yend = V2), color = 'blue', size = 1) +
    geom_text(data=V2.radius, aes(x = V1, y = V2, label = rn), fontface = 'bold') +
    lims(x = max.radius*c(-1,1), y = max.radius*c(-1,1))
}

clr_biplot_cov = function(X, group = NULL){
  clr_biplot(X, group, alpha = 0)
}

clr_biplot_form = function(X, group = NULL){
  clr_biplot(X, group, alpha = 1)
}
