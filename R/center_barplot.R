center_data = function(X, group){
  if(!is.matrix(X)){
    X = as.matrix(X)
  }
  CLR = coda.base::coordinates(X, 'clr')
  colnames(CLR) = colnames(X)
  # LX.mean = apply(LX, 2, function(lx) tapply(lx, group, mean) - mean(lx))
  l_dplot = lapply(colnames(CLR), function(part){
    data.frame(
      'group' = group,
      'part' = part,
      'lx.diff' = CLR[,part] -  mean(CLR[,part]),
      stringsAsFactors = FALSE
    )
  })
  dplot = do.call(rbind, l_dplot)
  dplot$part = factor(dplot$part, levels = colnames(X))
  return(dplot)

}
center_barplot = function(X, group, group_label = "",
                                  title = 'Center bar plot',
                                  xlab = '',
                                  ylab = 'ln(center) - ln(overall center)',
                                  facets = TRUE, x_show_parts = TRUE){
  dplot = center_data(X, group)
  if(x_show_parts){
    if(facets){
      p = ggplot() +
        geom_hline(yintercept = 0) +
        geom_bar(data = dplot, aes(x = part, y = lx.diff, fill = group), width = 0.6,
                 col = 'black', position = position_dodge(width = 0.6), stat = 'summary', fun = 'mean', alpha = 0.8) +
        facet_wrap(~part, nrow = 1, drop = TRUE, scales = 'free_x') +
        theme_minimal() +
        theme(panel.grid.minor.x = element_blank(),
              panel.grid.major.x = element_blank(),
              axis.text.x = element_blank()) +
        labs(title = title, x = xlab, y = ylab, fill = group_label)
    }else{
      p = ggplot() +
        geom_hline(yintercept = 0) +
        geom_bar(data = dplot, aes(x = part, y = lx.diff, fill = group), width = 0.6,
                 col = 'black', position = position_dodge(width = 0.6), stat = 'summary', fun = 'mean', alpha = 0.8) +
        theme_minimal() +
        theme(panel.grid.minor.x = element_blank(),
              panel.grid.major.x = element_blank(),
              axis.text.x = element_text(colour = 'black')) +
        scale_x_discrete(position = 'top') +
        labs(title = title, x = xlab, y = ylab, fill = group_label)
    }
  }else{
    if(facets){
      p = ggplot() +
        geom_hline(yintercept = 0) +
        geom_bar(data = dplot, aes(x = group, y = lx.diff, fill = part), width = 0.6,
                 col = 'black', position = position_dodge(width = 0.6), stat = 'summary', fun = 'mean', alpha = 0.8) +
        facet_wrap(~group, nrow = 1, drop = TRUE, scales = 'free_x') +
        theme_minimal() +
        theme(panel.grid.minor.x = element_blank(),
              panel.grid.major.x = element_blank(),
              axis.text.x = element_blank()) +
        labs(title = title, x = xlab, y = ylab, fill = group_label)
    }else{
      p = ggplot() +
        geom_hline(yintercept = 0) +
        geom_bar(data = dplot, aes(x = group, y = lx.diff, fill = part), width = 0.6,
                 col = 'black', position = position_dodge(width = 0.6), stat = 'summary', fun = 'mean', alpha = 0.8) +
        theme_minimal() +
        theme(panel.grid.minor.x = element_blank(),
              panel.grid.major.x = element_blank(),
              axis.text.x = element_text(colour = 'black')) +
        scale_x_discrete(position = 'top') +
        labs(title = title, x = xlab, y = ylab, fill = group_label)
    }
  }
  p
}
