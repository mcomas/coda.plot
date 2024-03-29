geometric_mean_data = function(X, group){
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
#' Geometric Mean Barplot
#'
#'
#' @param X Composition Matrix
#'
#' @param group Groups to compare
#' @param group_label Label for groups
#' @param title Plot title
#' @param xlab X label
#' @param ylab Y label
#' @param facets Should the barplot be separated by facets.
#' @param x_show_parts Should the barplots show the relation within parts.
#'
#' @export
geometric_mean_barplot = function(X, group, group_label = "",
                                  title = 'Geometric mean bar plot',
                                  xlab = '',
                                  ylab = 'ln(group mean)-ln(overall mean)',
                                  facets = TRUE, x_show_parts = TRUE){
  dplot = geometric_mean_data(X, group)
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
