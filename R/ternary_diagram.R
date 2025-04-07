#' Ternary Diagram
#'
#' Generates a ternary diagram from compositional data, with options to center, scale,
#' and color the points by group. Optionally overlays principal components.
#'
#' @param X A numeric matrix or data frame of compositional data with exactly three columns.
#' @param group A factor or character vector indicating groups for color coding (optional).
#' @param center Logical. Should the data be centered before plotting? Default is FALSE.
#' @param scale Logical. Should the data be scaled to unit variance? Default is FALSE.
#' @param show_pc Logical. If TRUE, principal components are overlaid. Default is FALSE.
#'
#' @return A \code{ggtern} plot object (inherits from \code{ggplot}).
#'
#' @examples
#' X = milk_cows[,5:7]
#' group = milk_cows$group
#' ternary_diagram(X, group = group)
#'
#' @export
ternary_diagram = function(X, group = NULL,
                           center = FALSE, scale = FALSE,
                           show_pc = FALSE){

  composition = function(x, ...) suppressWarnings(coda.base::composition(x, ...))
  geom_point = function(x, ...) suppressWarnings(ggplot2::geom_point(x, ...))
  geom_path = function(x, ...) suppressWarnings(ggplot2::geom_path(x, ...))

  if(!is.matrix(X)){
    X = as.matrix(X)
  }

  GROUPED = !is.null(group)

  xyz_labs = colnames(X)
  if(ncol(X) != 3){
    stop("three columns needed")
  }
  if(center | scale){
    X = composition(scale(coordinates(X), center = center, scale = scale))
  }
  dplot = as.data.frame(matrix(X, ncol = ncol(X)))
  names(dplot) = c('c1', 'c2','c3')

  if(GROUPED) dplot$group = group

  p = ggtern(data = dplot) +
    geom_mask()

  if(GROUPED){
    p = p + geom_point(aes(x=c2, y=c1, z=c3, col=group)) +
      labs(color = '')
  }else{
    p = p + geom_point(aes(x=c2, y=c1, z=c3))
  }

  if(show_pc){
    H = coordinates(X)
    if(GROUPED){

      for(group_k in unique(group)){

        H_k = H[group == group_k,]
        eig = eigen(cov(H_k))

        n_lims = 3/min(abs(ilr_basis(3) %*% (eig$values[2] * eig$vectors[2,])))

        l_X_pc = lapply(1:ncol(H), function(i){
          h_pc = t(colMeans(H_k) + sapply(seq(-n_lims, n_lims, length=500),  `*`, eig$values[i] * eig$vectors[i,]))
          composition(h_pc)
        })
        dplot1 = as.data.frame(l_X_pc[[1]])
        dplot1$group = group_k
        dplot2 = as.data.frame(l_X_pc[[2]])
        dplot2$group = group_k
        p = p +
          geom_path(data = dplot1, aes(x = c1, y = c2, z = c3, col = group, linetype = 'Prin.Comp.1')) +
          geom_path(data = dplot2, aes(x = c1, y = c2, z = c3, col = group, linetype = 'Prin.Comp.2'))

      }
      p = p  +
        scale_linetype_manual(values = c('dashed', 'dotted')) +
        labs(col = '', linetype = '') +
        guides(color  = guide_legend(order = 1),
               linetype = guide_legend(order = 2))
    }else{
      eig = eigen(cov(H))
      n_lims = 3/min(abs(ilr_basis(3) %*% (eig$values[2] * eig$vectors[2,])))
      l_X_pc = lapply(1:ncol(H), function(i){
        h_pc = t(colMeans(H) + sapply(seq(-n_lims, n_lims, length=500),  `*`, eig$values[i] * eig$vectors[i,]))
        composition(h_pc)
      })

      p = p +
        geom_path(data = l_X_pc[[1]], aes(x = c1, y = c2, z = c3, linetype = 'Prin.Comp.1'),
                  col='blue') +
        geom_path(data = l_X_pc[[2]], aes(x = c1, y = c2, z = c3, linetype = 'Prin.Comp.2'),
                  col = 'blue') +
        scale_linetype_manual(values = c('dashed', 'dotted')) +
        labs(linetype = '')

    }
  }
  p +
    theme_bw() +
    labs(x = xyz_labs[2], y = xyz_labs[1], z = xyz_labs[3])

}
