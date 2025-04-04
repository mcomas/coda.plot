#' Ternary diagram
#'
#' @param X Compositional data
#' @param group Colorize according to this vector
#' @param center Should the data be centered?
#' @param scale Should the data be scaled to have unit variance?
#' @return a ggtern object with the ternary diagram
#' @export
ternary = function(X, group = NULL, center = FALSE, scale = FALSE){
  if(!is.matrix(X)){
    X = as.matrix(X)
  }
  xyz_labs = colnames(X)
  if(ncol(X) != 3){
    stop("three columns needed")
  }
  if(center | scale){
    X = composition(scale(coordinates(X), center = center, scale = scale))
  }
  dplot = as.data.frame(matrix(X, ncol = ncol(X)))
  names(dplot) = c('c1', 'c2','c3')
  if(is.null(group)){
    p = ggtern(data = dplot) +
      geom_mask() +
      suppressWarnings(geom_point(aes(x=c2, y=c1, z=c3))) +
      theme_bw()
  }else{
    dplot$group = group
    p = ggtern(data = dplot) +
      #geom_mask() +
      suppressWarnings(geom_point(aes(x=c2, y=c1, z=c3, col=group))) +
      theme_bw() +
      labs(color = '')
  }
  p +
    labs(x = xyz_labs[2], y = xyz_labs[1], z = xyz_labs[3])
}
