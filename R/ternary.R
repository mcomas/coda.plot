#' Ternary diagram
#'
#' @param X Compositional data
#'
#' @return a ggtern object with the ternary diagram
#' @export
ternary = function(X, groups = NULL, center = FALSE, scale = FALSE){
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
  if(is.null(groups)){
    p = ggtern(data = dplot) +
      geom_mask() +
      suppressWarnings(geom_point(aes(x=c2, y=c1, z=c3))) +
      theme_bw()
  }else{
    dplot$group = groups
    p = ggtern(data = dplot) +
      geom_mask() +
      suppressWarnings(geom_point(aes(x=c2, y=c1, z=c3, col=group))) +
      theme_bw() +
      labs(color = '')
  }
  p +
    labs(x = xyz_labs[2], y = xyz_labs[1], z = xyz_labs[3])
}
