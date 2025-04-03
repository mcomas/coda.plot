hclust_dendrogram = function(B){
  MERGE = matrix(0, nrow = ncol(B), ncol = 2)
  ORD = order(colSums(B != 0))
  for(i in 1:ncol(B)){
    if(sum(B[,ORD[i]] < 0) == 1){
      MERGE[i,1] = -which(B[,ORD[i]] < 0)
    }else{
      MERGE[i,1] = which(sapply(1:(i-1), function(j) all(B[which(B[,ORD[i]] < 0), ORD[j]] * B[which(B[,ORD[i]] < 0), ORD[i]] != 0)))
    }
    if(sum(B[,ORD[i]] > 0) == 1){
      MERGE[i,2] = -which(B[,ORD[i]] > 0)
    }else{
      MERGE[i,2] = which(sapply(1:(i-1), function(j) all(B[which(B[,ORD[i]] > 0), ORD[j]] * B[which(B[,ORD[i]] > 0), ORD[i]] != 0)))
    }
  }
  left = function(pair){
    B_ = sign(B)[pair,]
    which.min(rowSums(B_[,apply(B_, 2, function(x) all(x != 0)), drop=FALSE]))
  }
  ORDER = 1:nrow(B)
  for(i in 1:nrow(B)){
    if(i != nrow(B)){
      for(j in (i+1):nrow(B)){
        x = c(ORDER[i],ORDER[j])
        ileft_ = left(x)
        ORDER[i] = x[ileft_]
        ORDER[j] = x[3-ileft_]
      }
    }
  }
  HEIGHT = rep(0, ncol(B))
  for(i in 1:nrow(MERGE)){
    l_ = 1
    r_ = 1
    if(MERGE[i, 1] > 0){
      l_ = HEIGHT[MERGE[i, 1]] + 1
    }
    if(MERGE[i, 2] > 0){
      r_ = HEIGHT[MERGE[i, 2]] + 1
    }
    HEIGHT[i] = max(l_, r_)
  }
  structure(
    list(merge = MERGE, height = HEIGHT, order = ORDER, labels = rownames(B)), class = 'hclust'
  )
}

############
# # X = parliament2017[,2:6]
# X = alimentation[,1:9]
# B = ilr_basis(ncol(X))
# B = pb_basis(X, method = 'exact')
# B = cdp_basis(ncol(X))

#' Compositional balance dendrogram
#'
#' @param X Compositional data set
#' @param B Balance matrix
#'
#' @return ggplot2 object with the balance dendrogram
#' @export
balance_dendrogram = function(X, B){
  if(!is.matrix(X)){
    X = as.matrix(X)
  }
  rownames(B) = colnames(X)
  h = apply(coordinates(X, B), 2, identity, simplify = FALSE)
  hc = hclust_dendrogram(B)
  nodes = list()
  for(i in 1:nrow(hc$merge)){
    bal = hc$merge[i,]
    if(all(bal < 0)){
      l = hc$labels[-bal[1]]
      r = hc$labels[-bal[2]]
      i_bal = which(apply(B[l,,drop=FALSE] < 0, 2, all) & apply(B[r,,drop=FALSE] > 0, 2, all))
      x = match(-bal, hc$order)
      y = c(0, var(h[[i_bal]]))
    }else{
      if(all(bal > 0)){
        l = c(nodes[[bal[1]]]$l, nodes[[bal[1]]]$r)
        r = c(nodes[[bal[2]]]$l, nodes[[bal[2]]]$r)
        i_bal = which(apply(B[l,,drop=FALSE] < 0, 2, all) & apply(B[r,,drop=FALSE] > 0, 2, all))
        x = c(nodes[[bal[1]]]$x_mean,
              nodes[[bal[2]]]$x_mean)
        y = c(0, var(h[[i_bal]])) + max(nodes[[bal[1]]]$y[2], nodes[[bal[2]]]$y[2])
      }else{
        if(bal[1] < 0){
          l = hc$labels[-bal[1]]
          r = c(nodes[[bal[2]]]$l, nodes[[bal[2]]]$r)
          i_bal = which(apply(B[l,,drop=FALSE] < 0, 2, all) & apply(B[r,,drop=FALSE] > 0, 2, all))
          x = c(match(-bal[1], hc$order),
                nodes[[bal[2]]]$x_mean)
          y = c(0, var(h[[i_bal]])) + nodes[[bal[2]]]$y[2]
        }else{
          l = c(nodes[[bal[1]]]$l, nodes[[bal[1]]]$r)
          r = hc$labels[-bal[2]]
          i_bal = which(apply(B[l,,drop=FALSE] < 0, 2, all) & apply(B[r,,drop=FALSE] > 0, 2, all))
          x = c(nodes[[bal[1]]]$x_mean,
                match(-bal[2], hc$order))
          y = c(0, var(h[[i_bal]])) + nodes[[bal[1]]]$y[2]
        }
      }
    }

    x_min = -4 #min(h[[i]])
    x_max = +4 #max(h[[i]])
    # x_min = min(unlist(h))
    # x_max = max(unlist(h))
    nodes[[i]] = list(l = l,
                      r = r,
                      x = x, y = y,
                      i_bal = i_bal,
                      x_min = x_min,
                      x_max = x_max,
                      x_mean = x[1] + (mean(h[[i]])-x_min)/(x_max-x_min) * diff(x),
                      x_median = x[1] + (median(h[[i]])-x_min)/(x_max-x_min) * diff(x),
                      x_q1 = x[1] + (quantile(h[[i]],0.25)-x_min)/(x_max-x_min) * diff(x),
                      x_q3 = x[1] + (quantile(h[[i]],0.75)-x_min)/(x_max-x_min) * diff(x),
                      x_05 = x[1] + (quantile(h[[i]],0.05)-x_min)/(x_max-x_min) * diff(x),
                      x_95 = x[1] + (quantile(h[[i]],0.95)-x_min)/(x_max-x_min) * diff(x)
    )
  }

  add_height = function(inode, height_max){
    nodes[[inode]]$y <<- nodes[[inode]]$y + (height_max - nodes[[inode]]$y[2])
    ihcnode = hc$merge[inode,]
    to_add = 0
    if(ihcnode[1] > 0){
      add_height(ihcnode[1], nodes[[inode]]$y[1])
    }
    if(ihcnode[2] > 0){
      add_height(ihcnode[2], nodes[[inode]]$y[1])
    }
  }
  iroot = nrow(hc$merge)
  total_var = sum(sapply(h, var))
  add_height(iroot, total_var)


  l_dhoriz = lapply(nodes, function(nod){
    data.frame(x=nod$x[1],xend=nod$x[2],y=nod$y[1],yend=nod$y[1])
  })
  dhoriz = do.call(rbind, l_dhoriz)
  VMIN = min(dhoriz$y)-0.025*total_var
  l_dvert = lapply(nodes, function(nod){
    data.frame(x=nod$x_mean,xend=nod$x_mean,y=nod$y[1],yend=nod$y[2])
  })
  dvert = do.call(rbind, l_dvert)
  l_dbase = lapply(seq_along(nodes), function(inode){
    d = data.frame(x=numeric(0),xend=numeric(0),y=numeric(0),yend=numeric(0))
    if(hc$merge[inode,1]<0){
      d = rbind(d,
                data.frame(x = nodes[[inode]]$x[1],
                           xend = nodes[[inode]]$x[1],
                           y = nodes[[inode]]$y[1],
                           yend = VMIN))
    }
    if(hc$merge[inode,2]<0){
      d = rbind(d,
                data.frame(x = nodes[[inode]]$x[2],
                           xend = nodes[[inode]]$x[2],
                           y = nodes[[inode]]$y[1],
                           yend = VMIN))
    }
    return(d)
  })
  dbase = do.call(rbind, l_dbase)
  p = ggplot() +
    geom_segment(data = dhoriz, aes(x = x, xend = xend, y = y, yend = yend))+
    geom_segment(data = dvert, aes(x = x, xend = xend, y = y, yend = yend))+
    geom_segment(data = dbase, aes(x = x, xend = xend, y = y, yend = yend),
                 linetype = 'dotted', col = 'grey')

  l_box = lapply(nodes, function(nod){
    data.frame(xmin = nod$x[1], xlower = nod$x_q1, xlower5 = nod$x_05,
               xmiddle = nod$x_median, xupper = nod$x_q3, xmax = nod$x[2],
               xupper5 = nod$x_95,
               y = nod$y[1])
  })
  dbox = do.call(rbind, l_box)
  dbox$balance = 1:nrow(dbox)
  dbox$W = 0.008
  dbox$total_var = total_var
  p = p +
    geom_rect(data=dbox, aes(xmin = xlower5, xmax = xmiddle,
                             ymin = y+0.75*W*total_var, ymax = y-0.75*W*total_var),
              fill = 'white', col= 'black') +
    geom_rect(data=dbox, aes(xmin = xmiddle, xmax = xupper5,
                             ymin = y+0.75*W*total_var, ymax = y-0.75*W*total_var),
              fill = 'white', col= 'black') +
    geom_rect(data=dbox, aes(xmin = xlower, xmax = xmiddle,
                             ymin = y+W*total_var, ymax = y-W*total_var),
              fill = 'white', col= 'black') +
    geom_rect(data=dbox, aes(xmin = xmiddle, xmax = xupper,
                             ymin = y+W*total_var, ymax = y-W*total_var),
              fill = 'white', col= 'black')

  # p1
  #   geom_boxplot(data=dbox, stat = 'identity', orientation = 'y',
  #                aes(xmin = xmin, xlower = xlower,
  #                    xmiddle = xmiddle, xupper = xupper, xmax = xmax,
  #                    y = y, weight = 2*y, group = factor(balance)), varwidth = T)

  p +
    scale_x_continuous(breaks = 1:ncol(X), labels = hc$labels[hc$order]) +
    scale_y_continuous(limits = c(VMIN, total_var),
                       breaks = seq(0,1,0.1) * total_var,
                       minor_breaks = seq(0,1,0.05) * total_var,
                       labels = sprintf("%d%%", seq(0,100,10))) +
    theme_minimal() +
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.text.x = element_text(colour = )) +
    labs(x = NULL, y = 'Variance explained (%)')
}
