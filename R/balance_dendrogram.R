#' Compositional Balance Dendrogram
#'
#' Plots a balance dendrogram based on a compositional data set and a corresponding balance matrix.
#' This visualization helps interpret the structure of balances in compositional data analysis.
#'
#' @param X A numeric matrix or data frame representing the compositional data.
#'          Rows are observations and columns are components (must be strictly positive).
#' @param B A numeric matrix representing the balance basis (e.g., an isometric log-ratio (ilr) balance matrix).
#' @param group Show boxplot by groups
#'
#' @return A '\code{ggplot2}' object representing the balance dendrogram.
#'
#' @details
#' The dendrogram shows the hierarchical structure of balances derived from the balance matrix \code{B}.
#' Each internal node corresponds to a balance, and the height or color may reflect the variance explained or other statistics.
#'
#' @examples
#' # Simulated compositional data and balances
#' X = matrix(runif(50, 1, 10), ncol = 5)
#' colnames(X) = LETTERS[1:5]
#' B = pb_basis(X, method = 'exact')
#' balance_dendrogram(X, B)
#'
#' @export
balance_dendrogram = function(X, B, group = NULL){

  if(!is.matrix(X)){
    X = as.matrix(X)
  }

  GROUPED = TRUE
  if(is.null(group)) GROUPED = FALSE

  rownames(B) = colnames(X)
  h = apply(coordinates(X, B), 2, identity, simplify = FALSE)

  BAL_LABELS = paste0("bal", 1:ncol(B))
  if(!is.null(colnames(B))) BAL_LABELS = colnames(B)

  hc = hclust_dendrogram(B)
  nodes = list()
  XLIMS = c(-1, 1) * ceiling(max(abs(unlist(h))))

  if(GROUPED){
    group_labels = unique(group)
    h_group = lapply(group_labels, function(g)
      lapply(h, function(h_) h_[group == g]))
    names(h_group) = group_labels

    nodes_group = list()
  }

  for(i in 1:nrow(hc$merge)){
    bal = hc$merge[i,]
    if(all(bal < 0)){ # a-ll leafs
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
    ## l, r, i_bal, x, y
    h_scaled = x[1] + (x[2] - x[1]) * (h[[i]] - XLIMS[1]) / (XLIMS[2] - XLIMS[1])
    nodes[[i]] = list(l = l,
                      r = r,
                      x = x, y = y,
                      i_bal = i_bal,
                      x_min = min(h_scaled),
                      x_max = max(h_scaled),
                      x_mean = mean(h_scaled),
                      x_median = median(h_scaled),
                      x_q1 = quantile(h_scaled, 0.25),
                      x_q3 = quantile(h_scaled, 0.75),
                      x_05 = quantile(h_scaled, 0.05),
                      x_95 = quantile(h_scaled, 0.95)
    )

    if(GROUPED){
      nodes_group[[i]] = lapply(group_labels, function(g){
        h_scaled = x[1] + (x[2] - x[1]) * (h_group[[g]][[i]] - XLIMS[1]) / (XLIMS[2] - XLIMS[1])
        list(l = l,
             r = r,
             x = x, y = y,
             i_bal = i_bal,
             x_min = min(h_scaled),
             x_max = max(h_scaled),
             x_mean = mean(h_scaled),
             x_median = median(h_scaled),
             x_q1 = quantile(h_scaled, 0.25),
             x_q3 = quantile(h_scaled, 0.75),
             x_05 = quantile(h_scaled, 0.05),
             x_95 = quantile(h_scaled, 0.95)
        )
      })
      names(nodes_group[[i]]) = group_labels
    }
  }

  add_height = function(inode, height_max, lab_pos = 1){
    nodes[[inode]]$lab_pos <<- lab_pos
    nodes[[inode]]$y <<- nodes[[inode]]$y + (height_max - nodes[[inode]]$y[2])
    if(GROUPED){
      for(g in group_labels){
        nodes_group[[inode]][[g]]$y <<- nodes_group[[inode]][[g]]$y + (height_max - nodes_group[[inode]][[g]]$y[2])
      }
    }
    ihcnode = hc$merge[inode,]
    to_add = 0
    if(ihcnode[1] > 0){
      add_height(ihcnode[1], nodes[[inode]]$y[1], lab_pos = -1)
    }
    if(ihcnode[2] > 0){
      add_height(ihcnode[2], nodes[[inode]]$y[1], lab_pos = 1)
    }
  }
  iroot = nrow(hc$merge)
  TOTAL_VAR = sum(sapply(h, var))
  add_height(iroot, TOTAL_VAR)


  l_dhoriz = lapply(nodes, function(nod){
    data.frame(x=nod$x[1],xend=nod$x[2],y=nod$y[1],yend=nod$y[1])
  })
  dhoriz = do.call(rbind, l_dhoriz)
  VMIN = min(dhoriz$y)-0.025*TOTAL_VAR

  l_dvert = lapply(nodes, function(nod){
    data.frame(x=nod$x_mean,xend=nod$x_mean,y=nod$y[1],yend=nod$y[2])
  })
  dvert = do.call(rbind, l_dvert)

  l_label = lapply(nodes, function(nod){
    data.frame(x=nod$x_mean, y=nod$y[1], label = BAL_LABELS[nod$i_bal],
               nudge_x = nod$lab_pos)
  })
  dlabel = do.call(rbind, l_label)

  l_ticks = lapply(nodes, function(nod){
    i_ticks = (XLIMS[1]+1):(XLIMS[2]-1)
    w_ticks = 0.008
    data.frame(
      x = nod$x[1] + (nod$x[2] - nod$x[1]) * (i_ticks-XLIMS[1])/(XLIMS[2]-XLIMS[1]),
      y = rep(nod$y[1] - w_ticks*TOTAL_VAR, length(i_ticks)),
      yend = nod$y[1] + (i_ticks==0)*rep(w_ticks*TOTAL_VAR,length(i_ticks)))
  })
  dticks = do.call(rbind, l_ticks)

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
    geom_segment(data = dticks, aes(x = x, xend = x, y = y, yend = yend))+
    geom_segment(data = dvert, aes(x = x, xend = xend, y = y, yend = yend))+
    geom_segment(data = dbase, aes(x = x, xend = xend, y = y, yend = yend),
                 linetype = 'dotted', col = 'grey') +
    geom_text(data = dlabel, aes(x = x + 0.15 * nudge_x, y = y, label = label),
              nudge_y = 0.025 * TOTAL_VAR, col = 'blue')



  W = 0.008
  DOWN = 2.5*W*TOTAL_VAR
  PROP_BOX = 2/3
  if(GROUPED){
    l_box_grouped = lapply(nodes_group, function(nod_grouped){
      lg = lapply(1:length(group_labels), function(i){
        g = group_labels[i]
        nod = nod_grouped[[g]]
        data.frame(xmin = nod$x[1], xlower = nod$x_q1, xlower5 = nod$x_05,
                   xmiddle = nod$x_median, xupper = nod$x_q3, xmax = nod$x[2],
                   xupper5 = nod$x_95, y = nod$y[1] - i*DOWN,
                   ymax_major = nod$y[1] - i*DOWN + W*TOTAL_VAR,
                   ymin_major = nod$y[1] - i*DOWN - W*TOTAL_VAR,
                   ymax_minor = nod$y[1] - i*DOWN + PROP_BOX*W*TOTAL_VAR,
                   ymin_minor = nod$y[1] - i*DOWN - PROP_BOX*W*TOTAL_VAR,
                   group = g)
      })
      do.call(rbind, lg)
    })
    dbox = do.call(rbind, l_box_grouped)

    p = p +
      geom_rect(data=dbox, aes(xmin = xlower5, xmax = xmiddle,
                               ymin = ymin_minor,
                               ymax = ymax_minor,
                               fill = group), col= 'black') +
      geom_rect(data=dbox, aes(xmin = xmiddle, xmax = xupper5,
                               ymin = ymin_minor,
                               ymax = ymax_minor,
                               fill = group), col= 'black') +
      geom_rect(data=dbox, aes(xmin = xlower, xmax = xmiddle,
                               ymin = ymin_major,
                               ymax = ymax_major,
                               fill = group), col= 'black') +
      geom_rect(data=dbox, aes(xmin = xmiddle, xmax = xupper,
                               ymin = ymin_major,
                               ymax = ymax_major,
                               fill = group), col= 'black')
  }else{
    l_box = lapply(nodes, function(nod){
      data.frame(xmin = nod$x[1], xlower = nod$x_q1, xlower5 = nod$x_05,
                 xmiddle = nod$x_median, xupper = nod$x_q3, xmax = nod$x[2],
                 xupper5 = nod$x_95, y = nod$y[1] - DOWN,
                 ymax_major = nod$y[1] - DOWN + W*TOTAL_VAR,
                 ymin_major = nod$y[1] - DOWN - W*TOTAL_VAR,
                 ymax_minor = nod$y[1] - DOWN + PROP_BOX*W*TOTAL_VAR,
                 ymin_minor = nod$y[1] - DOWN - PROP_BOX*W*TOTAL_VAR)
    })
    dbox = do.call(rbind, l_box)

    p = p +
      geom_rect(data=dbox, aes(xmin = xlower5, xmax = xmiddle,
                               ymin = ymin_minor,
                               ymax = ymax_minor),
                fill = 'white', col= 'black') +
      geom_rect(data=dbox, aes(xmin = xmiddle, xmax = xupper5,
                               ymin = ymin_minor,
                               ymax = ymax_minor),
                fill = 'white', col= 'black') +
      geom_rect(data=dbox, aes(xmin = xlower, xmax = xmiddle,
                               ymin = ymin_major,
                               ymax = ymax_major),
                fill = 'white', col= 'black') +
      geom_rect(data=dbox, aes(xmin = xmiddle, xmax = xupper,
                               ymin = ymin_major,
                               ymax = ymax_major),
                fill = 'white', col= 'black')
  }

  # p1
  #   geom_boxplot(data=dbox, stat = 'identity', orientation = 'y',
  #                aes(xmin = xmin, xlower = xlower,
  #                    xmiddle = xmiddle, xupper = xupper, xmax = xmax,
  #                    y = y, weight = 2*y, group = factor(balance)), varwidth = T)

  p +
    scale_x_continuous(breaks = 1:ncol(X), labels = hc$labels[hc$order]) +
    scale_y_continuous(limits = c(NA, TOTAL_VAR),
                       breaks = seq(0,1,0.1) * TOTAL_VAR,
                       minor_breaks = seq(0,1,0.05) * TOTAL_VAR,
                       labels = sprintf("%d%%", seq(0,100,10))) +
    theme_minimal() +
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.text.x = element_text(colour = )) +
    labs(x = NULL, y = 'Variance explained (%)', fill = '')
}


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
