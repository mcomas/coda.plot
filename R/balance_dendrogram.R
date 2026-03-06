#' Compositional Balance Dendrogram
#'
#' Plots a balance dendrogram based on a compositional data set and a corresponding balance matrix.
#' This visualization helps interpret the structure of balances in compositional data analysis.
#'
#' @param X A numeric matrix or data frame representing the compositional data.
#'          Rows are observations and columns are components (must be strictly positive).
#' @param B A numeric matrix representing the balance basis (e.g., an isometric log-ratio (ilr) balance matrix).
#' @param group Optional. If provided, show grouped box summaries under each node.
#'
#' @return A \code{ggplot2} object representing the balance dendrogram.
#'
#' @examples
#' # Simulated compositional data and balances
#' X = matrix(runif(50, 1, 10), ncol = 5)
#' colnames(X) = LETTERS[1:5]
#' B = coda.base::pb_basis(X, method = 'exact')
#' balance_dendrogram(X, B)
#'
#' @export
balance_dendrogram <- function(X, B, group = NULL){

  if(!is.matrix(X)) X <- as.matrix(X)
  if(any(!is.finite(X))) stop("X contains non-finite values.")
  if(any(X <= 0)) stop("X must be strictly positive (required for log-ratio transforms).")

  if(!is.matrix(B)) B <- as.matrix(B)
  if(nrow(B) != ncol(X)) stop("nrow(B) must match ncol(X).")

  # Prefer not to overwrite existing rownames(B) unless missing
  if(is.null(rownames(B))) {
    if(is.null(colnames(X))) stop("colnames(X) are required when rownames(B) are missing.")
    rownames(B) <- colnames(X)
  }

  GROUPED <- !is.null(group)
  if(GROUPED){
    if(length(group) != nrow(X)) stop("length(group) must equal nrow(X).")
    group <- as.factor(group)
    group_labels <- levels(group)
  }

  # ilr coordinates for each balance column
  H <- coda.base::coordinates(X, B)               # n x (D-1)
  h <- lapply(seq_len(ncol(H)), function(j) H[,j])# list of numeric vectors

  BAL_LABELS <- paste0("bal", 1:ncol(B))
  if(!is.null(colnames(B))) BAL_LABELS <- colnames(B)

  hc <- hclust_dendrogram(B)
  nodes <- list()

  XL <- max(abs(unlist(h)))
  if(!is.finite(XL) || XL <= 0) XL <- 1
  XLIMS <- c(-1, 1) * ceiling(XL)

  if(GROUPED){
    h_group <- lapply(group_labels, function(g){
      idx <- group == g
      lapply(h, function(h_) h_[idx])
    })
    names(h_group) <- group_labels
    nodes_group <- list()
  }

  # Helper to compute i_bal safely (must be a single balance index)
  pick_i_bal <- function(l, r){
    idx <- which(
      apply(B[l,,drop=FALSE] < 0, 2, all) &
        apply(B[r,,drop=FALSE] > 0, 2, all)
    )
    if(length(idx) == 0) return(NA_integer_)
    idx[1]
  }

  for(i in 1:nrow(hc$merge)){
    bal <- hc$merge[i,]

    if(all(bal < 0)){ # all leaves
      l <- hc$labels[-bal[1]]
      r <- hc$labels[-bal[2]]
      i_bal <- pick_i_bal(l, r)

      x <- match(-bal, hc$order)
      y <- c(0, if(is.na(i_bal)) 0 else stats::var(h[[i_bal]]))

    } else if(all(bal > 0)){ # both internal nodes
      l <- c(nodes[[bal[1]]]$l, nodes[[bal[1]]]$r)
      r <- c(nodes[[bal[2]]]$l, nodes[[bal[2]]]$r)
      i_bal <- pick_i_bal(l, r)

      x <- c(nodes[[bal[1]]]$x_mean, nodes[[bal[2]]]$x_mean)
      y <- c(0, if(is.na(i_bal)) 0 else stats::var(h[[i_bal]])) +
        max(nodes[[bal[1]]]$y[2], nodes[[bal[2]]]$y[2])

    } else {
      if(bal[1] < 0){ # left leaf, right internal
        l <- hc$labels[-bal[1]]
        r <- c(nodes[[bal[2]]]$l, nodes[[bal[2]]]$r)
        i_bal <- pick_i_bal(l, r)

        x <- c(match(-bal[1], hc$order), nodes[[bal[2]]]$x_mean)
        y <- c(0, if(is.na(i_bal)) 0 else stats::var(h[[i_bal]])) + nodes[[bal[2]]]$y[2]

      } else {        # left internal, right leaf
        l <- c(nodes[[bal[1]]]$l, nodes[[bal[1]]]$r)
        r <- hc$labels[-bal[2]]
        i_bal <- pick_i_bal(l, r)

        x <- c(nodes[[bal[1]]]$x_mean, match(-bal[2], hc$order))
        y <- c(0, if(is.na(i_bal)) 0 else stats::var(h[[i_bal]])) + nodes[[bal[1]]]$y[2]
      }
    }

    if(is.na(i_bal)) {
      # Fallback if balance cannot be identified (should not happen in valid PB bases)
      i_bal <- 1L
    }

    # scale balance values to [x1,x2] segment
    h_scaled <- x[1] + (x[2] - x[1]) * (h[[i_bal]] - XLIMS[1]) / (XLIMS[2] - XLIMS[1])

    nodes[[i]] <- list(
      l = l, r = r,
      x = x, y = y,
      i_bal = i_bal,
      x_min = min(h_scaled),
      x_max = max(h_scaled),
      x_mean = mean(h_scaled),
      x_median = stats::median(h_scaled),
      x_q1 = stats::quantile(h_scaled, 0.25, names = FALSE),
      x_q3 = stats::quantile(h_scaled, 0.75, names = FALSE),
      x_05 = stats::quantile(h_scaled, 0.05, names = FALSE),
      x_95 = stats::quantile(h_scaled, 0.95, names = FALSE)
    )

    if(GROUPED){
      nodes_group[[i]] <- lapply(group_labels, function(g){
        hg <- h_group[[g]][[i_bal]]
        h_scaled_g <- x[1] + (x[2] - x[1]) * (hg - XLIMS[1]) / (XLIMS[2] - XLIMS[1])
        list(
          l = l, r = r,
          x = x, y = y,
          i_bal = i_bal,
          x_min = min(h_scaled_g),
          x_max = max(h_scaled_g),
          x_mean = mean(h_scaled_g),
          x_median = stats::median(h_scaled_g),
          x_q1 = stats::quantile(h_scaled_g, 0.25, names = FALSE),
          x_q3 = stats::quantile(h_scaled_g, 0.75, names = FALSE),
          x_05 = stats::quantile(h_scaled_g, 0.05, names = FALSE),
          x_95 = stats::quantile(h_scaled_g, 0.95, names = FALSE)
        )
      })
      names(nodes_group[[i]]) <- group_labels
    }
  }

  add_height <- function(inode, height_max, lab_pos = 1){
    nodes[[inode]]$lab_pos <<- lab_pos
    nodes[[inode]]$y <<- nodes[[inode]]$y + (height_max - nodes[[inode]]$y[2])

    if(GROUPED){
      for(g in group_labels){
        nodes_group[[inode]][[g]]$y <<- nodes_group[[inode]][[g]]$y + (height_max - nodes_group[[inode]][[g]]$y[2])
      }
    }

    ihcnode <- hc$merge[inode,]
    if(ihcnode[1] > 0) add_height(ihcnode[1], nodes[[inode]]$y[1], lab_pos = -1)
    if(ihcnode[2] > 0) add_height(ihcnode[2], nodes[[inode]]$y[1], lab_pos =  1)
  }

  iroot <- nrow(hc$merge)
  TOTAL_VAR <- sum(vapply(h, stats::var, numeric(1)))
  if(!is.finite(TOTAL_VAR) || TOTAL_VAR <= 0) TOTAL_VAR <- 1
  add_height(iroot, TOTAL_VAR)

  # segments: horizontal
  dhoriz <- do.call(rbind, lapply(nodes, function(nod){
    data.frame(x=nod$x[1], xend=nod$x[2], y=nod$y[1], yend=nod$y[1])
  }))
  VMIN <- min(dhoriz$y) - 0.025*TOTAL_VAR

  # segments: vertical
  dvert <- do.call(rbind, lapply(nodes, function(nod){
    data.frame(x=nod$x_mean, xend=nod$x_mean, y=nod$y[1], yend=nod$y[2])
  }))

  # labels at node
  dlabel <- do.call(rbind, lapply(nodes, function(nod){
    data.frame(x=nod$x_mean, y=nod$y[1],
               label = BAL_LABELS[nod$i_bal],
               nudge_x = nod$lab_pos)
  }))

  # ticks under each node (guard against empty tick ranges)
  dticks <- do.call(rbind, lapply(nodes, function(nod){
    i_from <- ceiling(XLIMS[1]) + 1
    i_to   <- floor(XLIMS[2]) - 1
    if(i_from > i_to) return(data.frame(x=numeric(0), y=numeric(0), yend=numeric(0)))

    i_ticks <- i_from:i_to
    w_ticks <- 0.008
    data.frame(
      x = nod$x[1] + (nod$x[2] - nod$x[1]) * (i_ticks - XLIMS[1])/(XLIMS[2] - XLIMS[1]),
      y = rep(nod$y[1] - w_ticks*TOTAL_VAR, length(i_ticks)),
      yend = nod$y[1] + (i_ticks == 0) * rep(w_ticks*TOTAL_VAR, length(i_ticks))
    )
  }))

  # dotted base down to VMIN for leaves
  dbase <- do.call(rbind, lapply(seq_along(nodes), function(inode){
    d <- data.frame(x=numeric(0), xend=numeric(0), y=numeric(0), yend=numeric(0))
    if(hc$merge[inode,1] < 0){
      d <- rbind(d, data.frame(x=nodes[[inode]]$x[1], xend=nodes[[inode]]$x[1],
                               y=nodes[[inode]]$y[1], yend=VMIN))
    }
    if(hc$merge[inode,2] < 0){
      d <- rbind(d, data.frame(x=nodes[[inode]]$x[2], xend=nodes[[inode]]$x[2],
                               y=nodes[[inode]]$y[1], yend=VMIN))
    }
    d
  }))

  p <- ggplot2::ggplot() +
    ggplot2::geom_segment(data = dhoriz, ggplot2::aes(x = x, xend = xend, y = y, yend = yend)) +
    ggplot2::geom_segment(data = dticks, ggplot2::aes(x = x, xend = x, y = y, yend = yend)) +
    ggplot2::geom_segment(data = dvert,  ggplot2::aes(x = x, xend = xend, y = y, yend = yend)) +
    ggplot2::geom_segment(data = dbase,  ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
                          linetype = "dotted", colour = "grey") +
    ggplot2::geom_text(data = dlabel,
                       ggplot2::aes(x = x + 0.15 * nudge_x, y = y, label = label),
                       nudge_y = 0.025 * TOTAL_VAR, colour = "blue")

  # “Box-like” summaries under each node
  W <- 0.008
  DOWN <- 2.5*W*TOTAL_VAR
  PROP_BOX <- 2/3

  if(GROUPED){
    dbox <- do.call(rbind, lapply(nodes_group, function(nod_grouped){
      do.call(rbind, lapply(seq_along(group_labels), function(ii){
        g <- group_labels[ii]
        nod <- nod_grouped[[g]]
        data.frame(
          xmin = nod$x[1], xlower = nod$x_q1, xlower5 = nod$x_05,
          xmiddle = nod$x_median, xupper = nod$x_q3, xmax = nod$x[2],
          xupper5 = nod$x_95,
          y = nod$y[1] - ii*DOWN,
          ymax_major = nod$y[1] - ii*DOWN + W*TOTAL_VAR,
          ymin_major = nod$y[1] - ii*DOWN - W*TOTAL_VAR,
          ymax_minor = nod$y[1] - ii*DOWN + PROP_BOX*W*TOTAL_VAR,
          ymin_minor = nod$y[1] - ii*DOWN - PROP_BOX*W*TOTAL_VAR,
          group = g
        )
      }))
    }))

    p <- p +
      ggplot2::geom_rect(data=dbox, ggplot2::aes(xmin = xlower5, xmax = xmiddle,
                                                 ymin = ymin_minor, ymax = ymax_minor,
                                                 fill = group),
                         colour = "black", alpha = 0.45) +
      ggplot2::geom_rect(data=dbox, ggplot2::aes(xmin = xmiddle, xmax = xupper5,
                                                 ymin = ymin_minor, ymax = ymax_minor,
                                                 fill = group),
                         colour = "black", alpha = 0.45) +
      ggplot2::geom_rect(data=dbox, ggplot2::aes(xmin = xlower, xmax = xmiddle,
                                                 ymin = ymin_major, ymax = ymax_major,
                                                 fill = group),
                         colour = "black") +
      ggplot2::geom_rect(data=dbox, ggplot2::aes(xmin = xmiddle, xmax = xupper,
                                                 ymin = ymin_major, ymax = ymax_major,
                                                 fill = group),
                         colour = "black")

  } else {
    dbox <- do.call(rbind, lapply(nodes, function(nod){
      data.frame(
        xmin = nod$x[1], xlower = nod$x_q1, xlower5 = nod$x_05,
        xmiddle = nod$x_median, xupper = nod$x_q3, xmax = nod$x[2],
        xupper5 = nod$x_95,
        y = nod$y[1] - DOWN,
        ymax_major = nod$y[1] - DOWN + W*TOTAL_VAR,
        ymin_major = nod$y[1] - DOWN - W*TOTAL_VAR,
        ymax_minor = nod$y[1] - DOWN + PROP_BOX*W*TOTAL_VAR,
        ymin_minor = nod$y[1] - DOWN - PROP_BOX*W*TOTAL_VAR
      )
    }))

    p <- p +
      ggplot2::geom_rect(data=dbox, ggplot2::aes(xmin = xlower5, xmax = xmiddle,
                                                 ymin = ymin_minor, ymax = ymax_minor),
                         colour = "black", fill = "grey80") +
      ggplot2::geom_rect(data=dbox, ggplot2::aes(xmin = xmiddle, xmax = xupper5,
                                                 ymin = ymin_minor, ymax = ymax_minor),
                         colour = "black", fill = "grey80") +
      ggplot2::geom_rect(data=dbox, ggplot2::aes(xmin = xlower, xmax = xmiddle,
                                                 ymin = ymin_major, ymax = ymax_major),
                         colour = "black", fill = "grey70") +
      ggplot2::geom_rect(data=dbox, ggplot2::aes(xmin = xmiddle, xmax = xupper,
                                                 ymin = ymin_major, ymax = ymax_major),
                         colour = "black", fill = "grey70")
  }

  p_dendrogram <- p +
    ggplot2::scale_x_continuous(breaks = 1:ncol(X), labels = hc$labels[hc$order]) +
    ggplot2::scale_y_continuous(
      limits = c(NA, TOTAL_VAR),
      breaks = seq(0,1,0.1) * TOTAL_VAR,
      minor_breaks = seq(0,1,0.05) * TOTAL_VAR,
      labels = sprintf("%d%%", seq(0,100,10))
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text()
    ) +
    ggplot2::labs(x = NULL, y = "Variance explained (%)", fill = "")

  p_dendrogram
}


hclust_dendrogram <- function(B){
  B <- as.matrix(B)

  MERGE <- matrix(0, nrow = ncol(B), ncol = 2)
  ORD <- order(colSums(B != 0))

  for(i in 1:ncol(B)){
    # left side
    if(sum(B[,ORD[i]] < 0) == 1){
      MERGE[i,1] <- -which(B[,ORD[i]] < 0)[1]
    } else {
      if(i == 1) stop("Invalid structure: first column is not a simple balance.")
      MERGE[i,1] <- which(vapply(1:(i-1), function(j){
        all(B[which(B[,ORD[i]] < 0), ORD[j]] * B[which(B[,ORD[i]] < 0), ORD[i]] != 0)
      }, logical(1)))[1]
    }

    # right side
    if(sum(B[,ORD[i]] > 0) == 1){
      MERGE[i,2] <- -which(B[,ORD[i]] > 0)[1]
    } else {
      if(i == 1) stop("Invalid structure: first column is not a simple balance.")
      MERGE[i,2] <- which(vapply(1:(i-1), function(j){
        all(B[which(B[,ORD[i]] > 0), ORD[j]] * B[which(B[,ORD[i]] > 0), ORD[i]] != 0)
      }, logical(1)))[1]
    }
  }

  left <- function(pair){
    B_ <- sign(B)[pair, , drop=FALSE]
    good_cols <- apply(B_, 2, function(x) all(x != 0))
    if(!any(good_cols)) return(1)
    which.min(rowSums(B_[, good_cols, drop=FALSE] != 0))
  }

  ORDER <- 1:nrow(B)
  for(i in 1:nrow(B)){
    if(i != nrow(B)){
      for(j in (i+1):nrow(B)){
        x <- c(ORDER[i], ORDER[j])
        ileft_ <- left(x)
        ORDER[i] <- x[ileft_]
        ORDER[j] <- x[3-ileft_]
      }
    }
  }

  HEIGHT <- rep(0, ncol(B))
  for(i in 1:nrow(MERGE)){
    l_ <- 1
    r_ <- 1
    if(MERGE[i,1] > 0) l_ <- HEIGHT[MERGE[i,1]] + 1
    if(MERGE[i,2] > 0) r_ <- HEIGHT[MERGE[i,2]] + 1
    HEIGHT[i] <- max(l_, r_)
  }

  structure(
    list(merge = MERGE, height = HEIGHT, order = ORDER, labels = rownames(B)),
    class = "hclust"
  )
}
