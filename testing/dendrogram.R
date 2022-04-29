library(coda.base)
library(data.table)
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


plot_balance = function(B, data = NULL, main = 'Balance dendrogram', ...){
  if(is.null(data)){
    plot(as.dendrogram(hclust_dendrogram(B)), main = main, ylab = "", axes = FALSE, ...)
  }else{

  }

}
left_ = function(X, B, nparts, x0, y0){
  ipart = which(colSums(B == 0) == (nrow(B)-nparts))
  dv = data.table(x = x0, y = y0, xend = x0, yend = y0+p.sigma[ipart])
  dh = data.table(x = x0-1, xend = x0+1, y = y0+p.sigma[ipart], yend = y0+p.sigma[ipart])
  d = rbind(dv, dh)
  i_right = B[,ipart] > 0
  if(sum(i_right) > 1){
    Br = B[,-ipart, drop=FALSE]
    Br[!i_right,] = 0
    dr = right_(X, Br, sum(i_right), x0+1, y0+p.sigma[ipart])
    d = rbind(d, dr)
  }
  i_left = B[,ipart] < 0
  if(sum(i_left) > 1){
    Bl = B[,-ipart, drop=FALSE]
    Bl[!i_left,] = 0
    dl = left_(X, Bl, sum(i_left), x0-1, y0+p.sigma[ipart])
    d = rbind(d, dl)
  }
  d
}
right_ = function(X, B, nparts, x0, y0){
  ipart = which(colSums(B == 0) == (nrow(B)-nparts))
  dv = data.table(x = x0, y = y0, xend = x0, yend = y0+p.sigma[ipart])
  dh = data.table(x = x0-1, xend = x0+1, y = y0+p.sigma[ipart], yend = y0+p.sigma[ipart])
  d = rbind(dv, dh)
  i_right = B[,ipart] > 0
  if(sum(i_right) > 1){
    Br = B[,-ipart, drop=FALSE]
    Br[!i_right,] = 0
    dr = right_(X, Br, sum(i_right), x0+1, y0+p.sigma[ipart])
    d = rbind(d, dr)
  }
  i_left = B[,ipart] < 0
  if(sum(i_left) > 1){
    Bl = B[,-ipart, drop=FALSE]
    Bl[!i_left,] = 0
    dl = left_(X, Bl, sum(i_left), x0-1, y0+p.sigma[ipart])
    d = rbind(d, dl)
  }
  d
}
den = function(X, B){
  B_ = as.matrix(B)

  ipart = which(colSums(B_ == 0) == 0)
  dv = data.table(x = 0, y = 0, xend = 0, yend = p.sigma[ipart])
  dh = data.table(x = -1, xend = +1, y = p.sigma[ipart], yend = p.sigma[ipart])
  d = rbind(dv, dh)
  i_right = B_[,ipart] > 0
  if(sum(i_right) > 1){
    Br = B_[,-ipart, drop=FALSE]
    Br[!i_right,] = 0
    dr = right_(X, Br, sum(i_right), +1, p.sigma[ipart])
    d = rbind(d, dr)
  }
  i_left = B_[,ipart] < 0
  if(sum(i_left) > 1){
    Bl = B_[,-ipart, drop=FALSE]
    Bl[!i_left,] = 0
    dl = left_(X, Bl, sum(i_left), -1, p.sigma[ipart])
    d = rbind(d, dl)
  }
  d
}

X = parliament2017[,2:6]
B = cdp_basis(ncol(X))
SIGMA = cov(coordinates(X, B))
p.sigma = prop.table(diag(SIGMA))

dplot = den(X, B)
ggplot() +
  geom_segment(data=dplot, aes(x=x, y=y, xend=xend, yend=yend))

#######
