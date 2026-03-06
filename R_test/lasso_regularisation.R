library(coda.base)
library(genlasso)
library(ggplot2)
summary(y <- waste$floating_population)
summary(X <- waste[,5:9])

lasso_path = function(y, X, method = 'coda-l1'){

  B = ilr_basis(X)
  H = coordinates(X, B)
  if(method == 'clr-l1'){
    M = cbind(1,as.matrix(H))
    D = cbind(0,B)
    m = genlasso(y, M, D, svd = TRUE)
    cpath = do.call(`rbind`, lapply(1:length(m$lambda), function(i){
      betas = m$beta[-1,i] %*% t(B)
      data.frame(
        lambda = m$lambda[i],
        variable = colnames(betas),
        value = as.vector(betas)
      )
    }))
  }
  if(method == 'pw-l1'){
    M = cbind(1,as.matrix(H))
    D = cbind(0,t(pairwise_basis(nrow(B))) %*% B)
    m = genlasso(y, M, D, svd = TRUE)
    cpath = do.call(`rbind`, lapply(1:length(m$lambda), function(i){
      betas = m$beta[-1,i] %*% t(B)
      data.frame(
        lambda = m$lambda[i],
        variable = colnames(betas),
        value = as.vector(betas)
      )
    }))
  }
  if(method == 'coda-l1'){
    M = cbind(1,as.matrix(H),0)
    D = cbind(0,B,1)
    m = genlasso(y, M, D, svd = TRUE)
    cpath = do.call(`rbind`, lapply(1:length(m$lambda), function(i){
      betas = m$beta[-c(1,1+nrow(B)),i] %*% t(B)
      data.frame(
        lambda = m$lambda[i],
        variable = colnames(betas),
        value = as.vector(betas)
      )
    }))
  }
  ggplot(data=cpath) +
    #geom_vline(xintercept = m$lambda, linetype = 'dotted', alpha = 0.5) +
    geom_line(aes(x = lambda, y = value, col = variable)) +
    theme_minimal() + labs(col = '')

}
p1 = lasso_path(y, X, 'coda-l1') + ggtitle('CoDa L1')
p2 = lasso_path(y, X, 'pw-l1') + ggtitle('Pairwise L1')
p3 = lasso_path(y, X, 'clr-l1') + ggtitle('CLR L1')
library(patchwork)
p1 / p2 / p3
