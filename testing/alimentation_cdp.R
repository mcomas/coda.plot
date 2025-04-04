library(coda.plot)
X = milk_cows[,5:10]
group = milk_cows$group

ternary(X[,1:3], group = group)
geometric_mean_barplot(X, group = group)
geometric_mean_barplot(X, group = group, clr_scale = TRUE)
center_barplot(X, group)
clr_biplot(X, group = group, alpha = 1)
balance_dendrogram(X, ilr_basis(ncol(X)))

apply(coordinates(X), 2, var)
# geometric_mean_barplgroup# geometric_mean_barplot(X, group, type = 'line')
# geometric_mean_barplot(X, group, type = 'box')

# biplot_form(X, group)

X = alimentation[,1:3]
group = alimentation$NorthMed
ternary(X, group)
geometric_mean_barplot(X, group)
geometric_mean_barplot(X, group, x_show_parts = F)

clr_biplot(X, col_group = group, biplot_type = 'covariance')
