library(coda.plot)
X = milk_cows[,5:10]
group = milk_cows$group

ternary_diagram(X[,1:3])
ternary_diagram(X[,1:3], group = group)
ternary_diagram(X[,1:3], show_pc = TRUE)
ternary_diagram(X[,1:3], group = group, show_pc = TRUE)
ternary_diagram(X[,1:3], group = group, center = TRUE, show_pc = TRUE)
ternary_diagram(X[,1:3], group = group, center = TRUE, scale = TRUE, show_pc = TRUE)
ternary_diagram(X[,1:3], group = group, scale = TRUE, show_pc = TRUE)


geometric_mean_barplot(X, group = group)
geometric_mean_barplot(X, group = group, include_boxplot = TRUE)

geometric_mean_barplot(X, group = group, clr_scale = TRUE)
geometric_mean_barplot(X, group = group, clr_scale = TRUE, include_boxplot = TRUE)

geometric_mean_barplot(X, group = group, x_show_parts = FALSE)
geometric_mean_barplot(X, group = group, x_show_parts = FALSE, include_boxplot = TRUE)

clr_biplot(X, group = group, alpha = 1)

balance_dendrogram(X, ilr_basis(ncol(X)))
balance_dendrogram(X, ilr_basis(ncol(X)), group) + theme(legend.position = 'bottom')
balance_dendrogram(X, pb_basis(X, method = 'exact'))
balance_dendrogram(X, pb_basis(X, method = 'exact'), group = group)

apply(coordinates(X), 2, var)
# geometric_mean_barplgroup# geometric_mean_barplot(X, group, type = 'line')
# geometric_mean_barplot(X, group, type = 'box')

# biplot_form(X, group)

X = alimentation[,1:3]
group = alimentation$NorthMed
ternary_diagram(X, group)
geometric_mean_barplot(X, group)
geometric_mean_barplot(X, group, x_show_parts = F)

clr_biplot(X, col_group = group, biplot_type = 'covariance')

X = alimentation[,1:9]
balance_dendrogram(X, pb_basis(X, method = 'exact'))
balance_dendrogram(X, pb_basis(X, method = 'exact'), group = group)
