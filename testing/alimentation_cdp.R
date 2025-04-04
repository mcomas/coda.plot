library(coda.plot)
X = milk_cows[,5:10]
group = milk_cows$group
geometric_mean_barplot(X, group, x_show_parts = F, facets = T)
# geometric_mean_barplot(X, group, type = 'line')
# geometric_mean_barplot(X, group, type = 'box')

# biplot_form(X, group)

X = alimentation[,1:9]
group = alimentation$NorthMed
geometric_mean_barplot(X, group)
geometric_mean_barplot(X, group, x_show_parts = F) +
  guides(fill = guide_legend(nrow = 1)) +
  theme(legend.position = 'top')

clr_biplot(df[,5:10], group = df$group)
