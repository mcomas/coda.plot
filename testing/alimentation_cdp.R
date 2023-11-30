library(coda.plot)
df = readxl::read_excel("~/Software/CoDaPack/data/milkcows.xls")
X = df[,5:10]
group = df$group
geometric_mean_bar_plot(X, group)
geometric_mean_bar_plot(X, group, type = 'line')
geometric_mean_bar_plot(X, group, type = 'box')

biplot_form(X, group)

library(coda.base)
alimentation = read_cdp("~/Software/CoDaPack/data/alimentation.cdp")

X = alimentation[,2:10]
group = alimentation$NorthMed
geometric_mean_bar_plot(X, group)
geometric_mean_bar_plot(X, group, type = 'box')

clr_biplot(df[,5:10], group = df$group)
