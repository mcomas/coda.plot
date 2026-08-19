# coda.plot

`coda.plot` provides a collection of easy-to-use `ggplot2`
visualisations for compositional data. It supports ternary diagrams, clr
and log-contrast biplots, group comparisons based on geometric means,
and balance dendrograms.

Compositional parts must be strictly positive. Rows do not need to sum
to 1 or 100 because the functions work with relative information.

## Installation

Install the development version from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("mcomas/coda.plot")
```

Then load the package:

``` r
library(coda.plot)
```

## Key functions

| Function | Use |
|----|----|
| [`ternary_plot()`](https://mcomas.github.io/coda.plot/reference/ternary_plot.md) | Create a complete ternary diagram for three-part compositions. |
| [`ternary_frame()`](https://mcomas.github.io/coda.plot/reference/ternary_frame.md) and [`ternary_base()`](https://mcomas.github.io/coda.plot/reference/ternary_base.md) | Set up a ternary diagram for layer-by-layer customisation. |
| [`add_ternary_grid()`](https://mcomas.github.io/coda.plot/reference/add_ternary_grid.md), [`add_ternary_points()`](https://mcomas.github.io/coda.plot/reference/add_ternary_points.md), [`add_ternary_path()`](https://mcomas.github.io/coda.plot/reference/add_ternary_path.md), and [`add_ternary_pc()`](https://mcomas.github.io/coda.plot/reference/add_ternary_pc.md) | Add grids, observations, paths, and principal directions to a ternary diagram. |
| [`clr_biplot()`](https://mcomas.github.io/coda.plot/reference/clr_biplot.md) | Explore observations and parts in centred log-ratio coordinates. |
| [`logcontrast_biplot()`](https://mcomas.github.io/coda.plot/reference/logcontrast_biplot.md) | Plot observations using two user-defined log-contrasts. |
| [`geometric_mean_barplot()`](https://mcomas.github.io/coda.plot/reference/geometric_mean_barplot.md) | Compare compositional parts across groups. |
| [`balance_dendrogram()`](https://mcomas.github.io/coda.plot/reference/balance_dendrogram.md) | Visualise and interpret a balance basis. |

See the [function
reference](https://mcomas.github.io/coda.plot/reference/index.md) for
the complete API and the [getting-started
guide](https://mcomas.github.io/coda.plot/articles/getting-started.md)
for a more detailed walkthrough.

## Examples

Create some positive three-part compositional data and display it in a
ternary diagram:

``` r
set.seed(2026)
X <- matrix(rexp(90), ncol = 3)
colnames(X) <- c("Protein", "Fat", "Carbohydrates")
group <- factor(rep(c("Control", "Treatment"), each = 15))

ternary_plot(X, group = group)
```

The modular ternary interface gives control over individual layers:

``` r
frame <- ternary_frame(X)

p <- ternary_base(frame, show_grid = FALSE)
p <- add_ternary_grid(p, ticks = c(0.25, 0.50, 0.75), colour = "grey80")
add_ternary_points(p, X, group = group, size = 2)
```

For compositions with more than three parts, use a clr biplot to show
both observations and part directions:

``` r
X6 <- matrix(rexp(180), ncol = 6)
colnames(X6) <- paste0("Part_", 1:6)

clr_biplot(X6, group = group)
```

Compare the three parts between groups using deviations from their
overall geometric means:

``` r
geometric_mean_barplot(X, group, include_boxplot = TRUE)
```
