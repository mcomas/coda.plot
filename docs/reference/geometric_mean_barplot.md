# Geometric Mean Barplot for Compositional Data

Generates a barplot based on the geometric mean of compositional parts.
Optionally, it can compare groups, display the parts on the x-axis,
overlay boxplots, or use centered log-ratio (clr) transformation.

## Usage

``` r
geometric_mean_barplot(
  X,
  group,
  x_show_parts = TRUE,
  include_boxplot = FALSE,
  clr_scale = FALSE
)
```

## Arguments

- X:

  A numeric matrix or data frame representing compositional data. Each
  row is an observation and each column is a part (must be strictly
  positive).

- group:

  A factor or character vector indicating group membership for each
  observation. Must have length `nrow(X)`.

- x_show_parts:

  Logical. If `TRUE`, the x-axis displays parts instead of group labels.
  Default is `TRUE`.

- include_boxplot:

  Logical. If `TRUE`, a boxplot is overlaid on top of the barplot.
  Default is `FALSE`.

- clr_scale:

  Logical. If `TRUE`, the data are transformed to clr coordinates before
  computing means. Default is `FALSE`.

## Value

A `ggplot2` object representing the geometric mean barplot.

## Details

For each part, the function computes (within each group) the mean of
either `log(X)` (default) or `clr(X)` (`clr_scale = TRUE`), and
subtracts the overall mean across all observations. Therefore, bars
represent deviations from the overall (global) mean on the chosen scale.
Overlaying a boxplot can help visualize within-group variability.

## Examples

``` r
set.seed(1)
X <- matrix(runif(30, 1, 10), ncol = 3)
colnames(X) <- c("A", "B", "C")
group <- rep(c("G1", "G2"), each = 5)
geometric_mean_barplot(X, group, include_boxplot = TRUE)

geometric_mean_barplot(X, group, clr_scale = TRUE)

```
