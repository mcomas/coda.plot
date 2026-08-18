# Ternary plot for compositional data (D = 3)

Create a ternary plot from compositional data with exactly three parts.
Optionally center and/or scale the data in log-ratio coordinates, color
points by group, and overlay the first two principal component
directions computed in *ilr* coordinates.

## Usage

``` r
ternary_plot(X, group = NULL, center = FALSE, scale = FALSE, show_pc = FALSE)
```

## Arguments

- X:

  A numeric matrix or data frame with exactly three columns (the parts
  of the composition). Values should be positive. Column names (if
  present) are used as corner labels.

- group:

  Optional. A factor or character vector of length `nrow(X)` used to
  color points by group.

- center:

  Logical. If `TRUE`, center the log-ratio coordinates before plotting.
  Default is `FALSE`.

- scale:

  Logical or numeric. If `FALSE`, no scaling is applied. If `TRUE`,
  log-ratio coordinates are scaled by their empirical standard
  deviations. If a single positive numeric value is supplied, centered
  log-ratio coordinates are multiplied by that value, so values larger
  than 1 increase visual spread and values between 0 and 1 shrink it.

- show_pc:

  Logical. If `TRUE`, overlay the first two principal component
  directions computed on log-ratio coordinates (recommended: *ilr*).
  Default is `FALSE`.

## Value

A `ggplot2` object.

## Details

This function is the convenient wrapper around the modular ternary API:
[`ternary_frame()`](https://mcomas.github.io/coda.plot/reference/ternary_frame.md),
[`ternary_base()`](https://mcomas.github.io/coda.plot/reference/ternary_base.md),
[`add_ternary_points()`](https://mcomas.github.io/coda.plot/reference/add_ternary_points.md),
and
[`add_ternary_pc()`](https://mcomas.github.io/coda.plot/reference/add_ternary_pc.md).

## See also

[`ternary_frame`](https://mcomas.github.io/coda.plot/reference/ternary_frame.md),
[`ternary_base`](https://mcomas.github.io/coda.plot/reference/ternary_base.md),
[`add_ternary_points`](https://mcomas.github.io/coda.plot/reference/add_ternary_points.md),
[`add_ternary_pc`](https://mcomas.github.io/coda.plot/reference/add_ternary_pc.md)

## Examples

``` r
X <- milk_cows[, 5:7]
group <- milk_cows$group

ternary_plot(X, group = group)

ternary_plot(X, group = group, center = TRUE, scale = TRUE)

ternary_plot(X, group = group, center = TRUE, scale = 1.5)

ternary_plot(X, show_pc = TRUE)

```
