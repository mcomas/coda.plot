# Add compositional points to a ternary plot

Add compositional points to a ternary plot

## Usage

``` r
add_ternary_points(p, X, group = NULL, transform = TRUE, ...)
```

## Arguments

- p:

  A `ggplot2` object created by
  [`ternary_base()`](https://mcomas.github.io/coda.plot/reference/ternary_base.md).

- X:

  A numeric matrix or data frame with exactly three columns.

- group:

  Optional grouping variable of length `nrow(X)`.

- transform:

  Logical. If `TRUE`, apply the frame transformation.

- ...:

  Further arguments passed to
  [`ggplot2::geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html).

## Value

A `ggplot2` object.
