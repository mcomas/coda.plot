# Add a ternary grid layer

Add a ternary grid layer

## Usage

``` r
add_ternary_grid(p, ticks = seq(0.1, 0.9, 0.1), n = 300, eps = 1e-06, ...)
```

## Arguments

- p:

  A `ggplot2` object created by
  [`ternary_base()`](https://mcomas.github.io/coda.plot/reference/ternary_base.md).

- ticks:

  Numeric vector of grid levels.

- n:

  Number of sampled points per grid line.

- eps:

  Small positive offset used before log-ratio transformation.

- ...:

  Further arguments passed to
  [`ggplot2::geom_path()`](https://ggplot2.tidyverse.org/reference/geom_path.html).

## Value

A `ggplot2` object.
