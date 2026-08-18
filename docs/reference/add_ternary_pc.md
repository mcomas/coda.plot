# Add principal component paths to a ternary plot

Add principal component paths to a ternary plot

## Usage

``` r
add_ternary_pc(
  p,
  X,
  group = NULL,
  pcs = 1:2,
  basis = coda.base::ilr_basis(3),
  n = 600,
  eps = 0.001,
  ...
)
```

## Arguments

- p:

  A `ggplot2` object created by
  [`ternary_base()`](https://mcomas.github.io/coda.plot/reference/ternary_base.md).

- X:

  A numeric matrix or data frame with exactly three columns.

- group:

  Optional grouping variable of length `nrow(X)`. If supplied, PCs are
  computed separately by group.

- pcs:

  Integer vector indicating which principal components to draw.

- basis:

  An ilr basis. Default is `coda.base::ilr_basis(3)`.

- n:

  Number of sampled points per PC path.

- eps:

  Small positive threshold used to keep the path inside the simplex.

- ...:

  Further arguments passed to
  [`ggplot2::geom_path()`](https://ggplot2.tidyverse.org/reference/geom_path.html).

## Value

A `ggplot2` object.
