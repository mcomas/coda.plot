# Log-contrast biplot

Represents compositional observations in the two-dimensional coordinate
system defined by two log-contrasts, and overlays the projected unit
directions for each part as rays from the center of the observations.

## Usage

``` r
logcontrast_biplot(
  X,
  lc,
  group = NULL,
  shape_group = NULL,
  part_labels = NULL,
  standardize_lc = FALSE,
  rescale_rays = TRUE,
  ray_scale = 1,
  return_data = FALSE,
  repel = TRUE,
  repel_force = 1,
  repel_max_overlaps = Inf
)
```

## Arguments

- X:

  A matrix or data frame containing compositional data (strictly
  positive).

- lc:

  Numeric matrix with `ncol(X)` rows and at least two columns. Each
  column must be a log-contrast vector with zero sum. If more than two
  columns are supplied, only the first two are used. Column names, when
  available, are used as axis labels.

- group:

  Optional factor/character used to color the observations.

- shape_group:

  Optional factor/character used to assign shapes to observations.

- part_labels:

  Optional character vector with part labels. If `NULL`, `colnames(X)`
  are used when available.

- standardize_lc:

  Logical. If `TRUE`, normalize each log-contrast vector to Euclidean
  norm 1 before computing coordinates.

- rescale_rays:

  Logical. If `TRUE`, rescale the part rays to have a range comparable
  to the observation coordinates.

- ray_scale:

  Numeric multiplier applied to ray lengths after optional rescaling.

- return_data:

  Logical. If TRUE, returns a list with data frames for observations,
  parts, and the ggplot object.

- repel:

  Logical. If TRUE (default), use ggrepel for part labels when
  available.

- repel_force:

  Numeric. Repulsion force passed to
  [`ggrepel::geom_text_repel()`](https://ggrepel.slowkow.com/reference/geom_text_repel.html).

- repel_max_overlaps:

  Numeric. Maximum overlaps allowed (ggrepel).

## Value

A `ggplot2` object. If `return_data = TRUE`, a list with elements `obs`,
`parts`, and `plot`.

## Examples

``` r
X <- milk_cows[, 5:10]
lc <- cbind(
  protein = c(1, -1, 0, 0, 0, 0),
  fat = c(0, 0, 1, -1, 0, 0)
)
logcontrast_biplot(X, lc, group = milk_cows$group)
#> Ignoring unknown labels:
#> • shape : ""

```
