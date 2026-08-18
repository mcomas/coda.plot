# Ternary frame for compositional data (D = 3)

Build a ternary plotting frame for compositional data with exactly three
parts. The returned object stores the geometric and log-ratio
transformation machinery needed to add multiple data layers consistently
to the same ternary diagram.

## Usage

``` r
ternary_frame(X, center = FALSE, scale = FALSE, labels = NULL)
```

## Arguments

- X:

  A numeric matrix or data frame with exactly three columns. This data
  defines the reference frame used for centering/scaling in log-ratio
  space.

- center:

  Logical. If `TRUE`, center log-ratio coordinates using the mean
  coordinates of `X`. Default is `FALSE`.

- scale:

  Logical or numeric. If `FALSE`, no scaling is applied. If `TRUE`,
  log-ratio coordinates are scaled by their empirical standard
  deviations. If a single positive numeric value is supplied, centered
  log-ratio coordinates are multiplied by that value.

- labels:

  Optional character vector of length 3 used as corner labels. If
  `NULL`, `colnames(X)` are used when available. Otherwise, temporary
  labels `c1`, `c2`, `c3` are used.

## Value

An object of class `"ternary_frame"`.
