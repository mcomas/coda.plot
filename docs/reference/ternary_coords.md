# Transform compositional data into ternary plotting coordinates

Convert a compositional dataset into ternary plotting coordinates under
a given `ternary_frame`.

## Usage

``` r
ternary_coords(frame, X, transform = TRUE, group = NULL)
```

## Arguments

- frame:

  A `ternary_frame` object.

- X:

  A numeric matrix or data frame with exactly three columns.

- transform:

  Logical. If `TRUE`, apply the frame transformation before converting
  to ternary coordinates. Default is `TRUE`.

- group:

  Optional grouping variable of length `nrow(X)`.

## Value

A data frame with compositional columns `c1`, `c2`, `c3`, ternary
coordinates `.x`, `.y`, and optionally `group`.
