# Create a base ternary plot

Create the base ggplot object associated with a `ternary_frame`.

## Usage

``` r
ternary_base(
  frame = NULL,
  show_grid = TRUE,
  show_outline = TRUE,
  show_labels = TRUE,
  grid_ticks = ppoints(9, 0)
)
```

## Arguments

- frame:

  Optional `ternary_frame` object. If `NULL`, a default ternary frame is
  created with no centering and no scaling.

- show_grid:

  Logical. If `TRUE`, draw the ternary grid.

- show_outline:

  Logical. If `TRUE`, draw the ternary triangle outline.

- show_labels:

  Logical. If `TRUE`, draw corner labels.

- grid_ticks:

  Numeric vector of grid levels.

## Value

A `ggplot2` object with the `ternary_frame` attached as attribute.
