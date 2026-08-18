# Compositional Balance Dendrogram

Plots a balance dendrogram based on a compositional data set and a
corresponding balance matrix. This visualization helps interpret the
structure of balances in compositional data analysis.

## Usage

``` r
balance_dendrogram(X, B, group = NULL)
```

## Arguments

- X:

  A numeric matrix or data frame representing the compositional data.
  Rows are observations and columns are components (must be strictly
  positive).

- B:

  A numeric matrix representing the balance basis (e.g., an isometric
  log-ratio (ilr) balance matrix).

- group:

  Optional. If provided, show grouped box summaries under each node.

## Value

A `ggplot2` object representing the balance dendrogram.

## Examples

``` r
# Simulated compositional data and balances
X = matrix(runif(50, 1, 10), ncol = 5)
colnames(X) = LETTERS[1:5]
B = coda.base::pb_basis(X, method = 'exact')
balance_dendrogram(X, B)
#> Ignoring unknown labels:
#> • fill : ""

```
