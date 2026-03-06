library(ggplot2)

.as_df <- function(x) {
  if (is.matrix(x)) {
    x <- as.data.frame(x, check.names = FALSE, stringsAsFactors = FALSE)
  }
  if (!is.data.frame(x)) stop("`df` ha de ser data.frame o matriu.", call. = FALSE)
  x
}

.check_cols <- function(df, cols) {
  miss <- setdiff(cols, names(df))
  if (length(miss)) {
    stop(sprintf("Falten columnes: %s.\nDisponibles: %s",
                 paste(miss, collapse = ", "),
                 paste(names(df), collapse = ", ")),
         call. = FALSE)
  }
}

ternary_close <- function(df, A, B, C, allow_na = FALSE) {
  df <- .as_df(df)
  .check_cols(df, c(A, B, C))

  a <- df[[A]]; b <- df[[B]]; c <- df[[C]]

  if (!allow_na && any(!is.finite(a) | !is.finite(b) | !is.finite(c))) {
    stop("Hi ha valors NA/Inf a A, B o C. Neteja'ls o usa `allow_na = TRUE`.", call. = FALSE)
  }

  s <- a + b + c
  bad_sum <- which(!(is.finite(s) & s > 0))
  if (length(bad_sum)) {
    stop(sprintf("Files amb suma no positiva o no finita a A+B+C: %s",
                 paste(head(bad_sum, 10), collapse = ", ")),
         call. = FALSE)
  }

  df$.A <- a / s
  df$.B <- b / s
  df$.C <- c / s
  df
}

ternary_xy <- function(df, A = ".A", B = ".B", C = ".C") {
  df <- .as_df(df)
  .check_cols(df, c(A, B, C))
  x <- df[[B]] + 0.5 * df[[C]]
  y <- (sqrt(3)/2) * df[[C]]
  df$.x <- x
  df$.y <- y
  df
}

prepare_ternary <- function(df, A, B, C, keep_props = TRUE, allow_na = FALSE) {
  d0 <- ternary_close(df, A, B, C, allow_na = allow_na)
  d1 <- ternary_xy(d0, ".A", ".B", ".C")
  if (!keep_props) d1 <- d1[, c(".x", ".y")]
  d1
}

ternary_outline_df <- function() {
  data.frame(.x = c(0, 1, 0.5, 0), .y = c(0, 0, sqrt(3)/2, 0))
}

ternary_isolines_df <- function(var = c("A","B","C"), ticks = seq(0.1, 0.9, 0.1)) {
  var <- match.arg(var)
  out <- lapply(ticks, function(tk) {
    n <- 100
    if (var == "A") {
      Bv <- seq(0, 1 - tk, length.out = n); Cv <- (1 - tk) - Bv; Av <- rep(tk, n)
    } else if (var == "B") {
      Av <- seq(0, 1 - tk, length.out = n); Cv <- (1 - tk) - Av; Bv <- rep(tk, n)
    } else {
      Av <- seq(0, 1 - tk, length.out = n); Bv <- (1 - tk) - Av; Cv <- rep(tk, n)
    }
    df <- data.frame(.A = Av, .B = Bv, .C = Cv, var = var, tick = tk)
    ternary_xy(df, ".A", ".B", ".C")
  })
  do.call(rbind, out)
}

geom_ternary_outline <- function(...) {
  geom_path(data = ternary_outline_df(), aes(.x, .y), inherit.aes = FALSE, ...)
}

geom_ternary_grid <- function(ticks = seq(0.1, 0.9, 0.1), alpha = 0.3, ...) {
  gA <- ternary_isolines_df("A", ticks)
  gB <- ternary_isolines_df("B", ticks)
  gC <- ternary_isolines_df("C", ticks)
  list(
    geom_path(data = gA, aes(.x, .y), inherit.aes = FALSE, alpha = alpha, ...),
    geom_path(data = gB, aes(.x, .y), inherit.aes = FALSE, alpha = alpha, ...),
    geom_path(data = gC, aes(.x, .y), inherit.aes = FALSE, alpha = alpha, ...)
  )
}

geom_ternary_corner_labels <- function(labels = c(A = "A", B = "B", C = "C"), ...) {
  labs <- data.frame(
    lab = unname(labels),
    .x  = c(0, 1, 0.5),
    .y  = c(0, 0, sqrt(3)/2)
  )
  geom_text(data = labs, aes(.x, .y, label = lab),
            inherit.aes = FALSE, vjust = c(1.2, 1.2, -0.4), ...)
}

theme_ternary <- function() {
  theme_void() + theme(plot.margin = margin(10,10,10,10))
}


set.seed(1)
df <- as.data.frame(matrix(rexp(300, 1), ncol = 3))
names(df) <- c("A","B","C")

d2 <- prepare_ternary(df, "A","B","C")

ggplot(d2, aes(.x, .y)) +
  # geom_ternary_grid() +
  geom_ternary_outline(linewidth = 1) +
  geom_point() +
  geom_ternary_corner_labels() +
  coord_equal() +
  theme_ternary()

