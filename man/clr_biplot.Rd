% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clr_ciplot.R
\name{clr_biplot}
\alias{clr_biplot}
\title{Compositional CLR Biplot}
\usage{
clr_biplot(
  X,
  group = NULL,
  biplot_type = "covariance",
  alpha = NULL,
  shape_group = NULL
)
}
\arguments{
\item{X}{A matrix or data frame containing compositional data.}

\item{group}{factor used to color the observations.}

\item{biplot_type}{Character string specifying the type of biplot. Either `"covariance"` (default) or `"form"`.}

\item{alpha}{Optional numeric value between 0 and 1. If provided, this overrides \code{biplot_type}. Controls the type of biplot:
\itemize{
  \item 0 = covariance biplot
  \item 1 = form biplot
}}

\item{shape_group}{Optional factor used to assign shapes to the observations.}
}
\value{
A \code{ggplot} object displaying the biplot.
}
\description{
Generates a centered log-ratio (CLR) biplot for compositional data.
}
