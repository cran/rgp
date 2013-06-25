\name{customDist}
\alias{customDist}
\title{A \code{dist} function that supports custom metrics}
\usage{
  customDist(x, metric, diag = FALSE, upper = FALSE)
}
\arguments{
  \item{x}{A vector or list of objects.}

  \item{metric}{A metric, i.e. a function of two arguments
  that returns a numeric. Note that a metric must be
  definite and symmetric, otherwise the results will be
  undefined.}

  \item{diag}{\code{TRUE} iff the diagonal of the distance
  matrix should be printed by \code{print.dist}.}

  \item{upper}{\code{TRUE} iff the upper triangle of the
  distance matrix should be printed by \code{print.dist}.}
}
\value{
  A distance matrix.
}
\description{
  This function computes and returns the distance matrix
  computed by using the given metric to compute the
  distances between the rows of a data list or vector. Note
  that in contrast to \code{\link{dist}}, \code{x} has to
  be a vector and the the distance \code{metric} is an
  arbitrary function that must be symmetric and definite.
}
\seealso{
  \code{\link{dist}}
}

