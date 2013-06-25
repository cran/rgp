\name{latinHypercubeDesign}
\alias{latinHypercubeDesign}
\title{Create a latin hypercube design (LHD)}
\usage{
  latinHypercubeDesign(dimension,
    size = max(11 * dimension, 1 + 3 * dimension + dimension * (dimension - 1)/2 + 1),
    lowerBounds = replicate(dimension, 0),
    upperBounds = replicate(dimension, 1),
    retries = 2 * dimension)
}
\arguments{
  \item{dimension}{Dimension of the problem (will be no. of
  columns of the result matrix).}

  \item{size}{Number of design points, defaults to
  \code{max(11 * dimension, 1 + 3 * dimension + dimension *
  (dimension - 1) / 2 + 1)}.}

  \item{lowerBounds}{Numeric vector of length
  \code{dimension} giving lower bounds for sampling,
  defaults to \code{c(0.0, ...)}.}

  \item{upperBounds}{Numeric vector of length
  \code{dimension} giving upper bounds for sampling,
  defaults to \code{c(1.0, ...)}.}

  \item{retries}{Number of retries, which is the number of
  trials to find a design with the lowest distance, default
  is \code{2 * dimension}.}
}
\value{
  A LHD matrix.
}
\description{
  Produces a LHD matrix with \code{dimension} columns and
  \code{size} rows.
}

