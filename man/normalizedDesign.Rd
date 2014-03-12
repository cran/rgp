\name{normalizedDesign}
\alias{normalizedDesign}
\title{Create a normalized design matrix}
\usage{
normalizedDesign(dimension, size, calcMinDistance = FALSE)
}
\arguments{
  \item{dimension}{Dimension of the problem (will be no. of
  columns of the result matrix).}

  \item{size}{Number of points with that dimension needed.
  (will be no. of rows of the result matrix).}

  \item{calcMinDistance}{Indicates whether a minimal
  distance should be calculated.}
}
\value{
List \code{L} consists of a matrix and nd (if required) a
minimal distance.
}
\description{
Produces a normalized design and calculates the minimal
distance if required. Returns a design is a matrix with
\code{dim} columns and \code{size} rows.
}

