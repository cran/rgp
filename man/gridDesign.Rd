\name{gridDesign}
\alias{gridDesign}
\title{Create a regular grid design matrix}
\usage{
gridDesign(dimension, points = seq(from = 0, to = 1, length.out = 10))
}
\arguments{
  \item{dimension}{The number of columns in the design
  matrix to create.}

  \item{points}{A vector of points to sample at in each
  dimension.}
}
\value{
The regular grid design matrix.
}
\description{
Returns a n = length(points)**dimension times m = dimension
matrix containing the coordinates of sample points from a
hypervolume of the given dimension. Points are sampled in a
grid defined by the vector points.
}

