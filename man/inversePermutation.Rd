\name{inversePermutation}
\alias{inversePermutation}
\title{Calculate the inverse of a permutation}
\usage{
  inversePermutation(x)
}
\arguments{
  \item{x}{The permutation to return the inverse for.}
}
\value{
  The inverse of the permutation \code{x}.
}
\description{
  Returns the inverse of a permutation \code{x} given as an
  integer vector. This function is useful to turn a ranking
  into an ordering and back, for example.
}
\seealso{
  \code{\link{rank}}, \code{\link{order}}
}

