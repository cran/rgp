\name{randelt}
\alias{randelt}
\title{Choose a random element from a list or vector}
\usage{
  randelt(x, prob = NULL)
}
\arguments{
  \item{x}{The vector or list to chose an element from.}

  \item{prob}{A vector of probability weights for obtaining
  the elements of the vector or list being sampled.}
}
\value{
  A uniformly random element of \code{x}.
}
\description{
  Returns a unformly random chosen element of the vector or
  list \code{x}.
}

