\name{plotFunction3d}
\alias{plotFunction3d}
\title{Plot a 2D function as a 3D surface}
\usage{
plotFunction3d(func = function(x) sum(x^2), lo = c(0, 0), up = c(1, 1),
  samples = 10, palette = gray.colors(256), ...)
}
\arguments{
  \item{func}{A 2D function to plot.}

  \item{lo}{A vector of lower limits of the plot (one entry
  for each dimension).}

  \item{up}{A vector of upper limits of the plot (one entry
  for each dimension).}

  \item{samples}{The number of samples in each dimension.}

  \item{palette}{The color palette, use \code{NULL} to
  disable.}

  \item{...}{Graphic parameters for \code{\link{persp}}.}
}
\description{
Creates and shows and perspective plot of a 2D function of
either the form \eqn{z = f(x, y)} or \eqn{z = f(xv)}, where
\eqn{xv} is a numeric of length 2.
}

