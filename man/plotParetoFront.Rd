\name{plotParetoFront}
\alias{plotParetoFront}
\title{Plot a GP Pareto Front}
\usage{
plotParetoFront(x, y, z, indicesToMark = integer(), maxZ = 50,
  main = sprintf("Population Pareto Front Plot (\% Individuals)", length(x)),
  ...)
}
\arguments{
  \item{x}{A vector of type \code{numeric} representing
  individual fitness.}

  \item{y}{A vector of type \code{numeric} representing
  individual complexity.}

  \item{z}{A vector of type \code{integer} representing
  individual age.}

  \item{indicesToMark}{A index vector of points to mark
  with red crosses.}

  \item{maxZ}{The individual age at the large end of the
  age color scale.}

  \item{main}{The plot's title.}

  \item{...}{Graphic parameters for \code{\link{par}} and
  further arguments to \code{plot}.  For example, use the
  \code{main} parameter to set a title.}
}
\description{
Plots fitness/complexity/age Pareto fronts for
multi-objective GP. The z-coordinate represents individual
age and is shown in form of a color scale, where younger
individuals are bright green, individuals with age
\code{maxZ} are black. Individuals not on the first Pareto
front are shown as small gray circles, regardless of age.
}
\seealso{
\code{\link{funcToIgraph}}
}

