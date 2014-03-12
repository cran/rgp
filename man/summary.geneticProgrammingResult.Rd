\name{summary.geneticProgrammingResult}
\alias{summary.geneticProgrammingResult}
\title{Summary reports of genetic programming run result objects}
\usage{
\method{summary}{geneticProgrammingResult}(object, reportFitness = FALSE,
  orderByFitness = TRUE, ...)
}
\arguments{
  \item{object}{The genetic programming run result object
  to report on.}

  \item{reportFitness}{Whether to report detailed fitness
  values of each individual in the result population. Note
  that calculating fitness values may take a long time.
  Defaults to \code{FALSE}. Either way, basic fitness
  values for each individual is reported.}

  \item{orderByFitness}{Whether the report of the result
  population should be ordered by fitness. This does not
  have an effect if \code{reportFitness} is set to
  \code{FALSE}. Defaults to \code{TRUE}.}

  \item{...}{Ignored in this summary function.}
}
\description{
Create a summary report of a genetic programming result
object as returned by \code{\link{geneticProgramming}} or
\code{\link{symbolicRegression}}, for example.
}
\seealso{
\code{\link{geneticProgramming}},
\code{\link{symbolicRegression}}
}

