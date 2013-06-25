\name{functionVariablePresenceMap}
\alias{functionVariablePresenceMap}
\alias{populationVariablePresenceMap}
\title{Variable Presence Maps}
\usage{
  functionVariablePresenceMap(f)

  populationVariablePresenceMap(pop)
}
\arguments{
  \item{f}{A R function to return a variable presence map
  for.}

  \item{pop}{A RGP population to return a variable presence
  map for.}
}
\value{
  A data frame with variables (formal parameters) in the
  columns, individuals (function) in the rows and variable
  counts in the cells.
}
\description{
  Counts the number of input variables (formal arguments)
  present in the body of a individual function. Applied to
  a population of individuals, this information is useful
  to identify driving variables in a modelling task.
  \code{functionVariablePresenceMap} returns a (one row)
  variable presence map for a function,
  \code{populationVariablePresenceMap} returns a variable
  presence map for a population of RGP individuals (a list
  of R functions).
}

