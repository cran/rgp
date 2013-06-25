\name{makeClosure}
\alias{makeClosure}
\title{Create a new R closure given a function body expression and an argument list}
\usage{
  makeClosure(fbody, fargs, envir = globalenv())
}
\arguments{
  \item{fbody}{The function body, given as a R expression.}

  \item{fargs}{The formal arguments, given as a list or
  vector of strings.}

  \item{envir}{The new function closure's environment,
  defaults to \code{globalenv()}.}
}
\value{
  A formal argument list, ready to be passed via
  \code{\link{formals}}.
}
\description{
  Creates a R closure (i.e. a function object) from a body
  expression and an argument list. The closure's
  environment will be the default environment.
}

