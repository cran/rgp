\name{new.function}
\alias{new.function}
\title{Create a new function stub}
\usage{
new.function(envir = globalenv())
}
\arguments{
  \item{envir}{The new function closure's environment,
  defaults to \code{globalenv()}.}
}
\value{
A new function that does not take any arguments and always
returns \code{NULL}.
}
\description{
Creates and returns a new function stub without capturing
any environment variables.
}
\note{
Always use this function to dynamically generate new
functions that are not clojures to prevent hard to find
memory leaks.
}

