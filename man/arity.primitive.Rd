\name{arity.primitive}
\alias{arity.primitive}
\title{Determine the number of arguments of a primitive function}
\usage{
  arity.primitive(f)
}
\arguments{
  \item{f}{The primitive to determine the arity for.}
}
\value{
  The arity of the primitive \code{f}.
}
\description{
  Tries to determine the number of arguments of a primitive
  R function by lookup in a builtin table.
}

