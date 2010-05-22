\name{searchSpaceDefinition}
\alias{functionSet}
\title{Functions for defining the search space for symbolic regression...}
\usage{functionSet(..., list)
inputVariableSet(..., list)
constantFactorySet(..., list)
c.functionSet(..., recursive=FALSE)
c.inputVariableSet(..., recursive=FALSE)
c.constantFactorySet(..., recursive=FALSE)
}
\description{Functions for defining the search space for symbolic regression}
\details{\code{functionSet}: The GP search space is defined by a set of functions, a set of
input variables, a set of constant constructor functions, and some
rules how these functions, input variables, and constants may be
combined to form valid symbolic expressions.  The function set is
simply given as a set of strings naming functions in the global
environment. Input variables are also given as strings.
Combination rules are implemented by a type system and defined by
assigning sTypes to functions, input variables, and constant
constructors.

Function sets and input variable sets are S3 classes containing
the following fields: \code{\$all} contains a list of all
functions, or input variables, or constant factories.
\code{\$byRange} contains a table of all input variables, or
functions, or constant factories, indexed by the string label of
their sTypes for input variables, or by the string label of their
range sTypes for functions, or by the string label of their range
sTypes for constant factories. This field exists mainly for
quickly finding a function, input variable, or constant factory
that matches a given type.

Multiple function sets, or multiple input variable sets, or
multiple constant factory sets can be combined using the
\code{\link{c}} function. \code{functionSet} creates a function
set. \code{inputVariableSet} creates an input variable set.
\code{constantFactorySet} creates a constant factory set.

}
\value{\code{functionSet}: A function set or input variable set.

}
\arguments{\item{...}{Names of functions or input variables given as strings.}
\item{list}{Names of functions or input variables given as a list of strings.}
\item{recursive}{Ignored when concatenating function- or input variable sets.}
}
\examples{# creating an untyped search space description...
functionSet("+", "-", "*", "/", "expt", "log", "sin", "cos", "tan")
inputVariableSet("x", "y")
constantFactorySet(function() runif(1, -1, 1))}
\alias{inputVariableSet}
\alias{constantFactorySet}
\alias{c.functionSet}
\alias{c.inputVariableSet}
\alias{c.constantFactorySet}

