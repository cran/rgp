\name{functionSet}
\alias{c.constantFactorySet}
\alias{c.functionSet}
\alias{c.inputVariableSet}
\alias{constantFactorySet}
\alias{functionSet}
\alias{getPw}
\alias{hasPw}
\alias{inputVariableSet}
\alias{pw}
\title{Functions for defining the search space for Genetic Programming}
\usage{
  functionSet(..., list = NULL, parentEnvironmentLevel = 1)

  inputVariableSet(..., list = NULL)

  constantFactorySet(..., list = NULL)

  pw(x, pw)

  hasPw(x)

  getPw(x, default = 1)

  \method{c}{functionSet} (..., recursive = FALSE)

  \method{c}{inputVariableSet} (..., recursive = FALSE)

  \method{c}{constantFactorySet} (..., recursive = FALSE)
}
\arguments{
  \item{...}{Names of functions or input variables given as
  strings.}

  \item{list}{Names of functions or input variables given
  as a list of strings.}

  \item{parentEnvironmentLevel}{Level of the parent
  environment used to resolve function names.}

  \item{recursive}{Ignored when concatenating function- or
  input variable sets.}

  \item{x}{An object (function name, input variable name,
  or constant factory) to tag with a probability
  \code{pw}.}

  \item{pw}{A probability weight.}

  \item{default}{A default probability weight to return iff
  no probability weight is associated with an object.}
}
\value{
  A function set or input variable set.
}
\description{
  The GP search space is defined by a set of functions, a
  set of input variables, a set of constant constructor
  functions, and some rules how these functions, input
  variables, and constants may be combined to form valid
  symbolic expressions.  The function set is simply given
  as a set of strings naming functions in the global
  environment. Input variables are also given as strings.
  Combination rules are implemented by a type system and
  defined by assigning sTypes to functions, input
  variables, and constant constructors.
}
\details{
  Function sets and input variable sets are S3 classes
  containing the following fields: \code{\$all} contains a
  list of all functions, or input variables, or constant
  factories. \code{\$byRange} contains a table of all input
  variables, or functions, or constant factories, indexed
  by the string label of their sTypes for input variables,
  or by the string label of their range sTypes for
  functions, or by the string label of their range sTypes
  for constant factories. This field exists mainly for
  quickly finding a function, input variable, or constant
  factory that matches a given type.

  Multiple function sets, or multiple input variable sets,
  or multiple constant factory sets can be combined using
  the \code{\link{c}} function. \code{functionSet} creates
  a function set. \code{inputVariableSet} creates an input
  variable set. \code{constantFactorySet} creates a
  constant factory set.

  Probability weight for functions, input variables, and
  constants can be given by tagging constant names, input
  variables, and constant factory functions via the
  \code{pw} function (see the examples). The predicate
  \code{hasPw} can be used to check if an object \code{x}
  has an associated probability weight. The function
  \code{getPw} returns the probability weight associated
  with an object \code{x}, if available.
}
\examples{
# creating an untyped search space description...
functionSet("+", "-", "*", "/", "exp", "log", "sin", "cos", "tan")
inputVariableSet("x", "y")
constantFactorySet(function() runif(1, -1, 1))
# creating an untyped function set with probability weights...
functionSet(pw("+", 1.2), pw("-", 0.8), pw("*", 1.0), pw("/", 1.0))
}

