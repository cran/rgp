## search_space.r
##   - Functions for defining the search space for Genetic Programming
##
## RGP - a GP system for R
## 2010 Oliver Flasch (oliver.flasch@fh-koeln.de)
## with contributions of Thomas Bartz-Beielstein, Olaf Mersmann and Joerg Stork
## released under the GPL v2
##

##' @include stypes.r
NA

##' Functions for defining the search space for Genetic Programming
##'
##' The GP search space is defined by a set of functions, a set of
##' input variables, a set of constant constructor functions, and some
##' rules how these functions, input variables, and constants may be
##' combined to form valid symbolic expressions.  The function set is
##' simply given as a set of strings naming functions in the global
##' environment. Input variables are also given as strings.
##' Combination rules are implemented by a type system and defined by
##' assigning sTypes to functions, input variables, and constant
##' constructors.
##'
##' Function sets and input variable sets are S3 classes containing
##' the following fields: \code{\$all} contains a list of all
##' functions, or input variables, or constant factories.
##' \code{\$byRange} contains a table of all input variables, or
##' functions, or constant factories, indexed by the string label of
##' their sTypes for input variables, or by the string label of their
##' range sTypes for functions, or by the string label of their range
##' sTypes for constant factories. This field exists mainly for
##' quickly finding a function, input variable, or constant factory
##' that matches a given type.
##' 
##' Multiple function sets, or multiple input variable sets, or
##' multiple constant factory sets can be combined using the
##' \code{\link{c}} function. \code{functionSet} creates a function
##' set. \code{inputVariableSet} creates an input variable set.
##' \code{constantFactorySet} creates a constant factory set.
##'
##' @param ... Names of functions or input variables given as strings.
##' @param list Names of functions or input variables given as a list of strings.
##' @param recursive Ignored when concatenating function- or input variable sets.
##' @return A function set or input variable set.
##'
##' @examples
##' # creating an untyped search space description...
##' functionSet("+", "-", "*", "/", "expt", "log", "sin", "cos", "tan")
##' inputVariableSet("x", "y")
##' constantFactorySet(function() runif(1, -1, 1))
##' 
##' @rdname searchSpaceDefinition
##' @export
functionSet <- function(..., list = NULL) {
  ll <- if (missing(list)) list(...) else c(list, ...)
  funcset <- list()
  class(funcset) <- c("functionSet", "list")
  funcset$all <- lapply(ll, function(o) as.name(o) %::% sType(o))  # convert to names
  funcset$byType <- sortByType(funcset$all)
  funcset$byRange <- sortByRange(funcset$all)
  funcset
}

##' @rdname searchSpaceDefinition
##' @export
inputVariableSet <- function(..., list = NULL) {
  ll <- if (missing(list)) list(...) else c(list, ...)
  inset <- list()
  class(inset) <- c("inputVariableSet", "list")
  inset$all <- lapply(ll, function(o) as.name(o) %::% sType(o))  # convert to names
  inset$byType <- sortByType(inset$all)
  inset$byRange <- sortByRange(inset$all) # TODO remove this field
  inset
}

##' @rdname searchSpaceDefinition
##' @export
constantFactorySet <- function(..., list = NULL) {
  ll <- if (missing(list)) list(...) else c(list, ...)
  constfacset <- list()
  class(constfacset) <- c("constantFactorySet", "list")
  constfacset$all <- ll
  constfacset$byType <- sortByType(constfacset$all)
  constfacset$byRange <- sortByRange(constfacset$all) # TODO remove this field
  constfacset
}

##' @rdname searchSpaceDefinition
##' @export
c.functionSet <- function(..., recursive = FALSE) {
  fSets <- list(...)
  combinedFsets <- list()
  for (fSet in fSets) combinedFsets <- append(fSet$all, combinedFsets)
  functionSet(list = combinedFsets)
}

##' @rdname searchSpaceDefinition
##' @export
c.inputVariableSet <- function(..., recursive = FALSE) {
  iSets <- list(...)
  combinedIsets <- list()
  for (iSet in iSets) combinedIsets <- append(iSet$all, combinedIsets)
  inputVariableSet(list = combinedIsets)
}

##' @rdname searchSpaceDefinition
##' @export
c.constantFactorySet <- function(..., recursive = FALSE) {
  cSets <- list(...)
  combinedCsets <- list()
  for (cSet in cSets) combinedCsets <- append(cSet$all, combinedCsets)
  constantFactorySet(list = combinedCsets)
}

##' Tabulate a list of functions or input variables by their sTypes
##'
##' @param x A list of functions or input variables to sort by sType.
##' @return A table of the objects keyed by their sTypes.
sortByType <- function(x) {
  byTypeTable <- list()
  for (o in x) {
    if (hasStype(o)) {
      oStype <- sType(o)
      if (is.null(byTypeTable[[oStype$string]])) byTypeTable[[oStype$string]] <- list()
      byTypeTable[[oStype$string]] <- append(byTypeTable[[oStype$string]], list(o))
    }
  }
  byTypeTable
}

##' Tabulate a list of functions or input variables by the range part of their sTypes
##'
##' @param x A list of functions or input variables to sort by range sType.
##' @return A table of the objects keyed by their range sTypes.
sortByRange <- function(x) {
  byRangeTable <- list()
  for (o in x) {
    if (hasStype(o)) {
      oStype <- sType(o)
      oStypeRange <- if (inherits(oStype, "sFunctionType")) oStype$range else oStype
      if (is.null(byRangeTable[[oStypeRange$string]])) byRangeTable[[oStypeRange$string]] <- list()
      byRangeTable[[oStypeRange$string]] <- append(byRangeTable[[oStypeRange$string]], list(o))
    }
  }
  byRangeTable
}
