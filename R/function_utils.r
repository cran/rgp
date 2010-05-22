## function_utils.r
##   - Utility functions for R functions
##
## RGP - a GP system for R
## 2010 Oliver Flasch (oliver.flasch@fh-koeln.de)
## with contributions of Thomas Bartz-Beielstein, Olaf Mersmann and Joerg Stork
## released under the GPL v2
##

##' Create a new function stub
##'
##' Creates and returns a new function stub without capturing any environment variables.
##'
##' @note Always use this function to dynamically generate new functions that are not clojures
##' to prevent hard to find memory leaks.
##' @return A new function that does not take any arguments and always returns \code{NULL}.
new.function <- function() {
  # The following line has to be inside this function to prevent capturing the lexical
  # environment, which would cause a hard to find memory leak:
  fun <- function() NULL
  # If you use the last line outside of this function, by shure to initialize the
  # environment of the generated function object to the global environment, like so:
  # environment(fun) <- globalenv() 
  fun  
}

##' Create a new function argument list from a list or vector of strings
##'
##' Creates a formal argument list from a list or vector of strings, ready to be assigned via
##' \code{\link{formals}}.
##'
##' @param fargs The formal arguments, given as a list or vector of strings.
##' @return A formal argument list, ready to be passed via \code{\link{formals}}.
new.alist <- function(fargs) {
  alistargs <- Reduce(function(a,b) paste(a,b,sep="=,") , fargs, "", right = TRUE)
  alistargslen <- nchar(alistargs)
  if (alistargslen != 0)
    alistargs <- substr(alistargs, 1, alistargslen-1)
  eval(parse(text = paste("alist(", alistargs, ")", sep="")), baseenv())
}

##' Determine the number of arguments of a function
##'
##' Tries to determine the number of arguments of function.
##'
##' @param f The function to determine the arity for.
##' @return The arity of the function \code{f}.
arity <- function(f) {
  if (is.primitive(f))
    arity.primitive(f)
  else if (is.function(f))
    length(formals(f))
  else if (is.name(f))
    arity(eval(f))
  else if (is.character(f))
    arity(as.name(f))
  else
    stop("could not determine arity of object \"", f, "\" (of class ", class(f), ")")
}

##' Determine the number of arguments of a primitive function
##'
##' Tries to determine the number of arguments of a primitive R function by lookup in a
##' builtin table.
##'
##' @param f The primitive to determine the arity for.
##' @return The arity of the primitive \code{f}.
arity.primitive <- function(f) {
  if (identical(f, `+`) ||
      identical(f, `*`) ||
      identical(f, `-`) ||
      identical(f, `/`) ||
      identical(f, `^`) ||
      identical(f, `%%`) ||
      identical(f, `%/%`) ||
      identical(f, `<`) ||
      identical(f, `<=`) ||
      identical(f, `==`) ||
      identical(f, `!=`) ||
      identical(f, `>=`) ||
      identical(f, `>`) ||
      identical(f, `&`) ||
      identical(f, `|`) ||
      identical(f, `&&`) ||
      identical(f, `||`) ||
      identical(f, xor))
    2
  else if (identical(f, log) ||
           identical(f, logb) ||
           identical(f, atan2))
    2
  else if (identical(f, log10) ||
           identical(f, log2) ||
           identical(f, log1p) ||
           identical(f, exp) ||
           identical(f, expm1) ||
           identical(f, sin) ||
           identical(f, cos) ||
           identical(f, tan) ||
           identical(f, asin) ||
           identical(f, acos) ||
           identical(f, atan) ||
           identical(f, abs) ||
           identical(f, sqrt) ||
           identical(f, `!`))
    1
  else
    stop("could not determine arity of primitive")
}
