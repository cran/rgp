## list_utils.r
##   - Utility functions for lists and vectors
##
## RGP - a GP system for R
## 2010 Oliver Flasch (oliver.flasch@fh-koeln.de)
## with contributions of Thomas Bartz-Beielstein, Olaf Mersmann and Joerg Stork
## released under the GPL v2
##

##' Functions for Lisp-like list processing
##'
##' Simple wrapper functions that allow Lisp-like list processing in
##' R: \code{first} to \code{fifth} return the first to fifth element
##' of the list \code{x}. \code{rest} returns all but the first
##' element of the list \code{x}. \code{is.empty} returns \code{TRUE}
##' iff the list \code{x} is of length 0. \code{is.atom} returns
##' \code{TRUE} iff the list \code{x} is of length 1.
##' \code{is.composite} returns \code{TRUE} iff the list \code{x} is
##' of length > 1.
##'
##' @param x A list or vector.
##'
##' @rdname lispLists
first <- function(x) x[[1]]

##' @rdname lispLists
rest <- function(x) x[-1]

##' @rdname lispLists
second <- function(x) x[[2]]

##' @rdname lispLists
third <- function(x) x[[3]]

##' @rdname lispLists
fourth <- function(x) x[[4]]

##' @rdname lispLists
fifth <- function(x) x[[5]]

##' @rdname lispLists
is.empty <- function(x) length(x) == 0

##' @rdname lispLists
is.atom <- function(x) length(x) == 1

##' @rdname lispLists
is.composite <- function(x) length(x) > 1

##' Sort a vector or list by the result of applying a function
##'
##' Sorts a vector or a list by the numerical result of applying the function \code{byFunc}.
##'
##' @param xs A vector or list.
##' @param byFunc A function from elements of \code{xs} to \code{numeric}.
##' @return The result of sorting \code{xs} by \code{byfunc}.
sortBy <- function(xs, byFunc) {
  if (missing(byFunc))
    o <- order(xs)
  else
    o <- order(sapply(xs, byFunc))
  xs[o]
}

##' Sort a vector or list via a given ranking
##'
##' Reorders a vector or list according to a given ranking \code{ranking}.
##'
##' @param xs The vector or list to reorder.
##' @param ranking The ranking to sort \code{xs} by, defaults to \code{rank(xs)}.
##' @return The result of reordering \code{xs} by \code{ranking}.
sortByRanking <- function(xs, ranking = rank(xs)) {
  sorted <- xs
  sorted[ranking] <- xs
  sorted
}

##' Calculate the inverse of a permutation
##'
##' Returns the inverse of a permutation \code{x} given as an integer vector.
##' This function is useful to turn a ranking into an ordering and back, for example.
##'
##' @param x The permutation to return the inverse for.
##' @return The inverse of the permutation \code{x}.
##' @seealso \code{\link{rank}}, \code{\link{order}}
inversePermutation <- function(x) {
  l <- length(x); o <- numeric(l)
  o[x] <- 1:l
  o
}

##' Choose a random element from a list or vector
##'	
##' Returns a unformly random chosen element of the vector or list \code{x}.
##'
##' @param x The vector or list to chose an element from.
##' @return A uniformly random element of \code{x}.
randelt <- function(x) {
  l <- length(x)
  if (l == 0)
    NULL
  else
    sample(x, 1)[[1]] # sample(x, 1) always yields a list of exactly one element
}
