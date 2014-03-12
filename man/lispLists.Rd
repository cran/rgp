\name{first}
\alias{contains}
\alias{fifth}
\alias{first}
\alias{fourth}
\alias{is.atom}
\alias{is.composite}
\alias{is.empty}
\alias{rest}
\alias{second}
\alias{third}
\title{Functions for Lisp-like list processing}
\usage{
first(x)

rest(x)

second(x)

third(x)

fourth(x)

fifth(x)

is.empty(x)

is.atom(x)

is.composite(x)

contains(x, elt)
}
\arguments{
  \item{x}{A list or vector.}

  \item{elt}{An element of a list or vector.}
}
\description{
Simple wrapper functions that allow Lisp-like list
processing in R: \code{first} to \code{fifth} return the
first to fifth element of the list \code{x}. \code{rest}
returns all but the first element of the list \code{x}.
\code{is.empty} returns \code{TRUE} iff the list \code{x}
is of length 0. \code{is.atom} returns \code{TRUE} iff the
list \code{x} is of length 1. \code{is.composite} returns
\code{TRUE} iff the list \code{x} is of length > 1.
\code{contains} return \code{TRUE} iff the list \code{x}
contains an element identical to \code{elt}.
}

