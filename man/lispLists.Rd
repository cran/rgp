\name{lispLists}
\alias{first}
\title{Functions for Lisp-like list processing...}
\usage{first(x)
rest(x)
second(x)
third(x)
fourth(x)
fifth(x)
is.empty(x)
is.atom(x)
is.composite(x)
}
\description{Functions for Lisp-like list processing}
\details{\code{first}: Simple wrapper functions that allow Lisp-like list processing in
R: \code{first} to \code{fifth} return the first to fifth element
of the list \code{x}. \code{rest} returns all but the first
element of the list \code{x}. \code{is.empty} returns \code{TRUE}
iff the list \code{x} is of length 0. \code{is.atom} returns
\code{TRUE} iff the list \code{x} is of length 1.
\code{is.composite} returns \code{TRUE} iff the list \code{x} is
of length > 1.

}
\arguments{\item{x}{A list or vector.}
}
\alias{rest}
\alias{second}
\alias{third}
\alias{fourth}
\alias{fifth}
\alias{is.empty}
\alias{is.atom}
\alias{is.composite}

