\name{iterate}
\alias{iterate}
\title{Repeatedly apply a function...}
\usage{iterate(n, f, arg, ...)
}
\description{Repeatedly apply a function}
\details{Repeatedly apply a function \code{f} to an argument \code{arg}, additional arguments \code{...}
are supplied unchanged in each call. E.g. \code{iterate(3, foo, 42.14, "bar")} is equivalent to
\code{foo(foo(foo(42.14, "bar"), "bar"), "bar")}.}
\value{The result of repeatedly applying \code{f}.}
\arguments{\item{n}{The number of times to apply \code{f}, must be >= 0. If 0, \code{arg} is returned.}
\item{f}{The function to apply.}
\item{arg}{The argument to repeatedly apply \code{f} to.}
\item{...}{Additional argument to pass to \code{f} at each application.}
}

