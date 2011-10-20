\name{do.call.ignore.unused.arguments}
\alias{do.call.ignore.unused.arguments}
\title{A variant of do...}
\usage{do.call.ignore.unused.arguments(what, args, quote=FALSE, envir=parent.frame())
}
\description{A variant of do.call that ignores unused arguments}
\value{The result of the call.}
\arguments{\item{what}{What to call (either a function or a character vector naming a function in
\code{envir}.}
\item{args}{The args for the call, these may include arguments not used by \code{what}.}
\item{quote}{Whether to quote the arguments.}
\item{envir}{The environment within which to evaluate the call.}
}

