\name{sTypeTags}
\alias{sType}
\title{Tagging objects with sTypes...}
\usage{sType(x)
`sType<-`(x, value)
hasStype(x)
%::\%(x, value)
}
\description{Tagging objects with sTypes}
\details{\code{sType}: Objects may be tagged with sTypes. Type tags are stored in the "sType" attribute.
\code{sType} returns the sType tag for an object. Assign to the result of this function
to set the sType tag for an object (e.g. \code{sType(foo) <- st("integer")}).
The \code{\%::\%} operator returns its first argument tagged with the sType given as its
second argument, without modifying the sType of its first argument.
\code{hasStype} returns true if \code{x} is tagged with an sType.

}
\seealso{sTypeConstructors, attr}
\arguments{\item{x}{The object to retreive, check, or set the sType tag for.}
\item{value}{An sType.}
}
\examples{foo <- "foo"
sType(foo) <- st("string")
sType(foo)
foo \%::\% st("string")}
\alias{sType<-}
\alias{hasStype}
\alias{\%::\%}

