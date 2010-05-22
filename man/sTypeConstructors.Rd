\name{sTypeConstructors}
\alias{st}
\title{Type constructors for types in the Rsymbolic type system...}
\usage{st(baseTypeName)
%->\%(domainTypes, rangeType)
}
\description{Type constructors for types in the Rsymbolic type system}
\details{\code{st}: These functions create types for the Rsymbolic type system, called \emph{sTypes}
from here on. These functions are used mostly in literal expressions denoting sTypes.
\code{st} creates a \emph{base sType} from a string. A base sType is a type without
any further structure. Example include \code{st("numeric")}, \code{st("character")}
or \code{st("logical")}.
\code{\%->\%} creates a \emph{function sType}, i.e. the type of function, from a
vector of argument sTypes and a result sType. A function sType has \code{domain}
and \code{range} containing its argument and result types.
Every sType has a \code{string} field containing a unambiguous string representation
that can serve as a hash table key.
STypes can be checked for equality via \code{\link{identical}}.

}
\value{\code{st}: The created sType.

}
\seealso{sTypeTags}
\arguments{\item{baseTypeName}{The name of the base sType to create.}
}
\examples{st("numeric")
list(st("numeric"), st("numeric")) \%->\% st("logical")}
\alias{\%->\%}

