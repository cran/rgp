\name{expressionNames}
\alias{toName}
\title{Functions for handling R symbols / names...}
\usage{toName(x, copyAttributes=TRUE)
extractLeafSymbols(expr)
}
\description{Functions for handling R symbols / names}
\details{\code{toName}: \code{toName} converts a character string \code{x} to an R symbol / name,
while copying all attributes iff \code{copyAttributes} is \code{TRUE}.
In the case that \code{x} is not a character string, a copy of the object
is returned as-is.
\code{extractLeafSymbols} returns the set of symbols (names) at the leafs
of an expression \code{expr}. The symbols are returned as character strings.

}
\value{\code{toName}: The result.

}
\arguments{\item{x}{The object to operate on.}
\item{expr}{An R expression.}
\item{copyAttributes}{Whether to copy all attributes of \code{x} to the
result object.}
}
\alias{extractLeafSymbols}

