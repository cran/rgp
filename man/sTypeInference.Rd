\name{sTypeInference}
\alias{calculateSTypeRecursive}
\title{Inference of sTypes...}
\usage{calculateSTypeRecursive(x, typeEnvir=rgpSTypeEnvironment, formalsStack=list())
getSTypeFromFormalsStack(x, formalsStack)
setSTypeOnFormalsStack(x, value, formalsStack)
}
\description{Inference of sTypes}
\details{\code{calculateSTypeRecursive}: RGP internally infers the sTypes of compound expressions like function applications
and function definitions from the sTypes of atomic expressions. The sTypes of building
blocks are defined by the user via the \code{\%::\%} operator and are stored in the
package-internal global variable \code{rgpSTypeEnvironment}.
\code{calculateSTypeRecursive} calculates the sType of the R expression \code{x}.
SType inference of function definitions relies on a typed stack of formal arguments of
the function definition to infer the sType for.
\code{getSTypeFromFormalsStack} and \code{setSTypeOnFormalsStack} get or set the sType
of a formal argument \code{x} and a \code{formalsStack}, respectively.

}
\seealso{sTypeConstructors, sTypeTags}
\arguments{\item{x}{The object to operate on.}
\item{value}{An sType.}
\item{typeEnvir}{The type environment, containing user-supplied sTypes of building blocks.}
\item{formalsStack}{A stack of formal arguments with their sTypes.}
}
\alias{getSTypeFromFormalsStack}
\alias{setSTypeOnFormalsStack}

