\name{sType}
\alias{\%::\%}
\alias{calculateSTypeRecursive}
\alias{configureSTypeInference}
\alias{getSTypeFromFormalsStack}
\alias{hasStype}
\alias{sType}
\alias{sTypeq}
\alias{setSTypeOnFormalsStack}
\title{Inference of sTypes}
\usage{
sType(x, typeEnvir = rgpSTypeEnvironment, returnNullOnFailure = FALSE)

configureSTypeInference(constantSTypeFunction = NA)

calculateSTypeRecursive(x, typeEnvir = rgpSTypeEnvironment,
  formalsStack = list(), returnNullOnFailure = FALSE)

sTypeq(x, typeEnvir = rgpSTypeEnvironment, returnNullOnFailure = FALSE)

getSTypeFromFormalsStack(x, formalsStack)

setSTypeOnFormalsStack(x, value, formalsStack)

hasStype(x)

x \%::\% value
}
\arguments{
  \item{x}{The object to operate on.}

  \item{value}{An sType.}

  \item{typeEnvir}{The type environment, containing
  user-supplied sTypes of building blocks.}

  \item{formalsStack}{A stack of formal arguments with
  their sTypes.}

  \item{returnNullOnFailure}{Return NULL on failure instead
  of stopping, defaults to FALSE.}

  \item{constantSTypeFunction}{A function of one parameter
  to be used to calculate constant types.  If set to
  \code{NA} (the default), types of constants are named
  after the constant's R class.}
}
\description{
RGP internally infers the sTypes of compound expressions
like function applications and function definitions from
the sTypes of atomic expressions. The sTypes of building
blocks are defined by the user via the \code{\%::\%}
operator and are stored in the package-internal global
variable \code{rgpSTypeEnvironment}. \code{sType}
calculates the sType of the R expression \code{x}.
\code{sTypeq} quotes its argument \code{x} before calling
\code{sType}. SType inference of function definitions
relies on a typed stack of formal arguments of
\code{getSTypeFromFormalsStack} and
\code{setSTypeOnFormalsStack} get or set the sType of a
formal argument \code{x} and a \code{formalsStack},
respectively.
}
\details{
The function \code{configureSTypeInference} is used to
configure the type inference engine for special needs.
}
\seealso{
sTypeConstructors
}

