## stypes.r
##   - A type system for R functions and values to be used in symbolic regression
##
## RGP - a GP system for R
## 2010 Oliver Flasch (oliver.flasch@fh-koeln.de)
## with contributions of Thomas Bartz-Beielstein, Olaf Mersmann and Joerg Stork
## released under the GPL v2
##

##' Type constructors for types in the Rsymbolic type system
##'
##' These functions create types for the Rsymbolic type system, called \emph{sTypes}
##' from here on. These functions are used mostly in literal expressions denoting sTypes.
##' \code{st} creates a \emph{base sType} from a string. A base sType is a type without
##' any further structure. Example include \code{st("numeric")}, \code{st("character")}
##' or \code{st("logical")}.
##' \code{\%->\%} creates a \emph{function sType}, i.e. the type of function, from a
##' vector of argument sTypes and a result sType. A function sType has \code{domain}
##' and \code{range} containing its argument and result types.
##' Every sType has a \code{string} field containing a unambiguous string representation
##' that can serve as a hash table key.
##' STypes can be checked for equality via \code{\link{identical}}.
##'
##' @param baseTypeName The name of the base sType to create.
##' @return The created sType.
##'
##' @examples
##' st("numeric")
##' list(st("numeric"), st("numeric")) \%->\% st("logical")
##' 
##' @seealso sTypeTags
##' @rdname sTypeConstructors
##' @export
st <- function(baseTypeName) {
  sBaseType <- list(base = baseTypeName, string = baseTypeName)
  class(sBaseType) <- c("sBaseType", "sType", "character")
  sBaseType
}

##' @rdname sTypeConstructors
##' @export `%->%`
`%->%` <- function(domainTypes, rangeType) {
  domainTypeStrings <- Map(function(x) x$string, domainTypes)
  sFunctionTypeString <-
     paste("(", paste(domainTypeStrings, collapse=", "), ") -> ", rangeType$string, sep ="")
  sFunctionType <-
    list(domain = domainTypes,
         range = rangeType,
         string = sFunctionTypeString)
  class(sFunctionType) <- c("sFunctionType", "sType")
  sFunctionType
}

##' Prints a sType and returns it invisible.
##'
##' @param x The sType to print.
##' @param ... Optional parameters to print are ignored in this method.
##' @export
print.sType <- function(x, ...) {
  cat(x$string, "\n")
  invisible(x)
}

##' Tagging objects with sTypes
##'
##' Objects may be tagged with sTypes. Type tags are stored in the "sType" attribute.
##' \code{sType} returns the sType tag for an object. Assign to the result of this function
##' to set the sType tag for an object (e.g. \code{sType(foo) <- st("integer")}).
##' The \code{\%::\%} operator returns its first argument tagged with the sType given as its
##' second argument, without modifying the sType of its first argument.
##' \code{hasStype} returns true if \code{x} is tagged with an sType.
##' 
##' @param x The object to retreive, check, or set the sType tag for.
##' @param value An sType.
##'
##' @examples
##' foo <- "foo"
##' sType(foo) <- st("string")
##' sType(foo)
##' foo \%::\% st("string")
##' 
##' @seealso sTypeConstructors, attr
##' @rdname sTypeTags
##' @export
sType <- function(x) attr(x, "sType", exact = TRUE)

##' @rdname sTypeTags
##' @export `sType<-`
`sType<-` <- function(x, value) {
  attr(x, "sType") <- value
  x
}

##' @rdname sTypeTags
##' @export
hasStype <- function(x) !is.null(sType(x))

##' @rdname sTypeTags
##' @export `%::%`
`%::%` <- function(x, value) {
  attr(x, "sType") <- value
  x
}

##' Return the range type if t is a function type, otherwise just return t
##'
##' @param t The type to extract the range type from.
##' @return The range type.
##' @export
rangeTypeOfType <- function(t)
  if (inherits(t, "sFunctionType")) t$range else t
