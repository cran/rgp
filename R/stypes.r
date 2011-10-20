## stypes.r
##   - A type system for R functions and values to be used in symbolic regression
##
## RGP - a GP system for R
## 2010 Oliver Flasch (oliver.flasch@fh-koeln.de)
## with contributions of Thomas Bartz-Beielstein, Olaf Mersmann and Joerg Stork
## released under the GPL v2
##

## The global type environment for RGP
rgpSTypeEnvironment <- new.env(parent = emptyenv())

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
##' \code{sObject} is the root of the sType hierarchy, i.e. the most general type.
##'
##' @param baseTypeName The name of the base sType to create.
##' @return The created sType.
##'
##' @examples
##' st("numeric")
##' list(st("numeric"), st("numeric")) \%->\% st("logical")
##' is.sType(st("logical"))
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

##' @rdname sTypeConstructors
##' @export
sObject <- st("sObject")

##' Check if an object is an sType
##'
##' Returns \code{TRUE} iff its argument is an sType.
##'
##' @param x The object to check.
##' @return \code{TRUE} iff \code{x} is an sType.
##' @export
is.sType <- function(x) inherits(x, "sType")

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

# TODO remove old sType function and rename this one to "sType"
calculateSType <- function(x, typeEnvir = rgpSTypeEnvironment)
  calculateSTypeRecursive(x, typeEnvir = typeEnvir)$type

calculateSTypeq <- function(x, typeEnvir = rgpSTypeEnvironment)
  calculateSType(substitute(x), typeEnvir = typeEnvir)

##' Inference of sTypes
##'
##' RGP internally infers the sTypes of compound expressions like function applications
##' and function definitions from the sTypes of atomic expressions. The sTypes of building
##' blocks are defined by the user via the \code{\%::\%} operator and are stored in the
##' package-internal global variable \code{rgpSTypeEnvironment}.
##' \code{calculateSTypeRecursive} calculates the sType of the R expression \code{x}.
##' SType inference of function definitions relies on a typed stack of formal arguments of
##' the function definition to infer the sType for.
##' \code{getSTypeFromFormalsStack} and \code{setSTypeOnFormalsStack} get or set the sType
##' of a formal argument \code{x} and a \code{formalsStack}, respectively.
##'
##' @param x The object to operate on.
##' @param value An sType. 
##' @param typeEnvir The type environment, containing user-supplied sTypes of building blocks.
##' @param formalsStack A stack of formal arguments with their sTypes.
##'
##' @seealso sTypeConstructors, sTypeTags
##' @rdname sTypeInference
calculateSTypeRecursive <- function(x, typeEnvir = rgpSTypeEnvironment, formalsStack = list()) {
  if (!is.language(x)) {
    ## a constant: get type from the constant' R class
    list(type = st(as.character(class(x))),
         formalsStack = formalsStack)
  } else if (is.symbol(x)) {
    ## a symbol: get type from formals Stack or from type environment
    sTypeFromFormalsStack <- getSTypeFromFormalsStack(as.character(x), formalsStack)
    if (!is.null(sTypeFromFormalsStack)) {
      list(type = sTypeFromFormalsStack,
           formalsStack = formalsStack)
    } else if (exists(as.character(x), envir = rgpSTypeEnvironment)) {
      list(type = get(as.character(x), envir = rgpSTypeEnvironment),
           formalsStack = formalsStack)
    } else stop("sType: Could not determine sType of object ", x, ".")
  } else if (is.call(x) && identical(x[[1]], as.symbol("function"))) {
    ## a function definition: calculate function type
    formalNames <- names(x[[2]])
    formalsFrame <- Map(function(formal) list(formal, sObject), formalNames)
    typeAndStackOfBody <- calculateSTypeRecursive(x[[3]], typeEnvir = rgpSTypeEnvironment,
                                                  formalsStack = c(list(formalsFrame), formalsStack))
    bodyFormalsStack <- typeAndStackOfBody$formalsStack
    domainTypes <- Map(function(formalName) {
                         getSTypeFromFormalsStack(as.character(formalName), bodyFormalsStack)
                       }, formalNames)
    rangeType <- typeAndStackOfBody$type
    functionType <- domainTypes %->% rangeType
    list(type = functionType,
         formalsStack = bodyFormalsStack[-1])
  } else if (is.call(x) && exists(as.character(x[[1]]), envir = rgpSTypeEnvironment)) {
    ## a function application: get range (result) type from type environment
    if (identical(formalsStack, list())) {
      ## use shortcut if no formal variable types have to be inferred
      list(type = calculateSTypeRecursive(x[[1]], typeEnvir = rgpSTypeEnvironment,
                                          formalsStack = formalsStack)$type$range,
           formalsStack = formalsStack)
    } else {
      functionType <- calculateSTypeRecursive(x[[1]], typeEnvir = rgpSTypeEnvironment,
                                              formalsStack = formalsStack)$type
      updatedFormalsStack <- formalsStack
      for (i in 1:length(functionType$domain)) {
        actualArgument <- x[[i + 1]]
        expectedArgumentType <- functionType$domain[[i]]
        actualArgumentTypeAndStack <- calculateSTypeRecursive(actualArgument, typeEnvir = rgpSTypeEnvironment,
                                                              formalsStack = updatedFormalsStack)
        actualArgumentType <- actualArgumentTypeAndStack$type
        updatedFormalsStack <- actualArgumentTypeAndStack$formalsStack
        if (!is.null(getSTypeFromFormalsStack(as.character(actualArgument), updatedFormalsStack))) {
          ## the current argument is a formal argument, update its type on the formal argument stack
          updatedFormalsStack <- setSTypeOnFormalsStack(as.character(actualArgument),
                                                        expectedArgumentType, updatedFormalsStack)
        }
      }
      list(type = functionType$range,
           formalsStack = updatedFormalsStack)
    }
  } else stop("sType: Could not determine sType of object ", x, ".")
}

##' @rdname sTypeInference
getSTypeFromFormalsStack <- function(x, formalsStack) {
  for (frame in formalsStack) {
    for (pair in frame) {
      if (identical(x, pair[[1]])) return(pair[[2]])
    }
  }
  NULL
}

##' @rdname sTypeInference
setSTypeOnFormalsStack <- function(x, value, formalsStack) {
  for (frameNum in 1:length(formalsStack)) {
    for (pairNum in 1:length(formalsStack[[frameNum]])) {
      if (identical(x, formalsStack[[frameNum]][[pairNum]][[1]])) {
        if (identical(formalsStack[[frameNum]][[pairNum]][[2]], sObject)
            || identical(formalsStack[[frameNum]][[pairNum]][[2]], value)) {
          formalsStack[[frameNum]][[pairNum]][[2]] <- value
          return(formalsStack)
        } else stop("sType: Type error at formal argument ", x, ", expected type ",
                    value$string, " but got argument of type ",
                    formalsStack[[frameNum]][[pairNum]][[2]]$string, ".")
      }
    }
  }
  formalsStack
}

## TODO remove this
##' @rdname sTypeTags
##' @export `sType<-`
`sType<-` <- function(x, value) {
  attr(x, "sType") <- value
  x
}

## TODO remove this
##' @rdname sTypeTags
##' @export
hasStype <- function(x) !is.null(sType(x))

##' @rdname sTypeTags
##' @export `%::%`
`%::%` <- function(x, value) {
  if (!is.sType(value)) stop("%::%: ", value, " is not an sType.")
  #xString <- as.character(x)
  #if (exists(xString, envir = rgpSTypeEnvironment) &&
  #    !identical(get(xString, envir = rgpSTypeEnvironment), value)) {
  #  stop("%::%: The symbol ", xString, " already has an sType of ",
  #       get(xString, envir = rgpSTypeEnvironment)$string, ".")
  #}
  #assign(xString, value, envir = rgpSTypeEnvironment)
  ## TODO remove the sType attribute entirely ...
  attr(x, "sType") <- value # TODO
  ## ... .
  x
}

##' Return the range type if t is a function type, otherwise just return t
##'
##' @param t The type to extract the range type from.
##' @return The range type.
##' @export
rangeTypeOfType <- function(t)
  if (inherits(t, "sFunctionType")) t$range else t

##' Return expr with the attributes of sourceExpr
##'
##' @param expr The expression to receive the attributes of \code{sourceExpr}.
##' @param sourceExpr The expression to provide the attributes for \code{expr}.
##' @return A copy of \code{expr}, tagged with the attributes and sType of
##'   \code{sourceExpr}.
withAttributesOf <- function(expr, sourceExpr) {
  mostattributes(expr) <- attributes(sourceExpr)
  if (hasStype(sourceExpr)) expr %::% sType(sourceExpr) else expr
}
