\name{randomCall}
\alias{randomCall}
\title{Creates a random R call object of a given type...}
\usage{randomCall(type, funcset, conset, inset=inputVariableSet(), maxdepth=8,
    constprob=0.2, subtreeprob=0.5, curdepth=1, formalidx=1)
}
\description{Creates a random R call object of a given type}
\details{Creates a R call object of a given type by randomly growing an expression tree. The call
will denote a function, if the given type is a function type, or a value, if the given
type is a value type. In each step of growth, with probability \code{subtreeprob}, an
operator is chosen from the function set \code{funcset}. The operands are then generated
by recursive calls. If no subtree is generated, a constant will be generated with probability
\code{constprob}. If no constant is generated, an input variable will be chosen randomly.
The depth of the resulting expression trees can be bounded by the \code{maxdepth} parameter.
This function respects sType tags of functions, input variables, and constant factories.
Only well-typed expressions are created, all nodes in the created expression tree will be
tagged with their sTypes.}
\value{A randomly generated well-typed R call object.}
\arguments{\item{type}{The type of the R call object to create randomly.}
\item{funcset}{The function set.}
\item{conset}{The set of constant factories.}
\item{inset}{The set of input variables, may be empty.}
\item{maxdepth}{The maximum expression tree depth.}
\item{constprob}{The probability of generating a constant in a step of growth, if no subtree
is generated. If neither a subtree nor a constant is generated, a randomly chosen input variable
will be generated. Defaults to \code{0.2}.}
\item{subtreeprob}{The probability of generating a subtree in a step of growth.}
\item{curdepth}{(internal) The depth of the random expression currently generated, used internally
in recursive calls.}
\item{formalidx}{(internal) The current start index for freshly generated formal parameters, used
internally in recursive calls.}
}

