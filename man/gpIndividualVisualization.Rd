\name{gpIndividualVisualization}
\alias{funcToIgraph}
\title{Visualization of functions and expressions as trees...}
\usage{funcToIgraph(func)
exprToIgraph(expr)
exprToGraph(expr)
}
\description{Visualization of functions and expressions as trees}
\details{\code{funcToIgraph}: The following functions plot R expressions and functions as trees. The igraph package
is required for most of these functions.
\code{exprToGraph} transforms an R expression into a graph given as a character vector
of vertices V and a even-sized numeric vector of edges E. Two elements i and i+1 in E
encode a directed edge from V[i] to V[i+1]. 
\code{funcToIgraph} and \code{exprToIgraph} return an igraph graph object for an R
function or an R expression.

}
\value{\code{funcToIgraph}: The result (see the details section).

}
\seealso{\code{\link{funcToPlotmathExpr}}}
\arguments{\item{func}{An R function.}
\item{expr}{An R expression.}
}
\alias{exprToIgraph}
\alias{exprToGraph}

