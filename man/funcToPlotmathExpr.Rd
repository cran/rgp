\name{funcToPlotmathExpr}
\alias{funcToPlotmathExpr}
\title{Convert a function to an expression plottable by plotmath...}
\usage{funcToPlotmathExpr(func)
}
\description{Convert a function to an expression plottable by plotmath}
\details{Tries to convert a function \code{func} to an expression plottable by \code{\link{plotmath}}
by replacing arithmetic operators and "standard" functions by plottable counterparts.}
\value{An expression plottable by \code{\link{plotmath}}.}
\seealso{\code{\link{funcToIgraph}}}
\arguments{\item{func}{The function to convert.}
}

