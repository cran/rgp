\name{funcToPlotmathExpr}
\alias{funcToPlotmathExpr}
\title{Convert a function to an expression plottable by plotmath}
\usage{
funcToPlotmathExpr(func)
}
\arguments{
  \item{func}{The function to convert.}
}
\value{
An expression plottable by \code{\link{plotmath}}.
}
\description{
Tries to convert a function \code{func} to an expression
plottable by \code{\link{plotmath}} by replacing arithmetic
operators and "standard" functions by plottable
counterparts.
}
\seealso{
\code{\link{funcToIgraph}}
}

