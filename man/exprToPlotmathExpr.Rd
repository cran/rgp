\name{exprToPlotmathExpr}
\alias{exprToPlotmathExpr}
\title{Convert any expression to an expression that is plottable by plotmath}
\usage{
  exprToPlotmathExpr(expr)
}
\arguments{
  \item{expr}{The GP-generated expression to convert.}
}
\value{
  An expression plottable by \code{\link{plotmath}}.
}
\description{
  Tries to convert a GP-generated expression \code{expr} to
  an expression plottable by \code{\link{plotmath}} by
  replacing GP variants of arithmetic operators by their
  standard counterparts.
}

