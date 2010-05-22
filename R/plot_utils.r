## plot_utils.r
##   - Simple plot utilities
##
## RGP - a GP system for R
## 2010 Oliver Flasch (oliver.flasch@fh-koeln.de)
## with contributions of Thomas Bartz-Beielstein, Olaf Mersmann and Joerg Stork
## released under the GPL v2
##

##' Show an overlayed plot of multiple functions
##'
##' Creates and shows and overlayed plot of one or more functions of one variable \eqn{y = f(x)}.
##'
##' @param funcs A list of functions of one variable to plot.
##' @param from The left bound of the plot, i.e. the minimum \eqn{x} value to plot.
##' @param to The right bound of the plot, i.e. the maximum \eqn{x} value to plot.
##' @param steps The number of steps, or samples, to plot.
##' @param type The plot type (e.g. l = line) as passed on to \code{\link{matplot}}.
##' @param lty The line types as passed on to \code{\link{matplot}}.
##' @param lwd The line widths as passed on to \code{\link{matplot}}.
##' @param lend The line end cap types as passed on to \code{\link{matplot}}.
##' @param pch The plot chars as passed on to \code{\link{matplot}}.
##' @param col The plot colors as passed on to \code{\link{matplot}}.
##' @param cex The character expansion sizes as passed on to \code{\link{matplot}}.
##' @param bg The background (fill) colors as passed on to \code{\link{matplot}}.
##' @param xlab The x axis label as passed on to \code{\link{matplot}}.
##' @param ylab The y axis label as passed on to \code{\link{matplot}}.
##' @param legendpos The position of the legend, passed as the \code{x} parameter to
##'   \code{\link{legend}}. 
##' @param bty The box type parameter of the legend, passed as the \code{bty} parameter to
##'   \code{\link{legend}}.
##' @param ... Grapical parameters for \code{\link{par}} and further arguments to \code{plot}.
##'   For example, use the \code{main} parameter to set a title.
##'
##' @examples
##' plotFunctions(list(function(x) sin(x), function(x) cos(x), function(x) 0.5*sin(2*x)+1), -pi, pi, 256)
##' @export
plotFunctions <- function(funcs, from = 0, to = 1, steps = 1024,
                          type = "l", lty = 1:5, lwd = 1, lend = par("lend"),
                          pch = NULL,
                          col = 1:6, cex = NULL, bg = NA,
                          xlab = "x", ylab = "y",
                          legendpos = "bottomright", bty = "n", ...) {
  xs <- seq(from, to, length = steps)
  yslists <- Map(function(f) as.vector(Map(f, xs), mode = "numeric"), funcs)
  ys <- c(); for (ylist in yslists) ys <- c(ys, ylist)
  ysmatrix <- matrix(ys, ncol = length(funcs))

  legendBuilder <- function(f) {
    fformals <- names(formals(f))
    fbody <- exprToPlotmathExpr(body(f))
    as.expression(bquote(f(.(fformals[[1]])) == .(fbody)))
  }
  legendContent <- sapply(funcs, legendBuilder)

  matplot(xs, ysmatrix, type = type, xlab = xlab, ylab = ylab,
          lty = lty, lwd = lwd, lend = lend, pch = pch,
          col = col, cex = cex, bg = bg, ...)
  legend(legendpos, legendContent, bty = bty,
         col = col, bg = bg, lty = lty, lwd = lwd, pch = pch)
  invisible()
}

##' Convert a function to an expression plottable by plotmath
##'
##' Tries to convert a function \code{func} to an expression plottable by \code{\link{plotmath}}
##' by replacing arithmetic operators and "standard" functions by plottable counterparts.
##'
##' @param func The function to convert.
##' @return An expression plottable by \code{\link{plotmath}}.
##' @export
funcToPlotmathExpr <- function(func)
  bquote(f(.(names(formals(func)))) == .(exprToPlotmathExpr(body(func))))

##' Convert any expression to an expression that is plottable by plotmath
##'
##' Tries to convert a GP-generated expression \code{expr} to an expression plottable by
##' \code{\link{plotmath}} by replacing GP variants of arithmetic operators by their standard
##' counterparts.
##'
##' @param expr The GP-generated expression to convert.
##' @return An expression plottable by \code{\link{plotmath}}.
##' @export
exprToPlotmathExpr <- function(expr)
  if (is.call(expr)) {
    expl <- as.list(expr)
    func <- expl[1]
    if (func == as.name("`/`") || func == as.name("safeDivide"))
      bquote(frac(.(exprToPlotmathExpr(expl[[2]])), .(exprToPlotmathExpr(expl[[3]]))))
    else if (func == as.name("`*`"))
      bquote(.(exprToPlotmathExpr(expl[[2]])) %.% .(exprToPlotmathExpr(expl[[3]])))
    else if (func == as.name("sqrt") || func == as.name("safeSqroot"))
      bquote(sqrt(.(exprToPlotmathExpr(expl[[2]]))))
    else if (func == as.name("loge") || func == as.name("safeLogn"))
      bquote(ln(.(exprToPlotmathExpr(expl[[2]]))))
    else
      as.call(c(func, Map(exprToPlotmathExpr, rest(expl))))
  } else expr

