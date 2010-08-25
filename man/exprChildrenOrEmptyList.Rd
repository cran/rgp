\name{exprChildrenOrEmptyList}
\alias{exprChildrenOrEmptyList}
\title{Return the Children of an Expression or the Empty List if there are None...}
\usage{exprChildrenOrEmptyList(expr)
}
\description{Return the Children of an Expression or the Empty List if there are None}
\details{Internal tool function that returns the children expressions of an R expression or the
empty list if there are no children, i.e. if the expression is atomic or \code{NULL}.
If the expression is a "function" expression, i.e. an expression that would evaluate
to a function, \code{exprChildrenOrEmptyList} will return the function body expression
as the only child.}
\value{The expression's children as a list, or the empty list if there are none.}
\arguments{\item{expr}{The expression to return the children for.}
}

