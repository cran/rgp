\name{buildingBlockTags}
\alias{buildingBlockTag}
\title{Building block tags...}
\usage{buildingBlockTag(x)
`buildingBlockTag<-`(x, value)
hasBuildingBlockTag(x)
}
\description{Building block tags}
\details{\code{buildingBlockTag}: To implement buidling blocks, i.e. subexpression protected from variation,
expression nodes may be tagged with \code{buildingBlockTags}. TODO

}
\arguments{\item{x}{An expression node.}
\item{value}{The value of the building block tag. Must be a numerical
in the interval [0.0 1.0].}
}
\alias{buildingBlockTag<-}
\alias{hasBuildingBlockTag}

