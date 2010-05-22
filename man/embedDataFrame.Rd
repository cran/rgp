\name{embedDataFrame}
\alias{embedDataFrame}
\title{Embed columns in a data frame...}
\usage{embedDataFrame(x, cols, dimension=1)
}
\description{Embed columns in a data frame}
\details{Embeds the columns named \code{cols} in the data frame \code{x} into a space of dimension
\code{dimension}.}
\value{The data frame, augmented with embedded columns, shortended by \code{dimension} rows.}
\arguments{\item{x}{The data frame containing the columns to embed.}
\item{cols}{A vector a list of the names of the columns to embed.}
\item{dimension}{The additional dimensions to generate when embedding.}
}

