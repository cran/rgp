\name{extractAttributes}
\alias{extractAttributes}
\title{Extract a given attribute of all objects in a list and tag that list with the
list of extracted attributes}
\usage{
extractAttributes(x, extractAttribute, tagAttribute = extractAttribute,
  default = NULL)
}
\arguments{
  \item{x}{A list with objects containing the attribute
  \code{attribute}.}

  \item{extractAttribute}{The attribute to extract from all
  objects in the list \code{x}.}

  \item{tagAttribute}{The name of the attribute for
  \code{x} holding the list of extracted attributes.}

  \item{default}{A default value to return if an object in
  \code{x} has no attribute \code{attribute}.}
}
\value{
The list \code{x}, tagged with a new attribute
\code{tagAttribute}.
}
\description{
Extract a given attribute of all objects in a list and tag
that list with the list of extracted attributes
}

