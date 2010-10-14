\name{listSplittingAndGrouping}
\alias{splitList}
\title{Splitting and grouping of lists...}
\usage{splitList(l, groupAssignment)
groupListConsecutive(l, numberOfGroups)
groupListDistributed(l, numberOfGroups)
}
\description{Splitting and grouping of lists}
\details{\code{splitList}: Functions for splitting and grouping lists into sublists.
\code{splitList} splits a list \code{l} into \code{max(groupAssignment)} groups.
The integer indices of \code{groupAssignment} determine in which group each
element of \code{l} goes.
\code{groupListConsecutive} splits \code{l} into \code{numberOfGroups} consecutive
sublists (or groups).
\code{groupListDistributed} distributes \code{l} into \code{numberOfGroups}
sublists (or groups).

}
\value{\code{splitList}: A list of lists, where each member represents a group.

}
\arguments{\item{l}{A list.}
\item{groupAssignment}{A vector of group assignment indices.}
\item{numberOfGroups}{The number of groups to create, must be <= length(l)}
}
\alias{groupListConsecutive}
\alias{groupListDistributed}

