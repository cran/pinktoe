\name{PTTclQ}
\alias{PTTclQ}
\title{ Traverse tree structure calling appropriate GUI widgets}
\description{
	PTTclQ takes a tree and a row number (indexing a node).
	If that row describes a leaf then the function returns the
	yval for that leaf. If the row describes a decision node
	the function calls a GUI Tcl/Tk widget to ask the user
	what decision is necessary for that node. The result of
	the decision is used to recursively call PTTclQ for the next
	node. 
}
\usage{
PTTclQ(rpart, row, textfn, tittext, stateprintfn)
}
\arguments{
  \item{rpart}{ \code{rpart} object that you wish to traverse }
  \item{row}{ \code{row} the node at which the current decision is to be made }
  \item{textfn}{ Function to provide text given a variable name }
  \item{tittext}{ Currently unused. Reserved for future use }
  \item{stateprintfn}{ Currently unused. Reserved for future use}
}
\details{
	Description says it all.
}
\value{This function is recursive. However, eventually a leaf must be
	reached and a yval returned back to the calling function. The
	yval is either a numeric value corresponding to a predicted
	value for the case in regression, or an indexed into a factor
	for a classification tree.
}
\references{  }
\author{Guy P Nason}
\note{ Not intended for general use }


\seealso{ \code{\link{PT}}}
\examples{
#
# Not intended for general user use
#
}
\keyword{ internal }% at least one, from doc/KEYWORDS
