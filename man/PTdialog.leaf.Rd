\name{PTdialog.leaf}
\alias{PTdialog.leaf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Display text and ok button in a Tcl/Tk widget }
\description{
	Merely displays some text and an ok button in a Tcl/Tk widget
	which disappears when OK is clicked.
}
\usage{
PTdialog.leaf(leaftext)
}
\arguments{
  \item{leaftext}{ Character string to be displayed }
}
\details{
	As description
}
\value{
  None
}
\references{  }
\author{Guy P Nason }
\note{ Not intended for general user use }

\seealso{ \code{\link{PT}}}
\examples{
#
# Not intended for general use.
#
# Could do:
#
library("tcltk")
\dontrun{PTdialog.leaf("Hello World")}
#
# which displays a box with text and ok button!
#
}
\keyword{ internal }% at least one, from doc/KEYWORDS
\keyword{ IO }% __ONLY ONE__ keyword per line
