\name{makeEDMtree}
\alias{makeEDMtree}
\title{ Construct a tree for one of the examples showing how pinktoe works}
\description{
	Merely constructs a tree (\link{rpart}) object.
}
\usage{
makeEDMtree()
}
%- maybe also 'usage' for other objects documented here.
\details{
	Constructs a particular tree using a given data frame and a list of variables (the ones in
	\code{\link{edmnumswehave}}. When transferring code between S and R sometimes tree objects
	(in S) differ from their "equivalent" partners (\code{\link{rpart}} objects) in R. Hence it is
	useful to sometimes be able to recompute them for the particular package in question.
}
\value{ An object of class \code{rpart} which is the made tree.}
\references{ 
\url{http://www.stats.bris.ac.uk/~magpn/Research/Pinktoe/Welcome.html}
}
\author{Guy P Nason }
\note{ Useful for constructing an example}

\seealso{ \code{\link{pinktoe}}}
\examples{
#
# Build the tree after loading rpart library
#
library("rpart")
data("mpincdf99")
mpincdf99tree <- makeEDMtree()
}
\keyword{ datagen }% at least one, from doc/KEYWORDS
