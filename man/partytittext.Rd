\name{partytittext}
\alias{partytittext}
\title{ Returns the variable label}
\description{
	For the EDM example the title text for each node is simply the EDM number which gets supplied as a character
	argument. So it just gets returned to be used as a title (for the eventual HTML page). Of course, one could modify this function
	to do something more complicated.
}
\usage{
partytittext(v)
}
\arguments{
  \item{v}{ A character string describing the variable under consideration, like "EDM29", etc}
}
\details{
	Just returns the variable name which is good enough for a title in this situation.
}
\value{
	Returns its argument, the variable name.
}
\references{ 
\url{http://www.stats.bris.ac.uk/~magpn/Research/Pinktoe/Welcome.html}
}
\author{Guy P Nason }
\note{ Not intended for general user use}

\seealso{ \code{\link{pinktoe}}}
\examples{
#
# Try it with the "EDM29" variable name
#
partytittext("EDM29")
# [1] "EDM29"
}
\keyword{ datagen }% at least one, from doc/KEYWORDS
