\name{kyphosistittext}
\alias{kyphosistittext}
\title{ An example of a tittext function for the pinktoe function with the kyphosis example.}
\description{
	Returns a character string in response to a variable. Serves as an example \code{tittext} function for the
	\code{\link{pinktoe}} function.
}
\usage{
kyphosistittext(label)
}
\arguments{
  \item{label}{ A variable label. Could be "Start", "Age", "Number".}
}
\details{
	This is an example function that could be supplied as the \code{tittext} argument for the \code{\link{pinktoe}}
	function. All this function does is return a particular character string in response to a text string
	argument (which is supplied by the calling function as the variable from the tree in question). 
	The returned character string is used as the title for the associated web page that gets produced for each
	HTML file produced by \code{\link{pinktoe}}.
}
\value{
	A character string containing the response text.
}
\references{ 
\url{http://www.stats.bris.ac.uk/~magpn/Research/Pinktoe/Welcome.html}
}
\author{Guy P Nason }
\note{ Not intended for general user use}

\seealso{ \code{\link{pinktoe}}}
\examples{
#
# Try it out with the variable name "Start"
#
kyphosistittext("Start")
#[1] "Variable: Start"
}
\keyword{ datagen }% at least one, from doc/KEYWORDS
