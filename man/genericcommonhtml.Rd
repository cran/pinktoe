\name{genericcommonhtml}
\alias{genericcommonhtml}
\title{ An example of some common html to go at the end of HTML pages for Pinktoe}
\description{
	Title says it all. This function could be supplied to \code{\link{pinktoe}} as the \code{commonhtml} argument.
}
\usage{
genericcommonhtml(file, append)
}
\arguments{
  \item{file}{ The name of the HTML file to append the HTML to}
  \item{append}{ Should be TRUE }
}
\details{
	Title says it all, see example below.
}
\value{ No value returned.
}
\references{ 
\url{http://www.stats.bris.ac.uk/~magpn/Research/Pinktoe/Welcome.html}
}
\author{Guy P Nason }
\note{ Not intended for general user use}

\seealso{ \code{\link{pinktoe}}}
\examples{
#
# The "" makes the output go to standard output (ie on the console)
#
genericcommonhtml(file="", append=TRUE)
#<a href="http://www.stats.bris.ac.uk/~magpn">Return to <EM>Guy Nason's</EM> home page</a>
#
# Of course this is an example function which you can modify to say whatever you like!
}
\keyword{ misc }% at least one, from doc/KEYWORDS
