\name{kyphosis.text}
\alias{kyphosis.text}
\title{ Print explanatory text concerning kyphosis variables.}
\description{
	Given a variable name this function prints out explanatory text in HTML
}
\usage{
kyphosis.text(n, file = "", append = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{ A \code{kyphosis} variable. Could be "Start", "Age", "Number"}
  \item{file}{ File to write HTML to}
  \item{append}{ Should be TRUE}
}
\details{
	As Description. This is an example function of a possible argument to the \code{textfn}
    argument of \code{\link{pinktoe}}
}
\value{No value is returned
}
\references{ 
\url{http://www.stats.bris.ac.uk/~magpn/Research/Pinktoe/Welcome.html}
}
\author{Guy P Nason }
\note{ Not intended for general user use}

\seealso{ \code{\link{pinktoe}}}
\examples{
#
# Here is a simple example
#
kyphosis.text("Start")
#<BR>
#Variable:  Start : The beginning of the range of vertabrae involved<BR>
}
\keyword{ datagen }% at least one, from doc/KEYWORDS
