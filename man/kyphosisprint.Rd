\name{kyphosisprint}
\alias{kyphosisprint}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Example stateprintfn for the pinktoe function arising from the kyphosis example}
\description{
	This function is an example of the type of function that could be supplied to the \code{stateprintfn} argument of the
	\code{\link{pinktoe}} function. Given a \code{yval} from \code{z.kyphosis} tree (such as "present" or "absent") it produces
	perl code to call an appropriate perl function (it actually calls the present or absent perl functions that are
	defined by the \code{requirelib} argument of \code{\link{pinktoe}} function
}
\usage{
kyphosisprint(yval, file = "", append = FALSE)
}
\arguments{
  \item{yval}{ A \code{z.kyphosis} yval. This is either "present" or "absent" }
  \item{file}{ Where to cat the output }
  \item{append}{ Should be TRUE }
}
\details{
	As Description
}
\value{No value returned }
\references{ 
\url{http://www.stats.bris.ac.uk/~magpn/Research/Pinktoe/Welcome.html}
}
\author{Guy P Nason }
\note{ Not intended for general user use}

\seealso{ \code{\link{pinktoe}}}
\examples{
#
# A quick example. The file here is specified as "" to cat to standard output
#
kyphosisprint("present")
# &present;
}
\keyword{ datagen }% at least one, from doc/KEYWORDS
