\name{partyprint}
\alias{partyprint}
\title{ Function suitable for use as the stateprintfn argument for pinktoe for the EDM example}
\description{
	In response to a variable (from the var component of the frame component of the relevant
	tree or \code{\link{rpart}} object this function issues perl commands which call functions
	in the partylib object (which can be supplied as a \code{requirelib} object for \code{\link{pinktoe}}
}
\usage{
partyprint(yval, file = "", append = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{yval}{ One of "Lab", "Con", "LDem", "UU", "PC", "SNP": parties in the British Political system }
  \item{file}{ perl file to write the output code to}
  \item{append}{ Should be TRUE}
}
\details{
	This file should produce perl code to do something when a yval is produced. A yval is one of the
	elements of the var column in the frame dataframe of a tree or \link{rpart} object. In this case
	the elements are all political parties indicated by the possibilities described in the \code{yval}
	argument to this function. Here perl code is generated which calls a function, one for each political
	party. See example below.
}
\value{ Nothing.  }
\references{ 
\url{http://www.stats.bris.ac.uk/~magpn/Research/Pinktoe/Welcome.html}
}
\author{Guy P Nason }

\seealso{ \code{\link{pinktoe}}}
\examples{
partyprint("Lab", file="", append=TRUE)
# &labour;
#
#
# So the perl function labour gets called.


}
\keyword{ datagen }% at least one, from doc/KEYWORDS
