\name{writeperloption}
\alias{writeperloption}
\title{
  Generate part of the perl file.
}
\description{ If \code{childvar} is a leaf then call \code{stateprintfn} to issue perl code to call appropriate leaf functionality,
	alternatively issue perl code to call next HTML file in the tree.}
\usage{
writeperloption(childvar, childrow, childnodeid, treeobj, perlname, append = TRUE, htmldir, stateprintfn)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{childvar}{ Variable name of next item in tree, either "<leaf>" or actual variable name}
  \item{childrow}{ The row number into frame for next item.}
  \item{childnodeid}{ The node number (row name of row number in frame component)}
  \item{treeobj}{ The \code{rpart} or \code{tree} object that all this is about}
  \item{perlname}{ The name of the perl file that all this is being written to}
  \item{append}{ Should be TRUE} 
  \item{htmldir}{ The directory that the HTML files are getting written to (the perl file needs to know this to set up a call to HTML files}
  \item{stateprintfn}{A user-supplied function that decides what to do when
        supplied with the \code{yval} reached at the leaf of a tree. Some
        text can be output, or maybe a perl function call.  }

}
\details{
	As description.
}
\value{
	No value returned.
}
\references{ 
\url{http://www.stats.bris.ac.uk/~magpn/Research/Pinktoe/Welcome.html}
}
\author{Guy P Nason }
\note{ Not intended for general user use}

\seealso{ \code{\link{pinktoe}}}
\examples{
#
#Not intended for general user use
#
}
\keyword{ internal }% at least one, from doc/KEYWORDS
