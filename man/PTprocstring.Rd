\name{PTprocstring}
\alias{PTprocstring}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Converts splits information present in an rpart object to an S like format }
\description{
	Splits information (which can be obtained indirectly from path.rpart)
	in R comes in phrases like "Country=abc" (for factor variables)
	or "Weight<=6" (for numeric variables) needs to be converted into
	the format ":abc" and "<=6" to match the S-like format.
}
\usage{
PTprocstring(s, vtype)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{s}{ Splits information string from \code{rpart} (via \link{path.rpart}) }
  \item{vtype}{ Whether the variable is "factor" or not }
}
\details{
	If \code{vtype} is not a factor then the string is treated as if
	it were numeric.
}
\value{
	A splits string but in S format.
}
\references{ \code{\link{PT}}, \code{\link{pinktoe}} }
\author{Guy P Nason }
\note{ Not intended for general use}


\seealso{ \code{\link{PT}}, \code{\link{pinktoe}} }
\examples{
#
# Process string "Country=abc" for factor variable
#
    PTprocstring("Country=abc", "factor")
#
# [1] ":abc"
#
# Process string "Weight<=6" for numeric variable
#
    PTprocstring("Weight<=6", vtype="numeric")
#
# [1] "<=6"
#
}
\keyword{ internal }% at least one, from doc/KEYWORDS
\keyword{ manip }% __ONLY ONE__ keyword per line
