\name{edmnum99}
\alias{edmnum99}
\docType{data}
\title{Complete list of EDM numbers for 1999} 
\description{
	A complete list of EDM numbers for 1999. This is also the second dimnames component of
	\code{\link{mpinc99}}.
}
\usage{data(edmnum99)}
\format{
	A vector of 1243 EDM numbers including amendments. The numbers are stored as character
	strings since some of them have letters in! E.g. "930A1" is an amendment of "930".
}
\source{Originally the UK Houses of Parliament, but either through Her Majesty's Stationary Office or
\code{edm.ais.co.uk}. 
}
\examples{
data(edmnum99)
#
# Let's just have a look at a few
#
edmnum99[11:20]
# [1] "11"   "12"   "12A1" "13"   "14"   "15"   "16"   "17"   "18"   "19"  
#
# Note that EDM12 has an amendment also, EDM12A1
#
}
\keyword{datasets}
