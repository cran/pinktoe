\name{edmnumswehave}
\alias{edmnumswehave}
\docType{data}
\title{ List of the EDM numbers that can be accessed by the edm.text function} 
\description{
	Only some of the EDMs listed in \code{edmnum99} are present in the \code{edmbigtext}
	vector which edm.text accesses. The ones that are present are listed in the
	edmnumswehave vector.
}
\usage{data(edmnumswehave)}
\format{An integer vector of 17 elements. Each one is an EDM number that can be accessed through edm.text
}
\source{Originally the UK Houses of Parliament, but either through Her Majesty's Stationary Office or
\code{edm.ais.co.uk}. 
}
\examples{
data(edmnumswehave)
#
# Not really worth an example
#
}
\keyword{datasets}
