\name{mpinc99}
\alias{mpinc99}
\docType{data}
\title{ EDM signing database for 1999} 
\description{
	A binary matrix containing a zero/one entry for each cell which represents whether or not a
	given MP signed a given Early Day Motion.
}
\usage{data(mpinc99)}
\format{There are 549 MPs and 1243 EDMs.
}
\source{Nason, G.P. (2001) Early Day Motions: exploring backbench opinion during 1997-2000. \emph{Technical Report} 01:11, Department
 of Mathematics, University of Bristol.
}
\examples{
data(mpinc99)
## maybe str(mpinc99) ; plot(mpinc99) ...
#
# Did Irene Adams sign EDM3 ?
#
mpinc99[2, 3]
# [1] 0
#
# No, she didn't!
}
\keyword{datasets}
