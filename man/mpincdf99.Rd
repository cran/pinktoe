\name{mpincdf99}
\alias{mpincdf99}
\docType{data}
\title{ EDM signing database for 1999 and party affiliation for MPs} 
\description{
	A dataframe. Each row represents an MP. The first entry in each row contains the party affiliation for that MP.
	Subsequent entries correspond to the factor SIGNED/NOT SIGNED indicating whether that MP did or did not sign the
	EDM described by that column. 
}
\usage{data(mpincdf99)}
\format{A dataframe containing 549 MPs (rows) and 1244 columns (one for party membership and the rest for EDMs).
}
\source{Nason, G.P. (2001) Early Day Motions: exploring backbench opinion during 1997-2000. \emph{Technical Report} 01:11, Department
 of Mathematics, University of Bristol.
}
\examples{
data(mpincdf99)
## maybe str(mpincdf99) ; plot(mpincdf99) ...
#
# Let's look at the first 5 MPs and the first 4 EDMs.
#
mpincdf99[1:5,1:5]
#                  party   EDM1 EDM2   EDM3   EDM4
#Abbott/Diane        Lab SIGNED  NOT SIGNED    NOT
#Adams/Irene         Lab    NOT  NOT    NOT SIGNED
#Ainger/Nick         Lab    NOT  NOT    NOT    NOT
#Ainsworth/Peter     Con    NOT  NOT    NOT    NOT
#Alexander/Douglas   Lab    NOT  NOT    NOT    NOT
#
# So, Diane signed EDM1 and EDM3 and not EDM2 and EDM4 etc....
}
\keyword{datasets}
