\name{pv99tompall}
\alias{pv99tompall}
\docType{data}
\title{ Permutation vector from MP lists in 99 into the mpall3 database}
\description{
	In 1999 a selection of MPs signed Early Day Motions and hence turn up
	in databases that record which MPs signed in a given year. The lists 
	that record MPs signing records implicitly define an "index position"
	for those MPs in those records. To link these MPs to the global
	\code{\link{mpall3}} database this vector gives the index into
	\code{\link{mpall3}} from their implicity index position.
}
\usage{data(pv99tompall)}
\format{A permutation vector containing 549 integers each one identifying an MP
	and their position within the \code{\link{mpall3}} database.
}
\source{Nason, G.P. (2001) Early Day Motions: exploring backbench opinion during 1997-2000. \emph{Technical Report} 01:11, Department
 of Mathematics, University of Bristol.
}
\examples{
data(pv99tompall)
data(mpall3)
data(mpinc99)
#
# In the mpincdf object (which records EDM signings) Irene Adams is MP number 2. Let's check this
#
dimnames(mpinc99)[[1]][1:5]
#[1] "Abbott/Diane"      "Adams/Irene"       "Ainger/Nick"      
#[4] "Ainsworth/Peter"   "Alexander/Douglas"
#
# What number is she in the mpall3 database?
#
pv99tompall[2]
# [1] 3
#
# She is the third MP in this structure. (You see possibly number 1 or 2 in mpall3 did not sign anything
# in 1999.
#
# Let's check
#
mpall3$mpn[3]
#[1] Adams, Irene
#670 Levels: Abbott, Miss Diane Adams, Gerry# Adams, Irene ... Young, Rt Hon Sir George Bt
#
# Yep, that's her!
}
\keyword{datasets}
