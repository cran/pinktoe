\name{mpall3}
\alias{mpall3}
\docType{data}
\title{ Master database of MP information concerned with EDM example}
\description{
	A list containing several components. Each component is some kind of vector with each row of each vector
	containing information about a given MP. Not really used in the examples but useful for the author to have
	around.
}
\usage{data(mpall3)}
\format{
  The format is:
List of 7 components
 \$ mpn     : Factor w/ 670 levels containing the names of the MPs
 \$ mpcon   : Factor w/ 660 levels containing the constituencies of MPs
 \$ mpparty : Factor w/ 16 levels containing MPs' party affiliation.
 \$ mpregion: Factor w/ 9 levels containing the region containing the MPs' constituencies.
 \$ mpgender: Factor w/ 2 levels gender of MP.
 \$ mpage   : num [1:670] Age of MP at given date.
 \$ mpagecod: num [1:670] Age rangeof MP
}
\source{Nason, G.P. (2001) Early Day Motions: exploring backbench opinion during 1997-2000. \emph{Technical Report} 01:11, Department
 of Mathematics, University of Bristol.
}
\examples{
data(mpall3)
#
# The name of MP number 3
#
mpall3$mpn[3]
#
# This record corresponds to Irene Adams
#
#[1] Adams, Irene
#670 Levels: Abbott, Miss Diane Adams, Gerry# Adams, Irene ... Young, Rt Hon Sir George Bt
mpall3$mpcon[3]
#
# Her constituency is Paisley North
#
#[1] Paisley North
#660 Levels: Aberavon Aberdeen Central Aberdeen North ... York, City of
mpall3$mpparty[3]
#
# She is a Labour MP
#
#[1] Lab
#16 Levels: Con DCWM DU Ind LDem Lab Lib PC SDLP SF SNP SPK Scot Lab UKU ... WW
mpall3$mpregion[3]
#
# Her constituency is in the North West of England
#
#[1] NorthWest
#9 Levels: London Midland NI NorthEast NorthWest Scotland ... Wales
#
# She is Female
#
mpall3$mpgender[3]
#[1] F
#Levels: F M
mpall3$mpage[3]
#
# She was 53 at the census date
#
#[1] 53
mpall3$mpagecod[3]
#
# She was in her fifties
#
#[1] 5
}
\keyword{datasets}
