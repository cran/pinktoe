\name{edmbigtext}
\alias{edmbigtext}
\docType{data}
\title{ Character vector containing all text necessary for the edm.text function} 
\description{
	The function edm.text uses this data set to extract text for
	various EDMs. The numbers of the EDMs stored in this vector are listed
	in the vector \code{\link{edmnumswehave}}
}
\usage{data(edmbigtext)}
\format{ A character vector containing 1729 elements.
}
\source{Originally the UK Houses of Parliament, but either through Her Majesty's Stationary Office or
\code{edm.ais.co.uk}. 
}
\examples{
data(edmbigtext)
#
# Using edm.text
#
edm.text("EDM939")
#<EM>USE  OF  PARLIAMENTARY  TIME  </EM>
#<P>
#That  this  House  welcomes  the  recommendation  by  the  
#Modernisation  Committee  to  timetable  business  so  that  the  
#last  stand  alone  votes  are  taken  at  10.00  
#p.m.  thereby  ending  the  practice  of  making  legislation  
#in  the  early  hours  of  the  morning;  notes  
#that  backbenchers  and  opposition  parties  are  to  be  
#given  control  over  how  the  time  for  debating  
#a  bill  is  used;  and  believes  that  this  
#will  make  for  better  scrutiny,  better  legislation  and  
#better  government.  <BR>
}
\keyword{datasets}
