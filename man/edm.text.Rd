\name{edm.text}
\alias{edm.text}
\title{ Example of a textfn function for pinktoe/PT}
\description{
	An example textfn text function for the EDM data set
}
\usage{
edm.text(n, web = TRUE, file = "", append = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{ A EDM variable name, such as "EDM29" }
  \item{web}{ If TRUE then the output is catted and appended to an HTML file. If FALSE
	the function returns a single character string with the desired text (including
	newlines, etc)}
  \item{file}{ The name of the file to cat the text to (if web==TRUE) }
  \item{append}{ Usually should be TRUE (I know the default is FALSE! Flip) }
}
\details{
	This functions produces text (either in HTML format which gets catted to a file, or returned as 
	a single character string) in response to an EDM number. The text describes the EDM. The actual text
	is stored in the character vector \code{edmbigtext}, the numbers of EDMs that this character vector contains
	is given in the object \code{edmnumswehave}.
}
\value{ If web==TRUE then nothing is returned. Otherwise, a single string containing the EDM text is produced.
}
\references{ 
\url{http://www.stats.bris.ac.uk/~magpn/Research/Pinktoe/Welcome.html}
}
\author{Guy P Nason }
\note{ Not intended for general user use}

\seealso{ \code{\link{pinktoe}}}
\examples{
#
# Which EDMs do we have text for
#
data(edmnumswehave)
data(edmbigtext)
edmnumswehave
#[1]  29  52  89 110 125 142 175 297 346 391 445 476 493 516 657 770 939
#
#
# Ok. Lets look at the text for EDM29. We'll set web=TRUE to get HTML output
#
edm.text("EDM29", web=TRUE)
#<EM>RACE  RELATIONS  </EM>
#<P>
#That  this  House  accepts  the  fact  that  the  
#United  Kingdom  is  a  multi-racial  society;  reasserts  the  
#right  of  all  individuals  to  enjoy  equal  rights  
#irrespective  of  their  race,  colour  or  creed;  deplores  
#the  fact  that  racial  discrimination  continues  to  blight  
#the  lives  of  many  British  citizens,  and  many  
#other  ethnic  minorities  who  live  in  the  United  
#Kingdom;  and  urges  the  Home  Secretary  to  implement  
#the  recommendations  by  the  Commission  for  Racial  Equality  
#and  reform  the  Race  Relations  Act  1976.  <BR>
#
# Do the same thing but return as a character string
#
edm.text("EDM29", web=FALSE)
#[1] " RACE RELATIONS \n\n That this House accepts the fact that the\n United Kingdom is a multi-racial society; reasserts the\n right of all individuals to enjoy equal rights\n irrespective of their race, colour or creed; deplores\n the fact that racial discrimination continues to blight\n the lives of many British citizens, and many\n other ethnic minorities who live in the United\n Kingdom; and urges the Home Secretary to implement\n the recommendations by the Commission for Racial Equality\n and reform the Race Relations Act 1976.\n"
}
\keyword{ datagen }% at least one, from doc/KEYWORDS
