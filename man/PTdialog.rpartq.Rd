\name{PTdialog.rpartq}
\alias{PTdialog.rpartq}
\title{ Display a dialog box with text, radiobuttons and submit using Tcl/Tk and return submitted answer }
\description{
	Opens a Tcl/Tk GUI widget with some text, a two-button radio button and decision
	text and submit/reset buttons. Once submit is pressed the
	state of the radiobutton is returned.
}
\usage{
PTdialog.rpartq(questiontext, qopts = NULL, qno = NULL, explaintext = NULL)
}
\arguments{
  \item{questiontext}{ Character string containing question relating to radiobutton }
  \item{qopts}{ Character string containing text describing one option of the radiobutton }
  \item{qno}{ Character string containing text describing the other option of the radiobutton. }
  \item{explaintext}{ Character string containing (extended) text presented before the radiobutton }
}
\details{
	A simple Tcl/Tk widget display function that permits some text
	to be displayed, a question to be displayed and two answers
	with two sets of text strings containing the answer text.
}
\value{Returns the qopts or qno string (indicating the answer) if these
	are not NULL. Otherwise returns "yes" or "no". 
}
\references{ \url{http://www.stats.bris.ac.uk/~magpn/Research/Pinktoe/pinktoe.htm} }
\author{Guy P Nason}
\note{ Not intended for general user use }
\examples{
    library("tcltk")
\dontrun{
PTdialog.rpartq("Is it possible for a swallow to carry a coconut?", qopts="Yes, certainly", qno="No way", explaintext="A question from Monty Python's Holy Grail")}
}
\keyword{ internal }% at least one, from doc/KEYWORDS
\keyword{ IO }% __ONLY ONE__ keyword per line
