\name{PT}
\alias{PT}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Traverse rpart object using Tcl Tk GUI}
\description{ 
	Function uses Tcl/Tk widgets to enable a tree (or, more generally,
	an rpart object) to be traversed graphically. This is useful
	for complicated trees and/or where variable descriptions are
	long.
}
\usage{
PT(rpart, textfn = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rpart}{ An rpart object }
  \item{textfn}{ A function that converts a variable name into text } }

\details{
	Suppose one has a rpart (or tree in Splus) classification or
	regression tree. A common task is to take a new case or
	individual and use the tree to make a decision about the
	new case (or to put it another way to classify the class membership
	of the new case).

	One could use the tree plot to decide this and at each node make
	a decision and follow the edges of the tree to a leaf which contains
	the final classification value. However, with some trees there are
	two problems that PT is designed to overcome. The first is that
	some trees are huge. Plots of such trees are often incomprehensible.
	The second is that a description of the decision to be made at a
	node (or even every node) might require a significant amount of
	supportive text. It might be impractical to augment a tree plot
	with a large amount of text at each node as this would, again,
	render the tree plot incomprehensible.

	This routine presents, for each node, starting with the root, a
	little window (widget) which asks the user to take their new case and
	answer a question in relation to the new case. The software
	takes their answer and automatically works out which is the next
	node to visit and question to ask. Eventually, it reaches a leaf
	and displays the predicated classification (or predicted value,
	for regression) for the new case (and it also returns it to the
	calling function).

	The questions are answered by clicking the relevant radiobutton and
	then clicking submit.

	A feature of PT is that for each question asked an optional
	amount of supportive text can be supplied. This text appears in
	the window where the question is being asked. This feature is
	useful as it means that the full information about the question
	being asked can be displayed altogether without, say, having to
	make a separate and inconvenient reference to a separate document.

	The optional text is supplied through the textfn argument.
	This function should have one argument which can be any of the
	variable names in the tree (which, of course, are some of the
	original variables in the dataframe from which the tree was
	constructed. The appropriate text should be supplied as a single
	character string (although it can contain newline characters, etc).
	The textfn function should also have the argument web such that
	when this argument is FALSE a single string of text is produced
	(for this function). [When the web argument is TRUE then this is
	designed for use with the pinktoe function and it would be
	desirable for textfn to produce the same text but printed out
	(to a specified file) in HTML format.]
}
\value{The classification (or predicted value) for the new case
whose characteristics that one specifies through answering the questions.
}
\references{ www.stats.bris.ac.uk/~magpn/Research/Pinktoe/pinktoe.htm }
\author{ Guy P Nason}
\note{ This package contains another function pinktoe which produces a similar
system but in terms of a CGI-HTML web based system (which is harder to use but
produces code that can be accessed by any web browser by any one)}

\seealso{ \code{\link{pinktoe}}}
\examples{
#
# Load rpart library (needed for tree building and execution of PT
#
library("rpart")
library("tcltk")
data("mpincdf99")
#
# Use supplied function to create a suitable example.
#
z.edm <- makeEDMtree()
#
# Plot tree to look at the basic structure
#
plot(z.edm)
text(z.edm)
#
# Now use PT to graphically traverse tree
#
data("edmbigtext")
\dontrun{ PT(z.edm, textfn=edm.text)}
#
# Successive widgets appear. You can answer each one using the
# default options if you like (the final classification will be
# "Conservative". 
#
# If you have more time then notice that the order of questioning
# and your answers follow the edge structure of the tree.
#
# Notice the large amount of text supplied with each question (variable)
# This is achieved by the edm.text function.
#
}
\keyword{ iplot }
\keyword{ tree }
