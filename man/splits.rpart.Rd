\name{splits.rpart}
\alias{splits.rpart}
\title{Create splits data frame that seems to be missing from R rpart code }
\description{
	The tree function in S returns a tree class object which has
	frame as one of its components. This frame has two columns called
	splits.cutleft and splits.cutright which illustrate how a variable
	split was a achieved for a given node. The R rpart function
	documentation says that it does produce these splits component
	but I have been unable to find it!! This function recreates,
	as best I can, the splits components which pinktoe makes use of.
}
\usage{
splits.rpart(tree, print.it = FALSE)
}
\arguments{
  \item{tree}{ The rpart object that you wish to produce splits info for }
  \item{print.it}{ If TRUE then informative messages are printed out (useful for debugging)   }
}
\details{ Produces the (missing) splits components for the frame
	component of an rpart object.
}
\value{ Returns a data.frame with two columns and the same number of
	rows as the frame component of the input tree object.
	The first (second) column contains information about the
	left hand (right hand) edge from a node.
}
\references{ None. }
\author{Guy P Nason}
\note{ Of course, the splits component could be lurking somewhere else
	in the rpart object but I have been unable to find it!}

\seealso{ \code{\link{pinktoe}}, \code{\link{PT}}}
\examples{
    library("rpart")
    data(kyphosis)	% Attach kyphosis data set
    z.kyphosis <- rpart(kyphosis) % Make CART tree
#
#   Now check that there is no splits.cutleft or splits.cutright component
#   of the frame component as there is in S
#
    dimnames(z.kyphosis$frame)[[2]]
#
#[1] "var"        "n"          "wt"         "dev"        "yval"      
#[6] "complexity" "ncompete"   "nsurrogate" "yval2"     
#
# Nope! So let's use our function to make them
#
    splits.rpart(z.kyphosis)
#
#  splits.cutleft splits.cutright
#1          >=8.5           < 8.5
#2         >=14.5          < 14.5
#3                               
#4           < 55            >=55
#5                               
#6          >=111           < 111
#7                               
#8                               
#9                               
#
# The resultant object could be installed into the frame component
# (as happens in the early code lines of PT and pinktoe) by
#
# z.kyphosis$frame <- cbind(z.kyphosis$frame, splits.rpart(z.kyphosis))
}
\keyword{ iplot }% at least one, from doc/KEYWORDS
\keyword{ tree }% __ONLY ONE__ keyword per line
