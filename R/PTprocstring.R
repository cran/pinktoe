"PTprocstring" <-
function (s, vtype) 
{
#
# Convert R "splits" information (from rpart) into S compatible splits string
# (like in tree())
#
#
# First decide whether this is a factor variable
#
ls <- nchar(s)
if (vtype=="factor")	{
	#
	# Get rid of initial variable name and =
	#
	eqpos <- regexpr("=", s)
	if (eqpos == -1)	{
		cat("Factor label was: ", s, "\n")
		stop("Factor variable without equals sign in its label?!?")
		}
	else
		s <- paste(":", substr(s, start=eqpos+1, stop=ls), sep="")
	}

else	{
	#
	# Get rid of text before < or >
	#
	pos <- regexpr("<", s)
	if (pos== -1)	{
		pos <- regexpr(">", s)
		if (pos== -1)	{
	  	  cat("Factor label was: ", s, "\n")
		  stop("Numeric variable has no < nor >?")
		  }
		}
	s <- substr(s, start=pos, stop=ls)
	}	
s
}
