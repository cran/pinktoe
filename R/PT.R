"PT" <-
function(rpart, textfn=NULL)
{
require("rpart")
require("tcltk")
require("sfsmisc")
rpart$frame <- cbind(rpart$frame, splits.rpart(rpart))
ans <- PTTclQ(rpart,1, textfn=textfn)
ylev <- attributes(rpart)$ylevels
if (is.null(ylev))	{
	#
	# y variable is Not a factor
	#
	anstext <- paste("Predicted value is: ", ans, sep="")
	}
else	{
	#
	# y variable IS a factor
	#
	anstext <- paste("Predicted classification is: ", ylev[ans], sep="")
	}
PTdialog.leaf(anstext)

if (is.null(ylev))
	return(ans)
else
	return(ylev[ans])
}
