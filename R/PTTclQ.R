"PTTclQ" <-
function (rpart, row, textfn, tittext, stateprintfn) 
{

#
# If row is a leaf then print variable output
#
leftdec <- as.character(rpart$frame[, "splits.cutleft"][row])
rightdec <- as.character(rpart$frame[, "splits.cutright"][row])
#
# Else if row is a question then ask question
#
var <- rpart$frame$var[row]
if (var=="<leaf>")	{
	ans <- rpart$frame$yval[row]
	}
else	{
	numvar <- TRUE
	if (substr(leftdec, 1,1) == ":")
		numvar <- FALSE

	if (numvar==TRUE)	{
		qtext <- paste("Is ", var, leftdec, "?")
		qno <- qopts <- NULL
		}
	else	{
		qtext <- paste("For variable: ", var, "")
		xlevs <- attributes(rpart)$xlevels[[as.character(var)]]
	
		leftdec <- substr(leftdec, 2, nchar(leftdec))
		rightdec <- substr(rightdec, 2, nchar(rightdec))
		cats <- AsciiToInt(leftdec) - 96
		catsright <- AsciiToInt(rightdec) - 96
		qopts <- xlevs[cats[1]]
		qno <- xlevs[catsright[1]]
		if (length(cats) > 1)	{
		for (i in 2:length(cats))
			qopts <- paste(qopts, ", ", xlevs[cats[i]], sep="")
			}
		if (length(catsright) > 1)	{
		for (i in 2:length(catsright))
			qno <- paste(qno, ", ", xlevs[catsright[i]], sep="")
			}
		qopts <- paste(qopts, ".", sep="")
		qno <- paste(qno, ".", sep="")
	}
	if (!is.null(textfn))	{
		explaintext <- textfn(n=as.character(var), web=FALSE)
		}
	else
		explaintext <- NULL

	ans <- PTdialog.rpartq(qtext, qopts, qno, explaintext)
	if (!is.null(qopts))
		if (qopts==ans)
			ans <- "yes"
		else
			ans <- "no"
	nodeids <- as.integer(dimnames(rpart$frame)[[1]])
	node <- nodeids[row]
	if (ans == "yes")
		newnode <- 2*node+1
	else
		newnode <- 2*node
	newrow <- which(nodeids==newnode)
	ans <- PTTclQ(rpart, newrow, textfn, tittext, stateprintfn) 
	}
ans
}
