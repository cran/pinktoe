"genedmhtml" <-
function(treeobj, noderow, edmtextfn, edmtittext, cgibindir, htmldir, localdir,
	stateprintfn, requirelib, commonhtml)
{
	edmnames <- as.character(treeobj$frame[, 1])
	edmix <- as.numeric(dimnames(treeobj$frame)[[1]])
	edmlabel <- as.character(treeobj$frame[noderow, "var"])
	uniqedmlab <- paste(edmlabel, "_", as.character(edmix[noderow]), sep
		 = "")	#
#
#	Look at the splits to see whether this is for a numerical or
#	binary variable
#
	split.type <- treeobj$frame[, "splits"][noderow, 2]
	if(substring(split.type, 1, 1) != ":") {
		numvar <- TRUE
		numstring <- split.type
	}
	else {
		numvar <- FALSE
		numstring <- split.type
	}
	perlname <- writehtml(edmlabel = edmlabel, uniqedmlab = uniqedmlab, 
		edmtextfn = edmtextfn, edmtittext = edmtittext, cgibindir = 
		cgibindir, localdir = localdir, commonhtml = commonhtml, 
		numvar = numvar, numstring = numstring, treeobj = treeobj)
	perlname <- paste(localdir, perlname, sep = "")
	cat("Frame row number is ", noderow, "\n")
	cat("Node number is ", edmix[noderow], "\n")
	nodenum <- edmix[noderow]
	childleft <- 2 * nodenum
	childright <- childleft + 1
	childleftrow <- match(childleft, edmix)
	childrightrow <- match(childright, edmix)
	writeperl(childleftrow, childrightrow, treeobj, perlname, edmlabel, 
		htmldir, edmix, stateprintfn = stateprintfn, requirelib = 
		requirelib)
}
"genericcommonhtml" <-
function(file, append)
{
	cat("<a href=\"http://www.stats.bris.ac.uk/~magpn\">Return to <EM>Guy Nason's</EM> home page</a>\n",
		file = file, append = append)
}
"kyphosis.text" <-
function(n, file = "", append = FALSE)
{
	cat("<BR>\n", file = file, append = append)
	cat("Variable: ", n, ": ", file = file, append = append)
	if(n == "Start")
		cat("The beginning of the range of vertabrae involved<BR>\n", 
			file = file, append = append)
	else if(n == "Age")
		cat("The age of the child in months<BR>\n", file = file, 
			append = append)
	else if(n == "Number")
		cat("The number of vertebrae involved in the operation<BR>\n", 
			file = file, append = append)
	else cat("print \"Unknown variable<BR>\"\n", file = file, append = 
			append)
}
"kyphosisprint" <-
function(yval, file = "", append = FALSE)
{
	if(yval == "present")
		cat(" &present;\n", file = file, append = append)
	else if(yval == "absent")
		cat(" &absent;\n", file = file, append = append)
	else cat("print \"Unknown kyphosis state<BR>\"\n", file = file, append
			 = append)
}
"kyphosistittext" <-
function(label)
return(paste("Variable:", label))
"pinktoe" <-
function(treeobj, textfn, tittext, treeid = "", cgibindir = paste(
	"/~magpn/cgi-bin/", treeid, "/", sep = ""), htmldir = paste(
	"/home/magpn/public_html/Research/Politics/TREE/", treeid, "/", sep = 
	""), localdir = "Tree/", stateprintfn = partyprint, requirelib = 
	"../party.lib", commonhtml)
{
	nnodes <- nrow(treeobj$frame)
	for(i in 1:nnodes) {
		nlabel <- as.character(treeobj$frame[i, "var"])
		if(nlabel != "<leaf>")
			genedmhtml(treeobj, noderow = i, textfn, tittext, 
				cgibindir, htmldir, localdir, stateprintfn = 
				stateprintfn, requirelib = requirelib, 
				commonhtml)
	}
}
"writehtml" <-
function(edmlabel, uniqedmlab, edmtextfn, edmtittext, cgibindir, localdir, 
	commonhtml, numvar, numstring, treeobj)
{
#
#
#	Construct the filename for the HTML file (basically consists of the
#		"localdir", they can be put in the proper HTML directory
#		later, the "uniqedmlab" which consists of the EDM label and
#		the node number.
#
	fn <- paste(localdir, uniqedmlab, ".htm", sep = "")	#
#
#	Construct similar name for the perl files (although these are not
#	written to here they are referred to by the HTML files).
#
	perlname <- paste(uniqedmlab, ".pl", sep = "")	#
#
#	Start writing HTML:	first HEAD
#
	cat("<HTML>\n<HEAD>\n", file = fn, append = FALSE)	#
#
#		.. TITLE ..
#
	cat(paste("<TITLE>", edmtittext(edmlabel), ": ", edmlabel, 
		"</TITLE>\n", sep = ""), file = fn, append = TRUE)
	cat("</HEAD>\n<BODY>\n", file = fn, append = TRUE)	#
#
#		.. BODY ..
#
#		.. say that this is a FORM and refer to the perl script to be
#		executed...
#
	cat(paste("<FORM ACTION=\"", cgibindir, perlname, "\" METHOD=POST>\n", 
		sep = ""), file = fn, append = TRUE)
	cat("<HR>\n<P>\n", file = fn, append = TRUE)	#
#
#		Write out the label for this HTML script
#
#
#		Write out the text
#
	if(numvar == TRUE) {
		cat(paste("Is ", edmlabel, " ", numstring, "?\n", sep = ""), 
			file = fn, append = TRUE)
		edmtextfn(edmlabel, file = fn, append = TRUE)
	}
	else {
		cat(paste(edmlabel, ": ", sep = ""), file = fn, append = TRUE)
		cat("\n Select the following categories:", file = fn, append
			 = TRUE)
		levs <- attr(treeobj, "xlevels")[edmlabel][[1]]	#
#
#	Get rid of leading :
#
		numstring <- substring(numstring, 2, nchar(numstring))
		cats <- AsciiToInt(numstring) - 96
		for(i in 1:length(cats))
			cat(levs[cats[i]], " ", file = fn, append = TRUE)
		cat("<BR>\n", file = fn, append = TRUE)
		edmtextfn(edmlabel, file = fn, append = TRUE)
	}
	cat("<P>\n", file = fn, append = TRUE)
	cat("<STRONG>Check the box if this is true or you agree", 
		"</STRONG>\n", file = fn, append = TRUE)
	cat(paste("<INPUT TYPE=\"checkbox\" NAME=\"EDMQ\" VALUE=\"", edmlabel, 
		"\">\n<BR>\n", sep = ""), file = fn, append = TRUE)
	cat("<INPUT TYPE=\"submit\" VALUE=\"Next\">\n", file = fn, append = TRUE)
	cat("<INPUT TYPE=\"reset\" VALUE=\"Reset\">\n", file = fn, append = TRUE)
	cat("<P>\n", file = fn, append = TRUE)	#
#
#	Add extras, if you like, to go on all web pages
#
	commonhtml(file = fn, append = TRUE)
	cat("</FORM>\n</BODY>\n</HTML>\n", file = fn, append = TRUE)	#
#
#	End of script. Return the name of the perl script to save us
#	  constructing it again
#
	return(perlname)
}
"writeperl" <-
function(childleftrow, childrightrow, treeobj, perlname, edmlabel, htmldir, 
	edmix, stateprintfn, requirelib)
{
#
#
#	A large amount of the PERL processing output contained within this
#	PERL script was taken from the "Inputting data from a form or a link"
#	on p. 66 of "Perl and CGI for the World Wide Web", by Elizabeth Castro, 
#	published by Peachpit Press in 1999. ISBN 0-201-35358-X
#
#	Initial perl script commands
#
#
	cat("#!/usr/local/bin/perl\n\n", file = perlname, append = FALSE)
	cat("use Fcntl;\n", file = perlname, append = TRUE)
	cat("use File::Cat;\n", file = perlname, append = TRUE)
	cat("use POSIX qw(tmpnam);\n\n", file = perlname, append = TRUE)	#
#
#	Put here any libraries that you want included in all of the PERL
#	scripts. This file will be in a subdirectory so make the library
#	one directory up.
#
	rlstring <- paste("require '", requirelib, "';\n", sep = "")
	cat(rlstring, file = perlname, append = TRUE)	#
#
#	More Castro PERL language that processes the HTML request.
#
	cat("if ($ENV{'REQUEST_METHOD'} eq 'GET') {\n", file = perlname, 
		append = TRUE)
	cat("\t@pairs = split(/&/, $ENV{'QUERY_STRING'});\n", file = perlname, 
		append = TRUE)
	cat("} elsif ($ENV{'REQUEST_METHOD'} eq 'POST') {\n", file = perlname, 
		append = TRUE)
	cat("\tread (STDIN, $buffer, $ENV{'CONTENT_LENGTH'});\n", file = 
		perlname, append = TRUE)
	cat("\t@pairs = split(/&/, $buffer);\n", file = perlname, append = TRUE)
	cat("} else {\n", file = perlname, append = TRUE)
	cat("\tprint \"Content-type: text/html\\n\\n\";\n", file = perlname, 
		append = TRUE)
	cat("\tprint \"<P>Use Post or Get\";\n", file = perlname, append = TRUE)
	cat("}\n\n", file = perlname, append = TRUE)
	cat("foreach $pair (@pairs) {\n", file = perlname, append = TRUE)
	cat("\t($key, $value) = split (/=/, $pair);\n", file = perlname, 
		append = TRUE)
	cat("\t$key =~ tr/+/ /;\n", file = perlname, append = TRUE)
	cat("\t$key =~ s/%([a-fA-F0-9] [a-fA-F0-9])/pack(\"C\", hex($1))/eg;\n",
		file = perlname, append = TRUE)
	cat("\t$value =~ tr/+/ /;\n", file = perlname, append = TRUE)
	cat("\t$value =~ s/%([a-fA-F0-9] [a-fA-F0-9])/pack(\"C\", hex($1))/eg;\n",
		file = perlname, append = TRUE)
	cat("\t$value =~s/<!--(.|\\n)*-->//g;\n\n", file = perlname, append = 
		TRUE)
	cat("\tif ($formdata{$key}) {\n", file = perlname, append = TRUE)
	cat("\t\t$formdata{$key} .= \", $value\";\n", file = perlname, append
		 = TRUE)
	cat("\t} else {\n", file = perlname, append = TRUE)
	cat("\t\t$formdata{$key} = $value;\n", file = perlname, append = TRUE)
	cat("\t}\n", file = perlname, append = TRUE)
	cat("}\n", file = perlname, append = TRUE)
	cat("print \"Content-type: text/html\\n\\n\";\n", file = perlname, 
		append = TRUE)
	cat("@edms = split(/,/, $formdata{'EDMQ'});\n", file = perlname, 
		append = TRUE)
	cat("undef %isedms;\n", file = perlname, append = TRUE)
	cat("foreach (@edms) { $isedms{$_} = 1 }\n", file = perlname, append
		 = TRUE)
	cat("\n\n", file = perlname, append = TRUE)	#
#
#	End of Castro 
#
#	Now get the variable names associated with the left and right kids
#
	childleftvar <- as.character(treeobj$frame[childleftrow, "var"])
	childrightvar <- as.character(treeobj$frame[childrightrow, "var"])	#
#	And also the unique tree node indices
#
	childleftnodeid <- edmix[childleftrow]
	childrightnodeid <- edmix[childrightrow]	#
#
#	See whether the parent variable exists here or not.	
#
#		yes....
#
	cat(paste("if (exists $isedms{'", edmlabel, "'})\t{\n", sep = ""), 
		file = perlname, append = TRUE)
	writeperloption(childrightvar, childrightrow, childrightnodeid, 
		treeobj, perlname, append = TRUE, htmldir = htmldir, stateprintfn
		 = stateprintfn)	#
#
#		no....
#
	cat("}\n else\t{\n", file = perlname, append = TRUE)
	writeperloption(childleftvar, childleftrow, childleftnodeid, treeobj, 
		perlname, append = TRUE, htmldir = htmldir, stateprintfn = 
		stateprintfn)
	cat("}\n", file = perlname, append = TRUE)
}
"writeperloption" <-
function(childvar, childrow, childnodeid, treeobj, perlname, append = TRUE, 
	htmldir, stateprintfn)
{
	if(childvar == "<leaf>") {
		stateprintfn(treeobj$frame[childrow, "yval"], file = perlname, 
			append = append)
	}
	else {
		cat(paste("cat('", htmldir, childvar, "_", as.character(
			childnodeid), ".htm', \\*STDOUT)\n", sep = ""), file
			 = perlname, append = append)
		cat(paste("\tor die \"Can't cat ", htmldir, childvar, "_", 
			as.character(childnodeid), ".htm: $!\";\n", sep = ""), 
			file = perlname, append = TRUE)
	}
}
